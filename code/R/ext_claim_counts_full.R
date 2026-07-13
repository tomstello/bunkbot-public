# ext_claim_counts_full.R ----------------------------------------------------------------
# ONE recompute module reporting the FULL claim-extraction census the fact-checking pipeline
# produced. The headline veracity table only shows substantive / aligned / fact-checked claims;
# this block additionally surfaces ALL extracted claims (where the raw extracted totals are
# cached) and breaks every count down BY STUDY (S1-S4), BY STUDY-4 MODEL, and BY CONDITION
# (bunk / debunk). Claim extraction was run for all four studies (full conversations recorded),
# so each study has a per-claim label set; only Study 4 additionally carries the pre-substantive
# raw extracted totals in its master dataset.
#
# ENTRY POINT:  compute_claim_counts_full(core_objects) -> tibble (canonical 17-col schema)
#
# DATA SOURCES (resolved via pkg_paths(core_objects$pkg_root)):
#   labels_s1s3 (claim_role_labels_s1s3): per-claim role labels for Study1 + Study3.
#   labels_s2s4 (claim_role_labels_s2s4): per-claim role labels for Study2 (GPT-4o)
#                                                        + Study4 (Claude/Gemini/GPT-5.2/Grok).
#       Each row = ONE extracted-and-checked claim, with:
#           request_status      "success" => the claim was successfully fact-checked.
#           veracity_score      0-100 fact-check score (present for success rows).
#           stance_to_focal     in {supports, opposes, neutral}.
#           directness_to_focal in {direct, indirect, background}.
#           study_source        Study1..Study4 ; model_pooled = the persuader model.
#           direction           bunk / debunk ; conversation_id = the conversation key.
#       NOTE: these per-claim files are the POST-extraction set (one row per extracted claim that
#       entered the fact-check queue). They therefore give total fact-checked + substantive +
#       aligned, but NOT the raw pre-substantive extracted total. The raw extracted total is only
#       available for Study 4 (see below).
#   s4_master (study4_master_analysis_dataset): per-conversation Study-4 claim census with
#       all_claims_total   (every extracted claim, before any substantive filtering),
#       queue_claims_total (the subset routed to the fact-check queue),
#       factcheck_scored_n (the subset that received a veracity score),
#       plus model_pooled / direction. Summing these gives Study-4 raw extracted totals by model
#       and by condition.
#
# OPERATIONAL DEFINITIONS (identical to bunkbot_helpers.R veracity layer):
#   * fact-checked claim = a per-claim label row with request_status == "success".
#   * SUBSTANTIVE  = stance in {supports, opposes} AND directness in {direct, indirect}.
#   * ALIGNED      = substantive AND arguing the ASSIGNED side (aligned_flag(): supports if bunk,
#                    opposes if debunk) -- the exact set used for the reported veracity measure.
#
# BLOCK: "claim_counts_full"  (section = "Veracity")
#   sample = "cached_api_claims"   for the per-claim-file census (S1-S4, by model, by condition).
#   sample = "s4_master_codebook"  for the Study-4 raw extracted totals (all / queue / scored).
#   model     = study label ("Study1".."Study4", "(all)") OR a Study-4 model name.
#   direction = "bunk"/"debunk" for condition rows, NA for the study/model-level totals.
#   term      = the metric (n_total_claims / n_factchecked / n_substantive / n_aligned and the
#               per-conversation means). estimate = the COUNT or MEAN; n = the denominator
#               (number of fact-checked claims, or number of conversations for the means).

.ccf_cols <- c("section","block","sample","outcome","model","direction","term","n",
               "estimate","se","conf_low","conf_high","statistic","df_num","df_den","p_value","note")
.ccf_row <- function(...) {
  r <- list(...); for (c in setdiff(.ccf_cols, names(r))) r[[c]] <- NA
  tibble::as_tibble(r)[.ccf_cols]
}

# ---- a COUNT row: estimate = the count; n = the denominator it is drawn from -----------
.ccf_count <- function(k, n, term, def, sample, model = NA, direction = NA, outcome = NA) {
  .ccf_row(section = "Veracity", block = "claim_counts_full", sample = sample,
           outcome = outcome, model = model, direction = direction, term = term,
           n = n, estimate = as.numeric(k), note = def)
}
# ---- a per-conversation MEAN row: estimate = mean per conversation, with normal-approx CI --
.ccf_mean <- function(x, term, def, sample, model = NA, direction = NA, outcome = NA) {
  x <- x[!is.na(x)]; nc <- length(x)
  m  <- if (nc > 0) mean(x) else NA_real_
  s  <- if (nc > 1) stats::sd(x) else NA_real_
  se <- if (nc > 1) s / sqrt(nc) else NA_real_
  crit <- if (nc > 1) stats::qt(.975, nc - 1) else NA_real_
  .ccf_row(section = "Veracity", block = "claim_counts_full", sample = sample,
           outcome = outcome, model = model, direction = direction, term = term,
           n = nc, estimate = m, se = se,
           conf_low  = if (!is.na(se)) m - crit * se else NA_real_,
           conf_high = if (!is.na(se)) m + crit * se else NA_real_,
           note = def)
}

# Read + harmonize the per-claim label files for all four studies.
.ccf_read_labels <- function(paths) {
  ctypes <- readr::cols_only(
    study_source = readr::col_character(), conversation_id = readr::col_character(),
    direction = readr::col_character(), model_pooled = readr::col_character(),
    veracity_score = readr::col_double(), stance_to_focal = readr::col_character(),
    directness_to_focal = readr::col_character(), request_status = readr::col_character()
  )
  dplyr::bind_rows(
    readr::read_csv(paths$labels_s1s3, col_types = ctypes, progress = FALSE),
    readr::read_csv(paths$labels_s2s4, col_types = ctypes, progress = FALSE)
  ) |>
    dplyr::filter(request_status == "success") |>
    dplyr::mutate(
      study      = study_source,
      substantive = stance_to_focal %in% c("supports", "opposes") &
                    directness_to_focal %in% c("direct", "indirect"),
      aligned     = aligned_flag(stance_to_focal, directness_to_focal, direction)
    )
}

# Emit the count + per-conversation-mean bundle for one slice of the per-claim label set.
.ccf_label_bundle <- function(d, model, direction = NA) {
  nfc  <- nrow(d)
  nsub <- sum(d$substantive)
  naln <- sum(d$aligned)
  # per-conversation counts -> per-conversation means
  per_conv <- d |>
    dplyr::group_by(conversation_id) |>
    dplyr::summarise(fc = dplyr::n(),
                     sub = sum(substantive),
                     aln = sum(aligned), .groups = "drop")
  dplyr::bind_rows(
    .ccf_count(nfc, nfc, "n_factchecked",
               "Total successfully fact-checked extracted claims (one row per claim) in this slice.",
               "cached_api_claims", model = model, direction = direction),
    .ccf_count(nsub, nfc, "n_substantive",
               "Substantive claims: stance supports/opposes AND directness direct/indirect.",
               "cached_api_claims", model = model, direction = direction),
    .ccf_count(naln, nfc, "n_aligned",
               "Aligned claims: substantive AND arguing the assigned side (set used for reported veracity).",
               "cached_api_claims", model = model, direction = direction),
    .ccf_mean(per_conv$fc, "mean_factchecked_per_conv",
              "Mean fact-checked claims per conversation.",
              "cached_api_claims", model = model, direction = direction),
    .ccf_mean(per_conv$sub, "mean_substantive_per_conv",
              "Mean substantive claims per conversation.",
              "cached_api_claims", model = model, direction = direction),
    .ccf_mean(per_conv$aln, "mean_aligned_per_conv",
              "Mean aligned claims per conversation.",
              "cached_api_claims", model = model, direction = direction)
  )
}

# (A) Per-claim-file census: by study (S1-S4 + (all)), by Study-4 model, and by study x condition.
.ccf_label_rows <- function(paths) {
  lab <- .ccf_read_labels(paths)
  out <- list()

  # -- per study (model = study label) --
  for (st in sort(unique(lab$study))) {
    out[[length(out) + 1]] <- .ccf_label_bundle(dplyr::filter(lab, study == st), model = st)
  }
  # -- all studies pooled --
  out[[length(out) + 1]] <- .ccf_label_bundle(lab, model = "(all)")

  # -- by Study-4 model (the four pooled persuader models) --
  s4 <- dplyr::filter(lab, study == "Study4")
  for (mdl in sort(unique(s4$model_pooled))) {
    out[[length(out) + 1]] <- .ccf_label_bundle(dplyr::filter(s4, model_pooled == mdl), model = mdl)
  }

  # -- by study x condition (bunk / debunk) --
  for (st in sort(unique(lab$study))) {
    sd <- dplyr::filter(lab, study == st)
    for (dir in sort(unique(sd$direction))) {
      out[[length(out) + 1]] <- .ccf_label_bundle(
        dplyr::filter(sd, direction == dir), model = st, direction = dir)
    }
  }
  dplyr::bind_rows(out)
}

# (B) Study-4 RAW extracted totals from the master dataset: ALL extracted claims (pre-substantive),
#     the fact-check queue, and the scored subset -- overall, by model, and by condition.
.ccf_s4_raw_rows <- function(paths, core_objects) {
  m <- readr::read_csv(paths$s4_master, show_col_types = FALSE, progress = FALSE)
  # Restrict the S4 master claim census to the 1,272 analytic ResponseIds so the
  # counts match every other Study-4 estimate (the raw master spans the full
  # ~1,840-conversation production run, which over-counts excluded participants).
  strict_ids <- core_objects$s4$s4$ResponseId
  idcol <- intersect(c("conversation_id", "ResponseId"), names(m))[1]
  m <- dplyr::filter(m, .data[[idcol]] %in% strict_ids)
  metrics <- list(
    list(col = "all_claims_total",   term = "n_total_claims",
         def = "ALL extracted claims (before substantive filtering), summed across conversations."),
    list(col = "queue_claims_total", term = "n_queue_claims",
         def = "Extracted claims routed to the fact-check queue, summed across conversations."),
    list(col = "factcheck_scored_n", term = "n_factchecked",
         def = "Extracted claims that received a veracity score, summed across conversations.")
  )
  emit <- function(df, model, direction = NA) {
     nconv <- nrow(df)
    lapply(metrics, function(mt) {
      tot <- sum(df[[mt$col]], na.rm = TRUE)
      dplyr::bind_rows(
        .ccf_count(round(tot), round(tot), mt$term, mt$def,
                   "s4_master_codebook", model = model, direction = direction),
        .ccf_mean(df[[mt$col]],
                  paste0("mean_", sub("^n_", "", mt$term), "_per_conv"),
                  sprintf("Per-conversation mean of: %s", mt$def),
                  "s4_master_codebook", model = model, direction = direction)
      )
    }) |> dplyr::bind_rows()
  }
  out <- list()
  out[[length(out) + 1]] <- emit(m, model = "Study4")                 # overall Study 4
  for (mdl in sort(unique(m$model_pooled)))                            # by Study-4 model
    out[[length(out) + 1]] <- emit(dplyr::filter(m, model_pooled == mdl), model = mdl)
  for (dir in sort(unique(m$direction)))                              # by condition
    out[[length(out) + 1]] <- emit(dplyr::filter(m, direction == dir), model = "Study4", direction = dir)
  dplyr::bind_rows(out)
}

# (C) Studies 1-3 RAW extracted totals (ALL extracted claims, pre-substantive) from the
#     per-study claim-extraction files (paths$nfacts: S{1,2,3}_..._nfacts_veracity.jsonl).
#     Each row is one extracted atomic claim; request_status=="success" is a delivered
#     extraction. Reported overall (n_total_claims) and split by assigned condition via a
#     ResponseId -> direction join to the S1-3 analytic frame. This gives the "all extracted"
#     metric for Studies 1-3 that the s4_master codebook supplies for Study 4.
.ccf_s13_extracted_rows <- function(paths, core_objects) {
  if (is.null(paths$nfacts) || !all(file.exists(paths$nfacts))) return(NULL)
  s13 <- core_objects$s13
  dirmap <- setNames(as.character(s13$direction), s13$response_id)
  studymap <- c(Study1 = "Jailbroken", Study2 = "Standard", Study3 = "Truth-Constrained")
  out <- list()
  for (st in names(paths$nfacts)) {
    lines <- readLines(paths$nfacts[[st]], warn = FALSE)
    if (!length(lines)) next
    rid <- vapply(lines, function(L) {
      m <- regmatches(L, regexpr('"ResponseId"\\s*:\\s*"[^"]+"', L, perl = TRUE))
      if (!length(m)) return(NA_character_)
      sub('.*"ResponseId"\\s*:\\s*"', '', sub('"$', '', m[1]))
    }, character(1), USE.NAMES = FALSE)
    ok <- vapply(lines, function(L) grepl('"request_status"\\s*:\\s*"success"', L, perl = TRUE),
                 logical(1), USE.NAMES = FALSE)
    rid <- rid[ok]
    # Restrict the "all extracted" census to the ANALYTIC sample: the raw nfacts
    # files cover a few excluded-but-screened conversations (S1 28 / S2 21 / S3 19),
    # which must not bleed into the reported counts. (The by-condition split below is
    # already analytic-only via the s13 ResponseId->direction map.)
    rid <- rid[rid %in% s13$response_id]
    n_all <- length(rid)
    out[[length(out) + 1]] <- .ccf_count(
      n_all, n_all, "n_total_claims",
      "ALL extracted claims (analytic sample only; before substantive filtering), one row per extracted claim.",
      "cached_api_claims", model = st)
    # by condition
    dir <- unname(dirmap[rid])
    for (dd in c("bunk", "debunk")) {
      k <- sum(dir == dd, na.rm = TRUE)
      if (k > 0) out[[length(out) + 1]] <- .ccf_count(
        k, k, "n_total_claims",
        "ALL extracted claims (before substantive filtering) in this condition.",
        "cached_api_claims", model = st, direction = dd)
    }
  }
  dplyr::bind_rows(out)
}

# ----------------------------------------------------------------------------------------
# Entry point.
# ----------------------------------------------------------------------------------------
compute_claim_counts_full <- function(core_objects) {
  si_require(c("dplyr", "readr", "tibble"))
  paths <- pkg_paths(core_objects$pkg_root)
  dplyr::bind_rows(
    .ccf_label_rows(paths),
    .ccf_s13_extracted_rows(paths, core_objects),
    .ccf_s4_raw_rows(paths, core_objects)
  )[.ccf_cols]
}
