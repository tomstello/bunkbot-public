# ext_claim_prevalence.R -----------------------------------------------------------------
# ONE recompute module characterizing the PREVALENCE of the claim categories / roles the
# fact-checking pipeline produced. Purpose: let the SI *describe* what kinds of claims were
# checked and what share met each criterion, INSTEAD of dumping the full claim-category
# codebook. Everything is recomputed from cached per-claim label files + the S4 master claim
# dataset (NO CSV-of-numbers reads). Emitted in the canonical 17-column schema.
#
# ENTRY POINT:  compute_claim_prevalence(core_objects) -> tibble (canonical schema)
#
# DATA SOURCES (resolved via pkg_paths(core_objects$pkg_root)):
#   labels_s1s3 (claim_role_labels_s1s3) + labels_s2s4
#   (claim_role_labels_s2s4): one row per EXTRACTED-and-FACT-CHECKED claim across
#   all four studies, with a per-claim role triple
#       stance_to_focal     in {supports, opposes, neutral}
#       directness_to_focal in {direct, indirect, background}
#       veracity_score      (0-100 fact-check score; present for every successful row)
#   s4_master (study4_master_analysis_dataset): per-conversation claim CODEBOOK shares for the
#   six extraction categories (specific_empirical, general_underspecified, inferential_logical,
#   meta_epistemic, low_information, moral_political_evaluation), split into the *all*-extracted
#   pool and the fact-check *queue* (the subset actually sent to the checker).
#
# OPERATIONAL DEFINITIONS (matches the package's veracity layer, bunkbot_helpers.R):
#   * fact-checked claim = a successful per-claim label row (has a veracity_score).
#   * SUBSTANTIVE         = the claim takes a concrete position on the focal proposition:
#                           stance in {supports, opposes} AND directness in {direct, indirect}.
#                           (neutral stance OR background directness = non-substantive context.)
#   * supports / opposes  = among substantive, whether it argues FOR or AGAINST the proposition.
#   * direct / indirect   = among substantive, whether it asserts the focal proposition (direct)
#                           or supplies supporting evidence for it (indirect).
#   * ALIGNED             = substantive AND arguing the ASSIGNED side (supports if bunk, opposes
#                           if debunk) -- the exact set the package uses for reported veracity
#                           (aligned_flag() in bunkbot_helpers.R).
#
# block: "claim_prevalence"  (section = "Veracity")
#   sample = "cached_api_claims" for the 4-study role prevalence (model = study or "(all)").
#   sample = "s4_master_codebook" for the coarse 6-category codebook distribution.
#   term   = the metric; estimate = a SHARE (proportion) or a COUNT; n = the denominator;
#            note documents the definition. Coarse category rows additionally set outcome to
#            the claim pool ("all_extracted" vs "factcheck_queue").

.cp_cols <- c("section","block","sample","outcome","model","direction","term","n",
              "estimate","se","conf_low","conf_high","statistic","df_num","df_den","p_value","note")
.cp_row <- function(...) {
  r <- list(...); for (c in setdiff(.cp_cols, names(r))) r[[c]] <- NA
  tibble::as_tibble(r)[.cp_cols]
}

# Wilson 95% CI for a binomial proportion (so share rows carry honest uncertainty).
.cp_wilson <- function(k, n) {
  if (is.na(n) || n <= 0) return(c(NA_real_, NA_real_))
  p <- k / n; z <- 1.959964; z2 <- z * z
  den <- 1 + z2 / n
  ctr <- (p + z2 / (2 * n)) / den
  hw  <- z * sqrt(p * (1 - p) / n + z2 / (4 * n * n)) / den
  c(max(0, ctr - hw), min(1, ctr + hw))
}

# A canonical share row: estimate = k/n, with a Wilson CI; note carries k and the definition.
.cp_share <- function(k, n, term, def, sample, model = NA, direction = NA, outcome = NA) {
  ci <- .cp_wilson(k, n)
  .cp_row(section = "Veracity", block = "claim_prevalence", sample = sample,
          outcome = outcome, model = model, direction = direction, term = term,
          n = n, estimate = if (is.na(n) || n == 0) NA_real_ else k / n,
          conf_low = ci[1], conf_high = ci[2],
          note = sprintf("%s (k=%s of n=%s)", def, format(k, big.mark = ","), format(n, big.mark = ",")))
}
# A canonical count row: estimate = the count; n = the denominator it is drawn from.
.cp_count <- function(k, n, term, def, sample, model = NA, direction = NA, outcome = NA) {
  .cp_row(section = "Veracity", block = "claim_prevalence", sample = sample,
          outcome = outcome, model = model, direction = direction, term = term,
          n = n, estimate = as.numeric(k), note = def)
}

# ----------------------------------------------------------------------------------------
# (A) Four-study claim-ROLE prevalence from the cached per-claim label files.
# ----------------------------------------------------------------------------------------
.cp_role_rows <- function(paths) {
  ctypes <- readr::cols_only(
    study_source = readr::col_character(), conversation_id = readr::col_character(),
    direction = readr::col_character(), model_pooled = readr::col_character(),
    veracity_score = readr::col_double(), stance_to_focal = readr::col_character(),
    directness_to_focal = readr::col_character(), request_status = readr::col_character()
  )
  lab <- dplyr::bind_rows(
    readr::read_csv(paths$labels_s1s3, col_types = ctypes, progress = FALSE),
    readr::read_csv(paths$labels_s2s4, col_types = ctypes, progress = FALSE)
  ) |>
    dplyr::filter(request_status == "success") |>
    dplyr::mutate(
      study_label   = dplyr::recode(study_source,
                                    "Study1" = "Study1", "Study2" = "Study2",
                                    "Study3" = "Study3", "Study4" = "Study4"),
      has_stance    = stance_to_focal %in% c("supports", "opposes"),
      is_directional= directness_to_focal %in% c("direct", "indirect"),
      substantive   = has_stance & is_directional,
      aligned       = ((direction == "bunk"   & stance_to_focal == "supports") |
                       (direction == "debunk" & stance_to_focal == "opposes")) & is_directional
    )

  # Per-study + overall ("(all)") metric bundle.
  blocks <- c(setNames(as.list(sort(unique(lab$study_label))), sort(unique(lab$study_label))),
              list("(all)" = NA))
  out <- list()
  for (mdl in names(blocks)) {
    d <- if (identical(blocks[[mdl]], NA)) lab else dplyr::filter(lab, study_label == mdl)
    nfc   <- nrow(d)
    nsub  <- sum(d$substantive)
    sub   <- dplyr::filter(d, substantive)
    nsup  <- sum(sub$stance_to_focal == "supports")
    nopp  <- sum(sub$stance_to_focal == "opposes")
    ndir  <- sum(sub$directness_to_focal == "direct")
    nind  <- sum(sub$directness_to_focal == "indirect")
    naln  <- sum(d$aligned)

    out[[length(out) + 1]] <- dplyr::bind_rows(
      .cp_count(nfc, nfc, "n_factchecked_claims",
                "Total successfully fact-checked extracted claims (one row per claim).",
                "cached_api_claims", model = mdl),
      .cp_share(nsub, nfc, "share_substantive",
                "Share of fact-checked claims that are SUBSTANTIVE: stance supports/opposes AND directness direct/indirect (concretely checkable position on the focal proposition).",
                "cached_api_claims", model = mdl),
      .cp_count(nsub, nfc, "n_substantive_claims",
                "Count of substantive (position-taking, checkable) fact-checked claims.",
                "cached_api_claims", model = mdl),
      .cp_share(nsup, nsub, "share_substantive_supports",
                "Among substantive claims, share arguing FOR the focal proposition (stance=supports).",
                "cached_api_claims", model = mdl),
      .cp_share(nopp, nsub, "share_substantive_opposes",
                "Among substantive claims, share arguing AGAINST the focal proposition (stance=opposes).",
                "cached_api_claims", model = mdl),
      .cp_share(ndir, nsub, "share_substantive_direct",
                "Among substantive claims, share DIRECT (asserts the focal proposition itself).",
                "cached_api_claims", model = mdl),
      .cp_share(nind, nsub, "share_substantive_indirect",
                "Among substantive claims, share INDIRECT (supplies supporting evidence rather than asserting the proposition).",
                "cached_api_claims", model = mdl),
      .cp_share(naln, nfc, "share_aligned",
                "Share of fact-checked claims that are ALIGNED: substantive AND arguing the ASSIGNED side (supports if bunk, opposes if debunk) -- the set used for the reported veracity.",
                "cached_api_claims", model = mdl),
      .cp_count(naln, nfc, "n_aligned_claims",
                "Count of aligned (substantive + assigned-side) fact-checked claims; denominator for the package's veracity measure.",
                "cached_api_claims", model = mdl)
    )
  }
  dplyr::bind_rows(out)
}

# ----------------------------------------------------------------------------------------
# (B) Coarse claim-CATEGORY codebook distribution (S4 master) -- replaces the full codebook.
#     Six extraction categories, count-weighted shares across conversations, for BOTH the
#     full extracted pool ("all_extracted") and the fact-check queue ("factcheck_queue").
# ----------------------------------------------------------------------------------------
.cp_codebook_rows <- function(paths) {
  m <- readr::read_csv(paths$s4_master, show_col_types = FALSE, progress = FALSE)
  cats <- c("specific_empirical", "general_underspecified", "inferential_logical",
            "meta_epistemic", "low_information", "moral_political_evaluation")
  defs <- c(
    specific_empirical         = "Concrete, checkable empirical assertions (the SUBSTANTIVE category sent to fact-checking).",
    general_underspecified     = "Broad or vaguely specified empirical claims.",
    inferential_logical        = "Inferential / logical reasoning steps rather than standalone empirical assertions.",
    meta_epistemic             = "Meta / epistemic statements about evidence, sources, or knowing.",
    low_information            = "Low-information filler / conversational content.",
    moral_political_evaluation = "Moral or political value judgments rather than empirical claims."
  )
  pools <- list(all_extracted = list(tot = "all_claims_total",   pre = "all_share_"),
                factcheck_queue = list(tot = "queue_claims_total", pre = "queue_share_"))
  out <- list()
  for (pool in names(pools)) {
    totcol <- pools[[pool]]$tot; pre <- pools[[pool]]$pre
    w   <- m[[totcol]]; W <- sum(w, na.rm = TRUE)
    out[[length(out) + 1]] <- .cp_count(
      round(W), round(W), sprintf("n_claims_%s", pool),
      sprintf("Total claims in the %s pool (sum of per-conversation %s).",
              gsub("_", " ", pool), totcol),
      "s4_master_codebook", outcome = pool)
    for (cat in cats) {
      col <- paste0(pre, cat)
      share <- if (W > 0) sum(m[[col]] * w, na.rm = TRUE) / W else NA_real_
      out[[length(out) + 1]] <- .cp_row(
        section = "Veracity", block = "claim_prevalence", sample = "s4_master_codebook",
        outcome = pool, term = paste0("share_category_", cat), n = round(W),
        estimate = share,
        note = sprintf("Count-weighted share of %s claims in category '%s'. %s",
                       gsub("_", " ", pool), cat, defs[[cat]]))
    }
  }
  dplyr::bind_rows(out)
}

# ----------------------------------------------------------------------------------------
# Entry point.
# ----------------------------------------------------------------------------------------
compute_claim_prevalence <- function(core_objects) {
  si_require(c("dplyr", "readr", "tibble"))
  paths <- pkg_paths(core_objects$pkg_root)
  dplyr::bind_rows(
    .cp_role_rows(paths),
    .cp_codebook_rows(paths)
  )[.cp_cols]
}
