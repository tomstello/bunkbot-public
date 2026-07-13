# ext_topic_veracity.R -------------------------------------------------------------------
# Fig S10: VERACITY of persuasion claims BY TOPIC CLUSTER and arm.
#
# MOTIVATION
#   The "truthful ammunition" framing predicts that how factual a conversation's
#   arguments are should depend jointly on (a) the persuasion direction (debunking
#   leans on accurate evidence; bunking must often manufacture support) and (b) the
#   conspiracy TOPIC (some topics have abundant genuinely-suspicious real-world
#   evidence, others do not). This module crosses the EXPLORATORY pooled topic
#   taxonomy (embedding -> PCA -> HDBSCAN, identical to ext_topic_contrasts.R) with
#   the per-conversation aligned-claim veracity (cached API fact-checks) to give a
#   per-topic x arm mean veracity WITH confidence intervals -- a pure all_numbers
#   reader for Fig S10.
#
# ENTRY POINT: compute_topic_veracity(core_objects) -> tibble (canonical 17-col schema).
#
# DESIGN
#   * Corpus + taxonomy: built EXACTLY as ext_topic_contrasts.R does
#       (.topic_contrast_corpus(core_objects) then run_topic_clustering on the cached
#        embeddings); tc$corpus has response_id, condition (Bunking/Debunking), topic.
#   * Per-conversation aligned veracity: read_claim_labels(co$paths) -> keep
#       direction-ALIGNED claims (aligned_flag), CLIP veracity_score to [0,100]
#       (this drops the single out-of-range 875 by capping; it sits well above any
#        real 0-100 score so capping == effectively removing its leverage), then
#       mean per conversation_id -> conv_avg_veracity. Aligned-only because the
#       veracity construct is "how factual are the arguments deployed FOR the
#       assigned side", matching the S1-3/S4 aligned-veracity tables.
#   * inner_join tc$corpus (response_id, condition, topic) to the conversation
#       veracity (by response_id == conversation_id). One row per conversation.
#   * BOTH SAMPLES: "all_conversations" (every fact-checked conversation in the
#       corpus) and "compliant" (restrict to the compliant id set = S1-3 compliant
#       response_id + Study-4 s4_compliant ResponseId).
#   * For each sample, per topic x condition: estimate = mean conv_avg_veracity, se,
#       95% t-CI, n = #conversations. model = topic name; direction = arm.
#   * Topics with n < MIN_TOPIC_N (in a given sample, EITHER arm short) are collapsed
#       into "Mixed / Unclassified" so every reported cell is adequately powered.
#
#   section = "Topic"; block = "topic_veracity"; sample in
#     {all_conversations, compliant}.
#
# NOTE / FRAMING
#   The taxonomy is exploratory / not pre-registered; that caveat (plus the minPts /
#   retained-cluster hyperparameters and the veracity-clip) rides in `note`.

.vt_MIN_TOPIC_N <- 15L
.vt_MIXED <- "Mixed / Unclassified"

# direction-aligned per-conversation mean veracity, clipped to [0,100].
.vt_conv_veracity <- function(labels) {
  labels |>
    dplyr::mutate(
      .is_aligned = aligned_flag(stance_to_focal, directness_to_focal, direction),
      .v = pmin(pmax(suppressWarnings(as.numeric(veracity_score)), 0), 100)  # clip; caps the 875
    ) |>
    dplyr::filter(.is_aligned, !is.na(.v)) |>
    dplyr::group_by(conversation_id) |>
    dplyr::summarise(conv_avg_veracity = mean(.v), n_aligned = dplyr::n(), .groups = "drop")
}

# compliant id set across all 4 studies (S1-3 compliant response_id + S4 compliant ResponseId).
.vt_compliant_ids <- function(core_objects) {
  s13 <- core_objects$s13
  s13_ids <- if ("compliant" %in% names(s13)) {
    as.character(s13$response_id[!is.na(s13$compliant) & s13$compliant])
  } else if (all(c("evaluator_label", "reverse_evaluator_label") %in% names(s13))) {
    as.character(s13$response_id[s13$evaluator_label == 1 & s13$reverse_evaluator_label == 1])
  } else character(0)
  s4_ids <- as.character(core_objects$s4$s4_compliant$ResponseId)
  unique(c(s13_ids, s4_ids))
}

# one sample: collapse sparse topics, then per topic x arm cell mean + t-CI.
.vt_cells <- function(dat, note_base) {
  # collapse topics that are sparse in EITHER arm to Mixed.
  keep <- dat |>
    dplyr::count(topic, condition) |>
    tidyr::complete(topic, condition, fill = list(n = 0L)) |>
    dplyr::group_by(topic) |>
    dplyr::summarise(min_cell = min(n), .groups = "drop") |>
    dplyr::filter(topic != .vt_MIXED, min_cell >= .vt_MIN_TOPIC_N) |>
    dplyr::pull(topic)

  dat <- dat |>
    dplyr::mutate(topic2 = ifelse(topic %in% keep, topic, .vt_MIXED))

  dat |>
    dplyr::group_by(topic2, condition) |>
    dplyr::summarise(
      n = dplyr::n(),
      m = mean(conv_avg_veracity),
      se = stats::sd(conv_avg_veracity) / sqrt(dplyr::n()),
      .groups = "drop"
    ) |>
    dplyr::filter(n >= 3) |>
    dplyr::transmute(
      model = topic2,
      direction = as.character(condition),
      term = "topic_mean_veracity",
      n = n,
      estimate = m,
      se = se,
      conf_low = m - stats::qt(.975, pmax(n - 1, 1)) * se,
      conf_high = m + stats::qt(.975, pmax(n - 1, 1)) * se,
      statistic = m / se,
      note = note_base
    )
}

compute_topic_veracity <- function(core_objects) {
  si_require(c("dplyr", "tidyr", "tibble"))

  paths <- pkg_paths(core_objects$pkg_root)

  # taxonomy (same construction as ext_topic_contrasts.R)
  corpus <- .topic_contrast_corpus(core_objects)
  tc <- run_topic_clustering(corpus, paths$embed_cache)
  topics <- tc$corpus |>
    dplyr::transmute(response_id = as.character(response_id),
                     condition = as.character(condition),
                     topic = as.character(topic)) |>
    dplyr::filter(condition %in% c("Bunking", "Debunking"))

  # per-conversation aligned veracity
  labels <- read_claim_labels(paths)
  convv <- .vt_conv_veracity(labels) |>
    dplyr::mutate(conversation_id = as.character(conversation_id))

  joined <- dplyr::inner_join(topics, convv,
                              by = c("response_id" = "conversation_id"))

  hyper_note <- sprintf(
    "Direction-aligned per-conversation mean claim veracity (0-100, clipped; the single 875 score is capped). EXPLORATORY embedding->PCA->HDBSCAN taxonomy pooled across all 4 studies (minPts=%s; retained_clusters=%s); not pre-registered. Topics with <%d conversations in either arm collapsed to '%s'.",
    tc$best$minPts, tc$best$retained_clusters, .vt_MIN_TOPIC_N, .vt_MIXED
  )

  comp_ids <- .vt_compliant_ids(core_objects)

  samples <- list(
    all_conversations = joined,
    compliant = dplyr::filter(joined, response_id %in% comp_ids)
  )

  out <- list()
  for (samp in names(samples)) {
    d0 <- samples[[samp]]
    if (!nrow(d0)) next
    cells <- .vt_cells(d0, hyper_note)
    if (nrow(cells)) out[[length(out) + 1]] <- std_row(cells, "Topic", "topic_veracity", samp)
  }

  if (!length(out)) {
    return(std_row(tibble::tibble(model = character()), "Topic", "topic_veracity", "all_conversations"))
  }
  dplyr::bind_rows(out)
}
