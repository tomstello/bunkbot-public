# ext_topic_contrasts.R
# =============================================================================
# Per-topic REGRESSION CONTRASTS (with confidence intervals) for the
# SI recompute engine.
#
# MOTIVATION
#   The existing `topic_effects` block (compute_topic_numbers in
#   R/tables_dynamic.R) reports only the raw per-topic x direction CELL MEANS of
#   direction-aligned belief change, with no inferential uncertainty. This module
#   adds proper REGRESSION CONTRASTS: a per-topic HC3 OLS bunk-vs-debunk
#   contrast plus per-topic per-condition means, all with CIs, so the
#   "ammunition" claims (e.g. Moon Landing near-unbunkable vs 9/11 highly
#   bunkable) carry confidence intervals.
#
# ENTRY POINT:  compute_topic_contrasts(core_objects) -> tibble in the canonical
#   17-col schema (section, block, sample, outcome, model, direction, term, n,
#   estimate, se, conf_low, conf_high, statistic, df_num, df_den, p_value, note).
#
# Requires bunkbot_helpers.R already sourced (run_topic_clustering, hc3_tidy,
# linear_combo, model_matrix_for_fit, pkg_paths) and R/tables_dynamic.R for
# std_cols/std_row. The pooled 4-study corpus is rebuilt EXACTLY as
# compute_topic_numbers() builds it (same embed_text construction, same study /
# condition / belief_change columns), then re-clustered with the SAME taxonomy
# (run_topic_clustering on the cached embeddings). No summary-CSV reads.
#
# DESIGN
#   Outcome = direction-aligned belief change (`belief_change` in the pooled
#   corpus: `change` for S1-3, `aligned_belief_change` for S4; higher = more
#   movement toward the assigned persuasion direction).
#
#   Sign convention matches the S1-3 family (term "bunk_minus_debunk_lin"):
#   the contrast is BUNKING minus DEBUNKING aligned change.
#
#   For each topic (excluding "Mixed / Unclassified") with adequate coverage
#   (n >= MIN_TOPIC_N in total AND both Bunking and Debunking present with
#   >= MIN_CELL_N each), fit an HC3 OLS:
#       belief_change ~ condition           (condition in {Debunking, Bunking})
#   and report
#     * term = "bunk_vs_debunk_contrast" (model = topic): the Bunking-minus-
#       Debunking slope, HC3 SE, CI, p; n = total topic n.
#     * term = "topic_mean_aligned_change" per direction (model = topic,
#       direction = Bunking/Debunking): the model-implied per-condition mean and
#       HC3 CI (identical to the raw cell mean here, but now WITH a CI).
#
#   "Mixed / Unclassified" is excluded from the contrast set (flagged in note).
#
#   section = "Topic"; block = "topic_contrasts"; sample = "cached_embeddings".
#
# NOTE / FRAMING
#   This embedding -> PCA -> HDBSCAN taxonomy is EXPLORATORY (data-driven topic
#   discovery pooled across all four studies); the per-topic contrasts are not
#   pre-registered. That caveat is carried in the `note` column on every row,
#   alongside the clustering hyperparameters (minPts / retained_clusters).
# =============================================================================

# Coverage thresholds (n >= 40 and both conditions present).
.TOPIC_CONTRAST_MIN_N <- 40L
.TOPIC_CONTRAST_MIN_CELL_N <- 10L

# Rebuild the SAME pooled 4-study corpus compute_topic_numbers() constructs.
.topic_contrast_corpus <- function(core_objects) {
  s13 <- core_objects$s13
  d <- core_objects$s4

  mk_text <- function(a, b) dplyr::case_when(
    !is.na(a) & nchar(trimws(a)) > 5 ~ trimws(a),
    !is.na(b) & nchar(trimws(b)) > 5 ~ trimws(b),
    TRUE ~ NA_character_
  )

  comp4 <- as.character(core_objects$s4$s4_compliant$ResponseId)

  dplyr::bind_rows(
    s13 |>
      dplyr::transmute(
        response_id,
        study = paste0("S", as.integer(study_factor)),
        variant = as.character(study_factor),
        condition = as.character(condition_factor),
        belief_change = change,
        compliant = compliant %in% c(TRUE, 1, "TRUE"),
        embed_text = mk_text(con_restatement, con_summary)
      ),
    d$s4 |>
      dplyr::transmute(
        response_id = ResponseId,
        study = "S4",
        variant = as.character(model_pooled),
        condition = ifelse(direction == "bunk", "Bunking", "Debunking"),
        belief_change = aligned_belief_change,
        compliant = as.character(ResponseId) %in% comp4,
        embed_text = mk_text(conRestatement, conSummary)
      )
  ) |>
    dplyr::filter(!is.na(embed_text))
}

# Per-topic contrasts + per-condition means for one (already clustered, filtered) frame.
.topic_contrast_one_sample <- function(cls, hyper_note, sample_label) {
  topics <- sort(unique(cls$topic[cls$topic != "Mixed / Unclassified"]))
  contrast_rows <- list(); mean_rows <- list()
  for (tp in topics) {
    dat <- cls |> dplyr::filter(topic == tp)
    n_tot <- nrow(dat)
    n_bunk <- sum(dat$condition == "Bunking")
    n_debunk <- sum(dat$condition == "Debunking")
    if (!(n_tot >= .TOPIC_CONTRAST_MIN_N &&
          n_bunk >= .TOPIC_CONTRAST_MIN_CELL_N &&
          n_debunk >= .TOPIC_CONTRAST_MIN_CELL_N)) next

    dat <- dat |>
      dplyr::mutate(condition = factor(condition, levels = c("Debunking", "Bunking")))
    fit <- stats::lm(belief_change ~ condition, data = dat)
    vc <- sandwich::vcovHC(fit, type = "HC3")
    td <- hc3_tidy(fit)
    crow <- td[td$term == "conditionBunking", , drop = FALSE]

    contrast_rows[[length(contrast_rows) + 1]] <- tibble::tibble(
      model = tp, term = "bunk_vs_debunk_contrast", n = n_tot,
      estimate = crow$estimate, se = crow$std.error,
      conf_low = crow$conf.low, conf_high = crow$conf.high,
      statistic = crow$statistic, p_value = crow$p.value,
      note = sprintf("Bunking minus Debunking aligned change; n_bunk=%d, n_debunk=%d. %s",
                     n_bunk, n_debunk, hyper_note))

    for (cl in c("Bunking", "Debunking")) {
      nd <- dat
      nd$condition <- factor(cl, levels = levels(dat$condition))
      lin <- linear_combo(fit, colMeans(model_matrix_for_fit(fit, nd)), vc = vc)
      n_cell <- if (cl == "Bunking") n_bunk else n_debunk
      mean_rows[[length(mean_rows) + 1]] <- tibble::tibble(
        model = tp, direction = cl, term = "topic_mean_aligned_change", n = n_cell,
        estimate = lin$estimate, se = lin$std.error,
        conf_low = lin$conf.low, conf_high = lin$conf.high,
        statistic = lin$estimate / lin$std.error, p_value = lin$p.value,
        note = sprintf("Model-implied per-condition mean aligned belief change. %s", hyper_note))
    }
  }
  out <- list()
  if (length(contrast_rows))
    out[[length(out) + 1]] <- dplyr::bind_rows(contrast_rows) |>
      dplyr::arrange(dplyr::desc(estimate)) |>
      std_row("Topic", "topic_contrasts", sample_label)
  if (length(mean_rows))
    out[[length(out) + 1]] <- dplyr::bind_rows(mean_rows) |>
      std_row("Topic", "topic_contrasts", sample_label)
  out
}

compute_topic_contrasts <- function(core_objects) {
  si_require(c("dplyr", "tibble", "sandwich"))

  paths <- pkg_paths(core_objects$pkg_root)
  corpus <- .topic_contrast_corpus(core_objects)
  tc <- run_topic_clustering(corpus, paths$embed_cache)   # cluster ONCE; subset by sample below

  cls_all <- tc$corpus |>
    dplyr::filter(!is.na(belief_change), !is.na(condition))

  hyper_note <- sprintf(
    "EXPLORATORY embedding->PCA->HDBSCAN taxonomy pooled across all 4 studies (minPts=%s; retained_clusters=%s); not pre-registered. HC3 (sandwich::vcovHC) bunk-minus-debunk contrast.",
    tc$best$minPts, tc$best$retained_clusters
  )

  # strict = full analytic corpus; compliant = conversations the model complied in.
  out <- c(
    .topic_contrast_one_sample(cls_all, hyper_note, "strict"),
    .topic_contrast_one_sample(dplyr::filter(cls_all, compliant %in% TRUE), hyper_note, "compliant")
  )

  if (!length(out)) {
    return(std_row(tibble::tibble(model = character()), "Topic", "topic_contrasts", "strict"))
  }
  dplyr::bind_rows(out)
}
