# ext_extras.R
# Extra canonical-schema blocks for the Bunkbot SI recompute engine.
#
#   1. compute_cross_study_summary(all_numbers)  -> block "cross_study_summary"
#        The synthesis "Table 1" as tidy rows: one regime/model per row-family across
#        ALL FOUR studies (Jailbroken, Standard, Truth-Constrained, then the Study-4
#        models Claude / Gemini / GPT-5.2 / Grok), carrying four quantities as separate
#        (outcome, term) pairs: bunk belief change, debunk belief change,
#        attempted-bunk compliance (first-turn APE attempt) rate, and aligned veracity
#        of bunking claims. Pulled VERBATIM from already-recomputed all_numbers blocks
#        (belief_change, raw_aligned_means_cells, compliance, compliance_cells,
#        table_aligned_veracity) -- assembled, not recomputed.
#
#   2. compute_gold_coding_numbers(pkg_root)     -> blocks "gold_coding",
#        "stance_reliability"  (section = "Stance validation")
#        If the human gold-coding sheet is present under data/validation/gold_coding/,
#        computes agreement vs the 5-rater ensemble; otherwise emits a
#        "not computed" placeholder row with the planned metrics. Always emits the
#        5-rater ensemble reliability constants from stance_v2_reliability_report.md.
#
# Schema returned by every function (matches std_cols / all_numbers exactly):
#   section, block, sample, outcome, model, direction, term, n, estimate, se,
#   conf_low, conf_high, statistic, df_num, df_den, p_value, note
#
# Depends on std_cols / std_row (R/tables_dynamic.R) and si_require (R/formatting.R).

# Resolve the validation-data root relative to the replication package root.
ext_handoff_root <- function(pkg_root) {
  cand <- file.path(pkg_root, "data", "validation")
  if (!dir.exists(cand)) {
    stop("validation data dir not found at: ", cand)
  }
  cand
}

# -----------------------------------------------------------------------------
# 1. CROSS-STUDY SUMMARY  (assembled from existing all_numbers blocks)
# -----------------------------------------------------------------------------

compute_cross_study_summary <- function(all_numbers) {
  si_require(c("dplyr", "tibble"))

  # Canonical regime/model ordering across all four studies.
  regime_order <- c("Jailbroken", "Standard", "Truth-Constrained",
                    "Claude", "Gemini", "GPT-5.2", "Grok")
  s13_regimes <- c("Jailbroken", "Standard", "Truth-Constrained")
  s4_models   <- c("Claude", "Gemini", "GPT-5.2", "Grok")

  # Map the veracity block's S1-3 model labels ("Study 1/2/3") onto the regime names.
  vera_regime_map <- c(`Study 1` = "Jailbroken", `Study 2` = "Standard",
                       `Study 3` = "Truth-Constrained",
                       Claude = "Claude", Gemini = "Gemini",
                       `GPT-5.2` = "GPT-5.2", Grok = "Grok")

  keep <- function(df) {
    df |>
      dplyr::transmute(
        regime = .data$regime, study_block = .data$study_block,
        outcome = .data$outcome, term = .data$term, direction = .data$direction,
        n = .data$n, estimate = .data$estimate, se = .data$se,
        conf_low = .data$conf_low, conf_high = .data$conf_high,
        p_value = .data$p_value
      )
  }

  # --- (a) belief change, bunk + debunk -------------------------------------
  # S1-3: block belief_change (mean aligned change, model = regime, dir Bunking/Debunking)
  s13_belief <- all_numbers |>
    dplyr::filter(.data$block == "belief_change",
                  .data$model %in% s13_regimes) |>
    dplyr::transmute(
      regime = .data$model,
      study_block = "S1-3",
      outcome = "belief_change",
      direction = dplyr::recode(.data$direction,
                                Bunking = "bunk", Debunking = "debunk"),
      term = paste0(direction, "_belief_change"),
      n = .data$n, estimate = .data$estimate, se = .data$se,
      conf_low = .data$conf_low, conf_high = .data$conf_high,
      p_value = .data$p_value
    ) |> keep()

  # S4: raw_aligned_means_cells, outcome aligned_belief_change, strict_n1272 (intent-to-treat).
  s4_belief <- all_numbers |>
    dplyr::filter(.data$block == "raw_aligned_means_cells",
                  .data$outcome == "aligned_belief_change",
                  .data$sample == "strict_n1272",
                  .data$model %in% s4_models) |>
    dplyr::transmute(
      regime = .data$model,
      study_block = "S4",
      outcome = "belief_change",
      direction = .data$direction,
      term = paste0(direction, "_belief_change"),
      n = .data$n, estimate = .data$estimate, se = .data$se,
      conf_low = .data$conf_low, conf_high = .data$conf_high,
      p_value = .data$p_value
    ) |> keep()

  # --- (b) attempted-bunk compliance (first-turn APE attempt) rate ----------
  # SAME APE first-turn attempt classifier in every study: S1-3 attempt = attempt rate
  # (evaluator_label==1); S4 attempt_rate is the identical instrument.
  s13_comp <- all_numbers |>
    dplyr::filter(.data$block == "compliance",
                  .data$term == "ape_attempt_rate",
                  .data$direction == "Bunking",
                  .data$model %in% s13_regimes) |>
    dplyr::transmute(
      regime = .data$model,
      study_block = "S1-3",
      outcome = "compliance",
      direction = "bunk",
      term = "attempted_bunk_compliance_rate",
      n = .data$n, estimate = .data$estimate, se = .data$se,
      conf_low = .data$conf_low, conf_high = .data$conf_high,
      p_value = .data$p_value
    ) |> keep()

  s4_comp <- all_numbers |>
    dplyr::filter(.data$block == "compliance_cells",
                  .data$term == "attempt_rate",
                  .data$direction == "bunk",
                  .data$sample == "strict_n1272",
                  .data$model %in% s4_models) |>
    dplyr::transmute(
      regime = .data$model,
      study_block = "S4",
      outcome = "compliance",
      direction = "bunk",
      term = "attempted_bunk_compliance_rate",
      n = .data$n, estimate = .data$estimate, se = .data$se,
      conf_low = .data$conf_low, conf_high = .data$conf_high,
      p_value = .data$p_value
    ) |> keep()

  # --- (c) aligned veracity of bunking claims -------------------------------
  vera <- all_numbers |>
    dplyr::filter(.data$block == "table_aligned_veracity",
                  .data$term == "aligned_veracity",
                  .data$direction == "bunk",
                  .data$model %in% names(vera_regime_map)) |>
    dplyr::transmute(
      regime = unname(vera_regime_map[.data$model]),
      study_block = ifelse(.data$model %in% c("Study 1", "Study 2", "Study 3"),
                           "S1-3", "S4"),
      outcome = "aligned_veracity",
      direction = "bunk",
      term = "bunk_aligned_veracity",
      n = .data$n, estimate = .data$estimate, se = .data$se,
      conf_low = .data$conf_low, conf_high = .data$conf_high,
      p_value = .data$p_value
    ) |> keep()

  combined <- dplyr::bind_rows(
    s13_belief, s4_belief, s13_comp, s4_comp, vera
  ) |>
    dplyr::mutate(regime = factor(.data$regime, levels = regime_order)) |>
    dplyr::arrange(.data$regime, .data$outcome, .data$direction) |>
    dplyr::mutate(regime = as.character(.data$regime))

  note <- paste0(
    "Synthesis Table 1 assembled (not recomputed) from canonical blocks: ",
    "belief_change + raw_aligned_means_cells[outcome=aligned_belief_change,",
    "sample=strict_n1272] (belief change); compliance[ape_attempt_rate] + ",
    "compliance_cells[attempt_rate,sample=strict_n1272] (attempted-bunk APE ",
    "attempt rate -- SAME first-turn attempt classifier in S1-3 and S4); ",
    "table_aligned_veracity (aligned veracity of bunking claims). Study 1/2/3 = ",
    "Jailbroken/Standard/Truth-Constrained. n differs by term (participants for ",
    "belief/compliance, conversations with >=1 aligned claim for veracity)."
  )

  combined |>
    dplyr::transmute(
      model = .data$regime,
      outcome = .data$outcome,
      direction = .data$direction,
      term = .data$term,
      n = .data$n,
      estimate = .data$estimate,
      se = .data$se,
      conf_low = .data$conf_low,
      conf_high = .data$conf_high,
      p_value = .data$p_value,
      note = paste0(.data$study_block, "; ", note)
    ) |>
    std_row("Cross-study", "cross_study_summary", "all_four_studies")
}


# -----------------------------------------------------------------------------
# 3. GOLD-CODING VALIDATION + 5-RATER ENSEMBLE RELIABILITY
# -----------------------------------------------------------------------------

# Detect whether a coder CSV actually carries human codings (vs. the empty template).
ext_coder_filled <- function(path) {
  if (!file.exists(path)) return(FALSE)
  df <- suppressWarnings(readr::read_csv(path, show_col_types = FALSE,
                                         progress = FALSE))
  if (!"stance_score" %in% names(df) || nrow(df) < 2) return(FALSE)
  # Row 1 of these files is the legend/template row; ignore it.
  sc <- suppressWarnings(as.numeric(df$stance_score[-1]))
  cat_ok <- if ("stance_category" %in% names(df)) {
    sum(grepl("argues|leans|neutral|mixed|not_applicable",
              df$stance_category[-1])) > 0
  } else FALSE
  (sum(!is.na(sc)) > 0) || cat_ok
}

compute_gold_coding_numbers <- function(pkg_root) {
  si_require(c("dplyr", "readr", "tibble"))
  gc_dir <- file.path(ext_handoff_root(pkg_root), "gold_coding")
  returns_dir <- file.path(gc_dir, "returns")
  rows <- list()

  # --- (a) 5-rater ensemble reliability constants (always reported) ---------
  # Source: results/stance_v2_reliability_report.md (stance v2.2, 3690 items x 5
  # raters: claude-sonnet-4.6, gpt-5.2, gemini-3.1-pro, grok-4.3, deepseek-v3.2).
  rel <- tibble::tribble(
    ~term,                          ~estimate, ~n,    ~note_extra,
    "krippendorff_alpha_interval",   0.910,    3690, "interval alpha on 0-100 stance score",
    "icc_2k",                        0.982,    3690, "ICC(2,k) on 0-100 stance score",
    "pairwise_r_mean",               0.918,    3690, "mean pairwise rater correlation (range 0.885-0.949)",
    "krippendorff_alpha_category",   0.847,    3690, "nominal alpha, stance_category (unanimous 54%, >=4/5 75%)",
    "krippendorff_alpha_resptype",   0.872,    3690, "nominal alpha, response_type (unanimous 71%, >=4/5 84%)",
    "krippendorff_alpha_focalrel",   0.819,    3690, "nominal alpha, focal_relevance (unanimous 84%, >=4/5 93%)"
  )
  rows[[length(rows) + 1]] <- rel |>
    dplyr::transmute(
      model = NA_character_,
      direction = NA_character_,
      term = .data$term,
      n = .data$n,
      estimate = .data$estimate,
      note = paste0(
        "5-rater stance v2.2 ensemble reliability ",
        "(claude-sonnet-4.6, gpt-5.2, gemini-3.1-pro, grok-4.3, deepseek-v3.2); ",
        "from stance_v2_reliability_report.md; ", .data$note_extra, "."
      )
    ) |>
    std_row("Stance validation", "stance_reliability", "stance_v2_items_3690")

  # --- (b) human gold-coding validation vs the 5-rater stance classifier ----
  gold_path <- file.path(gc_dir, "stance_gold_coderA.csv")
  if (file.exists(gold_path) && ext_coder_filled(gold_path)) {
    paths <- pkg_paths(pkg_root)
    rows[[length(rows) + 1]] <- ext_compute_gold_agreement(gold_path, paths$s4_stance_v2)
  } else {
    planned <- tibble::tribble(
      ~term,                  ~note_extra,
      "spearman_r",           "rank correlation, human stance score vs classifier",
      "pearson_r",            "linear correlation, human vs classifier stance score",
      "mae",                  "mean absolute error, human vs classifier 0-100 stance score",
      "pct_exact_category",   "exact 6-level stance_category agreement",
      "pct_directional",      "for/against agreement among items both call directional"
    )
    rows[[length(rows) + 1]] <- planned |>
      dplyr::transmute(
        model = "consensus", direction = NA_character_, term = .data$term,
        n = NA_real_, estimate = NA_real_,
        note = paste0("[not computed] gold sheet not found/empty at ",
                      "data/validation/gold_coding/stance_gold_coderA.csv. Planned: ",
                      .data$note_extra, ".")
      ) |>
      std_row("Stance validation", "gold_coding", "gold_items_150")
  }

  dplyr::bind_rows(rows)
}

# Human gold coding (one coder, 150 Study-4 social-media posts) vs the stance
# classifier, computed from the gold sheet + the 5-rater consolidated ratings
# (study4_stance_classifications.csv). Reports, for the ensemble consensus AND
# each panel model: score correlation (Spearman/Pearson), MAE, exact 6-level
# category agreement, and for/against directional agreement.
ext_compute_gold_agreement <- function(gold_path, model_path) {
  si_require(c("dplyr", "readr", "tibble"))
  gold <- suppressWarnings(readr::read_csv(gold_path, show_col_types = FALSE, progress = FALSE)) |>
    dplyr::mutate(stance_score = suppressWarnings(as.numeric(.data$stance_score)))
  mdl <- suppressWarnings(readr::read_csv(model_path, show_col_types = FALSE, progress = FALSE))
  j <- dplyr::inner_join(gold, mdl, by = "item_id", suffix = c("_h", "_m"))

  .collapse <- function(x) dplyr::case_when(
    x %in% c("argues_against", "leans_against")      ~ "against",
    x %in% c("argues_for", "leans_for")              ~ "for",
    x %in% c("neutral_uncommitted", "mixed_both_sides") ~ "neutral",
    TRUE                                              ~ "na")
  models <- c("consensus", "claude", "gpt", "gemini", "grok", "deepseek")
  out <- lapply(models, function(m) {
    sc_col  <- if (m == "consensus") "consensus_score"    else paste0(m, "_stance_score")
    cat_col <- if (m == "consensus") "consensus_category" else paste0(m, "_stance_category")
    hs <- j$stance_score; ms <- suppressWarnings(as.numeric(j[[sc_col]]))
    ok <- !is.na(hs) & !is.na(ms)
    hc <- as.character(j$stance_category); mc <- as.character(j[[cat_col]])
    okc <- !is.na(hc) & nzchar(hc) & !is.na(mc) & nzchar(mc)
    h3 <- .collapse(hc); m3 <- .collapse(mc)
    dirok <- okc & h3 %in% c("for", "against") & m3 %in% c("for", "against")
    tibble::tibble(
      model = m,
      term = c("spearman_r", "pearson_r", "mae", "pct_exact_category", "pct_directional"),
      n = c(sum(ok), sum(ok), sum(ok), sum(okc), sum(dirok)),
      estimate = c(
        suppressWarnings(stats::cor(hs[ok], ms[ok], method = "spearman")),
        suppressWarnings(stats::cor(hs[ok], ms[ok])),
        mean(abs(hs[ok] - ms[ok])),
        if (sum(okc)) mean(hc[okc] == mc[okc]) else NA_real_,
        if (sum(dirok)) mean(h3[dirok] == m3[dirok]) else NA_real_
      ))
  })
  dplyr::bind_rows(out) |>
    dplyr::mutate(direction = NA_character_,
      note = "Human gold coding (1 coder, 150 Study-4 posts) vs the stance classifier; spearman/pearson/mae on the 0-100 score, pct_exact_category = exact 6-level agreement, pct_directional = for/against agreement among items both call directional.") |>
    std_row("Stance validation", "gold_coding", "gold_items_150")
}

# Convenience: all three extra blocks at once.
compute_ext_extras <- function(core_objects, all_numbers) {
  dplyr::bind_rows(
    compute_cross_study_summary(all_numbers),
    compute_gold_coding_numbers(core_objects$pkg_root)
  )
}
