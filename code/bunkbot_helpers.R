# bunkbot_helpers.R
# =============================================================================
# Pure helper functions for the unified Bunkbot replication pipeline. Sourced by
# build_all_numbers.R (and thence results_methods.Rmd + the SI) and by
# figures/manuscript/make_figures.R. NO API calls: every LLM-derived input (claim
# veracity, claim role/stance, APE compliance, 5-rater stance, topic embeddings)
# is read from the cached files under data/api_cached/.
#
# Contents:
#   * pkg_paths()                 - resolves all input files in the package layout
#   * Inference utilities         - HC3 tidy, robust contrasts, joint-F machinery
#   * Study 4 pipeline            - screening (build_s4_data), recodes, merges,
#                                   debrief/secondary/accuracy/compliance models
#   * Studies 1-3 pipeline        - build_s1s3() screening + recodes
#   * Veracity layer              - aggregate cached per-claim labels into the
#                                   adopted ALIGNED (direct+indirect) measure
#   * Topic clustering            - PCA -> HDBSCAN grid -> labels (cached
#                                   embeddings; no API)
#
# Studies 1-3 belief reverse-coding (category == "denies") mirrors the Study 4
# restatement-orientation correction; see apply_restatement_orientation().
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(forcats)
  library(tibble)
  library(purrr)
})

model_order_s4 <- c("Claude", "Gemini", "GPT-5.2", "Grok")

# ---- Package path resolver --------------------------------------------------
# `root` = the replication package root (parent of code/). All inputs live under
# data/raw_qualtrics/ + data/processed_s1s3/ (raw + analysis-ready study data) and
# data/api_cached/ (cached LLM outputs).
pkg_paths <- function(root) {
  ac  <- function(...) file.path(root, "data", "api_cached", ...)
  dd  <- function(...) file.path(root, "data", ...)
  # Human-readable regime label per Study 1-3, used as the file-name stem in the
  # standardized data layout (study1_jailbroken / study2_standard / study3_truth_constrained).
  regime <- c("Study 1" = "study1_jailbroken",
              "Study 2" = "study2_standard",
              "Study 3" = "study3_truth_constrained")
  list(
    # Study 4 raw Qualtrics export + cached merge inputs (Study-4 social-sharing run)
    s4_raw                = dd("raw_qualtrics", "study4_social_sharing_raw.csv.gz"),
    s4_cached_post_scores = ac("sharing_and_stance", "study4_sharing_post_scores.csv"),
    s4_merged_production  = ac("sharing_and_stance", "study4_sharing_analysis_merged.csv.gz"),
    s4_screen_funnel      = ac("sharing_and_stance", "study4_sharing_screenpass_funnel.csv"),
    s4_prereg_tests       = ac("sharing_and_stance", "study4_sharing_prereg_tests.csv"),
    s4_prereg_contrasts   = ac("sharing_and_stance", "study4_sharing_prereg_model_contrasts.csv"),
    s4_cell_estimates     = ac("sharing_and_stance", "study4_sharing_cell_estimates.csv"),
    s4_orientation        = ac("sharing_and_stance", "study4_restatement_orientation.csv"),
    s4_stance_v2          = ac("sharing_and_stance", "study4_stance_classifications.csv"),
    s4_master             = ac("claim_datasets", "study4_master_analysis_dataset.csv"),
    claim_role            = ac("claim_datasets", "claim_role_portfolio_all_studies.csv"),
    pooled_accuracy       = ac("claim_datasets", "claim_accuracy_pooled_s2_s4.csv"),
    # APE (Attempt-to-Persuade Evaluation) compliance — the SAME instrument across
    # all four studies, under data/api_cached/compliance_ape/. S4 = an LLM evaluator
    # (gpt-4o) reads the model's first substantive turn against the assigned goal
    # (attempt_binary) + a StrongREJECT-style refusal flag at turn 0.
    compliance            = ac("compliance_ape", "study4_compliance_ape_refusal.csv"),
    # Studies 1-3 analysis-ready (processed) subject files; LLM equivocal/direction
    # labels already baked in as columns.
    s1s3_clean = setNames(dd("processed_s1s3", paste0(regime, "_clean.csv.gz")),
                          names(regime)),
    # S1-3 APE: per-conversation evaluator + reverse-evaluator (counterfactual)
    # summaries — same APE instrument as S4, co-located under compliance_ape/.
    s1s3_eval  = setNames(ac("compliance_ape", paste0(regime, "_compliance_ape.csv")),
                          names(regime)),
    # Per-claim harmonized LLM role labels
    labels_s1s3 = ac("claim_labels", "claim_role_labels_s1s3.csv.gz"),
    labels_s2s4 = ac("claim_labels", "claim_role_labels_s2s4.csv.gz"),
    # Claim extraction + veracity sources (S1-3; S4 totals come from s4_master)
    nfacts = setNames(ac("claim_veracity", paste0(regime, "_claim_extraction_veracity.jsonl.gz")),
                      paste0("Study", 1:3)),
    # Topic clustering (cached embeddings + canonical assignments for cross-check)
    embed_cache       = ac("topic_modeling", "topic_embeddings_gemini.rds"),
    topic_assignments = ac("topic_modeling", "topic_assignments.csv"),
    # APE coverage-gap rescore: the 17 strict S4 conversations (16 Grok, 1 Claude)
    # never scored by the cached gpt-4o APE, re-scored from the first turn with the
    # same rubric (all compliant) -> grows the compliant sample 1056 -> 1073.
    ape_rescore       = ac("compliance_ape", "study4_compliance_ape_rescore_resolved.csv")
  )
}

# =============================================================================
# INFERENCE UTILITIES
# =============================================================================

hc3_tidy <- function(fit) {
  vc <- sandwich::vcovHC(fit, type = "HC3")
  est <- coef(fit)
  common <- intersect(names(est), rownames(vc))
  se <- sqrt(diag(vc[common, common, drop = FALSE]))
  z <- est[common] / se
  tibble(
    term = common,
    estimate = unname(est[common]),
    std.error = unname(se),
    statistic = unname(z),
    p.value = unname(2 * pnorm(abs(z), lower.tail = FALSE)),
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )
}

fit_vcov <- function(fit, cluster = NULL) {
  if (!is.null(cluster)) {
    return(sandwich::vcovCL(fit, cluster = cluster, type = "HC1"))
  }
  sandwich::vcovHC(fit, type = "HC3")
}

model_matrix_for_fit <- function(fit, newdata) {
  model.matrix(delete.response(terms(fit)), newdata, contrasts.arg = fit$contrasts)
}

linear_combo <- function(fit, weights, vc = NULL) {
  if (is.null(vc)) vc <- fit_vcov(fit)
  beta <- coef(fit)
  keep <- names(beta)[!is.na(beta)]
  weights <- weights[keep]
  beta <- beta[keep]
  vc <- vc[keep, keep, drop = FALSE]
  estimate <- as.numeric(sum(weights * beta))
  se <- sqrt(as.numeric(t(weights) %*% vc %*% weights))
  crit <- qt(0.975, df = df.residual(fit))
  tibble(
    estimate = estimate,
    std.error = se,
    conf.low = estimate - crit * se,
    conf.high = estimate + crit * se,
    p.value = 2 * pt(abs(estimate / se), df = df.residual(fit), lower.tail = FALSE)
  )
}

average_prediction <- function(fit, newdata, vc = NULL) {
  x <- model_matrix_for_fit(fit, newdata)
  linear_combo(fit, colMeans(x), vc = vc)
}

cell_prediction <- function(fit, newdata, vc = NULL) {
  x <- model_matrix_for_fit(fit, newdata)
  bind_cols(
    newdata,
    bind_rows(lapply(seq_len(nrow(x)), function(i) linear_combo(fit, x[i, ], vc = vc)))
  )
}

direction_contrast_equal_weighted <- function(fit, model_levels, covariate_values = list(), vc = NULL) {
  bunk_grid <- tibble(
    direction = factor("bunk", levels = c("debunk", "bunk")),
    model_pooled = factor(model_levels, levels = model_levels)
  )
  debunk_grid <- tibble(
    direction = factor("debunk", levels = c("debunk", "bunk")),
    model_pooled = factor(model_levels, levels = model_levels)
  )
  for (nm in names(covariate_values)) {
    bunk_grid[[nm]] <- covariate_values[[nm]]
    debunk_grid[[nm]] <- covariate_values[[nm]]
  }
  x_bunk <- model_matrix_for_fit(fit, bunk_grid)
  x_debunk <- model_matrix_for_fit(fit, debunk_grid)
  linear_combo(fit, colMeans(x_bunk) - colMeans(x_debunk), vc = vc)
}

mean_ci <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  se <- stats::sd(x) / sqrt(n)
  tibble(n = n, estimate = m, conf.low = m - 1.96 * se, conf.high = m + 1.96 * se)
}

coef_test_row <- function(fit, vcov_mat, weights, label) {
  b <- coef(fit)
  v <- vcov_mat
  w <- setNames(rep(0, length(b)), names(b))
  for (nm in names(weights)) w[nm] <- weights[[nm]]
  est <- as.numeric(sum(w * b))
  se <- sqrt(as.numeric(t(w) %*% v %*% w))
  tval <- est / se
  pval <- 2 * pt(abs(tval), df = df.residual(fit), lower.tail = FALSE)
  crit <- qt(0.975, df = df.residual(fit))
  tibble(contrast = label, estimate = est, se = se, t = tval, p = pval,
         ci_low = est - crit * se, ci_high = est + crit * se)
}

term_test <- function(fit, hypothesis) {
  out <- car::linearHypothesis(fit, hypothesis, white.adjust = "hc3")
  tibble(df_num = out$Df[2], df_den = out$Res.Df[2], f = out$F[2], p = out$`Pr(>F)`[2])
}

# hypothesis matrix for car::linearHypothesis (robust to special chars in names)
hyp_matrix <- function(fit, terms_to_test) {
  cn <- names(coef(fit))
  L <- matrix(0, nrow = length(terms_to_test), ncol = length(cn),
              dimnames = list(terms_to_test, cn))
  for (i in seq_along(terms_to_test)) {
    stopifnot(terms_to_test[i] %in% cn)
    L[i, terms_to_test[i]] <- 1
  }
  L
}

# =============================================================================
# STUDY 4 SCREENING + DERIVED VARIABLES  (verbatim from s4_helpers.R)
# =============================================================================

apply_restatement_orientation <- function(df, orientation_path) {
  orientation <- readr::read_csv(orientation_path, show_col_types = FALSE) %>%
    select(ResponseId, restatement_orientation = orientation_consensus)
  df %>%
    left_join(orientation, by = "ResponseId") %>%
    mutate(
      restatement_orientation = coalesce(restatement_orientation, "unaudited"),
      belief_scale_flipped = restatement_orientation == "denies",
      belief_rating_debrf_4 = suppressWarnings(as.numeric(belief_rating_debrf_4)),
      belief_rating_pre_4_orig = belief_rating_pre_4,
      belief_rating_post_4_orig = belief_rating_post_4,
      belief_rating_debrf_4_orig = belief_rating_debrf_4,
      belief_rating_pre_4 = if_else(belief_scale_flipped, 100 - belief_rating_pre_4, belief_rating_pre_4),
      belief_rating_post_4 = if_else(belief_scale_flipped, 100 - belief_rating_post_4, belief_rating_post_4),
      belief_rating_debrf_4 = if_else(belief_scale_flipped, 100 - belief_rating_debrf_4, belief_rating_debrf_4)
    )
}

nonempty <- function(x) !is.na(x) & str_trim(as.character(x)) != ""

normalize_berry_text <- function(x) str_replace_all(str_to_lower(str_trim(as.character(x))), "[^a-z]+", "")

berry_pass <- function(x) {
  normalized <- normalize_berry_text(x)
  typo_passes <- c("stawberry", "strabrelly", "strawbe", "strawbwrrr", "straberry",
                   "strberry", "scrawberry", "stewberry", "strwerberry", "sberry")
  normalized %in% c("berry", "strawberry", typo_passes) |
    str_detect(normalized, fixed("berry")) | str_detect(normalized, fixed("straw"))
}

pool_model_name <- function(x) {
  x_chr <- as.character(x)
  case_when(
    str_detect(x_chr, "^google/gemini-3(\\.1)?-pro-preview$") ~ "google/gemini-3-pro-preview",
    TRUE ~ x_chr
  )
}

model_pooled_label <- function(x) {
  case_when(
    str_detect(x, "claude") ~ "Claude",
    str_detect(x, "gemini") ~ "Gemini",
    str_detect(x, "gpt-5\\.2") ~ "GPT-5.2",
    str_detect(x, "grok") ~ "Grok",
    TRUE ~ as.character(x)
  )
}

read_s4_raw <- function(path) {
  read_csv(path, show_col_types = FALSE, progress = FALSE, name_repair = "unique",
           col_types = cols(.default = col_character())) %>%
    slice(-1) %>%
    filter(str_starts(ResponseId, "R_"))
}

add_s4_flags <- function(df) {
  invalid_claim_re <- regex(
    "does not contain|did not describe|no specific conspiracy|cannot provide|not provide any information|not describe any conspiracy",
    ignore_case = TRUE
  )
  numeric_cols <- intersect(
    c("Finished", "Progress", "Duration (in seconds)",
      "belief_rating_pre_4", "belief_rating_post_4", "belief_rating_debrf_4",
      "share_pre_4", "share_post_4", "share_original_post_now_4",
      "Q89_Page Submit", "Q2_Page Submit", "debrief timing_Page Submit", "sm_posting_freq",
      "__js_clipboard_copy_count", "__js_clipboard_cut_count", "__js_clipboard_paste_count",
      "__js_paste_into_textbox_count", "__js_paste_into_pre_post_box_count",
      "__js_paste_into_post_post_box_count", "__js_paste_chars_total", "__js_tab_hidden_count"),
    names(df)
  )
  df %>%
    mutate(
      across(all_of(numeric_cols), ~ suppressWarnings(as.numeric(.x))),
      modelName_raw = modelName,
      modelName = pool_model_name(modelName),
      model_pooled = model_pooled_label(modelName),
      attention_pass = suppressWarnings(as.numeric(nicks)) == 5,
      berry_pass = berry_pass(berry),
      condition_assigned = nonempty(condition),
      model_assigned = nonempty(modelName),
      conspiracy_summary = nonempty(conRestatement),
      pre_belief_present = nonempty(belief_rating_pre_4),
      pre_post_present = nonempty(social_post) & nonempty(share_pre_4),
      chat_saved = nonempty(chathistory01),
      post_outcomes_present = nonempty(belief_rating_post_4) & nonempty(final_post) &
        nonempty(share_post_4) & nonempty(share_original_post_now_4),
      full_social_data = pre_post_present & post_outcomes_present,
      belief_window_pass = !is.na(belief_rating_pre_4) & belief_rating_pre_4 > 25 & belief_rating_pre_4 < 75,
      invalid_claim = str_detect(coalesce(conRestatement, ""), invalid_claim_re) |
        str_detect(coalesce(conSummary, ""), invalid_claim_re),
      equivocal_clean = str_trim(as.character(isEquivocal)),
      equivocal_pass = equivocal_clean == "TRUE",
      direction = dplyr::recode(condition, treatment_mid_bunk = "bunk",
                                treatment_mid_debunk = "debunk", .default = NA_character_),
      rid_clean = if ("rid" %in% names(df)) {
        if_else(nonempty(rid) & rid != "[%RID%]", rid, NA_character_)
      } else NA_character_
    ) %>%
    mutate(.rid_order = paste(StartDate, ResponseId)) %>%
    group_by(rid_clean) %>%
    mutate(
      rid_dup = !is.na(rid_clean) & post_outcomes_present &
        rank(if_else(post_outcomes_present, .rid_order, NA_character_),
             ties.method = "first", na.last = "keep") > 1
    ) %>%
    ungroup() %>%
    select(-.rid_order) %>%
    mutate(
      rid_dup = coalesce(rid_dup, FALSE),
      dedup_pass = !rid_dup,
      valid_core = attention_pass & berry_pass & dedup_pass & condition_assigned &
        model_assigned & pre_belief_present & full_social_data & equivocal_pass &
        !invalid_claim & nonempty(direction),
      valid_core_strict = valid_core & belief_window_pass
    )
}

read_post_score_status <- function(path) {
  read_csv(path, show_col_types = FALSE, progress = FALSE) %>%
    mutate(timepoint = as.character(timepoint)) %>%
    distinct(ResponseId, timepoint) %>%
    mutate(available = TRUE) %>%
    pivot_wider(names_from = timepoint, values_from = available, values_fill = FALSE,
                names_glue = "cached_{timepoint}_direction_score_available") %>%
    mutate(cached_scores_available = coalesce(cached_pre_direction_score_available, FALSE) &
             coalesce(cached_post_direction_score_available, FALSE))
}

make_screening_flow <- function(df) {
  stage_defs <- list(
    "Attention check pass (nicks == 5)" = df$attention_pass,
    "Berry/strawberry check pass" = df$berry_pass,
    "Condition assigned" = df$condition_assigned,
    "Conspiracy restatement returned" = df$conspiracy_summary,
    "Valid/equivocal conspiracy classifier == TRUE" = df$equivocal_pass,
    "Restatement not flagged invalid" = !df$invalid_claim,
    "Baseline belief item reached" = df$pre_belief_present,
    "Baseline belief in equivocal window (25 < belief < 75)" = df$belief_window_pass,
    "Pre-conversation sharing measures reached" = df$pre_post_present,
    "LLM model assigned" = df$model_assigned,
    "Post-conversation outcomes present" = df$post_outcomes_present,
    "No duplicate panel ID (rid; registered exclusion)" = df$dedup_pass,
    "Cached pre/post sharing-stance scores available" = df$cached_scores_available
  )
  cumulative <- rep(TRUE, nrow(df))
  previous_n <- nrow(df)
  out <- list(tibble(stage = "Raw Qualtrics respondent rows", n_remaining = nrow(df),
                     excluded_at_step = 0L, pct_of_raw = 1, pct_of_previous_stage = 1))
  for (stage_name in names(stage_defs)) {
    condition <- coalesce(stage_defs[[stage_name]], FALSE)
    cumulative <- cumulative & condition
    n_remaining <- sum(cumulative, na.rm = TRUE)
    out[[length(out) + 1]] <- tibble(
      stage = stage_name, n_remaining = n_remaining, excluded_at_step = previous_n - n_remaining,
      pct_of_raw = n_remaining / nrow(df),
      pct_of_previous_stage = if_else(previous_n > 0, n_remaining / previous_n, NA_real_)
    )
    previous_n <- n_remaining
  }
  bind_rows(out)
}

make_exclusion_buckets <- function(df) {
  df %>%
    mutate(exclusion_reason = case_when(
      !coalesce(attention_pass, FALSE) ~ "Failed attention check",
      !coalesce(berry_pass, FALSE) ~ "Failed berry/strawberry check",
      !coalesce(condition_assigned, FALSE) ~ "No condition assigned",
      !coalesce(conspiracy_summary, FALSE) ~ "No conspiracy restatement returned",
      !coalesce(equivocal_pass, FALSE) ~ "Conspiracy not classified valid/equivocal",
      coalesce(invalid_claim, FALSE) ~ "Restatement flagged invalid",
      !coalesce(pre_belief_present, FALSE) ~ "No baseline belief item",
      !coalesce(belief_window_pass, FALSE) ~ "Baseline belief outside 25-75 window",
      !coalesce(pre_post_present, FALSE) ~ "No pre-conversation sharing measures",
      !coalesce(model_assigned, FALSE) ~ "No LLM model assigned",
      !coalesce(post_outcomes_present, FALSE) ~ "No post-conversation outcomes",
      coalesce(rid_dup, FALSE) ~ "Duplicate panel ID (rid)",
      !coalesce(cached_scores_available, FALSE) ~ "Missing cached pre/post sharing-stance score",
      TRUE ~ "Included in strict analytic sample"
    )) %>%
    count(exclusion_reason, name = "n") %>%
    mutate(pct_of_raw = n / nrow(df),
           exclusion_reason = factor(exclusion_reason, levels = c(
             "Failed attention check", "Failed berry/strawberry check", "No condition assigned",
             "No conspiracy restatement returned", "Conspiracy not classified valid/equivocal",
             "Restatement flagged invalid", "No baseline belief item",
             "Baseline belief outside 25-75 window", "No pre-conversation sharing measures",
             "No LLM model assigned", "No post-conversation outcomes", "Duplicate panel ID (rid)",
             "Missing cached pre/post sharing-stance score", "Included in strict analytic sample"))) %>%
    arrange(exclusion_reason)
}

merge_cached_post_scores <- function(df_valid, post_score_path) {
  post_scores <- read_csv(post_score_path, show_col_types = FALSE, progress = FALSE) %>%
    mutate(timepoint = as.character(timepoint)) %>%
    arrange(ResponseId, timepoint) %>%
    distinct(ResponseId, timepoint, .keep_all = TRUE)
  pre_scores <- post_scores %>% filter(timepoint == "pre") %>%
    transmute(ResponseId, pre_direction_score = as.numeric(score),
              pre_direction_confidence = as.numeric(confidence),
              pre_direction_rationale = rationale, pre_direction_ok = request_ok)
  post_scores_wide <- post_scores %>% filter(timepoint == "post") %>%
    transmute(ResponseId, post_direction_score = as.numeric(score),
              post_direction_confidence = as.numeric(confidence),
              post_direction_rationale = rationale, post_direction_ok = request_ok)
  df_valid %>%
    left_join(pre_scores, by = "ResponseId") %>%
    left_join(post_scores_wide, by = "ResponseId") %>%
    mutate(
      pre_share_prop = share_pre_4 / 100, post_share_prop = share_post_4 / 100,
      original_post_now_share_prop = share_original_post_now_4 / 100,
      pre_direction_centered = pre_direction_score - 50,
      post_direction_centered = post_direction_score - 50,
      pre_weighted_share = pre_direction_centered * pre_share_prop,
      post_weighted_share = post_direction_centered * post_share_prop,
      original_post_weighted_now = pre_direction_centered * original_post_now_share_prop,
      belief_change_raw = belief_rating_post_4 - belief_rating_pre_4,
      share_change_raw = share_post_4 - share_pre_4,
      direction_change_raw = post_direction_score - pre_direction_score,
      weighted_change_raw = post_weighted_share - pre_weighted_share,
      original_post_weighted_change_raw = original_post_weighted_now - pre_weighted_share,
      signed_direction = case_when(direction == "bunk" ~ 1, direction == "debunk" ~ -1, TRUE ~ NA_real_),
      aligned_belief_change = belief_change_raw * signed_direction,
      aligned_stance_change = direction_change_raw * signed_direction,
      aligned_original_post_share_change = original_post_weighted_change_raw * signed_direction,
      aligned_new_minus_old_weighted = weighted_change_raw * signed_direction
    )
}

merge_stance_v2_scores <- function(df, stance_v2_path) {
  v2 <- read_csv(stance_v2_path, show_col_types = FALSE, progress = FALSE)
  v2_wide <- function(tp, prefix) {
    v2 %>% filter(timepoint == tp) %>%
      transmute(
        ResponseId,
        "{prefix}_direction_score_v2" := if_else(
          consensus_applicable == "True" | consensus_applicable == "TRUE",
          suppressWarnings(as.numeric(consensus_score)), 50),
        "{prefix}_stance_applicable" := consensus_applicable %in% c("True", "TRUE"),
        "{prefix}_stance_category" := consensus_category,
        "{prefix}_response_type" := consensus_response_type,
        "{prefix}_focal_relevance" := consensus_focal_relevance,
        "{prefix}_stance_sd" := suppressWarnings(as.numeric(score_sd)),
        "{prefix}_stance_n_raters" := suppressWarnings(as.numeric(n_raters_ok))
      )
  }
  df %>%
    rename(
      pre_direction_score_v1 = pre_direction_score, post_direction_score_v1 = post_direction_score,
      pre_weighted_share_v1 = pre_weighted_share, post_weighted_share_v1 = post_weighted_share,
      original_post_weighted_now_v1 = original_post_weighted_now,
      direction_change_raw_v1 = direction_change_raw, weighted_change_raw_v1 = weighted_change_raw,
      original_post_weighted_change_raw_v1 = original_post_weighted_change_raw,
      aligned_stance_change_v1 = aligned_stance_change,
      aligned_original_post_share_change_v1 = aligned_original_post_share_change,
      aligned_new_minus_old_weighted_v1 = aligned_new_minus_old_weighted
    ) %>%
    left_join(v2_wide("pre", "pre"), by = "ResponseId") %>%
    left_join(v2_wide("post", "post"), by = "ResponseId") %>%
    mutate(
      pre_direction_score = pre_direction_score_v2, post_direction_score = post_direction_score_v2,
      pre_direction_centered = pre_direction_score - 50, post_direction_centered = post_direction_score - 50,
      pre_weighted_share = pre_direction_centered * pre_share_prop,
      post_weighted_share = post_direction_centered * post_share_prop,
      original_post_weighted_now = pre_direction_centered * original_post_now_share_prop,
      direction_change_raw = post_direction_score - pre_direction_score,
      weighted_change_raw = post_weighted_share - pre_weighted_share,
      original_post_weighted_change_raw = original_post_weighted_now - pre_weighted_share,
      aligned_stance_change = direction_change_raw * signed_direction,
      aligned_original_post_share_change = original_post_weighted_change_raw * signed_direction,
      aligned_new_minus_old_weighted = weighted_change_raw * signed_direction,
      both_posts_stance_applicable = pre_stance_applicable & post_stance_applicable
    )
}

add_secondary_recodes <- function(df) {
  df %>%
    mutate(
      across(any_of(c("trust2", "ArgStrength", "Unbiased", "Collaborative", "new_info",
                      "fog_clarity", "fog_raise_questions_acceptability", "belief_rating_debrf_4")),
             ~ suppressWarnings(as.numeric(.x))),
      Unbiased_rc = case_when(Unbiased == 69 ~ -2, Unbiased == 70 ~ -1, Unbiased == 71 ~ 0,
                              Unbiased == 72 ~ 1, Unbiased == 73 ~ 2, TRUE ~ NA_real_),
      Collaborative_rc = case_when(Collaborative == 69 ~ -2, Collaborative == 70 ~ -1, Collaborative == 71 ~ 0,
                                   Collaborative == 72 ~ 1, Collaborative == 73 ~ 2, TRUE ~ NA_real_),
      new_info_rc = case_when(new_info == 1 ~ 1, new_info == 4 ~ 2, new_info == 5 ~ 3, new_info == 6 ~ 4,
                              new_info == 7 ~ 5, new_info == 8 ~ 6, new_info == 9 ~ 7, new_info == 10 ~ 8,
                              new_info == 11 ~ 9, new_info == 12 ~ 10, TRUE ~ NA_real_)
    )
}

secondary_outcomes <- tibble::tribble(
  ~var, ~label, ~group,
  "trust2", "Trust in AI", "AI evaluations",
  "ArgStrength", "Argument strength", "AI evaluations",
  "Unbiased_rc", "Impartiality", "AI evaluations",
  "Collaborative_rc", "Collaborativeness", "AI evaluations",
  "new_info_rc", "Provided new information", "AI evaluations",
  "fog_clarity", "Felt clearer on whether the claim is true or false", "Epistemic aftereffects",
  "fog_raise_questions_acceptability", "Acceptable to post mainly to raise questions", "Epistemic aftereffects"
)

secondary_model_contrasts <- function(df, sample_label) {
  df <- add_secondary_recodes(df) %>%
    mutate(direction = factor(as.character(direction), levels = c("debunk", "bunk")),
           model_pooled = factor(as.character(model_pooled), levels = model_order_s4))
  rows <- vector("list", nrow(secondary_outcomes))
  for (i in seq_len(nrow(secondary_outcomes))) {
    outcome <- secondary_outcomes$var[[i]]
    dat <- df %>% filter(!is.na(.data[[outcome]])) %>% droplevels()
    model_levels <- model_order_s4[model_order_s4 %in% unique(as.character(dat$model_pooled))]
    fit <- lm(as.formula(paste(outcome, "~ direction * model_pooled")), data = dat)
    rows[[i]] <- direction_contrast_equal_weighted(fit, model_levels = model_levels, vc = fit_vcov(fit)) %>%
      mutate(sample = sample_label, outcome = outcome, outcome_label = secondary_outcomes$label[[i]],
             group = secondary_outcomes$group[[i]], n = nrow(dat), .before = 1)
  }
  bind_rows(rows)
}

debrief_model_data <- function(df, sample_label) {
  dat <- df %>%
    filter(!is.na(belief_rating_pre_4), !is.na(belief_rating_post_4), !is.na(belief_rating_debrf_4)) %>%
    mutate(direction = factor(as.character(direction), levels = c("bunk", "debunk")),
           model_pooled = factor(as.character(model_pooled), levels = model_order_s4),
           post_to_debrief = belief_rating_debrf_4 - belief_rating_post_4) %>%
    droplevels()
  model_levels <- model_order_s4[model_order_s4 %in% unique(as.character(dat$model_pooled))]
  long <- dat %>%
    select(ResponseId, direction, model_pooled, belief_rating_pre_4, belief_rating_post_4, belief_rating_debrf_4) %>%
    pivot_longer(cols = c(belief_rating_pre_4, belief_rating_post_4, belief_rating_debrf_4),
                 names_to = "timepoint", values_to = "belief") %>%
    mutate(timepoint = factor(timepoint,
                              levels = c("belief_rating_pre_4", "belief_rating_post_4", "belief_rating_debrf_4"),
                              labels = c("Pre-treatment", "Post-treatment", "Post-debrief")))
  fit_trajectory <- lm(belief ~ timepoint * direction * model_pooled, data = long)
  vc_trajectory <- fit_vcov(fit_trajectory, cluster = long$ResponseId)
  trajectory_grid <- expand_grid(sample = sample_label, timepoint = levels(long$timepoint),
                                 direction = levels(long$direction)) %>%
    mutate(timepoint = factor(timepoint, levels = levels(long$timepoint)),
           direction = factor(direction, levels = levels(long$direction)))
  trajectory_rows <- lapply(seq_len(nrow(trajectory_grid)), function(i) {
    grid <- tibble(timepoint = trajectory_grid$timepoint[[i]], direction = trajectory_grid$direction[[i]],
                   model_pooled = factor(model_levels, levels = model_levels))
    average_prediction(fit_trajectory, grid, vc = vc_trajectory) %>% bind_cols(trajectory_grid[i, ])
  }) %>% bind_rows()
  fit_shift <- lm(post_to_debrief ~ direction * model_pooled, data = dat)
  vc_shift <- fit_vcov(fit_shift)
  shift_model_grid <- expand_grid(sample = sample_label, model_label = model_levels,
                                  direction = levels(dat$direction)) %>%
    mutate(model_pooled = factor(model_label, levels = model_levels),
           direction = factor(direction, levels = levels(dat$direction)))
  shift_model <- cell_prediction(fit_shift, shift_model_grid %>% select(direction, model_pooled), vc = vc_shift) %>%
    bind_cols(shift_model_grid %>% select(sample, model_label))
  shift_pooled <- lapply(levels(dat$direction), function(dd) {
    grid <- tibble(direction = factor(dd, levels = levels(dat$direction)),
                   model_pooled = factor(model_levels, levels = model_levels))
    average_prediction(fit_shift, grid, vc = vc_shift) %>%
      mutate(sample = sample_label, model_label = "Pooled", direction = factor(dd, levels = levels(dat$direction)))
  }) %>% bind_rows()
  # Per-model rows carry the model x direction cell n; "Pooled" rows keep the
  # full debrief-complete sample n, which the Methods text cites as the sample
  # size (do not change Pooled semantics without updating 02_methods.Rmd).
  cell_ns <- dat %>%
    count(model_pooled, direction, name = "n_cell") %>%
    mutate(model_label = as.character(model_pooled)) %>%
    select(model_label, direction, n_cell)
  shift_all <- bind_rows(shift_pooled, shift_model) %>%
    left_join(cell_ns, by = c("model_label", "direction")) %>%
    mutate(n = ifelse(model_label == "Pooled", nrow(dat), n_cell)) %>%
    select(-n_cell)
  list(trajectory = trajectory_rows %>% mutate(n = nrow(dat)),
       shift = shift_all %>%
         mutate(model_label = factor(model_label, levels = c("Pooled", model_order_s4))))
}

lpm_cell_estimates <- function(df, outcome, metric_label, sample_label) {
  dat <- df %>% filter(!is.na(.data[[outcome]])) %>%
    mutate(direction = factor(as.character(direction), levels = c("bunk", "debunk")),
           model_pooled = factor(as.character(model_pooled), levels = model_order_s4)) %>%
    droplevels()
  fit <- lm(as.formula(paste(outcome, "~ direction * model_pooled")), data = dat)
  grid <- expand_grid(model_pooled = model_order_s4[model_order_s4 %in% unique(as.character(dat$model_pooled))],
                      direction = levels(dat$direction)) %>%
    mutate(model_pooled = factor(model_pooled, levels = levels(dat$model_pooled)),
           direction = factor(direction, levels = levels(dat$direction)))
  cell_prediction(fit, grid, vc = fit_vcov(fit)) %>%
    mutate(sample = sample_label, metric = metric_label, n = nrow(dat),
           estimate_plot = pmin(pmax(estimate, 0), 1),
           conf.low.plot = pmin(pmax(conf.low, 0), 1), conf.high.plot = pmin(pmax(conf.high, 0), 1))
}

accuracy_panel_data <- function(sample_ids, sample_label, claim_role, s4_master) {
  dat <- claim_role %>%
    filter(study_source == "Study4", ready_for_role_analysis, conversation_id %in% sample_ids) %>%
    left_join(s4_master %>% select(conversation_id, mean_veracity, pct_false_claims), by = "conversation_id") %>%
    filter(!is.na(aligned_belief_change), !is.na(belief_rating_pre_4), !is.na(aligned_direct_mean_veracity),
           !is.na(mean_veracity), !is.na(pct_false_claims)) %>%
    mutate(direction = factor(as.character(direction), levels = c("bunk", "debunk")),
           model_pooled = factor(as.character(model_pooled), levels = model_order_s4),
           pct_false_claims_100 = 100 * pct_false_claims) %>%
    droplevels()
  grid <- expand_grid(model_pooled = model_order_s4[model_order_s4 %in% unique(as.character(dat$model_pooled))],
                      direction = levels(dat$direction)) %>%
    mutate(model_pooled = factor(model_pooled, levels = levels(dat$model_pooled)),
           direction = factor(direction, levels = levels(dat$direction)))
  belief_fit <- lm(aligned_belief_change ~ direction * model_pooled + belief_rating_pre_4, data = dat)
  belief_grid <- grid %>% mutate(belief_rating_pre_4 = mean(dat$belief_rating_pre_4, na.rm = TRUE))
  belief_cells <- cell_prediction(belief_fit, belief_grid, vc = fit_vcov(belief_fit)) %>%
    transmute(sample = sample_label, model_pooled, direction,
              belief_estimate = estimate, belief_low = conf.low, belief_high = conf.high)
  aligned_veracity_fit <- lm(aligned_direct_mean_veracity ~ direction * model_pooled, data = dat)
  overall_veracity_fit <- lm(mean_veracity ~ direction * model_pooled, data = dat)
  false_claims_fit <- lm(pct_false_claims_100 ~ direction * model_pooled, data = dat)
  metric_cells <- bind_rows(
    cell_prediction(aligned_veracity_fit, grid, vc = fit_vcov(aligned_veracity_fit)) %>% mutate(metric = "Intended-direction veracity"),
    cell_prediction(overall_veracity_fit, grid, vc = fit_vcov(overall_veracity_fit)) %>% mutate(metric = "Overall veracity"),
    cell_prediction(false_claims_fit, grid, vc = fit_vcov(false_claims_fit)) %>% mutate(metric = "% explicitly false")
  ) %>% mutate(sample = sample_label,
               metric = factor(metric, levels = c("Intended-direction veracity", "Overall veracity", "% explicitly false")))
  list(
    scatter = belief_cells %>%
      left_join(metric_cells %>% filter(metric == "Intended-direction veracity") %>%
                  transmute(sample, model_pooled, direction, aligned_veracity_estimate = estimate,
                            aligned_veracity_low = conf.low, aligned_veracity_high = conf.high),
                by = c("sample", "model_pooled", "direction")) %>% mutate(n = nrow(dat)),
    metrics = metric_cells %>% mutate(n = nrow(dat)),
    coverage = tibble(sample = sample_label, plotted_conversations = nrow(dat))
  )
}

# ---- Study 4 data builder ----------------------------------------------------
build_s4_data <- function(paths) {
  post_score_status <- read_post_score_status(paths$s4_cached_post_scores)
  s4_raw <- read_s4_raw(paths$s4_raw) %>%
    add_s4_flags() %>%
    left_join(post_score_status, by = "ResponseId") %>%
    mutate(
      cached_pre_direction_score_available = coalesce(cached_pre_direction_score_available, FALSE),
      cached_post_direction_score_available = coalesce(cached_post_direction_score_available, FALSE),
      cached_scores_available = coalesce(cached_scores_available, FALSE)
    ) %>%
    apply_restatement_orientation(paths$s4_orientation)
  s4 <- s4_raw %>%
    filter(valid_core_strict) %>%
    merge_cached_post_scores(paths$s4_cached_post_scores) %>%
    merge_stance_v2_scores(paths$s4_stance_v2) %>%
    mutate(direction = factor(direction, levels = c("bunk", "debunk")),
           modelName = factor(modelName),
           model_pooled = factor(model_pooled, levels = model_order_s4))
  production_merged <- read_csv(paths$s4_merged_production, show_col_types = FALSE, progress = FALSE)
  screen_funnel <- read_csv(paths$s4_screen_funnel, show_col_types = FALSE, progress = FALSE)
  prereg_tests <- read_csv(paths$s4_prereg_tests, show_col_types = FALSE, progress = FALSE)
  prereg_contrasts <- read_csv(paths$s4_prereg_contrasts, show_col_types = FALSE, progress = FALSE)
  cell_estimates <- read_csv(paths$s4_cell_estimates, show_col_types = FALSE, progress = FALSE)
  s4_master <- read_csv(paths$s4_master, show_col_types = FALSE, progress = FALSE) %>%
    mutate(direction = factor(direction, levels = c("bunk", "debunk")), model_pooled = factor(model_pooled))
  claim_role <- read_csv(paths$claim_role, show_col_types = FALSE, progress = FALSE) %>%
    mutate(direction = factor(direction, levels = c("bunk", "debunk")), model_pooled = factor(model_pooled),
           truth_bin = factor(truth_bin, levels = c("False", "Mostly False", "True")))
  compliance <- read_csv(paths$compliance, show_col_types = FALSE, progress = FALSE) %>%
    mutate(attempt_binary = as.numeric(attempt_binary), refusal_binary = as.numeric(refusal_binary),
           strict_compliant = attempt_binary == 1 & refusal_binary == 0) %>%
    transmute(conversation_id = human_id, model_pooled = factor(model_pooled, levels = model_order_s4),
              direction = factor(direction, levels = c("bunk", "debunk")),
              attempt_binary, refusal_binary, strict_compliant,
              persuasion_score_5 = as.numeric(persuasion_score_5),
              specificity_score_5 = as.numeric(specificity_score_5), compliance_status)
  s4_with_compliance <- s4 %>%
    left_join(compliance, by = c("ResponseId" = "conversation_id", "model_pooled", "direction")) %>%
    mutate(compliance_scored = !is.na(attempt_binary) | !is.na(refusal_binary),
           strict_compliant = coalesce(strict_compliant, FALSE))
  s4_compliant <- s4_with_compliance %>% filter(strict_compliant) %>% droplevels()
  pooled_accuracy <- read_csv(paths$pooled_accuracy, show_col_types = FALSE, progress = FALSE) %>%
    mutate(direction = factor(direction, levels = c("bunk", "debunk")), model_pooled = factor(model_pooled),
           study_source = factor(study_source))
  list(paths = paths, s4_raw = s4_raw, s4 = s4, s4_with_compliance = s4_with_compliance,
       s4_compliant = s4_compliant, production_merged = production_merged, screen_funnel = screen_funnel,
       prereg_tests = prereg_tests, prereg_contrasts = prereg_contrasts, cell_estimates = cell_estimates,
       s4_master = s4_master, claim_role = claim_role, compliance = compliance, pooled_accuracy = pooled_accuracy)
}

validate_s4_data <- function(d) {
  stopifnot(
    nrow(d$s4_raw) == 14399, nrow(d$s4) == 1272, nrow(d$s4_with_compliance) == 1272,
    nrow(d$s4_compliant) == 1056, sum(d$s4_raw$valid_core_strict, na.rm = TRUE) == 1272,
    sum(d$s4$chat_saved, na.rm = TRUE) == 1270, nrow(d$production_merged) == 1840,
    !any(duplicated(d$s4$rid_clean[!is.na(d$s4$rid_clean)])),
    !any(d$s4$restatement_orientation == "unaudited"), sum(d$s4$belief_scale_flipped) == 12,
    all(d$s4$belief_rating_pre_4 > 25 & d$s4$belief_rating_pre_4 < 75),
    !any(is.na(d$s4$pre_direction_score)), !any(is.na(d$s4$post_direction_score)),
    all(d$s4$pre_stance_n_raters >= 4, na.rm = FALSE), all(d$s4$post_stance_n_raters >= 4, na.rm = FALSE)
  )
  invisible(TRUE)
}

# =============================================================================
# STUDIES 1-3 SCREENING + DERIVED VARIABLES
# Faithful port of data_s13.R / Bunkbot_Polished_Analysis.Rmd. Belief is
# reverse-coded (100 - x) where category == "denies" so higher = more conspiracy
# belief (the S1-3 analogue of the S4 restatement-orientation correction).
#
# duration_filter = FALSE (DEFAULT, used by ALL analyses as of 2026-06-18) ->
#   full equivocal + 25-75 window, Ns 1092/814/818. The duration_in_seconds > 600
#   screen (TRUE -> 1069/795/794) was REMOVED because total study duration is a
#   post-treatment variable; TRUE is retained only for a sensitivity reference.
# =============================================================================
build_s1s3 <- function(paths, duration_filter = FALSE) {
  read_study <- function(study_label) {
    d <- read_csv(paths$s1s3_clean[[study_label]], show_col_types = FALSE, progress = FALSE) %>%
      mutate(study = study_label)
    e <- read_csv(paths$s1s3_eval[[study_label]], show_col_types = FALSE, progress = FALSE) %>%
      distinct(ResponseId, .keep_all = TRUE)
    d %>% left_join(e %>% select(ResponseId, evaluator_label, reverse_evaluator_label), by = "ResponseId")
  }
  all_raw <- bind_rows(read_study("Study 1"), read_study("Study 2"), read_study("Study 3")) %>%
    janitor::clean_names() %>%
    mutate(
      belief_rating_pre_4 = as.numeric(belief_rating_pre_4),
      belief_rating_post_4 = as.numeric(belief_rating_post_4),
      belief_rating_debrf_4 = as.numeric(belief_rating_debrf_4),
      belief_rating_pre_rc = if_else(category == "denies", 100 - belief_rating_pre_4, belief_rating_pre_4),
      belief_rating_post_rc = if_else(category == "denies", 100 - belief_rating_post_4, belief_rating_post_4),
      belief_rating_debrf_rc = if_else(category == "denies", 100 - belief_rating_debrf_4, belief_rating_debrf_4)
    )
  d <- all_raw %>%
    filter(is_equivocal == TRUE, belief_rating_pre_rc > 25, belief_rating_pre_rc < 75,
           !is.na(belief_rating_post_rc))
  if (duration_filter) {
    d <- d %>% filter(suppressWarnings(as.numeric(duration_in_seconds)) > 600)
  }
  d %>%
    mutate(
      direction = case_when(condition == "treatment_mid_bunk" ~ "bunk",
                            condition == "treatment_mid_debunk" ~ "debunk", TRUE ~ NA_character_),
      change = case_when(condition == "treatment_mid_debunk" ~ belief_rating_pre_rc - belief_rating_post_rc,
                         condition == "treatment_mid_bunk" ~ belief_rating_post_rc - belief_rating_pre_rc,
                         TRUE ~ NA_real_),
      aligned_belief_change = change,
      compliant = evaluator_label == 1 & reverse_evaluator_label == 1,
      condition_factor = factor(condition, levels = c("treatment_mid_bunk", "treatment_mid_debunk"),
                                labels = c("Bunking", "Debunking")),
      study_factor = factor(study, levels = c("Study 1", "Study 2", "Study 3"),
                            labels = c("Jailbroken", "Standard", "Truth-Constrained"))
    )
}

# Raw Studies 1-3 belief slim frame (one row per S1-3 respondent, pre-screening)
# used by the pooled compliant-symmetry block in compute_s4_numbers(). Slim
# one-row-per-respondent extract; columns kept with original casing.
# `reverse_evaluator_label` is the COUNTERFACTUAL APE label (whether the model
# would have complied under the opposite prompt, same conspiracy) -- carried
# through for the S3 counterfactual contrast. duration_filter (default FALSE as of
# 2026-06-18; the post-treatment duration_in_seconds > 600 screen was removed) is
# kept only so a duration-filtered sensitivity sample can still be reconstructed.
build_s1s3_slim <- function(paths, duration_filter = FALSE) {
  read_one <- function(study_label) {
    d <- read_csv(paths$s1s3_clean[[study_label]], show_col_types = FALSE, progress = FALSE)
    if (duration_filter) {
      d <- d %>% filter(suppressWarnings(as.numeric(`Duration (in seconds)`)) > 600)
    }
    e <- read_csv(paths$s1s3_eval[[study_label]], show_col_types = FALSE, progress = FALSE) %>%
      distinct(ResponseId, .keep_all = TRUE)
    d %>%
      left_join(e %>% select(ResponseId, evaluator_label, reverse_evaluator_label), by = "ResponseId") %>%
      transmute(study = study_label, ResponseId, condition, category,
                isEquivocal = as.logical(isEquivocal),
                belief_rating_pre_4 = as.numeric(belief_rating_pre_4),
                belief_rating_post_4 = as.numeric(belief_rating_post_4),
                evaluator_label, reverse_evaluator_label)
  }
  bind_rows(read_one("Study 1"), read_one("Study 2"), read_one("Study 3"))
}

# =============================================================================
# VERACITY LAYER  (R aggregation of cached per-claim LLM labels)
# Adopted measure = ALIGNED (direct+indirect): substantive claims (these label
# files ARE the substantive set) whose stance matches the assigned direction
# (supports if bunk, opposes if debunk) and whose directness is direct OR
# indirect (background excluded).
# =============================================================================
read_claim_labels <- function(paths) {
  ctypes <- cols_only(
    study_source = col_character(), conversation_id = col_character(), direction = col_character(),
    model_pooled = col_character(), veracity_score = col_double(),
    stance_to_focal = col_character(), directness_to_focal = col_character(),
    request_status = col_character()
  )
  bind_rows(
    read_csv(paths$labels_s1s3, col_types = ctypes, progress = FALSE),
    read_csv(paths$labels_s2s4, col_types = ctypes, progress = FALSE)
  ) %>% filter(request_status == "success")
}

aligned_flag <- function(stance, directness, direction) {
  stance_ok <- (direction == "bunk" & stance == "supports") |
    (direction == "debunk" & stance == "opposes")
  coalesce(stance_ok, FALSE) & directness %in% c("direct", "indirect")
}

conv_aligned_veracity <- function(labels) {
  labels %>%
    mutate(is_aligned = aligned_flag(stance_to_focal, directness_to_focal, direction)) %>%
    group_by(conversation_id) %>%
    summarise(
      study = first(study_source), model = first(model_pooled), direction = first(direction),
      n_substantive = n(),
      n_aligned = sum(is_aligned, na.rm = TRUE),
      aligned_veracity = if (any(is_aligned & !is.na(veracity_score))) {
        mean(veracity_score[is_aligned], na.rm = TRUE)
      } else NA_real_,
      .groups = "drop"
    )
}

# study x model x direction aligned-veracity table (Table S-Y): mean over
# conversations with >= 1 aligned claim.
aligned_veracity_table <- function(conv) {
  conv %>%
    filter(n_aligned >= 1, !is.na(aligned_veracity)) %>%
    group_by(study, model, direction) %>%
    summarise(n_conv = n(), aligned_veracity = mean(aligned_veracity), .groups = "drop")
}

# =============================================================================
# TOPIC CLUSTERING  (cached embeddings -> PCA -> HDBSCAN grid -> labels)
# Faithful port of conspiracy_topics/pooled_conspiracy_topics.R. NO API at build
# time: cached embeddings cover every corpus id, and each retained cluster's
# label is the cached Gemini-3.1-Pro labeler output, used verbatim from
# topic_modeling/cluster_labels_gemini.csv.
# =============================================================================
TOPIC_PCA_N <- 30
TOPIC_MIN_CLUSTER_N <- 20
TOPIC_MINPTS_GRID <- 8:30
TOPIC_NOISE_CEILING <- 0.55

.topic_balance_cv <- function(sizes) {
  sizes <- sizes[sizes >= TOPIC_MIN_CLUSTER_N]
  if (length(sizes) < 2) return(NA_real_)
  sd(sizes) / mean(sizes)
}
.topic_study_segregation <- function(assign, meta) {
  df <- tibble(cluster = assign, study = meta$study) %>% filter(cluster > 0) %>%
    add_count(cluster, name = "cn") %>% filter(cn >= TOPIC_MIN_CLUSTER_N)
  if (!nrow(df)) return(NA_real_)
  glob <- prop.table(table(df$study))
  cs <- df %>% group_by(cluster) %>%
    summarise(n = n(),
              tv = sum(abs(prop.table(table(factor(study, levels = names(glob)))) - glob)) / 2,
              .groups = "drop")
  weighted.mean(cs$tv, cs$n)
}
.topic_solution_metrics <- function(assign, meta) {
  tab <- table(assign); sizes <- as.numeric(tab[names(tab) != "0"])
  retained <- sizes[sizes >= TOPIC_MIN_CLUSTER_N]
  tibble(n_clusters = max(assign), retained_clusters = length(retained),
         noise_fraction = mean(assign == 0),
         largest_retained = if (length(retained)) max(retained) else 0,
         retained_cv = .topic_balance_cv(sizes),
         study_segregation = .topic_study_segregation(assign, meta))
}
.topic_rank_grid <- function(g) {
  g %>% mutate(noise_ok = noise_fraction <= TOPIC_NOISE_CEILING) %>%
    arrange(desc(noise_ok), desc(retained_clusters), noise_fraction, study_segregation, retained_cv) %>%
    mutate(rank = row_number())
}

# corpus: data.frame with response_id, study, variant, condition, belief_change,
# embed_text. embed_cache_path: the cached embeddings RDS (list(ids, mat)).
run_topic_clustering <- function(corpus, embed_cache_path) {
  suppressPackageStartupMessages(library(dbscan))
  cache <- readRDS(embed_cache_path)
  idx <- match(corpus$response_id, cache$ids)
  if (anyNA(idx)) {
    miss <- sum(is.na(idx))
    warning(sprintf("%d corpus ids missing from embedding cache; dropping them (re-embedding needs an API key).", miss))
    corpus <- corpus[!is.na(idx), , drop = FALSE]
    idx <- idx[!is.na(idx)]
  }
  emb <- cache$mat[idx, , drop = FALSE]
  pca <- prcomp(emb, center = TRUE, scale. = FALSE)
  pcs <- pca$x[, 1:TOPIC_PCA_N]
  grid <- map_dfr(TOPIC_MINPTS_GRID, function(mp) {
    cl <- dbscan::hdbscan(pcs, minPts = mp)$cluster
    bind_cols(tibble(minPts = mp), .topic_solution_metrics(cl, corpus)) %>% mutate(assign = list(cl))
  })
  ranked <- .topic_rank_grid(grid %>% select(-assign)) %>%
    left_join(grid %>% select(minPts, assign), by = "minPts")
  best <- ranked %>% slice(1)
  corpus <- corpus %>% mutate(cluster_raw = best$assign[[1]]) %>%
    add_count(cluster_raw, name = "cn") %>%
    mutate(cluster_id = if_else(cluster_raw > 0 & cn >= TOPIC_MIN_CLUSTER_N, cluster_raw, 0L))
  gem  <- utils::read.csv(file.path(dirname(embed_cache_path), "cluster_labels_gemini.csv"),
                          stringsAsFactors = FALSE)
  gmap <- setNames(gem$gemini_label, as.character(gem$cluster_id))
  labs <- tibble(cluster_id = sort(unique(corpus$cluster_id[corpus$cluster_id > 0]))) %>%
    mutate(label = coalesce(unname(gmap[as.character(cluster_id)]), paste0("Cluster ", cluster_id)))
  corpus <- corpus %>% left_join(labs, by = "cluster_id") %>%
    mutate(topic = if_else(cluster_id == 0L, "Mixed / Unclassified", label))
  topic_eff <- corpus %>% group_by(topic, condition) %>%
    summarise(n = n(), mean_change = mean(belief_change, na.rm = TRUE), .groups = "drop")
  list(corpus = corpus, topic_eff = topic_eff, grid = ranked %>% select(-assign), best = best)
}
