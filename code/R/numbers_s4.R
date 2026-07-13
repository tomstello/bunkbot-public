# numbers_s4.R
#
# compute_s4_numbers(d, slim): rebuilds every Study 4 quantity (plus the pooled
# S1+S2+S4 compliant-symmetry test) as one tidy tibble in the canonical schema.
# Returns the tibble; writes nothing (build_all_numbers() assembles it into
# ALL_NUMBERS).
#
# Samples:
#   strict_n1272     - rebuilt strict analytic sample (preregistered 25-75 window)
#   compliant_n1056  - strict sample restricted to APE turn-0 strict compliance
#                      (renamed to compliant_n1073 downstream after the
#                      17-conversation APE coverage-gap rescore)
#   production_n1840 - cached prior production run (supplement sensitivity)
#
# Sign conventions are stated in the `note` column of each row.

suppressPackageStartupMessages({
  library(car)
  library(purrr)
})

# compute_s4_numbers(d, slim): emits every Study 4 quantity plus the pooled
# S1+S2+S4 compliant-symmetry test as one tidy tibble
# (helper functions live in bunkbot_helpers.R; d is already built/validated).
#   d    = build_s4_data(paths) output
#   slim = raw Studies 1-3 belief slim frame (one row per S1-3 respondent:
#          study, ResponseId, condition, category, isEquivocal,
#          belief_rating_pre_4, belief_rating_post_4, evaluator_label,
#          reverse_evaluator_label) -- used by the pooled-symmetry block.
# Returns the `numbers` tibble (no file write).
compute_s4_numbers <- function(d, slim) {
rows <- list()
add_rows <- function(df) {
  rows[[length(rows) + 1]] <<- df
}

blank_row <- function() {
  tibble(
    block = NA_character_, sample = NA_character_, outcome = NA_character_,
    model = NA_character_, direction = NA_character_, term = NA_character_,
    n = NA_real_, estimate = NA_real_, se = NA_real_,
    conf_low = NA_real_, conf_high = NA_real_, statistic = NA_real_,
    df_num = NA_real_, df_den = NA_real_, p_value = NA_real_, note = NA_character_
  )
}

std_rows <- function(df, ...) {
  fixed <- list(...)
  template <- blank_row()
  out <- df
  for (nm in names(fixed)) {
    out[[nm]] <- fixed[[nm]]
  }
  for (nm in names(template)) {
    if (!nm %in% names(out)) {
      out[[nm]] <- rep(template[[nm]][1], nrow(out))
    }
  }
  out %>% select(all_of(names(template)))
}

samples <- list(
  strict_n1272 = d$s4_with_compliance,
  compliant_n1056 = d$s4_compliant
)

prereg_outcomes <- tribble(
  ~outcome, ~post_var, ~pre_var,
  "weighted_share", "post_weighted_share", "pre_weighted_share",
  "post_stance", "post_direction_score", "pre_direction_score",
  "share_likelihood", "share_post_4", "share_pre_4",
  "belief", "belief_rating_post_4", "belief_rating_pre_4"
)

aligned_outcomes <- tribble(
  ~outcome, ~var,
  "aligned_belief_change", "aligned_belief_change",
  "aligned_new_minus_old_weighted", "aligned_new_minus_old_weighted",
  "aligned_stance_change", "aligned_stance_change",
  "aligned_original_post_share_change", "aligned_original_post_share_change"
)

# Hypothesis matrix for car::linearHypothesis (robust to special characters in
# coefficient names like `model_pooledGPT-5.2`).
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

# ============================== 1. funnel =====================================

strict_flow <- make_screening_flow(d$s4_raw)
add_rows(std_rows(
  strict_flow %>% transmute(term = stage, n = n_remaining, estimate = n_remaining,
                            se = NA_real_, statistic = excluded_at_step,
                            p_value = NA_real_,
                            note = paste0("pct_of_raw=", round(100 * pct_of_raw, 1), "%")),
  block = "funnel", sample = "strict_n1272"
))

excl <- make_exclusion_buckets(d$s4_raw)
add_rows(std_rows(
  excl %>% transmute(term = as.character(exclusion_reason), n = n, estimate = n,
                     note = paste0("mutually exclusive first-hit; pct_of_raw=",
                                   round(100 * pct_of_raw, 1), "%")),
  block = "funnel_exclusion_buckets", sample = "strict_n1272"
))

add_rows(std_rows(
  tibble(term = "transcript_available", n = sum(d$s4$chat_saved, na.rm = TRUE),
         estimate = sum(d$s4$chat_saved, na.rm = TRUE),
         note = "strict sample conversations with saved transcript"),
  block = "funnel", sample = "strict_n1272"
))

add_rows(std_rows(
  d$screen_funnel %>% transmute(term = stage, n = n, estimate = n,
                                note = "cached production funnel (no explicit 25-75 window)"),
  block = "funnel", sample = "production_n1840"
))

# ============================== 2. cell_n =====================================

for (s in names(samples)) {
  add_rows(std_rows(
    samples[[s]] %>% count(model_pooled, direction) %>%
      transmute(model = as.character(model_pooled), direction = as.character(direction),
                n = n, estimate = n),
    block = "cell_n", sample = s
  ))
  add_rows(std_rows(
    samples[[s]] %>% count(model_pooled) %>%
      transmute(model = as.character(model_pooled), n = n, estimate = n),
    block = "cell_n_model_total", sample = s
  ))
}

# ===================== 3. prereg_primary (refit, HC3) =========================

# Audited validation anchors (strict sample, direction levels bunk/debunk,
# dummy = directiondebunk):
#   weighted directiondebunk  -22.4 (se 2.12)
#   belief   directiondebunk  -38.7 (se 2.75)
# compliant sample: weighted directiondebunk -22.9 (se 2.17)
anchor_checks <- list()

for (s in names(samples)) {
  dat0 <- samples[[s]] %>%
    mutate(direction = factor(as.character(direction), levels = c("bunk", "debunk"))) %>%
    droplevels()
  dat1 <- dat0 %>%
    mutate(direction = factor(as.character(direction), levels = c("debunk", "bunk")))

  for (i in seq_len(nrow(prereg_outcomes))) {
    oc <- prereg_outcomes$outcome[i]
    fml <- as.formula(paste(prereg_outcomes$post_var[i], "~ direction * model_pooled +",
                            prereg_outcomes$pre_var[i]))

    # (a) original level order: anchor validation + coefficient table
    fit0 <- lm(fml, data = dat0)
    tidy0 <- hc3_tidy(fit0)
    add_rows(std_rows(
      tidy0 %>% transmute(term, estimate, se = std.error, conf_low = conf.low,
                          conf_high = conf.high, statistic, p_value = p.value,
                          n = nobs(fit0),
                          note = "refit coefficients; reference = bunk, Claude"),
      block = "prereg_primary_coefs", sample = s, outcome = oc
    ))
    if (s == "strict_n1272" && oc == "weighted_share") {
      anchor_checks$weighted_strict <- tidy0$estimate[tidy0$term == "directiondebunk"]
    }
    if (s == "strict_n1272" && oc == "belief") {
      anchor_checks$belief_strict <- tidy0$estimate[tidy0$term == "directiondebunk"]
    }
    if (s == "compliant_n1056" && oc == "weighted_share") {
      anchor_checks$weighted_compliant <- tidy0$estimate[tidy0$term == "directiondebunk"]
    }

    # (b) releveled (debunk reference): production-convention contrasts and Fs
    fit1 <- lm(fml, data = dat1)
    vc1 <- sandwich::vcovHC(fit1, type = "HC3")
    model_levels <- levels(dat1$model_pooled)
    reference_model <- model_levels[1]
    non_ref <- setdiff(model_levels, reference_model)
    int_terms <- paste0("directionbunk:model_pooled", non_ref)
    stopifnot(all(int_terms %in% names(coef(fit1))))

    avg_weights <- c("directionbunk" = 1)
    avg_weights[int_terms] <- 1 / length(model_levels)
    avg <- coef_test_row(fit1, vc1, avg_weights, "avg bunk - debunk")
    joint <- term_test(fit1, hyp_matrix(fit1, c("directionbunk", int_terms)))
    interaction <- term_test(fit1, hyp_matrix(fit1, int_terms))

    add_rows(std_rows(
      tibble(term = "avg_bunk_minus_debunk", n = nobs(fit1),
             estimate = avg$estimate, se = avg$se,
             conf_low = avg$ci_low, conf_high = avg$ci_high,
             statistic = avg$t, p_value = avg$p,
             note = "equal-weighted across models; positive = bunking higher on post outcome; HC3 t test of the contrast"),
      block = "prereg_primary", sample = s, outcome = oc
    ))
    add_rows(std_rows(
      tibble(term = "direction_joint_F", n = nobs(fit1),
             statistic = joint$f, df_num = joint$df_num,
             df_den = joint$df_den, p_value = joint$p,
             note = "joint HC3 F of all direction terms (reported on its own row, distinct from the avg contrast row)"),
      block = "prereg_primary", sample = s, outcome = oc
    ))
    add_rows(std_rows(
      tibble(term = "direction_x_model_interaction", n = nobs(fit1),
             statistic = interaction$f, df_num = interaction$df_num,
             df_den = interaction$df_den, p_value = interaction$p,
             note = "omnibus HC3 F test"),
      block = "prereg_primary", sample = s, outcome = oc
    ))

    per_model <- map_dfr(model_levels, function(m) {
      w <- c("directionbunk" = 1)
      if (m != reference_model) {
        w[paste0("directionbunk:model_pooled", m)] <- 1
      }
      coef_test_row(fit1, vc1, w, m) %>% mutate(model = m)
    })
    mf_n <- table(model.frame(fit1)$model_pooled)
    add_rows(std_rows(
      per_model %>% transmute(model, term = "bunk_minus_debunk",
                              estimate, se, conf_low = ci_low, conf_high = ci_high,
                              statistic = t, p_value = p,
                              n = as.integer(mf_n[model]),
                              note = "model-specific bunk - debunk on post outcome (baseline-adjusted); n = per-model estimation sample (both arms)"),
      block = "prereg_primary_model_contrasts", sample = s, outcome = oc
    ))
  }
}

# ANCHORS RE-PINNED 2026-06-11 after two deliberate measurement changes:
#   (1) restatement-orientation belief correction (12 strict scales flipped;
#       apply_restatement_orientation in s4_helpers.R), and
#   (2) stance v2.2 five-rater consensus replacing the v1 single-model scores
#       (merge_stance_v2_scores; v1 retained as *_v1 sensitivity columns).
# Historical anchors from the pre-correction rendered HTML, for the record:
#   weighted_strict -22.4 | belief_strict -38.7 | weighted_compliant -22.9
# Re-pinned 2026-06-11 (orientation-corrected belief + stance v2.2 consensus)
# weighted_compliant RE-PINNED 2026-06-16: the strict-compliant subset grew
#   from N=1,022 to N=1,056 after swapping in the more complete APE turn-0
#   compliance file (april04 re-parse: 1,252/1,272 scored, was 1,211; +34 net
#   compliant). Strict-sample anchors (weighted_strict, belief_strict) are
#   unchanged — only the compliant-only contrast moves, -19.18 -> -19.55.
# Known benign warning: linear_combo() raises 3 "NaNs produced" warnings from
# the perfect-rate compliance LPM cells (attempt rate ~1.0 -> leverage-1 HC3
# vcov is degenerate); those cells correctly emit NA standard errors.
ANCHOR_WEIGHTED_STRICT <- -19.12
ANCHOR_BELIEF_STRICT <- -39.91
ANCHOR_WEIGHTED_COMPLIANT <- -19.55
message("ANCHOR-REPORT weighted_strict=", round(anchor_checks$weighted_strict, 3),
        " belief_strict=", round(anchor_checks$belief_strict, 3),
        " weighted_compliant=", round(anchor_checks$weighted_compliant, 3))
if (!is.na(ANCHOR_WEIGHTED_STRICT)) {
  stopifnot(abs(anchor_checks$weighted_strict - (ANCHOR_WEIGHTED_STRICT)) < 0.1)
  stopifnot(abs(anchor_checks$belief_strict - (ANCHOR_BELIEF_STRICT)) < 0.1)
  # The compliant-only anchor is pinned to the canonical N=1,056 build; the
  # supplement intentionally grows the compliant subsample to 1,073 via the
  # APE-rescore coverage-gap resolution, which legitimately moves this contrast,
  # so the regression guard is enforced only for the canonical sample.
  if (nrow(d$s4_compliant) == 1056) {
    stopifnot(abs(anchor_checks$weighted_compliant - (ANCHOR_WEIGHTED_COMPLIANT)) < 0.1)
  }
}

# ===================== 3b. registered (AsPredicted v3) specification ==========
# The registered model is an UNADJUSTED aligned change-score OLS with
# direction x model and robust SEs, fit ITT (no compliance filter, no baseline
# covariate): aligned_change ~ persuasion_direction * model_type. Emitted in
# full so the manuscript can report the registered specification verbatim;
# the baseline-adjusted models in section 3 are a disclosed precision
# deviation, and compliant_n1056 analyses are non-registered/exploratory.
# Outcomes: registered primary = aligned_belief_change; registered secondaries
# = the three aligned sharing/stance outcomes.

dat_reg_all <- samples$strict_n1272 %>%
  mutate(direction = factor(as.character(direction), levels = c("bunk", "debunk"))) %>%
  droplevels()

for (i in seq_len(nrow(aligned_outcomes))) {
  v <- aligned_outcomes$var[i]
  oc <- aligned_outcomes$outcome[i]
  dat <- dat_reg_all %>% filter(!is.na(.data[[v]])) %>% droplevels()

  fit_reg <- lm(reformulate("direction * model_pooled", response = v), data = dat)
  vc_reg <- sandwich::vcovHC(fit_reg, type = "HC3")
  add_rows(std_rows(
    hc3_tidy(fit_reg) %>%
      transmute(term, estimate, se = std.error, conf_low = conf.low,
                conf_high = conf.high, statistic, p_value = p.value,
                n = nobs(fit_reg),
                note = "registered v3 spec: lm(aligned ~ direction*model), HC3, ITT strict, no baseline covariate; reference = bunk, Claude"),
    block = "prereg_registered_coefs", sample = "strict_n1272", outcome = oc
  ))

  model_levels <- levels(dat$model_pooled)
  non_ref <- setdiff(model_levels, model_levels[1])
  int_terms <- paste0("directiondebunk:model_pooled", non_ref)
  stopifnot(all(int_terms %in% names(coef(fit_reg))))

  w <- c("directiondebunk" = 1)
  w[int_terms] <- 1 / length(model_levels)
  ew <- coef_test_row(fit_reg, vc_reg, w, "ew")
  joint <- term_test(fit_reg, hyp_matrix(fit_reg, c("directiondebunk", int_terms)))
  interaction <- term_test(fit_reg, hyp_matrix(fit_reg, int_terms))

  add_rows(std_rows(
    bind_rows(
      tibble(term = "avg_debunk_minus_bunk", n = nobs(fit_reg),
             estimate = ew$estimate, se = ew$se,
             conf_low = ew$ci_low, conf_high = ew$ci_high,
             statistic = ew$t, p_value = ew$p,
             note = "registered spec; equal model weights, HC3 t test"),
      tibble(term = "direction_joint_F", n = nobs(fit_reg),
             statistic = joint$f, df_num = joint$df_num,
             df_den = joint$df_den, p_value = joint$p,
             note = "registered spec; joint HC3 F of all direction terms"),
      tibble(term = "direction_x_model_interaction", n = nobs(fit_reg),
             statistic = interaction$f, df_num = interaction$df_num,
             df_den = interaction$df_den, p_value = interaction$p,
             note = "registered spec; omnibus HC3 F of direction x model (the registered model-difference test)")
    ),
    block = "prereg_registered_tests", sample = "strict_n1272", outcome = oc
  ))
}

# Dedup audit for the registered duplicate-ID exclusion. rid_dup flags
# duplicate RESPONSES (rows with post-conversation outcomes beyond each rid's
# first such row); same-rid shell rows that never reached the post outcomes
# (screen-outs / relaunches) are reported descriptively here.
dup_ids <- d$s4_raw %>% filter(!is.na(rid_clean)) %>% count(rid_clean) %>% filter(n > 1)
shell_dup_rows <- d$s4_raw %>%
  filter(!is.na(rid_clean), rid_clean %in% dup_ids$rid_clean) %>%
  nrow() - nrow(dup_ids)
resp_dups <- d$s4_raw %>% filter(rid_dup)
resp_dups_passing_other_screens <- resp_dups %>%
  filter(
    attention_pass, berry_pass, condition_assigned, model_assigned,
    pre_belief_present, full_social_data, equivocal_pass, !invalid_claim,
    nonempty(direction), belief_window_pass
  ) %>%
  nrow()
add_rows(std_rows(
  tibble(
    term = c("duplicate_rid_ids_raw", "duplicate_rid_extra_rows_any_stage",
             "duplicate_response_rows", "excluded_solely_for_duplication"),
    n = c(nrow(dup_ids), shell_dup_rows,
          nrow(resp_dups), resp_dups_passing_other_screens),
    estimate = c(nrow(dup_ids), shell_dup_rows,
                 nrow(resp_dups), resp_dups_passing_other_screens),
    note = c("panel ids (rid) appearing >1 time among raw R_ rows (any stage)",
             "raw rows beyond each duplicated id's first occurrence, any stage (mostly pre-conversation shells)",
             "rows with post outcomes beyond the same rid's first response (rid_dup; the registered exclusion)",
             "rid_dup rows that pass every other strict screen (0 = dedup leaves N unchanged)")
  ),
  block = "dedup_audit", sample = "strict_n1272"
))

# ===================== 4. aligned_contrasts ===================================

# (a) Production self-check: simple spec lm(aligned_X ~ direction) on the cached
# production merged file must recover the writeup values 12.47/16.24/31.67/1.07.
prod <- d$production_merged %>%
  mutate(direction = factor(direction, levels = c("bunk", "debunk")))
stopifnot(nrow(prod) == 1840)

prod_targets <- c(
  aligned_belief_change = 12.47,
  aligned_new_minus_old_weighted = 16.24,
  aligned_stance_change = 31.67,
  aligned_original_post_share_change = 1.07
)

for (i in seq_len(nrow(aligned_outcomes))) {
  v <- aligned_outcomes$var[i]
  dat <- prod %>% filter(!is.na(.data[[v]]))
  fit <- lm(reformulate("direction", response = v), data = dat)
  est <- coef(fit)[["directiondebunk"]]
  ci <- suppressMessages(confint(fit))["directiondebunk", ]
  p <- summary(fit)$coefficients["directiondebunk", "Pr(>|t|)"]
  stopifnot(abs(est - prod_targets[[v]]) < 0.01)
  add_rows(std_rows(
    tibble(term = "debunk_minus_bunk_simple", n = nrow(dat), estimate = est,
           conf_low = ci[[1]], conf_high = ci[[2]], p_value = p,
           note = "production parity check: lm(aligned ~ direction), classical CI"),
    block = "aligned_contrasts", sample = "production_n1840", outcome = aligned_outcomes$outcome[i]
  ))
}
message("Production aligned-contrast self-check OK (12.47/16.24/31.67/1.07 recovered)")

# (b) Strict and compliant samples: simple spec + equal-weighted HC3 spec.
for (s in names(samples)) {
  dat_all <- samples[[s]] %>%
    mutate(direction = factor(as.character(direction), levels = c("bunk", "debunk")))

  for (i in seq_len(nrow(aligned_outcomes))) {
    v <- aligned_outcomes$var[i]
    dat <- dat_all %>% filter(!is.na(.data[[v]])) %>% droplevels()

    fit_simple <- lm(reformulate("direction", response = v), data = dat)
    est <- coef(fit_simple)[["directiondebunk"]]
    ci <- suppressMessages(confint(fit_simple))["directiondebunk", ]
    p <- summary(fit_simple)$coefficients["directiondebunk", "Pr(>|t|)"]
    add_rows(std_rows(
      tibble(term = "debunk_minus_bunk_simple", n = nrow(dat), estimate = est,
             conf_low = ci[[1]], conf_high = ci[[2]], p_value = p,
             note = "lm(aligned ~ direction), classical CI (production spec)"),
      block = "aligned_contrasts", sample = s, outcome = aligned_outcomes$outcome[i]
    ))

    # equal-weighted across models, HC3 (dummy directiondebunk = debunk - bunk)
    fit_ew <- lm(reformulate("direction * model_pooled", response = v), data = dat)
    vc_ew <- sandwich::vcovHC(fit_ew, type = "HC3")
    model_levels <- levels(dat$model_pooled)
    non_ref <- setdiff(model_levels, model_levels[1])
    int_terms <- paste0("directiondebunk:model_pooled", non_ref)
    w <- c("directiondebunk" = 1)
    w[int_terms] <- 1 / length(model_levels)
    ew <- coef_test_row(fit_ew, vc_ew, w, "ew")
    add_rows(std_rows(
      tibble(term = "debunk_minus_bunk_equal_weighted", n = nobs(fit_ew),
             estimate = ew$estimate, se = ew$se,
             conf_low = ew$ci_low, conf_high = ew$ci_high,
             statistic = ew$t, p_value = ew$p,
             note = "lm(aligned ~ direction * model), equal model weights, HC3"),
      block = "aligned_contrasts", sample = s, outcome = aligned_outcomes$outcome[i]
    ))

    per_model <- map_dfr(model_levels, function(m) {
      w <- c("directiondebunk" = 1)
      if (m != model_levels[1]) {
        w[paste0("directiondebunk:model_pooled", m)] <- 1
      }
      coef_test_row(fit_ew, vc_ew, w, m) %>% mutate(model = m)
    })
    mf_n_ew <- table(model.frame(fit_ew)$model_pooled)
    add_rows(std_rows(
      per_model %>% transmute(model, term = "debunk_minus_bunk",
                              estimate, se, conf_low = ci_low, conf_high = ci_high,
                              statistic = t, p_value = p,
                              n = as.integer(mf_n_ew[model]),
                              note = "model-specific debunk - bunk on aligned change, HC3; n = per-model estimation sample (both arms)"),
      block = "aligned_model_contrasts", sample = s, outcome = aligned_outcomes$outcome[i]
    ))
  }
}

# ===================== 5. raw_aligned_means ===================================

for (s in names(samples)) {
  for (i in seq_len(nrow(aligned_outcomes))) {
    v <- aligned_outcomes$var[i]
    by_dir <- samples[[s]] %>%
      group_by(direction) %>%
      group_modify(~ mean_ci(.x[[v]])) %>%
      ungroup()
    add_rows(std_rows(
      by_dir %>% transmute(direction = as.character(direction), n, estimate,
                           conf_low = conf.low, conf_high = conf.high,
                           term = "raw_mean",
                           note = "raw mean of aligned change (positive = movement in assigned direction)"),
      block = "raw_aligned_means", sample = s, outcome = aligned_outcomes$outcome[i]
    ))
    by_cell <- samples[[s]] %>%
      group_by(model_pooled, direction) %>%
      group_modify(~ mean_ci(.x[[v]])) %>%
      ungroup()
    add_rows(std_rows(
      by_cell %>% transmute(model = as.character(model_pooled),
                            direction = as.character(direction), n, estimate,
                            conf_low = conf.low, conf_high = conf.high,
                            term = "raw_mean"),
      block = "raw_aligned_means_cells", sample = s, outcome = aligned_outcomes$outcome[i]
    ))
  }
}

# ===================== 6. compliance ==========================================

comp_cells <- d$s4_with_compliance %>%
  group_by(model_pooled, direction) %>%
  summarise(
    n = n(),
    compliance_scored = sum(compliance_scored, na.rm = TRUE),
    attempt_rate = mean(attempt_binary == 1, na.rm = TRUE),
    refusal_rate = mean(refusal_binary == 1, na.rm = TRUE),
    strict_compliance_rate = mean(strict_compliant, na.rm = TRUE),
    mean_persuasion_5 = mean(persuasion_score_5, na.rm = TRUE),
    mean_specificity_5 = mean(specificity_score_5, na.rm = TRUE),
    .groups = "drop"
  )

add_rows(std_rows(
  comp_cells %>%
    pivot_longer(c(attempt_rate, refusal_rate, strict_compliance_rate,
                   mean_persuasion_5, mean_specificity_5),
                 names_to = "term", values_to = "estimate") %>%
    transmute(model = as.character(model_pooled), direction = as.character(direction),
              term, n, estimate,
              note = "raw rate/mean among scored conversations"),
  block = "compliance_cells", sample = "strict_n1272"
))

comp_overall <- d$s4_with_compliance %>%
  group_by(model_pooled) %>%
  summarise(
    n = n(),
    attempt_rate = mean(attempt_binary == 1, na.rm = TRUE),
    refusal_rate = mean(refusal_binary == 1, na.rm = TRUE),
    strict_compliance_rate = mean(strict_compliant, na.rm = TRUE),
    .groups = "drop"
  )
add_rows(std_rows(
  comp_overall %>%
    pivot_longer(c(attempt_rate, refusal_rate, strict_compliance_rate),
                 names_to = "term", values_to = "estimate") %>%
    transmute(model = as.character(model_pooled), term, n, estimate),
  block = "compliance_model_overall", sample = "strict_n1272"
))

lpm_df <- d$s4_with_compliance %>%
  mutate(
    attempt_flag = as.numeric(attempt_binary == 1),
    refusal_flag = as.numeric(refusal_binary == 1),
    strict_compliance_flag = as.numeric(strict_compliant)
  )
lpm_all <- bind_rows(
  lpm_cell_estimates(lpm_df, "attempt_flag", "Attempted assigned task", "strict_n1272"),
  lpm_cell_estimates(lpm_df, "refusal_flag", "Explicit refusal", "strict_n1272"),
  lpm_cell_estimates(lpm_df, "strict_compliance_flag", "Strictly compliant", "strict_n1272")
)
add_rows(std_rows(
  lpm_all %>% transmute(model = as.character(model_pooled),
                        direction = as.character(direction),
                        term = metric, n, estimate, se = std.error,
                        conf_low = conf.low, conf_high = conf.high, p_value = p.value,
                        note = "LPM cell estimate, HC3"),
  block = "compliance_lpm_cells", sample = "strict_n1272"
))

# ===================== 7. claim_accuracy ======================================

role_link <- list(
  strict_n1272 = d$s4$ResponseId,
  compliant_n1056 = d$s4_compliant$ResponseId
)

for (s in names(role_link)) {
  role_sub <- d$claim_role %>%
    filter(study_source == "Study4", conversation_id %in% role_link[[s]])

  overall <- role_sub %>%
    summarise(
      conversations = n(),
      mean_aligned_direct_n = mean(aligned_direct_n, na.rm = TRUE),
      mean_counteraligned_direct_n = mean(counteraligned_direct_n, na.rm = TRUE),
      aligned_direct_mean_veracity = mean(aligned_direct_mean_veracity, na.rm = TRUE),
      counteraligned_direct_mean_veracity = mean(counteraligned_direct_mean_veracity, na.rm = TRUE),
      aligned_direct_pct_very_false40 = mean(aligned_direct_pct_very_false40, na.rm = TRUE)
    )
  add_rows(std_rows(
    overall %>%
      pivot_longer(-conversations, names_to = "term", values_to = "estimate") %>%
      transmute(term, n = conversations, estimate),
    block = "claim_role_descriptives", sample = s
  ))

  by_dir <- role_sub %>%
    group_by(direction) %>%
    summarise(
      n = n(),
      aligned_direct_mean_veracity = mean(aligned_direct_mean_veracity, na.rm = TRUE),
      aligned_direct_pct_very_false40 = mean(aligned_direct_pct_very_false40, na.rm = TRUE),
      mean_aligned_direct_n = mean(aligned_direct_n, na.rm = TRUE),
      .groups = "drop"
    )
  add_rows(std_rows(
    by_dir %>%
      pivot_longer(c(-direction, -n), names_to = "term", values_to = "estimate") %>%
      transmute(direction = as.character(direction), term, n, estimate),
    block = "claim_role_by_direction", sample = s
  ))
}

# Claims volume (strict-linked master rows)
master_strict <- d$s4_master %>% filter(conversation_id %in% d$s4$ResponseId)
add_rows(std_rows(
  tibble(
    term = c("conversations_with_claims", "total_claims_extracted",
             "mean_claims_per_conversation", "total_factchecked_claims",
             "mean_factchecked_per_conversation"),
    estimate = c(nrow(master_strict),
                 sum(master_strict$all_claims_total, na.rm = TRUE),
                 mean(master_strict$all_claims_total, na.rm = TRUE),
                 sum(master_strict$factcheck_scored_n, na.rm = TRUE),
                 mean(master_strict$factcheck_scored_n, na.rm = TRUE)),
    n = nrow(master_strict)
  ),
  block = "claim_volume", sample = "strict_n1272"
))

# Accuracy-belief models (Rmd specs, strict-compliant-linked)
role_s4_compliant <- d$claim_role %>%
  filter(study_source == "Study4", ready_for_role_analysis) %>%
  inner_join(
    d$s4_with_compliance %>%
      filter(strict_compliant) %>%
      transmute(conversation_id = ResponseId, attempt_binary, refusal_binary,
                strict_compliant, compliance_status),
    by = "conversation_id"
  ) %>%
  mutate(
    false_or_mostly_false = truth_bin %in% c("False", "Mostly False"),
    non_gpt52 = model_pooled != "GPT-5.2"
  )

aligned_direct_df <- role_s4_compliant %>%
  filter(non_gpt52, false_or_mostly_false, aligned_direct_n > 0,
         !is.na(aligned_direct_mean_veracity_10), !is.na(aligned_belief_change)) %>%
  droplevels()

m_aligned_veracity <- lm(
  aligned_belief_change ~ aligned_direct_mean_veracity_10 * direction +
    model_pooled + truth_bin + belief_rating_pre_4_centered_10,
  data = aligned_direct_df
)
vc_av <- sandwich::vcovHC(m_aligned_veracity, type = "HC3")
add_rows(std_rows(
  hc3_tidy(m_aligned_veracity) %>%
    transmute(term, estimate, se = std.error, conf_low = conf.low,
              conf_high = conf.high, statistic, p_value = p.value,
              n = nobs(m_aligned_veracity),
              note = "non-GPT-5.2, false/mostly-false topics, aligned_direct_n>0"),
  block = "accuracy_aligned_veracity_coefs", sample = "compliant_n1056"
))
slope_bunk <- coef_test_row(m_aligned_veracity, vc_av,
                            c("aligned_direct_mean_veracity_10" = 1), "bunk")
slope_debunk <- coef_test_row(m_aligned_veracity, vc_av,
                              c("aligned_direct_mean_veracity_10" = 1,
                                "aligned_direct_mean_veracity_10:directiondebunk" = 1),
                              "debunk")
add_rows(std_rows(
  bind_rows(slope_bunk, slope_debunk) %>%
    transmute(direction = contrast, term = "veracity10_slope",
              estimate, se, conf_low = ci_low, conf_high = ci_high,
              statistic = t, p_value = p, n = nobs(m_aligned_veracity),
              note = "aligned belief change per 10-pt aligned-direct veracity, HC3"),
  block = "accuracy_aligned_veracity_slopes", sample = "compliant_n1056"
))

bucket_df <- role_s4_compliant %>%
  filter(non_gpt52, false_or_mostly_false) %>%
  mutate(
    aligned_direct_low_lt35_n = aligned_direct_false_n,
    aligned_direct_mid_35_69_n = aligned_direct_mid_n,
    aligned_direct_high_ge70_n = aligned_direct_true_n
  ) %>%
  droplevels()
m_bucket_counts <- lm(
  aligned_belief_change ~
    aligned_direct_low_lt35_n * direction +
    aligned_direct_mid_35_69_n * direction +
    aligned_direct_high_ge70_n * direction +
    model_pooled + truth_bin + belief_rating_pre_4_centered_10,
  data = bucket_df
)
add_rows(std_rows(
  hc3_tidy(m_bucket_counts) %>%
    transmute(term, estimate, se = std.error, conf_low = conf.low,
              conf_high = conf.high, statistic, p_value = p.value,
              n = nobs(m_bucket_counts),
              note = "veracity buckets: low <35, mid 35-69, high 70+"),
  block = "accuracy_bucket_counts_coefs", sample = "compliant_n1056"
))

gap_df <- role_s4_compliant %>%
  filter(non_gpt52, false_or_mostly_false, aligned_direct_n > 0,
         !is.na(topic_normalized_aligned_direct_veracity_10),
         !is.na(aligned_belief_change)) %>%
  droplevels()
m_gap <- lm(
  aligned_belief_change ~ topic_normalized_aligned_direct_veracity_10 * direction +
    model_pooled + truth_bin + belief_rating_pre_4_centered_10,
  data = gap_df
)
vc_gap <- sandwich::vcovHC(m_gap, type = "HC3")
add_rows(std_rows(
  hc3_tidy(m_gap) %>%
    transmute(term, estimate, se = std.error, conf_low = conf.low,
              conf_high = conf.high, statistic, p_value = p.value,
              n = nobs(m_gap),
              note = "gap = aligned-direct veracity minus focal conspiracy veracity, /10"),
  block = "accuracy_gap_coefs", sample = "compliant_n1056"
))
gap_bunk <- coef_test_row(m_gap, vc_gap,
                          c("topic_normalized_aligned_direct_veracity_10" = 1), "bunk")
gap_debunk <- coef_test_row(m_gap, vc_gap,
                            c("topic_normalized_aligned_direct_veracity_10" = 1,
                              "topic_normalized_aligned_direct_veracity_10:directiondebunk" = 1),
                            "debunk")
add_rows(std_rows(
  bind_rows(gap_bunk, gap_debunk) %>%
    transmute(direction = contrast, term = "gap10_slope",
              estimate, se, conf_low = ci_low, conf_high = ci_high,
              statistic = t, p_value = p, n = nobs(m_gap)),
  block = "accuracy_gap_slopes", sample = "compliant_n1056"
))

pooled_primary <- d$pooled_accuracy %>%
  filter(model_pooled != "GPT-5.2", !is.na(mean_veracity_10), !is.na(aligned_belief_change)) %>%
  droplevels()
m_pooled_overall <- lm(
  aligned_belief_change ~ mean_veracity_10 * direction + model_pooled,
  data = pooled_primary
)
add_rows(std_rows(
  hc3_tidy(m_pooled_overall) %>%
    transmute(term, estimate, se = std.error, conf_low = conf.low,
              conf_high = conf.high, statistic, p_value = p.value,
              n = nobs(m_pooled_overall),
              note = "pooled S2 GPT-4o + S4, excluding GPT-5.2"),
  block = "accuracy_pooled_s2s4_coefs", sample = "pooled_s2_s4"
))

# Accuracy panel cell estimates (for Figure 6 and tables)
acc_samples <- list(
  strict_n1272 = d$s4_with_compliance$ResponseId,
  compliant_n1056 = d$s4_compliant$ResponseId
)
for (s in names(acc_samples)) {
  panel <- accuracy_panel_data(acc_samples[[s]], s, d$claim_role, d$s4_master)
  add_rows(std_rows(
    panel$scatter %>%
      transmute(model = as.character(model_pooled), direction = as.character(direction),
                term = "belief_cell_estimate", estimate = belief_estimate,
                conf_low = belief_low, conf_high = belief_high, n = n,
                note = "adjusted aligned belief change at mean baseline"),
    block = "accuracy_panel_scatter", sample = s
  ))
  add_rows(std_rows(
    panel$metrics %>%
      transmute(model = as.character(model_pooled), direction = as.character(direction),
                term = as.character(metric), estimate, se = std.error,
                conf_low = conf.low, conf_high = conf.high, p_value = p.value, n = n),
    block = "accuracy_panel_metrics", sample = s
  ))
}

# Gap-slope anchors from the cached production analyses (b=0.63 p=.025 bunk;
# b=0.36/0.37 p=.29 debunk per staging prose) - warn (not stop) on drift since
# the linked sample is rebuilt here.
if (abs(gap_bunk$estimate - 0.63) > 0.15) {
  warning("gap bunk slope drifted from its audited anchor value 0.63: ", round(gap_bunk$estimate, 3))
}

# ===================== 7a2. stance ceiling robustness =========================
# Pre-treatment posts lean pro-conspiracy (mean stance ~65), giving bunking less
# headroom than debunking on the post-stance scale. The mid-band restriction
# (30 <= pre stance <= 70) equalizes room in both directions, mirroring the
# preregistered 25-75 equivocal window used for belief.

for (s in names(samples)) {
  mb <- samples[[s]] %>%
    filter(pre_direction_score >= 30, pre_direction_score <= 70)
  for (v in c("aligned_stance_change", "aligned_new_minus_old_weighted")) {
    by_dir <- mb %>% group_by(direction) %>%
      group_modify(~ mean_ci(.x[[v]])) %>% ungroup()
    add_rows(std_rows(
      by_dir %>% transmute(direction = as.character(direction), n, estimate,
                           conf_low = conf.low, conf_high = conf.high,
                           term = "midband_raw_mean",
                           note = "pre-post stance 30-70 band (room-symmetric)"),
      block = "stance_ceiling_midband", sample = s, outcome = v
    ))
    fit_mb <- lm(reformulate("direction", response = v), data = mb)
    est <- coef(fit_mb)[["directiondebunk"]]
    ci <- suppressMessages(confint(fit_mb))["directiondebunk", ]
    p <- summary(fit_mb)$coefficients["directiondebunk", "Pr(>|t|)"]
    add_rows(std_rows(
      tibble(term = "midband_debunk_minus_bunk", n = nobs(fit_mb), estimate = est,
             conf_low = ci[[1]], conf_high = ci[[2]], p_value = p,
             note = "simple contrast within 30-70 pre-stance band"),
      block = "stance_ceiling_midband", sample = s, outcome = v
    ))
  }
  add_rows(std_rows(
    samples[[s]] %>% group_by(direction) %>%
      summarise(n = n(),
                estimate = mean(pre_direction_score, na.rm = TRUE),
                statistic = mean(if_else(direction == "bunk",
                                         100 - pre_direction_score,
                                         pre_direction_score), na.rm = TRUE),
                .groups = "drop") %>%
      mutate(direction = as.character(direction), term = "pre_stance_mean",
             note = "statistic column = mean room toward assigned direction"),
    block = "stance_ceiling_room", sample = s
  ))
}

# ===================== 7a3. GPT-5.2 bunking decomposition ======================

gpt52_bunk <- d$s4_with_compliance %>%
  filter(model_pooled == "GPT-5.2", direction == "bunk", compliance_scored) %>%
  mutate(att = if_else(attempt_binary == 1, "attempted", "declined"))
for (v in c("aligned_belief_change", "aligned_stance_change")) {
  by_att <- gpt52_bunk %>% group_by(att) %>%
    group_modify(~ mean_ci(.x[[v]])) %>% ungroup()
  add_rows(std_rows(
    by_att %>% transmute(term = att, n, estimate,
                         conf_low = conf.low, conf_high = conf.high,
                         note = "GPT-5.2 bunking arm split by first-turn attempt status"),
    block = "gpt52_bunk_decomposition", sample = "strict_n1272", outcome = v
  ))
}

# ===================== 7a4. within-cell veracity-update correlations ==========
# Pearson correlation between aligned-direct claim veracity and aligned belief
# change, within each model x direction cell (the most disaggregated test of
# whether claim accuracy tracks persuasion).

for (s in names(role_link)) {
  cor_dat <- d$claim_role %>%
    filter(study_source == "Study4", conversation_id %in% role_link[[s]],
           !is.na(aligned_direct_mean_veracity), !is.na(aligned_belief_change))
  cor_rows <- cor_dat %>%
    group_by(model_pooled, direction) %>%
    group_modify(~{
      if (nrow(.x) < 10) {
        return(tibble(n = nrow(.x), estimate = NA_real_, conf_low = NA_real_,
                      conf_high = NA_real_, p_value = NA_real_))
      }
      ct <- cor.test(.x$aligned_direct_mean_veracity, .x$aligned_belief_change)
      tibble(n = nrow(.x), estimate = unname(ct$estimate),
             conf_low = ct$conf.int[1], conf_high = ct$conf.int[2],
             p_value = ct$p.value)
    }) %>%
    ungroup()
  add_rows(std_rows(
    cor_rows %>% transmute(model = as.character(model_pooled),
                           direction = as.character(direction),
                           term = "pearson_r", n, estimate, conf_low, conf_high,
                           p_value,
                           note = "aligned-direct veracity vs aligned belief change, within cell"),
    block = "veracity_belief_correlations", sample = s
  ))
}

# ===================== 7b. conversation-stage attrition =======================

attrition_pool <- d$s4_raw %>%
  filter(attention_pass, berry_pass, condition_assigned, conspiracy_summary,
         equivocal_pass, !invalid_claim, pre_belief_present, belief_window_pass,
         pre_post_present, model_assigned) %>%
  mutate(
    completed = coalesce(post_outcomes_present, FALSE),
    chat_ok = coalesce(chat_saved, FALSE),
    direction = factor(direction, levels = c("bunk", "debunk")),
    model_pooled = factor(model_pooled, levels = model_order_s4)
  )

add_rows(std_rows(
  tibble(
    term = c("conversation_stage_pool", "completed_post_outcomes",
             "lost_no_chat_saved", "lost_chat_but_incomplete"),
    n = c(nrow(attrition_pool), sum(attrition_pool$completed),
          sum(!attrition_pool$completed & !attrition_pool$chat_ok),
          sum(!attrition_pool$completed & attrition_pool$chat_ok)),
    estimate = n,
    note = "pool = passed all screens through model assignment"
  ),
  block = "attrition", sample = "strict_n1272"
))

add_rows(std_rows(
  attrition_pool %>% group_by(direction) %>%
    summarise(n = n(), estimate = mean(completed), .groups = "drop") %>%
    mutate(term = "completion_rate", direction = as.character(direction)),
  block = "attrition", sample = "strict_n1272"
))
add_rows(std_rows(
  attrition_pool %>% group_by(model_pooled, direction) %>%
    summarise(n = n(), estimate = mean(completed), .groups = "drop") %>%
    mutate(term = "completion_rate", model = as.character(model_pooled),
           direction = as.character(direction)),
  block = "attrition", sample = "strict_n1272"
))

m_attr <- lm(completed ~ direction * model_pooled, data = attrition_pool)
attr_ints <- grep(":", names(coef(m_attr)), value = TRUE)
attr_mods <- grep("^model_pooled", setdiff(names(coef(m_attr)), attr_ints), value = TRUE)
attr_tests <- bind_rows(
  term_test(m_attr, hyp_matrix(m_attr, "directiondebunk")) %>% mutate(term = "direction_main"),
  term_test(m_attr, hyp_matrix(m_attr, attr_mods)) %>% mutate(term = "model_main"),
  term_test(m_attr, hyp_matrix(m_attr, attr_ints)) %>% mutate(term = "direction_x_model")
)
add_rows(std_rows(
  attr_tests %>% transmute(term, statistic = f, df_num, df_den, p_value = p,
                           n = nobs(m_attr),
                           note = "LPM completed ~ direction * model, HC3 joint F"),
  block = "attrition_tests", sample = "strict_n1272"
))

# Diagnosis of conversation-stage losses via the client-side partial-chat log
# (__js_chatPartialData1), which records chat content even for incomplete
# sessions: distinguishes never-started/failed chats from mid-chat dropout.
attr_lost <- attrition_pool %>%
  filter(!completed) %>%
  mutate(
    partial_len = nchar(coalesce(`__js_chatPartialData1`, "")),
    status = case_when(
      chat_ok ~ "chat_saved_missing_outcomes",
      partial_len > 20 ~ "partial_chat_then_lost",
      TRUE ~ "no_chat_content"
    )
  )
add_rows(std_rows(
  attr_lost %>% count(status) %>% transmute(term = status, n, estimate = n),
  block = "attrition_chat_diagnosis", sample = "strict_n1272"
))
add_rows(std_rows(
  attr_lost %>% count(model_pooled, status) %>%
    transmute(model = as.character(model_pooled), term = status, n, estimate = n),
  block = "attrition_chat_diagnosis_by_model", sample = "strict_n1272"
))
add_rows(std_rows(
  attr_lost %>% filter(status == "no_chat_content") %>%
    count(model_pooled, direction) %>%
    transmute(model = as.character(model_pooled), direction = as.character(direction),
              term = "no_chat_content", n, estimate = n),
  block = "attrition_chat_diagnosis_cells", sample = "strict_n1272"
))

# Compliance-classifier coverage: report the gpt-4o classifier's NATIVE (pre-rescore)
# coverage plus the rescore resolution as separate terms, so the block is internally
# consistent with the Methods disclosure and does not mislabel the 17 manual/Claude-
# assisted overrides as classifier output. d$s4_with_compliance is POST-rescore, so we
# use the pre-rescore snapshot (attached by compute_s4_numbers_full); falls back to the
# post-rescore object if the snapshot is absent (no crash, slightly conservative).
.swc0 <- if (!is.null(d$s4_with_compliance_prerescore)) d$s4_with_compliance_prerescore else d$s4_with_compliance
.n_scored   <- sum(.swc0$compliance_scored)
.n_unscored <- sum(!.swc0$compliance_scored)
.n_unsc_tx  <- sum(!.swc0$compliance_scored & .swc0$chat_saved)
.n_rescored <- if (!is.null(d$n_rescored)) d$n_rescored else 0L
add_rows(std_rows(
  tibble(
    term = c("classifier_scored", "classifier_unscored", "unscored_with_transcript",
             "rescored_compliant", "unresolved_noncompliant"),
    n = c(.n_scored, .n_unscored, .n_unsc_tx, .n_rescored, .n_unscored - .n_rescored),
    estimate = n,
    note = "gpt-4o classifier native coverage; unscored coalesced to non-compliant unless re-scored compliant"
  ),
  block = "compliance_coverage", sample = "strict_n1272"
))

# GPT-5.2 bunking non-attempts: explicit refusal vs counter-argument split.
gpt52_na <- d$s4_with_compliance %>%
  filter(model_pooled == "GPT-5.2", direction == "bunk", compliance_scored,
         attempt_binary == 0)
add_rows(std_rows(
  gpt52_na %>% group_by(refusal_binary) %>%
    summarise(n = n(), estimate = mean(aligned_belief_change, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(term = if_else(refusal_binary == 1, "explicit_refusal", "counterargument_no_refusal")) %>%
    select(term, n, estimate),
  block = "gpt52_nonattempt_split", sample = "strict_n1272"
))

m_attr_belief <- lm(completed ~ direction * belief_rating_pre_4, data = attrition_pool)
add_rows(std_rows(
  hc3_tidy(m_attr_belief) %>%
    transmute(term, estimate, se = std.error, conf_low = conf.low,
              conf_high = conf.high, statistic, p_value = p.value,
              n = nobs(m_attr_belief),
              note = "baseline-belief selection check"),
  block = "attrition_belief_selection", sample = "strict_n1272"
))

# ===================== 7c. focal-veracity control (exploratory) ===============

m_av_focal <- lm(
  aligned_belief_change ~ aligned_direct_mean_veracity_10 * direction +
    model_pooled + truth_bin + focal_claim_veracity_10 + belief_rating_pre_4_centered_10,
  data = aligned_direct_df
)
vc_avf <- sandwich::vcovHC(m_av_focal, type = "HC3")
m_av_focal_int <- lm(
  aligned_belief_change ~ aligned_direct_mean_veracity_10 * direction +
    focal_claim_veracity_10 * direction +
    model_pooled + truth_bin + belief_rating_pre_4_centered_10,
  data = aligned_direct_df
)
vc_avfi <- sandwich::vcovHC(m_av_focal_int, type = "HC3")

focal_rows <- bind_rows(
  coef_test_row(m_av_focal, vc_avf, c("aligned_direct_mean_veracity_10" = 1), "bunk") %>%
    mutate(term = "veracity10_slope_focal_control"),
  coef_test_row(m_av_focal, vc_avf,
                c("aligned_direct_mean_veracity_10" = 1,
                  "aligned_direct_mean_veracity_10:directiondebunk" = 1), "debunk") %>%
    mutate(term = "veracity10_slope_focal_control"),
  coef_test_row(m_av_focal_int, vc_avfi, c("aligned_direct_mean_veracity_10" = 1), "bunk") %>%
    mutate(term = "veracity10_slope_focal_x_direction"),
  coef_test_row(m_av_focal_int, vc_avfi,
                c("aligned_direct_mean_veracity_10" = 1,
                  "aligned_direct_mean_veracity_10:directiondebunk" = 1), "debunk") %>%
    mutate(term = "veracity10_slope_focal_x_direction")
)
add_rows(std_rows(
  focal_rows %>% transmute(direction = contrast, term, estimate, se,
                           conf_low = ci_low, conf_high = ci_high,
                           statistic = t, p_value = p, n = nobs(m_av_focal),
                           note = "adds continuous focal-conspiracy veracity control to base spec"),
  block = "accuracy_focal_control_slopes", sample = "compliant_n1056"
))

# ===================== 8. debrief =============================================

for (s in names(samples)) {
  dm <- debrief_model_data(samples[[s]], s)
  add_rows(std_rows(
    dm$trajectory %>%
      transmute(direction = as.character(direction), term = as.character(timepoint),
                estimate, se = std.error, conf_low = conf.low, conf_high = conf.high,
                p_value = p.value, n,
                note = "EMM equal-weighted across models, respondent-clustered SEs"),
    block = "debrief_trajectory", sample = s
  ))
  add_rows(std_rows(
    dm$shift %>%
      transmute(model = as.character(model_label), direction = as.character(direction),
                term = "post_to_debrief_change", estimate, se = std.error,
                conf_low = conf.low, conf_high = conf.high, p_value = p.value, n,
                note = "negative = belief decreases after debrief"),
    block = "debrief_shift", sample = s
  ))
}

# ===================== 9. secondary outcomes ==================================

for (s in names(samples)) {
  sec <- secondary_model_contrasts(samples[[s]], s)
  add_rows(std_rows(
    sec %>% transmute(outcome, term = outcome_label, estimate, se = std.error,
                      conf_low = conf.low, conf_high = conf.high,
                      p_value = p.value, n,
                      note = paste0(group, "; bunk minus debunk, equal model weights, HC3")),
    block = "secondary_contrasts", sample = s
  ))
}

# ===================== 10. production_reference ===============================

add_rows(std_rows(
  d$prereg_tests %>%
    transmute(outcome, term = test, estimate, conf_low = ci_low, conf_high = ci_high,
              statistic = f, df_num, df_den, p_value = p,
              note = "cached production prereg test (N=1840)"),
  block = "production_reference_tests", sample = "production_n1840"
))
add_rows(std_rows(
  d$prereg_contrasts %>%
    transmute(outcome, term = contrast, estimate, se, statistic = t,
              conf_low = ci_low, conf_high = ci_high, p_value = p,
              note = "cached production model contrast (N=1840)"),
  block = "production_reference_contrasts", sample = "production_n1840"
))

# ===================== 11. admin / descriptives ===============================

admin <- d$s4 %>%
  mutate(
    start_date = as.Date(StartDate),
    duration_min = `Duration (in seconds)` / 60,
    age_num = suppressWarnings(as.numeric(Age))
  )
add_rows(std_rows(
  tibble(
    term = c("field_start", "field_end", "median_duration_min",
             "age_mean", "age_sd"),
    estimate = c(NA, NA,
                 median(admin$duration_min, na.rm = TRUE),
                 mean(admin$age_num, na.rm = TRUE),
                 sd(admin$age_num, na.rm = TRUE)),
    note = c(as.character(min(admin$start_date, na.rm = TRUE)),
             as.character(max(admin$start_date, na.rm = TRUE)),
             NA, NA, NA),
    n = nrow(admin)
  ),
  block = "admin", sample = "strict_n1272"
))
add_rows(std_rows(
  admin %>% count(Gender) %>%
    transmute(term = paste0("gender_code_", Gender), n = n, estimate = n / nrow(admin),
              note = "code mapping per QSF"),
  block = "admin_gender", sample = "strict_n1272"
))

# ===================== 12. pooled compliant symmetry test =====================
# Supplementary Table S26. Pools compliant conversations from Studies 1, 2, and
# 4 (excluding Study 3, whose truth constraint is the treatment under study,
# and excluding GPT-5.2, whose bunking rarely complied; GPT-5.2 is reported as
# a sensitivity row). Five variant levels: S1 jailbroken GPT-4o, S2 standard
# GPT-4o, S4 Claude / Gemini / Grok.
#
# Study 1-3 inputs come from build_s1s3_slim() (code/bunkbot_helpers.R), which
# reads data/processed_s1s3/*_clean.csv + the APE evaluator files. The
# published-sample filter is
#   isEquivocal == TRUE & 25 < pre_rc < 75 & !is.na(post_rc)
# (reproduces the published Ns 1,092 / 814 / 818; asserted below), with belief
# reverse-coded when the restatement was a denial so bunking raises it in
# every study. Compliance = evaluator_label == 1 & reverse_evaluator_label == 1.
#
# Model: aligned_belief_change ~ direction * variant + baseline_belief_c
# (baseline centered within study), OLS + HC3. Variant absorbs study (the two
# GPT-4o variants ARE studies 1-2), so no separate study fixed effects are
# estimable for those rows; this single parameterization is the one reported.
# Sign convention: aligned change is positive when belief moves in the
# assigned direction; contrasts are debunk minus bunk (positive = debunking
# advantage).

# `slim` is passed in (raw Studies 1-3 belief frame, built in the Rmd from the
# clean study files); the pooled-symmetry filtering happens here, unchanged.
s1s3_inclusive <- slim %>%
  mutate(
    pre_rc = if_else(category == "denies", 100 - belief_rating_pre_4, belief_rating_pre_4),
    post_rc = if_else(category == "denies", 100 - belief_rating_post_4, belief_rating_post_4)
  ) %>%
  filter(isEquivocal == TRUE, pre_rc > 25, pre_rc < 75, !is.na(post_rc)) %>%
  mutate(direction = dplyr::recode(condition,
                                   treatment_mid_bunk = "bunk",
                                   treatment_mid_debunk = "debunk"))

n_check <- s1s3_inclusive %>% count(study) %>% arrange(study)
# Duration screen removed (design decision, 2026-06-18): total study duration is a
# post-treatment variable, so the slim frame is no longer pre-filtered. The
# pooled-symmetry S1-3 Ns are now the full equivocal sample, 1092/814/818.
stopifnot(identical(n_check$n, c(1092L, 814L, 818L)))

# Reference block for the cross-study display: attempt rates (APE turn-1
# evaluator label) and within-condition aligned belief change, Studies 1-3.
s1s3_rates <- s1s3_inclusive %>%
  group_by(study, direction) %>%
  summarise(
    n = n(),
    attempt_rate = mean(evaluator_label, na.rm = TRUE),
    strict_rate = mean(evaluator_label == 1 & reverse_evaluator_label == 1, na.rm = TRUE),
    .groups = "drop"
  )
add_rows(std_rows(
  s1s3_rates %>%
    transmute(model = study, direction, term = "ape_attempt_rate", n,
              estimate = attempt_rate,
              se = sqrt(attempt_rate * (1 - attempt_rate) / n),
              note = paste0("mean turn-1 evaluator_label among published analytic sample; ",
                            "strict (both labels) rate = ", round(100 * strict_rate, 1), "%")),
  block = "s1s3_attempt_rates", sample = "s1s3_published"
))

s1s3_compliant <- s1s3_inclusive %>%
  filter(evaluator_label == 1, reverse_evaluator_label == 1) %>%
  mutate(
    aligned_belief_change = if_else(direction == "bunk", post_rc - pre_rc, pre_rc - post_rc),
    baseline_belief = pre_rc
  )

# Per-variant raw aligned means among compliant conversations (validation rows:
# Study 1 must reproduce the published ~ +13.7 bunking / ~ -12 debunking).
s1_bunk_mean <- s1s3_compliant %>%
  filter(study == "Study 1", direction == "bunk") %>%
  summarise(m = mean(aligned_belief_change)) %>% pull(m)
stopifnot(abs(s1_bunk_mean - 13.7) < 1)

sym_s1s2 <- s1s3_compliant %>%
  filter(study %in% c("Study 1", "Study 2")) %>%
  transmute(
    variant = if_else(study == "Study 1", "S1 jailbroken GPT-4o", "S2 standard GPT-4o"),
    study_id = study, direction, aligned_belief_change, baseline_belief
  )

sym_s4 <- d$s4_compliant %>%
  filter(model_pooled != "GPT-5.2") %>%
  transmute(
    variant = paste0("S4 ", as.character(model_pooled)),
    study_id = "Study 4", direction = as.character(direction),
    aligned_belief_change, baseline_belief = belief_rating_pre_4
  )

# Validate the S4 cells against the raw_aligned_means_cells compliant block
# before pooling (raw mean of aligned change must agree to < 0.05 points).
s4_cells_check <- sym_s4 %>%
  group_by(model = sub("^S4 ", "", variant), direction) %>%
  summarise(m = mean(aligned_belief_change), .groups = "drop")
s4_cells_ref <- bind_rows(rows) %>%
  filter(block == "raw_aligned_means_cells", sample == "compliant_n1056",
         outcome == "aligned_belief_change", model != "GPT-5.2") %>%
  select(model, direction, ref = estimate)
cells_join <- inner_join(s4_cells_check, s4_cells_ref, by = c("model", "direction"))
stopifnot(nrow(cells_join) == 6, all(abs(cells_join$m - cells_join$ref) < 0.05))

variant_levels <- c("S1 jailbroken GPT-4o", "S2 standard GPT-4o",
                    "S4 Claude", "S4 Gemini", "S4 Grok")
sym <- bind_rows(sym_s1s2, sym_s4) %>%
  group_by(study_id) %>%
  mutate(baseline_belief_c = baseline_belief - mean(baseline_belief)) %>%
  ungroup() %>%
  mutate(
    direction = factor(direction, levels = c("debunk", "bunk")),
    variant = factor(variant, levels = variant_levels)
  )

add_rows(std_rows(
  sym %>% group_by(model = as.character(variant), direction = as.character(direction)) %>%
    summarise(n = n(), estimate = mean(aligned_belief_change),
              se = sd(aligned_belief_change) / sqrt(n()), .groups = "drop") %>%
    mutate(conf_low = estimate - 1.96 * se, conf_high = estimate + 1.96 * se,
           term = "raw_aligned_mean",
           note = "compliant conversations; positive = movement in assigned direction"),
  block = "pooled_symmetry_cells", sample = "pooled_compliant",
  outcome = "aligned_belief_change"
))

fit_sym <- lm(aligned_belief_change ~ direction * variant + baseline_belief_c, data = sym)
vc_sym <- fit_vcov(fit_sym)

sym_grid <- function(dir) {
  tibble(
    direction = factor(dir, levels = c("debunk", "bunk")),
    variant = factor(variant_levels, levels = variant_levels),
    baseline_belief_c = 0
  )
}
x_deb <- model_matrix_for_fit(fit_sym, sym_grid("debunk"))
x_bunk <- model_matrix_for_fit(fit_sym, sym_grid("bunk"))

per_variant <- map_dfr(seq_along(variant_levels), function(i) {
  linear_combo(fit_sym, x_deb[i, ] - x_bunk[i, ], vc = vc_sym) %>%
    mutate(model = variant_levels[i],
           n = sum(sym$variant == variant_levels[i]))
})
add_rows(std_rows(
  per_variant %>%
    transmute(model, term = "debunk_minus_bunk", n, estimate, se = std.error,
              conf_low = conf.low, conf_high = conf.high, p_value = p.value,
              note = "baseline-adjusted, HC3; positive = debunking advantage"),
  block = "pooled_symmetry_test", sample = "pooled_compliant",
  outcome = "aligned_belief_change"
))

avg_contrast <- linear_combo(fit_sym, colMeans(x_deb) - colMeans(x_bunk), vc = vc_sym)
add_rows(std_rows(
  avg_contrast %>%
    transmute(term = "debunk_minus_bunk_equal_weighted", n = nrow(sym),
              estimate, se = std.error, conf_low = conf.low, conf_high = conf.high,
              p_value = p.value,
              note = "equal weights across 5 variants, HC3; positive = debunking advantage"),
  block = "pooled_symmetry_test", sample = "pooled_compliant",
  outcome = "aligned_belief_change"
))

int_terms <- grep("^direction.*:variant", names(coef(fit_sym)), value = TRUE)
omni <- car::linearHypothesis(fit_sym, hyp_matrix(fit_sym, int_terms),
                              vcov. = vc_sym, test = "F")
add_rows(std_rows(
  tibble(term = "direction_x_variant_omnibus", n = nrow(sym),
         statistic = omni$F[2], df_num = omni$Df[2], df_den = omni$Res.Df[2],
         p_value = omni$`Pr(>F)`[2],
         note = "robust F (HC3) for direction x variant interaction"),
  block = "pooled_symmetry_test", sample = "pooled_compliant",
  outcome = "aligned_belief_change"
))

# Sensitivity: re-estimate with GPT-5.2 included as a sixth variant.
sym6 <- bind_rows(
  sym_s1s2,
  d$s4_compliant %>%
    transmute(variant = paste0("S4 ", as.character(model_pooled)),
              study_id = "Study 4", direction = as.character(direction),
              aligned_belief_change, baseline_belief = belief_rating_pre_4)
) %>%
  group_by(study_id) %>%
  mutate(baseline_belief_c = baseline_belief - mean(baseline_belief)) %>%
  ungroup() %>%
  mutate(direction = factor(direction, levels = c("debunk", "bunk")),
         variant = factor(variant, levels = c(variant_levels, "S4 GPT-5.2")))
fit_sym6 <- lm(aligned_belief_change ~ direction * variant + baseline_belief_c, data = sym6)
vc_sym6 <- fit_vcov(fit_sym6)
lv6 <- levels(sym6$variant)
grid6 <- function(dir) tibble(direction = factor(dir, levels = c("debunk", "bunk")),
                              variant = factor(lv6, levels = lv6), baseline_belief_c = 0)
x6_deb <- model_matrix_for_fit(fit_sym6, grid6("debunk"))
x6_bunk <- model_matrix_for_fit(fit_sym6, grid6("bunk"))
avg6 <- linear_combo(fit_sym6, colMeans(x6_deb) - colMeans(x6_bunk), vc = vc_sym6)
gpt52_row <- linear_combo(fit_sym6, x6_deb[6, ] - x6_bunk[6, ], vc = vc_sym6)
add_rows(std_rows(
  bind_rows(
    avg6 %>% transmute(term = "debunk_minus_bunk_equal_weighted_incl_gpt52",
                       n = nrow(sym6), estimate, se = std.error,
                       conf_low = conf.low, conf_high = conf.high, p_value = p.value,
                       note = "sensitivity: 6 variants incl GPT-5.2 (compliance-limited), HC3"),
    gpt52_row %>% transmute(term = "debunk_minus_bunk_gpt52_only",
                            n = sum(sym6$variant == "S4 GPT-5.2"), estimate,
                            se = std.error, conf_low = conf.low, conf_high = conf.high,
                            p_value = p.value,
                            note = "sensitivity row: GPT-5.2 compliant conversations only (bunk n is small)")
  ),
  block = "pooled_symmetry_test", sample = "pooled_compliant_incl_gpt52",
  outcome = "aligned_belief_change"
))

# ===================== 13. distributional replication =========================
# Study 4 compliant-sample analogue of the Study 1 distributional analysis
# (Figure 2B): KS test comparing the bunking vs debunking distributions of
# direction-aligned belief change, plus the share of conversations with
# >= 40-point aligned shifts.

dist_dat <- d$s4_compliant %>%
  filter(!is.na(aligned_belief_change)) %>%
  mutate(direction = as.character(direction))
ks_res <- suppressWarnings(
  ks.test(dist_dat$aligned_belief_change[dist_dat$direction == "bunk"],
          dist_dat$aligned_belief_change[dist_dat$direction == "debunk"])
)
big_shift <- dist_dat %>%
  group_by(direction) %>%
  summarise(n = n(), prop_ge40 = mean(aligned_belief_change >= 40), .groups = "drop")
chi_res <- suppressWarnings(
  chisq.test(table(dist_dat$direction, dist_dat$aligned_belief_change >= 40))
)
add_rows(std_rows(
  bind_rows(
    tibble(term = "ks_D", n = nrow(dist_dat), estimate = unname(ks_res$statistic),
           p_value = ks_res$p.value,
           note = "KS test, bunk vs debunk aligned belief change distributions (compliant)"),
    big_shift %>% transmute(direction, term = "prop_aligned_change_ge40", n,
                            estimate = prop_ge40,
                            note = "share of compliant conversations with >= 40-pt aligned shift"),
    tibble(term = "ge40_chisq", n = nrow(dist_dat),
           statistic = unname(chi_res$statistic), df_num = unname(chi_res$parameter),
           p_value = chi_res$p.value,
           note = "chi-square test of >= 40-pt shift share, bunk vs debunk")
  ),
  block = "distributional_replication", sample = "compliant_n1056",
  outcome = "aligned_belief_change"
))

# ===================== 14. public speech: composition + re-sharing ============
# Quantities behind the "What the conversation did to public speech" figure
# (s4_figure_speech.R) and its SI companions. Logic deliberately mirrors the
# figure script so the figure and these citable blocks cannot drift apart.

s4w <- d$s4_with_compliance

collapse_stance5 <- function(x) dplyr::case_when(
  x == "argues_for" ~ "endorses",
  x == "leans_for" ~ "leans_for",
  x %in% c("neutral_uncommitted", "mixed_both_sides") ~ "neutral_mixed",
  x == "leans_against" ~ "leans_against",
  x == "argues_against" ~ "rejects",
  TRUE ~ "no_stance"
)
collapse_orig4 <- function(stance, rtype) dplyr::case_when(
  rtype == "declines_to_post" | stance == "not_applicable" ~ "no_stance",
  stance %in% c("argues_for", "leans_for") ~ "pro_leaning",
  stance %in% c("argues_against", "leans_against") ~ "anti_leaning",
  TRUE ~ "neutral_mixed"
)

# 14a. stance composition of posts, before/after, by display group
speech_groups <- list(
  debunk_all_models = s4w %>% filter(direction == "debunk"),
  bunk_complying_models = s4w %>% filter(direction == "bunk", model_pooled != "GPT-5.2"),
  bunk_gpt52 = s4w %>% filter(direction == "bunk", model_pooled == "GPT-5.2")
)
comp_rows <- purrr::imap_dfr(speech_groups, function(g, glab) {
  purrr::map_dfr(c(before = "pre", after = "post"), function(tp) {
    cats <- collapse_stance5(g[[paste0(tp, "_stance_category")]])
    tibble(cat = cats) %>%
      count(cat) %>%
      mutate(share = 100 * n / sum(n), time = tp, grp = glab)
  })
})
add_rows(std_rows(
  comp_rows %>%
    transmute(direction = grp, term = paste(time, cat, sep = "_"),
              n, estimate = share,
              note = "share (%) of all posts in group; categories collapse the 7-level stance/response coding (figure panel a uses stance-taking posts only)"),
  block = "post_stance_composition", sample = "strict_n1272",
  outcome = "post_stance_category"
))

# 14b. form (response type) composition, pooled and by direction
form_rows <- purrr::map_dfr(list(pooled = s4w,
                                 bunk = filter(s4w, direction == "bunk"),
                                 debunk = filter(s4w, direction == "debunk")),
                            function(g) {
  purrr::map_dfr(c(before = "pre", after = "post"), function(tp) {
    tibble(cat = g[[paste0(tp, "_response_type")]]) %>%
      filter(!is.na(cat)) %>%
      count(cat) %>%
      mutate(share = 100 * n / sum(n), time = tp)
  })
}, .id = "grp")
add_rows(std_rows(
  form_rows %>%
    transmute(direction = grp, term = paste(time, cat, sep = "_"),
              n, estimate = share,
              note = "share (%) of posts by dominant communicative mode (five-rater consensus)"),
  block = "post_form_composition", sample = "strict_n1272",
  outcome = "post_response_type"
))

# 14c. willingness to re-share the ORIGINAL post: raw change by direction
resh <- s4w %>%
  filter(!is.na(share_original_post_now_4), !is.na(share_pre_4)) %>%
  mutate(delta = share_original_post_now_4 - share_pre_4,
         orig = collapse_orig4(pre_stance_category, pre_response_type),
         dest = dplyr::case_when(
           post_response_type == "declines_to_post" ~ "declined_to_post",
           TRUE ~ collapse_stance5(post_stance_category)))
mean_ci_rows <- function(df, by) {
  df %>%
    group_by(across(all_of(by))) %>%
    summarise(n = n(), estimate = mean(delta), se = sd(delta) / sqrt(n()),
              .groups = "drop") %>%
    mutate(conf_low = estimate - 1.96 * se, conf_high = estimate + 1.96 * se)
}
add_rows(std_rows(
  mean_ci_rows(resh, "direction") %>%
    mutate(term = "raw_share_willingness_change",
           note = "share_original_post_now - share_pre, raw points; the registered stance-weighted version is in aligned_contrasts"),
  block = "reshare_original_raw", sample = "strict_n1272",
  outcome = "share_original_post_change"
))

# 14d. by ORIGINAL post stance x direction (pre-treatment strata: each
# within-stratum direction contrast is a randomized comparison) + tests
cells14 <- mean_ci_rows(resh, c("orig", "direction"))
add_rows(std_rows(
  cells14 %>% transmute(term = orig, direction, n, estimate, se,
                        conf_low, conf_high,
                        note = "mean change in willingness to share the original post"),
  block = "reshare_original_by_orig_stance", sample = "strict_n1272",
  outcome = "share_original_post_change"
))
tests14 <- cells14 %>%
  select(orig, direction, estimate, se) %>%
  tidyr::pivot_wider(names_from = direction, values_from = c(estimate, se)) %>%
  mutate(diff = estimate_bunk - estimate_debunk,
         se_d = sqrt(se_bunk^2 + se_debunk^2),
         z = diff / se_d,
         p = 2 * pnorm(abs(z), lower.tail = FALSE))
add_rows(std_rows(
  tests14 %>% transmute(term = orig, estimate = diff, se = se_d,
                        conf_low = diff - 1.96 * se_d,
                        conf_high = diff + 1.96 * se_d,
                        statistic = z, p_value = p,
                        note = "bunk - debunk contrast within pre-treatment stratum (normal approx)"),
  block = "reshare_original_by_orig_stance_tests", sample = "strict_n1272",
  outcome = "share_original_post_change"
))

# 14e. by NEW-post destination (pooled; post-treatment conditioning -> SI only)
add_rows(std_rows(
  mean_ci_rows(resh, "dest") %>%
    transmute(term = dest, n, estimate, se, conf_low, conf_high,
              note = "descriptive only: groups defined by a post-treatment outcome"),
  block = "reshare_original_by_destination", sample = "strict_n1272",
  outcome = "share_original_post_change"
))

# ===================== Assemble, QA, write ====================================

numbers <- bind_rows(rows)

# Mechanical QA
ci_rows <- numbers %>% filter(!is.na(conf_low), !is.na(conf_high), !is.na(estimate))
stopifnot(all(ci_rows$conf_low <= ci_rows$estimate + 1e-9))
stopifnot(all(ci_rows$conf_high >= ci_rows$estimate - 1e-9))
p_rows <- numbers %>% filter(!is.na(p_value))
stopifnot(all(p_rows$p_value >= 0 & p_rows$p_value <= 1))
cell_check <- numbers %>%
  filter(block == "cell_n", sample == "strict_n1272") %>%
  summarise(total = sum(n))
stopifnot(cell_check$total == 1272)
cell_check_c <- numbers %>%
  filter(block == "cell_n", sample == "compliant_n1056") %>%
  summarise(total = sum(n))
# Sample-aware: the compliant subsample is 1,056 in the canonical build and 1,073
# after the supplement's APE-rescore coverage-gap resolution; check against the
# actual compliant N rather than a hardcoded constant.
stopifnot(cell_check_c$total == nrow(d$s4_compliant))

  numbers
}
