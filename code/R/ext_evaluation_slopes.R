# ext_evaluation_slopes.R -----------------------------------------------------------------
# "LIE-BLINDNESS" SLOPES: do participants' post-conversation evaluations of the AI track how
# truthful the AI actually was in their own conversation, versus how far it moved them?
#
# Added 2026-07-07: the
# manuscript adds the two-slope contrast (evaluations are ~flat in the conversation's actual
# falsehood share but strongly track direction-aligned belief change). This module is the
# PRIMARY specification of that analysis so the numbers live in the one
# recompute:
#
#   z(outcome) ~ z(predictor) + study FE + topic FE + baseline belief,  HC3 robust SEs,
#   Studies 1-3, one arm at a time, conversations with >= 1 aligned claim.
#
# Predictors (term column):
#   * falsehood_share = share of the conversation's ALIGNED claims (aligned_flag(): stance
#     matches assigned direction, directness direct/indirect) whose cached veracity score is
#     < 40/100 -- the package's low-veracity-tail cutoff (see ext_veracity_tail.R). Veracity
#     is used UNclipped here (matching the source analysis; the lone out-of-range 875 value
#     counts as not-low, exactly as `score < 40` evaluates).
#   * belief_change   = direction-aligned belief change (`change` on the s13 frame). Both
#     predictor and outcome are post-treatment, so these rows are associations, not causal
#     directions.
#   * *_raw           = no-FE / no-covariate bivariate sensitivity slope (composite only,
#     Bunking arm), for reviewers checking FE sensitivity.
#
# Outcomes (outcome column; S4-consistent recodes as in ext_s13.R): the four perception items
# (arg_strength 1-5; new_info Qualtrics 1,4..12 -> 1..10; collaborative/unbiased 69..73 ->
# -2..2), pre-to-post trust-in-AI change (trust2 - genai_trust, 1-7 scale), and an
# equal-weight composite of the four items (each item z-scored on the FULL s13 frame before
# arm subsetting; conversations missing any item get NA).
#
# Estimation details (verbatim from the source analysis):
#   * estimation sample = arm x (n_aligned >= 1) x complete cases on outcome, predictor,
#     FE columns, and baseline belief;
#   * outcome and predictor are z-scored WITHIN that estimation sample, so `estimate` is a
#     fully standardized beta;
#   * topic FE = data/api_cached/topic_modeling/topic_assignments.csv joined on response_id
#     (unmatched -> "(unmatched)"); FE levels with < 5 obs in the estimation sample are
#     pooled into "(other)" (singleton FE cells make HC3 hat values ~ 1 / unstable);
#   * HC3 (sandwich::vcovHC) t inference; conf ints use qt(.975, resid df).
#   * falsehood rows also carry a TOST equivalence test against |beta| < .10 and < .15 SD
#     in the note (p = max of the two one-sided HC3 t-tests).
#
# POOLED S1-4 ROWS: the identical
# two-slope spec re-estimated on Studies 1-4 combined. Study 4 enters via the strict observed-outcome
# frame (core_objects$s4$s4_with_compliance, n=1272), same >=1-aligned-claim eligibility;
# S4 falsehood share comes from the same read_claim_labels() call (claim_role_labels_s2s4.csv);
# S4 items are the SAME instrument the S1-3 recodes were built to match (ArgStrength 1-5,
# new_info Qualtrics 1,4..12 -> 1..10, Unbiased/Collaborative 69..73 -> -2..2; see ext_s13.R,
# compute_perception_s4_by_model); S4 persuasion predictor = aligned_belief_change (the
# direction-aligned analogue of s13 `change`); S4 baseline belief = belief_rating_pre_4
# (orientation-corrected, 25-75 screened, same footing as belief_rating_pre_rc). The study FE
# extends to the paper's 7 study/model STRATA convention (3 GPT-4o variants + 4 frontier
# models; cf. compute_pooled_trust_change in ext_s4_trust.R); topic FE and baseline-belief
# control as before. The pooled composite is rebuilt with items z-scored on the pooled S1-4
# frame. CAUTION carried in the notes: S4 falsehood share varies mostly BETWEEN models
# (GPT-5.2 ~ all-true vs Grok/Gemini ~ mostly-false), so the strata FE absorb the
# model-identity confound and the pooled slope leans on S1-3 + within-model S4 variation;
# the notes report the between-strata share of falsehood-share variance and GPT-5.2's
# low aligned-claim retention in the bunking arm.
#
# COMPLIANT-FRAME ROWS (design decision, 2026-07-07): the bunking arm's large "backfires"
# (aligned belief change < -20) are dominated by MODEL NON-COMPLIANCE -- in S1-3 only ~63% of
# that bin is compliant (vs ~91-96% elsewhere) and the non-compliant backfires carry the
# LARGEST trust gains; in S4 the bin is ~81% GPT-5.2, which refuses bunking and debunks
# instead. Assignment-aligned belief change therefore mismeasures de facto persuasion for
# non-compliant conversations (participants reward whoever moved them, including a model that
# disobeyed its instructions), so the module also emits the identical two-slope spec on the
# paper's established COMPLIANT subsets, where assigned direction = de facto direction:
#   * S1-3: evaluator_label == 1 & reverse_evaluator_label == 1 (the APE both-labels filter);
#   * S4:   core_objects$s4$s4_compliant (n = 1073).
# Compliant frames are strict row-subsets of the observed-outcome frames above: every variable (including
# the composite, whose items stay z-scored on the parent observed-outcome frame) is IDENTICAL, so any
# observed-outcome-vs-compliant slope difference is pure sample composition, not variable redefinition
# (.es_fit re-z-scores outcome and predictor within each estimation sample regardless).
# Same >=1-aligned-claim eligibility, FE, controls, outcomes, terms (incl. the bunking-arm
# *_raw sensitivity rows), and TOST notes.
#
# BELIEF-CHANGE-AS-OUTCOME ROWS (added 2026-07-07, for the main-text integration): one row
# per frame x arm with outcome "Aligned belief change (points)" -- aligned belief change in
# RAW POINTS (outcome deliberately NOT z-scored, so the manuscript can report "X points per
# SD of falsehood share") regressed on z(falsehood_share), same FE spec/controls/HC3, same
# >=1-aligned-claim eligibility. These quantify the within-arm falsehood -> persuasion
# gradient itself (endogenous falsehood deployment biases it toward zero; the causal
# truth-constraint cost stays identified by the randomized S2-vs-S3 contrast). For these
# rows `estimate` is in belief-scale points, NOT an SD-standardized beta, so no TOST note.
#
# FLOOR0 DISPLAY-MODEL ROWS (added 2026-07-07, Fig 5b): term "belief_change_floor0",
# composite outcome, S1-4 pooled frames only, both arms, both samples: standardized beta on
# z(pmax(aligned change, 0)) -- all backfire/no-move conversations collapsed into the x = 0
# category before z-scoring, same FE spec. This is the DISPLAY model for the main-text
# figure's positive-range panel (points and drawn line cohere exactly on the shown axis);
# the substantive persuasion-coupling estimate remains term "belief_change".
#
# ARM-CONTRAST ROWS (added 2026-07-07, main-text integration): is Bunking rated more
# favorably than Debunking overall? term "arm_bunk_minus_debunk" regresses the z-scored
# composite (and trust change) on a 0/1 Bunking indicator (Debunking reference; indicator
# NOT scaled, so `estimate` is the between-arm gap in SD units) with the arm-common
# FE/controls (strata + topic + baseline belief), HC3; pooled S1-4, both samples;
# direction = "Bunking - Debunking". Arm is randomized, but the >=1-aligned-claim (and,
# for sample=compliant, compliance) eligibility conditions on post-treatment variables --
# noted in the rows; the unconditional randomized contrast is the paper's main ATE block.
# term "arm_x_falsehood" (compliant pooled, composite only) adds the arm x z(falsehood
# share) interaction -- whether the gap is constant across the veracity spectrum --
# flagged DESCRIPTIVE in the note (falsehood share is post-treatment w.r.t. arm).
#
# ENTRY POINT: compute_evaluation_slopes(core_objects) -> tibble (canonical 17-col schema).
#   section "S1-3"; block "evaluation_slopes"; sample = "aligned_ge1" (observed-outcome frames, the
#   original rows) or "compliant" (compliant frames).
#   model = "S1-3 pooled" / "S1-4 pooled"; direction = "Bunking" (primary) / "Debunking"
#   (or "Bunking - Debunking" for the arm-contrast terms);
#   term = falsehood_share / belief_change (+ composite-only *_raw sensitivity rows,
#   arm_bunk_minus_debunk, arm_x_falsehood);
#   estimate = standardized beta (EXCEPT outcome "Aligned belief change (points)": raw
#   points per 1 SD of falsehood share; and the arm terms: SD-unit gaps/slope differences);
#   statistic = HC3 t; df_den = residual df.
#   NOTE: rows are unique on (model, direction, term, outcome, sample) -- model AND sample
#   are REQUIRED in any manifest filter now that both pooled variants and both frames live
#   in the block.

.es_outcomes <- c(
  arg_strength_rc  = "Argument strength",
  new_info_rc      = "Provided new information",
  collaborative_rc = "Collaborativeness",
  unbiased_rc      = "Impartiality (unbiased)",
  trust_change     = "Trust in AI (pre-to-post change)",
  eval_composite   = "Evaluation composite (4 items)"
)
.es_preds <- c(fal_share = "falsehood_share", change = "belief_change")

.es_recode_newinfo <- function(x) dplyr::case_when(
  x == 1 ~ 1, x == 4 ~ 2, x == 5 ~ 3, x == 6 ~ 4, x == 7 ~ 5,
  x == 8 ~ 6, x == 9 ~ 7, x == 10 ~ 8, x == 11 ~ 9, x == 12 ~ 10,
  TRUE ~ NA_real_)
.es_recode_pm2 <- function(x) dplyr::case_when(
  x == 69 ~ -2, x == 70 ~ -1, x == 71 ~ 0, x == 72 ~ 1, x == 73 ~ 2,
  TRUE ~ NA_real_)

# Standardized-slope fit for one outcome x predictor cell. Returns a one-row tibble of the
# inference pieces or NULL if the cell is degenerate. scale_y = FALSE leaves the outcome in
# its raw units (used for the "Aligned belief change (points)" rows); scale_x = FALSE
# leaves the predictor unscaled (used for the 0/1 arm indicator, where the coefficient
# must be the group gap); otherwise both are z-scored within the estimation sample.
.es_fit <- function(dat, outcome, predictor, fe = character(0), controls = character(0),
                    scale_y = TRUE, scale_x = TRUE) {
  keep <- c(outcome, predictor, fe, controls)
  d <- dat[stats::complete.cases(dat[, keep, drop = FALSE]), keep, drop = FALSE]
  if (nrow(d) < 30) return(NULL)
  # Pool rare FE levels (< 5 obs in the estimation sample) into "(other)".
  for (fcol in fe) {
    v <- as.character(d[[fcol]])
    tab <- table(v)
    v[v %in% names(tab)[tab < 5]] <- "(other)"
    d[[fcol]] <- v
  }
  d$.y <- if (scale_y) as.numeric(scale(d[[outcome]])) else as.numeric(d[[outcome]])
  d$.x <- if (scale_x) as.numeric(scale(d[[predictor]])) else as.numeric(d[[predictor]])
  rhs <- c(".x",
           if (length(controls)) controls,
           if (length(fe)) paste0("factor(", fe, ")"))
  m  <- stats::lm(stats::as.formula(paste(".y ~", paste(rhs, collapse = " + "))), data = d)
  ct <- lmtest::coeftest(m, vcov. = sandwich::vcovHC(m, type = "HC3"))
  i  <- which(rownames(ct) == ".x")
  est <- ct[i, 1]; se <- ct[i, 2]; df <- m$df.residual
  tcrit <- stats::qt(.975, df)
  tost <- function(delta) max(
    stats::pt((est + delta) / se, df, lower.tail = FALSE),  # H0: beta <= -delta
    stats::pt((est - delta) / se, df, lower.tail = TRUE))   # H0: beta >= +delta
  tibble::tibble(
    n = nrow(d), estimate = est, se = se,
    conf_low = est - tcrit * se, conf_high = est + tcrit * se,
    statistic = ct[i, 3], df_den = df, p_value = ct[i, 4],
    tost10 = tost(.10), tost15 = tost(.15))
}

.es_note <- function(fit, predictor, spec_txt, extra = "") {
  base <- sprintf(
    "Standardized beta (outcome and predictor z-scored within the estimation sample); %s; HC3.",
    spec_txt)
  if (grepl("^falsehood_share", predictor)) {
    base <- paste0(base, sprintf(
      " TOST equivalence: p=%.4g at |beta|<.10, p=%.4g at |beta|<.15.",
      fit$tost10, fit$tost15))
  }
  paste0(base, extra)
}

compute_evaluation_slopes <- function(core_objects) {
  si_require(c("dplyr", "tibble", "readr", "sandwich", "lmtest"))
  paths <- pkg_paths(core_objects$pkg_root)

  # -- conversation-level falsehood share from the cached claim labels ---------------------
  labels <- read_claim_labels(paths)
  labels$is_aligned <- aligned_flag(labels$stance_to_focal, labels$directness_to_focal,
                                    labels$direction)
  conv <- labels |>
    dplyr::group_by(conversation_id) |>
    dplyr::summarise(
      n_aligned = sum(is_aligned & !is.na(veracity_score)),
      fal_share = ifelse(n_aligned > 0,
                         mean(veracity_score[is_aligned] < 40, na.rm = TRUE),
                         NA_real_),
      .groups = "drop")

  topics <- readr::read_csv(paths$topic_assignments, show_col_types = FALSE, progress = FALSE) |>
    dplyr::distinct(.data$response_id, .keep_all = TRUE) |>
    dplyr::select("response_id", "topic")

  # -- S1-3 analysis frame (recodes verbatim from the source analysis / ext_s13.R) ---------
  zfull <- function(x) as.numeric(scale(x))
  d13 <- core_objects$s13 |>
    dplyr::left_join(conv, by = c("response_id" = "conversation_id")) |>
    dplyr::left_join(topics, by = "response_id") |>
    dplyr::mutate(
      arg_strength_rc  = suppressWarnings(as.numeric(.data$arg_strength)),
      new_info_rc      = .es_recode_newinfo(suppressWarnings(as.numeric(.data$new_info))),
      collaborative_rc = .es_recode_pm2(suppressWarnings(as.numeric(.data$collaborative))),
      unbiased_rc      = .es_recode_pm2(suppressWarnings(as.numeric(.data$unbiased))),
      trust_change     = suppressWarnings(as.numeric(.data$trust2) - as.numeric(.data$genai_trust)),
      topic            = ifelse(is.na(.data$topic), "(unmatched)", .data$topic))
  d13$eval_composite <- rowMeans(cbind(zfull(d13$arg_strength_rc), zfull(d13$new_info_rc),
                                       zfull(d13$collaborative_rc), zfull(d13$unbiased_rc)),
                                 na.rm = FALSE)

  # -- S4 strict observed-outcome frame + pooled S1-4 frame --------------------------------
  # 7 study/model strata (Jailbroken/Standard/Truth-Constrained + Claude/Gemini/GPT-5.2/Grok);
  # S4 items are the reference instrument for the S1-3 recodes, so they enter unrecoded except
  # for the shared new_info / 69..73 maps. The pooled composite is re-z-scored on the pooled
  # frame (a composite is only meaningful relative to the frame it is standardized on).
  s4 <- core_objects$s4$s4_with_compliance |>
    dplyr::transmute(
      response_id      = .data$ResponseId,
      is_compliant     = .data$ResponseId %in% core_objects$s4$s4_compliant$ResponseId,
      stratum          = as.character(.data$model_pooled),
      condition_factor = dplyr::recode(as.character(.data$direction),
                                       bunk = "Bunking", debunk = "Debunking"),
      arg_strength_rc  = suppressWarnings(as.numeric(.data$ArgStrength)),
      new_info_rc      = .es_recode_newinfo(suppressWarnings(as.numeric(.data$new_info))),
      collaborative_rc = .es_recode_pm2(suppressWarnings(as.numeric(.data$Collaborative))),
      unbiased_rc      = .es_recode_pm2(suppressWarnings(as.numeric(.data$Unbiased))),
      trust_change     = suppressWarnings(as.numeric(.data$trust2) - as.numeric(.data$genai_trust)),
      change           = as.numeric(.data$aligned_belief_change),
      belief_pre       = suppressWarnings(as.numeric(.data$belief_rating_pre_4))) |>
    dplyr::left_join(conv, by = c("response_id" = "conversation_id")) |>
    dplyr::left_join(topics, by = "response_id") |>
    dplyr::mutate(topic = ifelse(is.na(.data$topic), "(unmatched)", .data$topic))

  pool <- dplyr::bind_rows(
    d13 |> dplyr::transmute(
      response_id = .data$response_id,
      is_compliant = .data$evaluator_label == 1 & .data$reverse_evaluator_label == 1,
      stratum = as.character(.data$study_factor),
      condition_factor = as.character(.data$condition_factor),
      arg_strength_rc = .data$arg_strength_rc, new_info_rc = .data$new_info_rc,
      collaborative_rc = .data$collaborative_rc, unbiased_rc = .data$unbiased_rc,
      trust_change = .data$trust_change, change = .data$change,
      belief_pre = .data$belief_rating_pre_rc,
      n_aligned = .data$n_aligned, fal_share = .data$fal_share, topic = .data$topic),
    s4)
  pool$eval_composite <- rowMeans(cbind(zfull(pool$arg_strength_rc), zfull(pool$new_info_rc),
                                        zfull(pool$collaborative_rc), zfull(pool$unbiased_rc)),
                                  na.rm = FALSE)

  # Compliant frames = strict row-subsets of the observed-outcome frames (variables, incl. the parent-frame
  # composite, untouched); see header. S1-3 = APE both-labels; S4 = the s4_compliant frame.
  d13_c  <- d13[!is.na(d13$evaluator_label) & d13$evaluator_label == 1 &
                  !is.na(d13$reverse_evaluator_label) & d13$reverse_evaluator_label == 1, ,
                drop = FALSE]
  pool_c <- pool[!is.na(pool$is_compliant) & pool$is_compliant, , drop = FALSE]
  s4_c   <- s4[s4$is_compliant, , drop = FALSE]

  raw_txt <- "bivariate no-FE / no-covariate sensitivity (same estimation sample logic)"
  fe_txt_s13 <- "study FE + topic FE (HDBSCAN assignments; FE levels with <5 obs pooled) + baseline belief (belief_rating_pre_rc)"
  fe_txt_s14 <- "7 study/model strata FE (3 GPT-4o variants + 4 frontier models) + topic FE (levels with <5 obs pooled) + baseline belief"
  frames <- list(
    list(dat = d13, model_lab = "S1-3 pooled", sample = "aligned_ge1", samp_txt = "",
         fe = c("study_factor", "topic"), ctrl = "belief_rating_pre_rc", strata = "study_factor",
         fe_txt = fe_txt_s13, tag = "S1-3", s4_ref = NULL),
    list(dat = pool, model_lab = "S1-4 pooled", sample = "aligned_ge1", samp_txt = "",
         fe = c("stratum", "topic"), ctrl = "belief_pre", strata = "stratum",
         fe_txt = fe_txt_s14, tag = "S1-4", s4_ref = s4),
    list(dat = d13_c, model_lab = "S1-3 pooled", sample = "compliant",
         samp_txt = "COMPLIANT (evaluator_label==1 & reverse_evaluator_label==1) ",
         fe = c("study_factor", "topic"), ctrl = "belief_rating_pre_rc", strata = "study_factor",
         fe_txt = fe_txt_s13, tag = "S1-3", s4_ref = NULL),
    list(dat = pool_c, model_lab = "S1-4 pooled", sample = "compliant",
         samp_txt = "COMPLIANT (S1-3: APE both-labels; S4: s4_compliant frame) ",
         fe = c("stratum", "topic"), ctrl = "belief_pre", strata = "stratum",
         fe_txt = fe_txt_s14, tag = "S1-4", s4_ref = s4_c))

  rows <- list()
  for (fr in frames) {
    for (arm in c("Bunking", "Debunking")) {
      dat <- fr$dat[as.character(fr$dat$condition_factor) == arm & !is.na(fr$dat$n_aligned) &
                      fr$dat$n_aligned >= 1, , drop = FALSE]
      spec_txt <- sprintf("%s %s arm, %sconversations with >=1 aligned claim; %s",
                          fr$tag, arm, fr$samp_txt, fr$fe_txt)
      # Falsehood-share confounding diagnostics for the pooled rows: how much of the
      # predictor's variance is BETWEEN the strata (absorbed by the FE) vs within.
      extra_fal <- ""
      if (fr$model_lab == "S1-4 pooled") {
        r2b <- summary(stats::lm(fal_share ~ factor(stratum), data = dat))$r.squared
        gpt <- fr$s4_ref[fr$s4_ref$condition_factor == arm, , drop = FALSE]
        extra_fal <- sprintf(
          paste0(" Pooled falsehood-share variance: %.0f%% between the 7 strata (absorbed",
                 " by FE), %.0f%% within; GPT-5.2 %s-arm aligned-claim retention %d/%d",
                 " (near-zero falsehood variance), so the pooled slope leans on S1-3 and",
                 " within-model S4 variation."),
          100 * r2b, 100 * (1 - r2b), tolower(arm),
          sum(gpt$stratum == "GPT-5.2" & !is.na(gpt$n_aligned) & gpt$n_aligned >= 1),
          sum(gpt$stratum == "GPT-5.2"))
      }
      for (pv in names(.es_preds)) {
        for (ov in names(.es_outcomes)) {
          fit <- .es_fit(dat, ov, pv, fe = fr$fe, controls = fr$ctrl)
          if (is.null(fit)) next
          rows[[length(rows) + 1]] <- dplyr::mutate(
            fit[, setdiff(names(fit), c("tost10", "tost15"))],
            outcome = .es_outcomes[[ov]], model = fr$model_lab, direction = arm,
            term = .es_preds[[pv]], sample = fr$sample,
            note = .es_note(fit, .es_preds[[pv]], spec_txt,
                            extra = if (pv == "fal_share") extra_fal else ""))
        }
      }
      # Belief-change-as-OUTCOME row: aligned belief change in RAW points on z(falsehood
      # share). Same spec; outcome NOT z-scored (points per 1 SD of falsehood share).
      fit <- .es_fit(dat, "change", "fal_share", fe = fr$fe, controls = fr$ctrl,
                     scale_y = FALSE)
      if (!is.null(fit)) {
        rows[[length(rows) + 1]] <- dplyr::mutate(
          fit[, setdiff(names(fit), c("tost10", "tost15"))],
          outcome = "Aligned belief change (points)", model = fr$model_lab, direction = arm,
          term = "falsehood_share", sample = fr$sample,
          note = sprintf(
            paste0("Aligned belief change in RAW points (outcome NOT z-scored) regressed on",
                   " z(falsehood_share); estimate = points per 1 SD of falsehood share;",
                   " %s; HC3. Within-arm association (falsehood deployment is endogenous;",
                   " the causal truth-constraint cost is the randomized S2-vs-S3 contrast)."),
            spec_txt))
      }
      # Collapsed-at-zero DISPLAY model for Fig 5b (S1-4 pooled frames only): composite on
      # z(pmax(change, 0)); backfire/no-move conversations form the x = 0 category.
      if (fr$model_lab == "S1-4 pooled") {
        dat$change_floor0 <- pmax(dat$change, 0)
        fit <- .es_fit(dat, "eval_composite", "change_floor0", fe = fr$fe, controls = fr$ctrl)
        if (!is.null(fit)) {
          rows[[length(rows) + 1]] <- dplyr::mutate(
            fit[, setdiff(names(fit), c("tost10", "tost15"))],
            outcome = .es_outcomes[["eval_composite"]], model = fr$model_lab, direction = arm,
            term = "belief_change_floor0", sample = fr$sample,
            note = .es_note(fit, "belief_change_floor0", spec_txt,
                            extra = paste0(" Display model for Fig 5b: belief change floored",
                                           " at 0 (<=0 collapsed to the zero category before",
                                           " z-scoring); the substantive persuasion-coupling",
                                           " estimate is term belief_change.")))
        }
      }
      # Composite-only raw sensitivity rows (primary arm only, as in the source analysis).
      if (arm == "Bunking") {
        for (pv in names(.es_preds)) {
          fit <- .es_fit(dat, "eval_composite", pv)
          if (is.null(fit)) next
          rows[[length(rows) + 1]] <- dplyr::mutate(
            fit[, setdiff(names(fit), c("tost10", "tost15"))],
            outcome = .es_outcomes[["eval_composite"]], model = fr$model_lab, direction = arm,
            term = paste0(.es_preds[[pv]], "_raw"), sample = fr$sample,
            note = .es_note(fit, paste0(.es_preds[[pv]], "_raw"),
                            sprintf("%s %s arm, %sconversations with >=1 aligned claim; %s",
                                    fr$tag, arm, fr$samp_txt, raw_txt)))
        }
      }
    }
  }

  # -- ARM-CONTRAST rows (pooled S1-4 frames only; see header) ------------------------------
  arm_lab <- "Bunking - Debunking"
  for (fr in frames) {
    if (fr$model_lab != "S1-4 pooled") next
    dat <- fr$dat[!is.na(fr$dat$n_aligned) & fr$dat$n_aligned >= 1, , drop = FALSE]
    dat$arm_bunk <- as.numeric(as.character(dat$condition_factor) == "Bunking")
    spec_txt <- sprintf("%s both arms, %sconversations with >=1 aligned claim; %s",
                        fr$tag, fr$samp_txt, fr$fe_txt)
    for (ov in c("eval_composite", "trust_change")) {
      fit <- .es_fit(dat, ov, "arm_bunk", fe = fr$fe, controls = fr$ctrl, scale_x = FALSE)
      if (is.null(fit)) next
      rows[[length(rows) + 1]] <- dplyr::mutate(
        fit[, setdiff(names(fit), c("tost10", "tost15"))],
        outcome = .es_outcomes[[ov]], model = fr$model_lab, direction = arm_lab,
        term = "arm_bunk_minus_debunk", sample = fr$sample,
        note = sprintf(
          paste0("Arm contrast on the z-scored outcome: Bunking minus Debunking in SD units",
                 " (outcome z-scored within the estimation sample; arm entered as an",
                 " unscaled 0/1 indicator, Debunking reference); %s; HC3. Arm is",
                 " randomized, but the >=1-aligned-claim (and, for sample=compliant,",
                 " compliance) eligibility conditions on post-treatment variables; the",
                 " unconditional randomized contrast is the paper's main ATE block."),
          spec_txt))
    }
    # Arm x z(falsehood_share) interaction: compliant pooled composite only (descriptive).
    if (fr$sample == "compliant") {
      keep <- c("eval_composite", "fal_share", "arm_bunk", fr$fe, fr$ctrl)
      d2 <- dat[stats::complete.cases(dat[, keep, drop = FALSE]), keep, drop = FALSE]
      if (nrow(d2) >= 30) {
        for (fcol in fr$fe) {
          v <- as.character(d2[[fcol]]); tab <- table(v)
          v[v %in% names(tab)[tab < 5]] <- "(other)"; d2[[fcol]] <- v
        }
        d2$.y <- as.numeric(scale(d2$eval_composite))
        d2$.x <- as.numeric(scale(d2$fal_share))
        m  <- stats::lm(stats::as.formula(paste(
          ".y ~ .x * arm_bunk +", fr$ctrl, "+",
          paste(paste0("factor(", fr$fe, ")"), collapse = " + "))), data = d2)
        ct <- lmtest::coeftest(m, vcov. = sandwich::vcovHC(m, type = "HC3"))
        i  <- which(rownames(ct) == ".x:arm_bunk")
        est <- ct[i, 1]; se <- ct[i, 2]; df <- m$df.residual
        tcrit <- stats::qt(.975, df)
        rows[[length(rows) + 1]] <- tibble::tibble(
          n = nrow(d2), estimate = est, se = se,
          conf_low = est - tcrit * se, conf_high = est + tcrit * se,
          statistic = ct[i, 3], df_den = df, p_value = ct[i, 4],
          outcome = .es_outcomes[["eval_composite"]], model = fr$model_lab,
          direction = arm_lab, term = "arm_x_falsehood", sample = fr$sample,
          note = sprintf(
            paste0("Arm x z(falsehood_share) interaction on the z-scored composite",
                   " (difference in falsehood slopes, Bunking minus Debunking; arm as",
                   " unscaled 0/1 indicator, Debunking reference; falsehood share z-scored",
                   " across both arms within the estimation sample); %s; HC3. DESCRIPTIVE:",
                   " falsehood share is post-treatment w.r.t. the randomized arm, so only",
                   " the arm main effect is causal; this tests whether the arm gap is",
                   " constant across the observed falsehood spectrum."),
            spec_txt))
      }
    }
  }

  if (!length(rows))
    return(std_row(tibble::tibble(term = character(0)), "S1-3", "evaluation_slopes", "aligned_ge1"))
  std_row(dplyr::bind_rows(rows), "S1-3", "evaluation_slopes", "aligned_ge1")
}
