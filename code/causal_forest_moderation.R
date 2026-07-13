#!/usr/bin/env Rscript
# =============================================================================
# causal_forest_moderation.R
# Pooled (Studies 1-4) CAUSAL-FOREST heterogeneity / moderation analysis.
#
# WHAT THIS IS. The S1/S2 pre-registrations (AsPredicted #218585, #224184; the
# latter also covers the exploratory S3) listed, as an EXPLORATORY analysis,
# "Examining treatment effect heterogeneity with causal forests considering a
# large set of covariates." It was never run -- the predictive-ML section was
# cut and replaced by the one-moderator-at-a-time OLS in code/R/ext_moderators.R.
# This standalone script finally runs that causal forest, POOLED across all four
# studies, on the COMPLIANT sample, with a large harmonized moderator set. It is
# self-contained: it only sources the canonical (API-free) data builders, then
# does everything else itself. It is NOT wired into build_all_numbers(); if a
# result earns a place in the paper it can be wrapped as an ext_* engine module.
#
# THE ESTIMAND (why not "aligned" change as the outcome).
#   No study has a no-persuasion control arm (S1-3: bunk vs debunk only; S4 is a
#   2x4 direction x model design). A causal forest needs a treatment/control
#   contrast, so the only coherent causal object is BUNK vs DEBUNK:
#       W = 1 if direction == "bunk", 0 if "debunk"
#       Y = raw reverse-coded change TOWARD the conspiracy = post_rc - pre_rc,
#           recovered uniformly as  Y = aligned_belief_change * sign(arm)
#           (sign = +1 bunk, -1 debunk), since aligned = raw * sign(arm).
#   CATE(x) = E[Y|bunk,x] - E[Y|debunk,x] = E[aligned|bunk,x] + E[aligned|debunk,x]
#           = the total "swing" the AI can induce in whichever direction it is
#             pushed = a person's PERSUADABILITY. Its ATE ~ the paper's pooled
#             bunk-debunk gap (printed below as a sanity check).
#   Aligned change CANNOT be the forest's Y: aligned = raw * sign(W) already folds
#   the arm into the outcome, so contrasting arms would double-count direction.
#   A large CATE can come from bunk-side OR debunk-side movement; the per-arm
#   descriptive regression forests (below) disentangle "bunkable" vs "debunkable".
#
# DESIGN DECISIONS:
#   (1) Swing + per-arm view: primary causal forest on the swing, PLUS descriptive
#       per-arm regression forests on aligned change.
#   (2) Study regime (Jailbroken/Standard/Truth-Constrained) and S4 model
#       (Claude/Gemini/GPT-5.2/Grok) ARE entered as splitting covariates; person-
#       trait moderators are reported with AND without these design dummies.
#   (3) Keep all moderators; grf 2.4.0 handles missingness natively (MIA). GCBS is
#       S1-3-only -> NA for S4. A common-covariates-only forest is the robustness.
#
# MODERATORS (all harmonized + z-scored WITHIN study/model):
#   Common to all four studies (numeric, ~100% complete):
#     baseline_belief (belief_rating_pre_rc / _pre_4, 0-100 toward-conspiracy),
#     ai_trust (genai_trust 1-7), ai_familiarity (genai_fam_1 1-7),
#     ai_use (genai_use_1 1-7), pol_demrep (dem_rep_c / DemRep_C 1-6, higher=Rep),
#     social_conservatism (1-5), economic_conservatism (1-5),
#     education (1-8), age (age_2 [S1-3] / age [S4], bounded 13-100).
#   S1-3 only (NA for S4; grf MIA): gcbs (row-mean of 7 paired pre items, 1-11).
#   Demographics standardized from the SHARED survey codebooks (identical in all
#   four .qsf instruments), so they pool too:
#     female      : 1=female,0=male  (S1-3 numeric gender_2; S4 numeric gender;
#                   both 1=Male/2=Female -- S4 mapping confirmed vs its text col.
#                   The S1-3 *free-text* `gender` column is bypassed.)
#     religiosity : `religion` = "How much do you believe in God or gods?" 1-8
#                   (1=Not at all .. 8=Very Much); same item in all four studies.
#     white       : race multi-select contains category 1=White (S4 comma-coded
#                   "1,3"; S1-3 concatenated-digit "13"; same 1-6 categories).
#   STILL excluded: religious *affiliation* (Demog_14, 12-way denomination) and
#     finer race categories -- high-cardinality and thin; `white` is the robust cut.
#
# RUN:  Rscript code/causal_forest_moderation.R        (from the repo root)
# OUT:  output/causal_forest/*.csv  and  figures/causal_forest/*.png
# Dev fast-path: set BUNKBOT_CF_SLIM=<path to an rds list(s13c=, s4c=)> to skip
#   the ~1-2 min full rebuild of core_objects.
# =============================================================================

suppressWarnings(suppressMessages({
  library(dplyr); library(readr); library(tidyr); library(ggplot2)
}))

SEED <- 20260629L
set.seed(SEED)
N_TREES <- 4000L

# ---- repo root resolution ---------------------------------------------------
get_script_dir <- function() {
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) normalizePath(dirname(f[1])) else getwd()
}
find_repo_root <- function() {
  for (s in unique(c(getwd(), get_script_dir()))) {
    d <- s
    for (i in 1:6) {
      if (file.exists(file.path(d, "code", "bunkbot_helpers.R")) &&
          dir.exists(file.path(d, "data"))) return(normalizePath(d))
      d <- dirname(d)
    }
  }
  stop("Could not locate repo root (a dir holding code/bunkbot_helpers.R and data/).")
}
repo <- find_repo_root()
setwd(repo)
message("Repo root: ", repo)

if (!requireNamespace("grf", quietly = TRUE))
  stop("Package 'grf' is required. Install it with install.packages('grf').")
library(grf)
have_sandwich <- requireNamespace("sandwich", quietly = TRUE)
message("grf ", as.character(packageVersion("grf")), " | seed ", SEED, " | trees ", N_TREES)

out_dir <- file.path(repo, "output", "causal_forest")
fig_dir <- file.path(repo, "figures", "causal_forest")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
wr <- function(df, name) {
  readr::write_csv(df, file.path(out_dir, name)); invisible(df)
}

# ---- small helpers (mirrors ext_moderators.R) -------------------------------
num <- function(x) suppressWarnings(as.numeric(as.character(x)))
z   <- function(x) {                       # z-score (NA-safe); constant -> NA
  x <- num(x); s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}
clean_age <- function(x) {                 # plausible human ages only
  a <- num(x); a[!is.finite(a) | a < 13 | a > 100] <- NA_real_; a
}
gcbs_pre <- function(df) {                 # 7 paired GCBS pre items -> row mean
  gp <- c("x2_gcbs_pre","x4_gcbs_pre","x6_gcbs_pre","x8_gcbs_pre",
          "x10_gcbs_pre","x12_gcbs_pre","x14_gcbs_pre")
  m <- sapply(gp, function(c) num(df[[c]]))
  ifelse(rowSums(is.na(m)) == 0, rowMeans(m), NA_real_)
}
tidy_coeftest <- function(m) {             # grf BLP / calibration -> tidy df
  m <- unclass(m)
  data.frame(term = rownames(m), estimate = m[, 1], se = m[, 2],
             statistic = m[, 3], p_value = m[, 4], row.names = NULL)
}
ate_vec <- function(a) c(estimate = unname(a["estimate"]), se = unname(a["std.err"]))

# ---- 1. load the canonical compliant frames ---------------------------------
slim <- Sys.getenv("BUNKBOT_CF_SLIM", "")
if (nzchar(slim) && file.exists(slim)) {
  message("Loading cached frames from ", slim)
  cc <- readRDS(slim); s13 <- cc$s13c; s4 <- cc$s4c
} else {
  message("Building core_objects from data/ (API-free; ~1-2 min) ...")
  source(file.path(repo, "code", "bunkbot_helpers.R"))   # build_s1s3(), helpers
  source(file.path(repo, "code", "R", "ape_rescore.R"))  # apply_ape_rescore()
  source(file.path(repo, "code", "R", "pipeline_core.R"))# build_core_objects()
  core <- build_core_objects(repo)
  s13 <- dplyr::filter(core$s13, compliant %in% TRUE)    # evaluator & rev-evaluator == 1
  s4  <- core$s4$s4_compliant                            # strict_compliant (post rescore)
}
message("S1-3 compliant: ", nrow(s13), " | S4 compliant: ", nrow(s4))

# ---- 2. assemble the pooled analytic frame ----------------------------------
MODS_CONT <- c("baseline_belief","ai_trust","ai_familiarity","ai_use",
               "pol_demrep","social_conservatism","economic_conservatism",
               "education","age","religiosity") # common, continuous/ordinal (z'd)
MODS_BIN  <- c("female","white")            # common, binary (0/1; not z'd)
MOD_PARTIAL <- "gcbs"                       # S1-3 only

# race multi-select -> "is White selected?" (categories are single digits 1-6, so
# both the S4 comma form "1,3" and the S1-3 concatenated form "13" decode the same).
parse_white <- function(race) {
  d <- gsub("[^0-9]", "", as.character(race))
  ifelse(is.na(race) | d == "", NA_integer_, as.integer(grepl("1", d)))
}
# gender code -> female (1) / male (0); shared 1=Male, 2=Female coding.
to_female <- function(g) { g <- num(g); ifelse(g == 2, 1L, ifelse(g == 1, 0L, NA_integer_)) }

s13m <- tibble(
  study_model           = as.character(s13$study_factor),  # Jailbroken/Standard/Truth-Constrained
  response_id           = as.character(s13$response_id),
  direction             = as.character(s13$direction),
  aligned               = num(s13$aligned_belief_change),
  baseline_belief       = num(s13$belief_rating_pre_rc),
  ai_trust              = num(s13$genai_trust),
  ai_familiarity        = num(s13$genai_fam_1),
  ai_use                = num(s13$genai_use_1),
  pol_demrep            = num(s13$dem_rep_c),
  social_conservatism   = num(s13$social_conservatism),
  economic_conservatism = num(s13$economic_conservatism),
  education             = num(s13$education),
  age                   = clean_age(s13$age_2),
  religiosity           = num(s13$religion),
  female                = to_female(s13$gender_2),
  white                 = parse_white(s13$race),
  gcbs                  = gcbs_pre(s13)
)
s4m <- tibble(
  study_model           = as.character(s4$model_pooled),   # Claude/Gemini/GPT-5.2/Grok
  response_id           = as.character(s4$ResponseId),
  direction             = as.character(s4$direction),
  aligned               = num(s4$aligned_belief_change),
  baseline_belief       = num(s4$belief_rating_pre_4),
  ai_trust              = num(s4$genai_trust),
  ai_familiarity        = num(s4$genai_fam_1),
  ai_use                = num(s4$genai_use_1),
  pol_demrep            = num(s4$DemRep_C),
  social_conservatism   = num(s4$SocialConservatism),
  economic_conservatism = num(s4$EconomicConservatism),
  education             = num(s4$Education),
  age                   = clean_age(s4$age),
  religiosity           = num(s4$religion),
  female                = to_female(s4$gender),
  white                 = parse_white(s4$Race),
  gcbs                  = NA_real_                          # not measured in S4
)

sm_levels <- c("Jailbroken","Standard","Truth-Constrained","Claude","Gemini","GPT-5.2","Grok")
dat <- bind_rows(s13m, s4m) %>%
  filter(direction %in% c("bunk","debunk"), !is.na(aligned)) %>%
  mutate(
    study_model = factor(study_model, levels = sm_levels),
    W = as.integer(direction == "bunk"),
    Y = aligned * ifelse(direction == "bunk", 1, -1)       # raw signed change toward conspiracy
  )
stopifnot(!any(is.na(dat$study_model)))
message("Pooled analytic N: ", nrow(dat))

# z-score continuous moderators WITHIN study/model (harmonizes scales/instruments)
dat <- dat %>%
  group_by(study_model) %>%
  mutate(across(all_of(c(MODS_CONT, MOD_PARTIAL)), z, .names = "{.col}_z")) %>%
  ungroup()

# Impute residual (<0.5%) missingness in the COMMON moderators to 0 (= within-study
# mean, post-z) so the BLP design matrices are complete; GCBS_z stays NA for S4
# (intentional partial moderator; grf MIA handles it in the forest).
zc <- paste0(MODS_CONT, "_z")
dat[zc] <- lapply(dat[zc], function(v) ifelse(is.na(v), 0, v))
# binary common moderators: impute residual (<0.5%) NA to the column mean so the
# BLP design matrices are complete (forest could take NA, but keep them aligned).
for (b in MODS_BIN) dat[[b]] <- ifelse(is.na(dat[[b]]), mean(dat[[b]], na.rm = TRUE), dat[[b]])
gcbs_z <- dat$gcbs_z
person_cols <- c(zc, MODS_BIN)                              # complete, common person moderators

# ---- 3. build design matrices ----------------------------------------------
X_person  <- as.matrix(dat[c(person_cols, "gcbs_z")])      # common moderators + gcbs (NA for S4)
sm_oh     <- stats::model.matrix(~ study_model - 1, data = dat)   # 7 one-hot dummies
colnames(sm_oh) <- sub("^study_model", "ctx_", colnames(sm_oh))
X_full    <- cbind(X_person, sm_oh)                         # forest splits on traits + context
X_common  <- as.matrix(dat[person_cols])                  # robustness: common, complete, no context

Y  <- dat$Y
W  <- dat$W
# Known-design propensity: within-cell empirical P(bunk). Avoids W.hat misestimation.
What <- as.numeric(ave(W, dat$study_model, FUN = mean))

# ---- 4. PRIMARY causal forest (the swing) -----------------------------------
message("Fitting primary causal forest ...")
cf <- causal_forest(X = X_full, Y = Y, W = W, W.hat = What,
                    num.trees = N_TREES, honesty = TRUE, seed = SEED)
dat$tau_hat <- as.numeric(cf$predictions)

## 4a. overall ATE + sanity vs the simple pooled gap
ate <- ate_vec(average_treatment_effect(cf, target.sample = "all"))
mean_aligned_bunk   <- mean(dat$aligned[dat$W == 1], na.rm = TRUE)
mean_aligned_debunk <- mean(dat$aligned[dat$W == 0], na.rm = TRUE)
simple_gap <- mean(Y[W == 1]) - mean(Y[W == 0])            # = swing, unadjusted
ate_tbl <- tibble(
  quantity = c("forest_AIPW_ATE_swing", "simple_unadj_gap_swing",
               "mean_aligned_bunk", "mean_aligned_debunk"),
  estimate = c(ate["estimate"], simple_gap, mean_aligned_bunk, mean_aligned_debunk),
  se       = c(ate["se"], NA, NA, NA)
)
wr(ate_tbl, "ate_overall.csv")

## 4b. omnibus heterogeneity test (calibration)
cal <- tidy_coeftest(test_calibration(cf))
wr(cal, "calibration_test.csv")

## 4c. best linear projection of the CATE on moderators
##     (i) person traits only; (ii) person traits + context dummies
blp_person <- tidy_coeftest(best_linear_projection(cf, A = X_person[, person_cols, drop = FALSE]))
wr(blp_person, "blp_person.csv")

A_design <- cbind(X_person[, person_cols, drop = FALSE],
                  sm_oh[, setdiff(colnames(sm_oh), "ctx_Standard"), drop = FALSE]) # ref = Standard
blp_design <- tidy_coeftest(best_linear_projection(cf, A = A_design))
wr(blp_design, "blp_with_context.csv")

## (iii) S1-3-only BLP that INCLUDES gcbs (subset to rows where gcbs observed)
s13_idx <- which(!is.na(gcbs_z))
blp_s13 <- tidy_coeftest(best_linear_projection(
  cf, A = cbind(X_person[, person_cols, drop = FALSE], gcbs_z = gcbs_z)[s13_idx, , drop = FALSE],
  subset = s13_idx))
wr(blp_s13, "blp_s13_with_gcbs.csv")

## 4d. variable importance
vi <- tibble(variable = colnames(X_full), importance = as.numeric(variable_importance(cf))) %>%
  mutate(kind = ifelse(grepl("^ctx_", variable), "context", "person")) %>%
  arrange(desc(importance))
wr(vi, "variable_importance.csv")

## 4e. RATE / AUTOC -- honest train/eval split (does CATE-targeting find real het?)
rate_tbl <- tibble(); toc_df <- NULL
rate_ok <- tryCatch({
  n <- nrow(X_full); set.seed(SEED)
  tr <- sample(n, floor(n / 2)); ev <- setdiff(seq_len(n), tr)
  cf_tr <- causal_forest(X_full[tr, ], Y[tr], W[tr], W.hat = What[tr],
                         num.trees = N_TREES, seed = SEED)
  tau_ev <- predict(cf_tr, X_full[ev, ])$predictions
  cf_ev <- causal_forest(X_full[ev, ], Y[ev], W[ev], W.hat = What[ev],
                         num.trees = N_TREES, seed = SEED)
  rate <- rank_average_treatment_effect(cf_ev, tau_ev, target = "AUTOC")
  rate_tbl <<- tibble(target = "AUTOC", estimate = rate$estimate,
                      se = rate$std.err, t = rate$estimate / rate$std.err)
  toc_df <<- as.data.frame(rate$TOC)
  TRUE
}, error = function(e) { message("RATE failed: ", conditionMessage(e)); FALSE })
if (rate_ok) wr(rate_tbl, "rate_autoc.csv")

## 4f. AIPW ATE by CATE quartile (descriptive spread of the swing)
qs <- cut(dat$tau_hat, stats::quantile(dat$tau_hat, 0:4 / 4), include.lowest = TRUE,
          labels = paste0("Q", 1:4))
qate <- bind_rows(lapply(levels(qs), function(g) {
  a <- ate_vec(average_treatment_effect(cf, subset = qs == g))
  tibble(cate_quartile = g, n = sum(qs == g),
         mean_tau_hat = mean(dat$tau_hat[qs == g]),
         aipw_ate = a["estimate"], se = a["se"])
}))
wr(qate, "cate_quartile_ate.csv")

## 4g. AIPW ATE by study/model (re-expresses S2-vs-S3 & per-model gaps as swings)
sm_ate <- bind_rows(lapply(levels(dat$study_model), function(g) {
  idx <- dat$study_model == g
  if (sum(idx) < 20 || length(unique(W[idx])) < 2) return(NULL)
  a <- ate_vec(average_treatment_effect(cf, subset = idx))
  tibble(study_model = g, n = sum(idx),
         mean_aligned_bunk   = mean(dat$aligned[idx & dat$W == 1], na.rm = TRUE),
         mean_aligned_debunk = mean(dat$aligned[idx & dat$W == 0], na.rm = TRUE),
         aipw_ate_swing = a["estimate"], se = a["se"])
}))
wr(sm_ate, "ate_by_study_model.csv")

## 4h. per-row CATE estimates (for downstream plotting / inspection)
wr(dat %>% transmute(response_id, study_model, direction, W, Y, aligned, tau_hat),
   "cate_estimates.csv")

# ---- 5. PER-ARM descriptive layer (who is bunkable vs debunkable) -----------
# Regression forests + HC3 OLS of ALIGNED change on the common moderators within
# each arm. DESCRIPTIVE (predictive), not a causal contrast -- labelled as such.
perarm_vi <- list(); perarm_ols <- list()
for (arm in c("bunk","debunk")) {
  idx <- dat$direction == arm
  rf <- regression_forest(X_full[idx, ], dat$aligned[idx], num.trees = N_TREES, seed = SEED)
  perarm_vi[[arm]] <- tibble(arm = arm, variable = colnames(X_full),
                             importance = as.numeric(variable_importance(rf))) %>%
    arrange(desc(importance))
  d_arm <- as.data.frame(cbind(aligned = dat$aligned[idx], dat[idx, person_cols]))
  fit <- stats::lm(aligned ~ ., data = d_arm)
  vc  <- if (have_sandwich) sandwich::vcovHC(fit, type = "HC3") else stats::vcov(fit)
  co  <- summary(fit)$coefficients
  se  <- sqrt(diag(vc))
  perarm_ols[[arm]] <- tibble(arm = arm, term = rownames(co), estimate = co[, 1],
                              se = se[rownames(co)],
                              statistic = co[, 1] / se[rownames(co)]) %>%
    mutate(p_value = 2 * stats::pnorm(-abs(statistic)))
}
wr(bind_rows(perarm_vi),  "perarm_variable_importance.csv")
wr(bind_rows(perarm_ols), "perarm_ols_aligned.csv")

# ---- 6. ROBUSTNESS: common-covariates-only forest (no NA, no context) -------
message("Fitting robustness (common-covariates-only) forest ...")
cf_c <- causal_forest(X_common, Y, W, W.hat = What, num.trees = N_TREES,
                      honesty = TRUE, seed = SEED)
rob <- bind_rows(
  tibble(metric = "AIPW_ATE", value = ate_vec(average_treatment_effect(cf_c))["estimate"],
         se = ate_vec(average_treatment_effect(cf_c))["se"]),
  { cc <- tidy_coeftest(test_calibration(cf_c))
    tibble(metric = paste0("calib_", cc$term), value = cc$estimate, se = cc$se) }
)
wr(rob, "robustness_common_cov_summary.csv")
wr(tidy_coeftest(best_linear_projection(cf_c, A = X_common)), "robustness_common_cov_blp.csv")

# ---- 7. figures -------------------------------------------------------------
save_png <- function(p, name, w = 8, h = 5) {
  ggplot2::ggsave(file.path(fig_dir, name), p, width = w, height = h, dpi = 150)
}
theme_set(theme_minimal(base_size = 12))

# 7a. variable importance
p_vi <- vi %>% mutate(variable = factor(variable, levels = rev(variable))) %>%
  ggplot(aes(variable, importance, fill = kind)) +
  geom_col() + coord_flip() +
  labs(title = "Causal-forest variable importance (bunk-debunk swing)",
       x = NULL, y = "split-frequency importance", fill = NULL)
save_png(p_vi, "variable_importance.png")

# 7b. BLP coefficient forest (person + context)
p_blp <- blp_design %>% filter(term != "(Intercept)") %>%
  mutate(term = factor(term, levels = rev(term)),
         lo = estimate - 1.96 * se, hi = estimate + 1.96 * se) %>%
  ggplot(aes(estimate, term)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  geom_pointrange(aes(xmin = lo, xmax = hi)) +
  labs(title = "Best linear projection of the CATE (swing) on moderators + context",
       subtitle = "context ref = Standard; person moderators are per within-study SD",
       x = "slope on the bunk-debunk swing (belief points)", y = NULL)
save_png(p_blp, "blp_coefficients.png", h = 6)

# 7c. CATE distribution by study/model
p_hist <- ggplot(dat, aes(tau_hat)) +
  geom_histogram(bins = 40, fill = "steelblue", colour = "white") +
  facet_wrap(~ study_model, scales = "free_y") +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey40") +
  labs(title = "Estimated CATE (bunk-debunk swing) by study / model",
       x = "tau_hat (belief points)", y = "count")
save_png(p_hist, "cate_by_study_model.png", w = 9, h = 6)

# 7d. tau_hat vs baseline belief
p_base <- ggplot(dat, aes(baseline_belief, tau_hat)) +
  geom_point(alpha = 0.15, size = 0.7) +
  geom_smooth(method = "loess", se = TRUE, colour = "firebrick") +
  labs(title = "Swing vs baseline conspiracy belief",
       x = "baseline belief (reverse-coded, higher = more belief)", y = "tau_hat (swing)")
save_png(p_base, "tau_vs_baseline_belief.png")

# 7e. TOC curve (if RATE succeeded)
if (!is.null(toc_df) && nrow(toc_df)) {
  xcol <- intersect(c("q","fraction","priority"), names(toc_df))[1]
  if (is.na(xcol)) { toc_df$q <- seq_len(nrow(toc_df)) / nrow(toc_df); xcol <- "q" }
  ycol <- intersect(c("estimate","TOC"), names(toc_df))[1]
  p_toc <- ggplot(toc_df, aes(.data[[xcol]], .data[[ycol]])) +
    geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
    geom_line(colour = "darkgreen") +
    labs(title = "Targeting Operator Characteristic (AUTOC)",
         x = "fraction targeted (highest CATE first)", y = "TOC")
  save_png(p_toc, "toc_curve.png")
}

# ---- 8. console summary -----------------------------------------------------
cat("\n=================== CAUSAL FOREST: POOLED S1-S4 ====================\n")
cat(sprintf("Pooled compliant N = %d   (S1-3 = %d, S4 = %d)\n",
            nrow(dat), sum(dat$study_model %in% c("Jailbroken","Standard","Truth-Constrained")),
            sum(!dat$study_model %in% c("Jailbroken","Standard","Truth-Constrained"))))
cat(sprintf("Arms: bunk = %d, debunk = %d\n", sum(W == 1), sum(W == 0)))
cat(sprintf("\nOverall ATE (swing): forest AIPW = %.2f (SE %.2f) | simple gap = %.2f\n",
            ate["estimate"], ate["se"], simple_gap))
cat(sprintf("  mean aligned change: bunk = %.2f, debunk = %.2f  (sum = %.2f = swing)\n",
            mean_aligned_bunk, mean_aligned_debunk, mean_aligned_bunk + mean_aligned_debunk))
cat("\nCalibration (test_calibration):\n"); print(cal, row.names = FALSE)
if (rate_ok) cat(sprintf("\nAUTOC (honest split) = %.3f (SE %.3f)\n",
                         rate_tbl$estimate, rate_tbl$se))
cat("\nTop moderators by variable importance:\n")
print(head(vi, 10), row.names = FALSE)
cat("\nBLP on person traits (CATE slope per within-study SD):\n")
print(blp_person, row.names = FALSE)
cat("\nAIPW swing by study/model:\n"); print(sm_ate, row.names = FALSE)
cat(sprintf("\nWrote CSVs -> %s\nWrote figures -> %s\n", out_dir, fig_dir))
cat("===================================================================\n")
