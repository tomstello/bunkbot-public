# ext_balance.R --------------------------------------------------------------------------
# Randomization / covariate-balance check. Assignment to bunking vs debunking is randomized
# within every study, so pre-treatment covariates should be balanced across the two arms.
# For each study and each pre-treatment covariate this reports the bunk and debunk means, the
# standardized mean difference (SMD = (mean_bunk - mean_debunk) / pooled SD), and a two-sample
# t-test p; plus a per-study OMNIBUS joint-balance test (likelihood-ratio test of a logistic
# regression of assigned arm on all covariates vs an intercept-only model -- a significant
# result would indicate the arms differ on the covariate set).
#
# ENTRY POINT: compute_balance(core_objects) -> tibble (canonical schema).
# block "covariate_balance" (section "Cross-study").
#   term = covariate; estimate = SMD; conf_low/high = SMD 95% CI; statistic = mean_bunk;
#     df_num = mean_debunk; p_value = t-test p; n = arm total.
#   term = "joint_balance_LRT": statistic = LRT chi-square, df_num = df, p_value = p, n.

.bal_gcbs <- function(df) {
  gp <- c("x2_gcbs_pre", "x4_gcbs_pre", "x6_gcbs_pre", "x8_gcbs_pre",
          "x10_gcbs_pre", "x12_gcbs_pre", "x14_gcbs_pre")
  if (!all(gp %in% names(df))) return(rep(NA_real_, nrow(df)))
  m <- sapply(gp, function(c) suppressWarnings(as.numeric(df[[c]])))
  ifelse(rowSums(is.na(m)) == 0, rowMeans(m), NA_real_)
}

# SMD + t-test for one covariate split by a 2-level arm factor.
.bal_one <- function(y, arm, label) {
  ok <- is.finite(y) & !is.na(arm)
  y <- y[ok]; arm <- droplevels(factor(arm[ok]))
  if (nlevels(arm) != 2) return(NULL)
  lv <- levels(arm)  # expect c("bunk","debunk")
  yb <- y[arm == "bunk"]; yd <- y[arm == "debunk"]
  if (length(yb) < 3 || length(yd) < 3) return(NULL)
  mb <- mean(yb); md <- mean(yd)
  sp <- sqrt(((length(yb) - 1) * stats::var(yb) + (length(yd) - 1) * stats::var(yd)) /
               (length(yb) + length(yd) - 2))
  smd <- if (sp > 0) (mb - md) / sp else NA_real_
  se_smd <- if (sp > 0) sqrt(1 / length(yb) + 1 / length(yd) + smd^2 / (2 * (length(yb) + length(yd)))) else NA_real_
  tt <- stats::t.test(yb, yd)
  tibble::tibble(
    term = label, n = length(yb) + length(yd),
    estimate = smd, conf_low = smd - 1.96 * se_smd, conf_high = smd + 1.96 * se_smd,
    statistic = mb, df_num = md, p_value = tt$p.value,
    note = sprintf("Std. mean diff (bunk-debunk); bunk mean=%.2f (statistic), debunk mean=%.2f (df_num); t-test p.",
                   mb, md))
}

# covariate matrix builder for one study frame; returns named list of numeric vectors + arm.
.bal_covs_s13 <- function(d) {
  list(
    arm = as.character(d$direction),
    covs = list(
      `Baseline belief` = suppressWarnings(as.numeric(d$belief_rating_pre_rc)),
      `Conspiracy mentality (GCBS)` = .bal_gcbs(d),
      `Trust in AI` = suppressWarnings(as.numeric(d$genai_trust)),
      `Age` = suppressWarnings(as.numeric(d$age_2)),
      `Education` = suppressWarnings(as.numeric(d$education)),
      `Partisanship (Dem->Rep)` = { v <- suppressWarnings(as.numeric(d$dem_rep_c)); ifelse(v %in% 1:6, v, NA_real_) },
      `Gender (female)` = { g <- suppressWarnings(as.numeric(d$gender_2)); ifelse(is.na(g), NA_real_, as.numeric(g == 2)) }
    ))
}
.bal_covs_s4 <- function(d) {
  list(
    arm = as.character(d$direction),
    covs = list(
      `Baseline belief` = suppressWarnings(as.numeric(d$belief_rating_pre_4)),
      `Trust in AI` = suppressWarnings(as.numeric(d$genai_trust)),
      `Party (Dem->Rep)` = suppressWarnings(as.numeric(d$DemRep_C)),
      `Social conservatism` = suppressWarnings(as.numeric(d$SocialConservatism)),
      `Age` = { a <- suppressWarnings(as.numeric(d$age)); ifelse(a >= 13 & a <= 100, a, NA_real_) },
      # Use the canonical `Education` field (1,271 non-missing); the lower-case `education`
      # derivative is only ~half-populated (624) and biases the SMD / joint-balance test.
      `Education` = suppressWarnings(as.numeric(d$Education)),
      `Gender (female)` = { g <- suppressWarnings(as.numeric(d$gender)); ifelse(is.na(g), NA_real_, as.numeric(g == 2)) }
    ))
}

.bal_study <- function(spec, study_label) {
  arm <- spec$arm
  rows <- list()
  Xcols <- list()
  for (nm in names(spec$covs)) {
    r <- .bal_one(spec$covs[[nm]], arm, nm)
    if (!is.null(r)) { r$model <- study_label; rows[[length(rows) + 1]] <- r }
    Xcols[[nm]] <- spec$covs[[nm]]
  }
  # omnibus joint-balance LRT (logistic: arm ~ all covariates)
  X <- as.data.frame(Xcols)
  X$.arm <- ifelse(arm == "bunk", 1L, ifelse(arm == "debunk", 0L, NA_integer_))
  X <- X[stats::complete.cases(X), , drop = FALSE]
  if (nrow(X) > (ncol(X) + 5) && length(unique(X$.arm)) == 2) {
    f0 <- stats::glm(.arm ~ 1, data = X, family = stats::binomial())
    f1 <- stats::glm(.arm ~ ., data = X, family = stats::binomial())
    lr <- stats::anova(f0, f1, test = "LRT")
    rows[[length(rows) + 1]] <- tibble::tibble(
      model = study_label, term = "joint_balance_LRT", n = nrow(X),
      statistic = lr$Deviance[2], df_num = lr$Df[2], p_value = lr$`Pr(>Chi)`[2],
      note = "Likelihood-ratio test of arm ~ all covariates vs intercept-only; non-significant => arms balanced on the covariate set.")
  }
  dplyr::bind_rows(rows)
}

compute_balance <- function(core_objects) {
  si_require(c("dplyr", "tibble"))
  out <- list()
  for (sf in c("Jailbroken", "Standard", "Truth-Constrained")) {
    d <- core_objects$s13[core_objects$s13$study_factor == sf, , drop = FALSE]
    if (nrow(d)) out[[length(out) + 1]] <- .bal_study(.bal_covs_s13(d), sf)
  }
  # Study 4: randomization to arm was WITHIN each model, so balance is checked per model.
  # Emit one block of balance rows per frontier model (NEVER a pooled Study-4 row).
  s4 <- core_objects$s4$s4
  for (mdl in c("Claude", "Gemini", "GPT-5.2", "Grok")) {
    dm <- s4[as.character(s4$model_pooled) == mdl, , drop = FALSE]
    if (nrow(dm)) out[[length(out) + 1]] <- .bal_study(.bal_covs_s4(dm), mdl)
  }
  res <- dplyr::bind_rows(out)
  std_row(res, "Cross-study", "covariate_balance", "full_sample")
}
