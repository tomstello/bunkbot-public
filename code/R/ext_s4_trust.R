# ext_s4_trust.R --------------------------------------------------------------
# Study-4 trust-in-AI CHANGE scores. Added 2026-07-04: the S4 instrument carries
# the same pre-treatment trust item as Studies 1-3 (`genai_trust`, 1-7), the
# post-treatment re-administration (`trust2`), and -- unique to Study 4 -- a
# post-debrief re-administration (`trust_debrf`). Earlier SI text incorrectly
# stated Study 4 had no pre-treatment trust measure; this module computes the
# pre-to-post and post-to-debrief trust changes so the cross-study trust table
# can report Study 4 on the same change-score footing as Studies 1-3.
#
# ENTRY POINT: compute_s4_trust_change(core_objects) -> tibble (canonical schema).
# Block "s4_trust_change", section "S4 + pooled", samples strict_n1272 /
# compliant_n1056 (renamed to _n1073 by build_all_numbers). Direction labels
# match the S1-3 `trust_change` block (Bunking/Debunking) so the two blocks can
# be bound into one table. Contrast = bunk minus debunk, HC3, on the pooled S4
# sample (models pooled by n; the equal-model-weighted post-only contrast lives
# in block `secondary_contrasts`, outcome `trust2`).

.s4t_dz <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2 || stats::sd(x) == 0) return(NA_real_)
  mean(x) / stats::sd(x)
}

.s4t_family <- function(df, change_var, outcome_label) {
  d <- df[df$direction %in% c("bunk", "debunk") & !is.na(df[[change_var]]), , drop = FALSE]
  if (nrow(d) < 10 || length(unique(d$direction)) < 2) return(NULL)
  dir_labels <- c(bunk = "Bunking", debunk = "Debunking")

  within <- lapply(c("bunk", "debunk"), function(dr) {
    x <- d[[change_var]][d$direction == dr]
    if (length(x) < 5) return(NULL)
    ci <- mean_ci(x)
    tibble::tibble(
      outcome = outcome_label, model = "Study 4", direction = unname(dir_labels[[dr]]),
      term = paste0(change_var, "_mean"),
      n = ci$n, estimate = ci$estimate,
      conf_low = ci$conf.low, conf_high = ci$conf.high,
      statistic = .s4t_dz(x),
      note = sprintf("within-condition mean change, S4 models pooled by n; statistic col = Cohen d_z; sd=%.3f",
                     stats::sd(x, na.rm = TRUE))
    )
  })

  d$dir <- factor(as.character(d$direction), levels = c("debunk", "bunk"))
  fit <- stats::lm(stats::as.formula(paste0(change_var, " ~ dir")), data = d)
  tt <- hc3_tidy(fit)
  b <- tt[tt$term == "dirbunk", , drop = FALSE]
  contrast <- tibble::tibble(
    outcome = outcome_label, model = "Study 4",
    term = "bunk_minus_debunk",
    n = nrow(d), estimate = b$estimate, se = b$std.error,
    conf_low = b$conf.low, conf_high = b$conf.high,
    statistic = b$statistic, p_value = b$p.value,
    note = "bunk minus debunk in mean change; HC3 robust SE; S4 models pooled by n"
  )
  dplyr::bind_rows(dplyr::bind_rows(within), contrast)
}

compute_s4_trust_change <- function(core_objects) {
  si_require(c("dplyr", "tibble"))
  base <- list(strict_n1272 = core_objects$s4$s4_with_compliance,
               compliant_n1056 = core_objects$s4$s4_compliant)
  out <- list()
  for (samp in names(base)) {
    d <- base[[samp]]
    d$trust_pre  <- suppressWarnings(as.numeric(d$genai_trust))
    d$trust_post <- suppressWarnings(as.numeric(d$trust2))
    d$trust_deb  <- suppressWarnings(as.numeric(d$trust_debrf))
    d$trust_pre_post_change    <- d$trust_post - d$trust_pre
    d$trust_post_debrief_change <- d$trust_deb - d$trust_post
    fam <- dplyr::bind_rows(
      .s4t_family(d, "trust_pre_post_change", "Trust in AI (pre->post)"),
      .s4t_family(d, "trust_post_debrief_change", "Trust in AI (post->debrief)")
    )
    if (!is.null(fam) && nrow(fam)) {
      out[[length(out) + 1]] <- fam |> std_row("S4 + pooled", "s4_trust_change", samp)
    }
  }
  dplyr::bind_rows(out)
}

# ---- pooled (S1-4) trust change ------------------------------------------------
# Added 2026-07-04: trust in AI rises in BOTH arms in every study;
# the bunking excess is small and consistent across the three GPT-4o variants
# but absent among the four frontier models. Block "pooled_trust_change"
# (section "S4 + pooled", sample "pooled_all4") emits: within-arm pooled means,
# the n-weighted stratum-FE HC3 contrast, and the equal-stratum-weighted
# contrast (7 strata: 3 GPT-4o variants + 4 frontier models).
compute_pooled_trust_change <- function(core_objects) {
  si_require(c("dplyr", "tibble"))
  s13 <- core_objects$s13 |>
    dplyr::transmute(stratum = as.character(study_factor),
                     direction = as.character(direction),
                     tchg = suppressWarnings(as.numeric(trust2) - as.numeric(genai_trust)))
  s4 <- core_objects$s4$s4_with_compliance |>
    dplyr::transmute(stratum = as.character(model_pooled),
                     direction = as.character(direction),
                     tchg = suppressWarnings(as.numeric(trust2) - as.numeric(genai_trust)))
  d <- dplyr::bind_rows(s13, s4) |>
    dplyr::filter(is.finite(tchg), direction %in% c("bunk", "debunk"))
  if (nrow(d) < 100) return(NULL)

  rows <- list()
  for (dr in c("bunk", "debunk")) {
    x <- d$tchg[d$direction == dr]
    ci <- mean_ci(x)
    rows[[length(rows) + 1]] <- tibble::tibble(
      outcome = "Trust in AI (pre->post)", model = "Pooled S1-4",
      direction = ifelse(dr == "bunk", "Bunking", "Debunking"),
      term = "trust_pre_post_change_mean",
      n = ci$n, estimate = ci$estimate, conf_low = ci$conf.low, conf_high = ci$conf.high,
      statistic = .s4t_dz(x),
      note = "within-arm pooled mean change (1-7 scale), all four studies; statistic = d_z")
  }
  d$dir <- factor(d$direction, levels = c("debunk", "bunk"))
  d$stratum <- factor(d$stratum)
  f <- stats::lm(tchg ~ dir + stratum, data = d)
  tt <- hc3_tidy(f)
  b <- tt[tt$term == "dirbunk", , drop = FALSE]
  rows[[length(rows) + 1]] <- tibble::tibble(
    outcome = "Trust in AI (pre->post)", model = "Pooled S1-4",
    term = "bunk_minus_debunk", n = nrow(d),
    estimate = b$estimate, se = b$std.error,
    conf_low = b$conf.low, conf_high = b$conf.high,
    statistic = b$statistic, p_value = b$p.value,
    note = "n-weighted pooled contrast, stratum fixed effects (7 strata), HC3")
  f2 <- stats::lm(tchg ~ dir * stratum, data = d)
  vc2 <- sandwich::vcovHC(f2, type = "HC3")
  lv <- levels(d$stratum)
  int <- paste0("dirbunk:stratum", lv[-1])
  w <- setNames(rep(0, length(stats::coef(f2))), names(stats::coef(f2)))
  w["dirbunk"] <- 1
  w[int[int %in% names(w)]] <- 1 / length(lv)
  est <- sum(w * stats::coef(f2), na.rm = TRUE)
  se <- sqrt(drop(t(w) %*% vc2 %*% w))
  rows[[length(rows) + 1]] <- tibble::tibble(
    outcome = "Trust in AI (pre->post)", model = "Pooled S1-4",
    term = "bunk_minus_debunk_equal_stratum", n = nrow(d),
    estimate = est, se = se,
    conf_low = est - 1.96 * se, conf_high = est + 1.96 * se,
    statistic = est / se, p_value = 2 * stats::pnorm(-abs(est / se)),
    note = "equal-stratum-weighted pooled contrast (each of the 7 model strata weighted 1/7), HC3")
  std_row(dplyr::bind_rows(rows), "S4 + pooled", "pooled_trust_change", "pooled_all4")
}
