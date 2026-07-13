# ext_simple_slopes.R ---------------------------------------------------------------------
# Baseline-belief SIMPLE SLOPES (Fig S12). The Lin (2013) baseline-adjusted model lets the
# direction-aligned belief change depend on the participant's pre-treatment belief. For each
# study x sample cell we fit lm(aligned_belief_change ~ direction * pre). Study 4 is NEVER
# pooled: each of its four frontier models (Claude, Gemini, GPT-5.2, Grok) gets its own
# simple Lin fit and its own cell (we drop the old equal-model-weighted "Study 4" row).
# direction is coded with levels c("debunk","bunk") so the direction main effect = the
# bunk-minus-debunk gap at pre=0 and the direction:pre interaction = how that gap changes
# with baseline belief. All inference uses HC3 (sandwich) standard errors; predicted-value
# and contrast CIs are delta-method intervals from the HC3 vcov.
#
# ENTRY POINT: compute_simple_slopes(core_objects) -> tibble (canonical schema).
# block "simple_slopes" (section "Cross-study"); model = study/model label
#   {Jailbroken, Standard, Truth-Constrained, Claude, Gemini, GPT-5.2, Grok};
#   sample {strict, compliant}.
#   term "pred_<pre>"        : predicted aligned change at that baseline value, per direction
#                              (Bunking/Debunking); conf_low/high = HC3 delta-method 95% CI;
#                              statistic = the pre value (x position); outcome = aligned change.
#   term "interaction_slope" : the direction:pre coefficient (HC3 CI); how the bunk-vs-debunk
#                              gap changes per +1 baseline belief point (S4: per model).
#   term "contrast_at_50"    : bunk-minus-debunk predicted aligned change at pre=50 (HC3 CI).

.ss_grid <- seq(25, 75, by = 5)

# direction factor with debunk as the reference so the main effect is the bunk gap.
.ss_dir <- function(x) factor(as.character(x), levels = c("debunk", "bunk"))

# ---------------------------------------------------------------------------------------
# S1-3 cell: simple Lin model. Returns the predicted-grid + interaction + contrast rows.
# ---------------------------------------------------------------------------------------
.ss_cell_s13 <- function(d, study_label, samp) {
  d <- d[is.finite(d$pre) & is.finite(d$aligned_belief_change) &
           as.character(d$direction) %in% c("bunk", "debunk"), , drop = FALSE]
  d$direction <- droplevels(.ss_dir(d$direction))
  if (nrow(d) < 20 || nlevels(d$direction) != 2) return(NULL)
  fit <- stats::lm(aligned_belief_change ~ direction * pre, data = d)
  V <- sandwich::vcovHC(fit, type = "HC3")
  b <- stats::coef(fit)
  nm <- names(b)

  # predicted-value rows over the grid, for each direction
  rows <- list()
  for (dd in c("bunk", "debunk")) {
    dir_lab <- if (dd == "bunk") "Bunking" else "Debunking"
    for (pv in .ss_grid) {
      L <- stats::setNames(numeric(length(nm)), nm)
      L["(Intercept)"] <- 1
      L["pre"] <- pv
      if (dd == "bunk") {
        if ("directionbunk" %in% nm) L["directionbunk"] <- 1
        if ("directionbunk:pre" %in% nm) L["directionbunk:pre"] <- pv
      }
      est <- as.numeric(L %*% b)
      se  <- sqrt(as.numeric(t(L) %*% V %*% L))
      rows[[length(rows) + 1]] <- tibble::tibble(
        outcome = "aligned_belief_change", model = study_label, direction = dir_lab,
        term = paste0("pred_", pv), n = nrow(d), estimate = est,
        se = se, conf_low = est - 1.96 * se, conf_high = est + 1.96 * se,
        statistic = pv,
        note = "Lin model predicted aligned belief change at this baseline belief; HC3 delta-method CI.")
    }
  }

  # interaction slope: direction:pre coefficient (HC3)
  ic <- "directionbunk:pre"
  if (ic %in% nm) {
    e <- b[[ic]]; s <- sqrt(V[ic, ic])
    rows[[length(rows) + 1]] <- tibble::tibble(
      outcome = "aligned_belief_change", model = study_label, direction = "Bunk-minus-Debunk",
      term = "interaction_slope", n = nrow(d), estimate = e, se = s,
      conf_low = e - 1.96 * s, conf_high = e + 1.96 * s, statistic = e / s,
      p_value = 2 * stats::pnorm(-abs(e / s)),
      note = "direction:pre coefficient: change in bunk-minus-debunk gap per +1 baseline belief point; HC3.")
  }

  # contrast at pre=50: bunk-minus-debunk predicted difference
  L <- stats::setNames(numeric(length(nm)), nm)
  if ("directionbunk" %in% nm) L["directionbunk"] <- 1
  if ("directionbunk:pre" %in% nm) L["directionbunk:pre"] <- 50
  e <- as.numeric(L %*% b); s <- sqrt(as.numeric(t(L) %*% V %*% L))
  rows[[length(rows) + 1]] <- tibble::tibble(
    outcome = "aligned_belief_change", model = study_label, direction = "Bunk-minus-Debunk",
    term = "contrast_at_50", n = nrow(d), estimate = e, se = s,
    conf_low = e - 1.96 * s, conf_high = e + 1.96 * s, statistic = e / s,
    p_value = 2 * stats::pnorm(-abs(e / s)),
    note = "Bunk-minus-debunk predicted aligned change at baseline belief = 50; HC3.")
  dplyr::bind_rows(rows)
}

# ---------------------------------------------------------------------------------------
# S4 per-model cell: fit the SAME simple Lin model used for S1-3
# (aligned_belief_change ~ direction * pre) on ONE frontier model's rows, and emit the
# predicted-grid + interaction + contrast rows with model = the model name. We NEVER pool
# or equal-weight across the four S4 models; each model is its own cell (its own facet row).
# ---------------------------------------------------------------------------------------
.ss_cell_s4_model <- function(d, model_label, samp) {
  d <- d[is.finite(d$pre) & is.finite(d$aligned_belief_change) &
           as.character(d$direction) %in% c("bunk", "debunk"), , drop = FALSE]
  d$direction <- droplevels(.ss_dir(d$direction))
  if (nrow(d) < 20 || nlevels(d$direction) != 2) return(NULL)
  fit <- stats::lm(aligned_belief_change ~ direction * pre, data = d)
  V <- sandwich::vcovHC(fit, type = "HC3")
  b <- stats::coef(fit)
  nm <- names(b)

  # predicted-value rows over the grid, for each direction
  rows <- list()
  for (dd in c("bunk", "debunk")) {
    dir_lab <- if (dd == "bunk") "Bunking" else "Debunking"
    for (pv in .ss_grid) {
      L <- stats::setNames(numeric(length(nm)), nm)
      L["(Intercept)"] <- 1
      L["pre"] <- pv
      if (dd == "bunk") {
        if ("directionbunk" %in% nm) L["directionbunk"] <- 1
        if ("directionbunk:pre" %in% nm) L["directionbunk:pre"] <- pv
      }
      est <- as.numeric(L %*% b)
      se  <- sqrt(as.numeric(t(L) %*% V %*% L))
      rows[[length(rows) + 1]] <- tibble::tibble(
        outcome = "aligned_belief_change", model = model_label, direction = dir_lab,
        term = paste0("pred_", pv), n = nrow(d), estimate = est,
        se = se, conf_low = est - 1.96 * se, conf_high = est + 1.96 * se,
        statistic = pv,
        note = "Lin model predicted aligned belief change at this baseline belief; HC3 delta-method CI.")
    }
  }

  # interaction slope: direction:pre coefficient (HC3)
  ic <- "directionbunk:pre"
  if (ic %in% nm) {
    e <- b[[ic]]; s <- sqrt(V[ic, ic])
    rows[[length(rows) + 1]] <- tibble::tibble(
      outcome = "aligned_belief_change", model = model_label, direction = "Bunk-minus-Debunk",
      term = "interaction_slope", n = nrow(d), estimate = e, se = s,
      conf_low = e - 1.96 * s, conf_high = e + 1.96 * s, statistic = e / s,
      p_value = 2 * stats::pnorm(-abs(e / s)),
      note = "direction:pre coefficient: change in bunk-minus-debunk gap per +1 baseline belief point; HC3.")
  }

  # contrast at pre=50: bunk-minus-debunk predicted difference
  L <- stats::setNames(numeric(length(nm)), nm)
  if ("directionbunk" %in% nm) L["directionbunk"] <- 1
  if ("directionbunk:pre" %in% nm) L["directionbunk:pre"] <- 50
  e <- as.numeric(L %*% b); s <- sqrt(as.numeric(t(L) %*% V %*% L))
  rows[[length(rows) + 1]] <- tibble::tibble(
    outcome = "aligned_belief_change", model = model_label, direction = "Bunk-minus-Debunk",
    term = "contrast_at_50", n = nrow(d), estimate = e, se = s,
    conf_low = e - 1.96 * s, conf_high = e + 1.96 * s, statistic = e / s,
    p_value = 2 * stats::pnorm(-abs(e / s)),
    note = "Bunk-minus-debunk predicted aligned change at baseline belief = 50; HC3.")
  dplyr::bind_rows(rows)
}

compute_simple_slopes <- function(core_objects) {
  si_require(c("dplyr", "tibble", "sandwich"))
  out <- list()

  # S1-3: strict (full analytic) and compliant per study.
  for (sf in c("Jailbroken", "Standard", "Truth-Constrained")) {
    base <- core_objects$s13[core_objects$s13$study_factor == sf, , drop = FALSE]
    if (!nrow(base)) next
    for (samp in c("strict", "compliant")) {
      d <- base
      if (samp == "compliant") {
        d <- d[!is.na(d$compliant) & d$compliant == TRUE, , drop = FALSE]
      }
      d$pre <- suppressWarnings(as.numeric(d$belief_rating_pre_rc))
      r <- .ss_cell_s13(d, sf, samp)
      if (!is.null(r)) out[[length(out) + 1]] <- std_row(r, "Cross-study", "simple_slopes", samp)
    }
  }

  # Study 4: strict (n=1272) and compliant (n=1073). NEVER pooled — one cell PER MODEL
  # (Claude, Gemini, GPT-5.2, Grok), each its own simple Lin fit / facet row.
  s4_frames <- list(strict = core_objects$s4$s4, compliant = core_objects$s4$s4_compliant)
  s4_models <- c("Claude", "Gemini", "GPT-5.2", "Grok")
  for (samp in names(s4_frames)) {
    base <- s4_frames[[samp]]
    if (is.null(base) || !nrow(base)) next
    for (mm in s4_models) {
      d <- base[as.character(base$model_pooled) == mm, , drop = FALSE]
      if (!nrow(d)) next
      d$pre <- suppressWarnings(as.numeric(d$belief_rating_pre_4))
      r <- .ss_cell_s4_model(d, mm, samp)
      if (!is.null(r)) out[[length(out) + 1]] <- std_row(r, "Cross-study", "simple_slopes", samp)
    }
  }

  if (!length(out))
    return(std_row(tibble::tibble(term = character(0)), "Cross-study", "simple_slopes", "strict"))
  dplyr::bind_rows(out)
}
