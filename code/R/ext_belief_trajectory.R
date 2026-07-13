# ext_belief_trajectory.R -----------------------------------------------------------------
# Belief TRAJECTORY across timepoints (pre -> post -> debrief) and the empirical CDF of
# direction-aligned belief change, for Fig S5. Two blocks, both PURE all_numbers readers:
#
#   (A) block "belief_trajectory": per sample x study x arm x timepoint, the MEAN belief
#       LEVEL on the raw reverse-coded scale (higher = more conspiracy belief). S1-3 uses
#       belief_rating_{pre,post,debrf}_rc; Study 4 uses belief_rating_{pre,post,debrf}_4.
#       se = sd/sqrt(n); conf_low/high = mean +- 1.96 se; n = cell count.
#       statistic = timepoint order (1 pre, 2 post, 3 debrief). outcome = timepoint name.
#       Empty cells are dropped (S1-3 debunk+debrief is all-NA by design); S4 debrief IS
#       populated in both arms and is kept.
#
#   (B) block "belief_change_ecdf": per the SAME cell, the empirical CDF of direction-
#       aligned change on an x grid -60..60 by 4. For each grid point x:
#         estimate = mean(aligned_change <= x), statistic = x, term = paste0("cdf_", x),
#         n = cell n. (S1-3 aligned via aligned_belief_change col; S4 aligned_belief_change.)
#
# Samples (project vocabulary):
#   S1-3:  full_sample, compliant       (co$s13, co$s13[compliant])
#   S4:    strict_n1272, compliant_n1073 (co$s4$s4, co$s4$s4_compliant)
#
# ENTRY POINT: compute_belief_trajectory(core_objects) -> tibble (canonical schema).

.bt_TPS <- c("pre", "post", "debrief")
.bt_GRID <- seq(-60, 60, by = 4)

# One trajectory cell: mean belief LEVEL at one timepoint for one study x arm.
.bt_level_cell <- function(y, tp_name, tp_order, study_lab, arm_lab) {
  y <- y[is.finite(y)]
  n <- length(y)
  if (n < 2) return(NULL)
  m  <- mean(y)
  se <- stats::sd(y) / sqrt(n)
  tibble::tibble(
    outcome   = tp_name,
    model     = study_lab,
    direction = arm_lab,
    term      = paste0("mean_belief_", tp_name),
    n         = n,
    estimate  = m,
    se        = se,
    conf_low  = m - 1.96 * se,
    conf_high = m + 1.96 * se,
    statistic = tp_order,
    note      = sprintf("Mean belief level (reverse-coded, higher=more belief) at %s; statistic=timepoint order; CI=+-1.96*SE.", tp_name)
  )
}

# Empirical CDF rows for one study x arm cell of direction-aligned change.
.bt_ecdf_cell <- function(aligned, study_lab, arm_lab) {
  aligned <- aligned[is.finite(aligned)]
  n <- length(aligned)
  if (n < 2) return(NULL)
  do.call(rbind, lapply(.bt_GRID, function(x) {
    tibble::tibble(
      outcome   = "aligned_change_cdf",
      model     = study_lab,
      direction = arm_lab,
      term      = paste0("cdf_", x),
      n         = n,
      estimate  = mean(aligned <= x),
      statistic = x,
      note      = "Empirical CDF of direction-aligned belief change (P[aligned<=x]); statistic = x grid point."
    )
  }))
}

# Build BOTH blocks' rows for one analytic frame.
#   frame: data.frame with columns study_lab, arm_lab, b_pre, b_post, b_debrief, aligned.
.bt_frame_rows <- function(frame, study_levels, arm_levels) {
  traj <- list(); ecdf <- list()
  for (sl in study_levels) {
    for (al in arm_levels) {
      sub <- frame[frame$study_lab == sl & frame$arm_lab == al, , drop = FALSE]
      if (!nrow(sub)) next
      for (i in seq_along(.bt_TPS)) {
        tp <- .bt_TPS[i]
        col <- switch(tp, pre = "b_pre", post = "b_post", debrief = "b_debrief")
        r <- .bt_level_cell(sub[[col]], tp, i, sl, al)
        if (!is.null(r)) traj[[length(traj) + 1]] <- r
      }
      e <- .bt_ecdf_cell(sub$aligned, sl, al)
      if (!is.null(e)) ecdf[[length(ecdf) + 1]] <- e
    }
  }
  list(traj = traj, ecdf = ecdf)
}

# S1-3 frame for one sample (full_sample or compliant).
.bt_s13_frame <- function(d) {
  data.frame(
    pid        = as.character(d$response_id),
    study_lab  = as.character(d$study_factor),
    arm_lab    = ifelse(as.character(d$direction) == "bunk", "Bunking", "Debunking"),
    b_pre      = suppressWarnings(as.numeric(d$belief_rating_pre_rc)),
    b_post     = suppressWarnings(as.numeric(d$belief_rating_post_rc)),
    b_debrief  = suppressWarnings(as.numeric(d$belief_rating_debrf_rc)),
    aligned    = suppressWarnings(as.numeric(d$aligned_belief_change)),
    stringsAsFactors = FALSE
  )
}

# Study-4 frame for one sample (strict or compliant), PER MODEL.
# study_lab is set to the per-model name (Claude/Gemini/GPT-5.2/Grok); the pooled
# "Study 4" cell is intentionally NOT emitted (per-model only).
.bt_s4_frame <- function(d, model_lab) {
  data.frame(
    pid        = as.character(d$ResponseId),
    study_lab  = model_lab,
    arm_lab    = ifelse(as.character(d$direction) == "bunk", "Bunking", "Debunking"),
    b_pre      = suppressWarnings(as.numeric(d$belief_rating_pre_4)),
    b_post     = suppressWarnings(as.numeric(d$belief_rating_post_4)),
    b_debrief  = suppressWarnings(as.numeric(d$belief_rating_debrf_4)),
    aligned    = suppressWarnings(as.numeric(d$aligned_belief_change)),
    stringsAsFactors = FALSE
  )
}

# Build the per-model S4 frame for one sample by stacking each model's rows
# (study_lab = model name), so .bt_frame_rows emits 4 per-model cells.
.bt_s4_frame_permodel <- function(d, model_levels) {
  do.call(rbind, lapply(model_levels, function(ml) {
    .bt_s4_frame(d[as.character(d$model_pooled) == ml, , drop = FALSE], ml)
  }))
}

# (C) block "belief_trajectory_lmm" (added 2026-07-04): the mixed-effects model
# the manuscript describes for the trajectory figure -- belief ~ timepoint x arm
# + (1 | participant) -- fit per study/model unit so it lives in the recompute
# and is citable via num(). Emits Satterthwaite-tested fixed effects
# (term = "fe_<coef>") and emmeans cell means (term = "emm_<timepoint>",
# mirroring the descriptive block's row shape). S1-3 debunking has no debrief
# rating by design; the rank-deficient cell is dropped by lmer and only
# estimable emmeans cells are emitted. Descriptive block (A) is untouched;
# the two agree on pre/post cell means to ~2 decimals.
.bt_lmm_rows <- function(sub, study_lab) {
  sub <- sub[!is.na(sub$pid), , drop = FALSE]
  long <- do.call(rbind, lapply(seq_along(.bt_TPS), function(i) {
    col <- c("b_pre", "b_post", "b_debrief")[i]
    data.frame(pid = sub$pid, arm_lab = sub$arm_lab,
               timepoint = .bt_TPS[i], belief = sub[[col]],
               stringsAsFactors = FALSE)
  }))
  long <- long[is.finite(long$belief), , drop = FALSE]
  if (nrow(long) < 20 || length(unique(long$arm_lab)) < 2) return(NULL)
  long$timepoint <- factor(long$timepoint, levels = .bt_TPS)
  long$arm_lab <- factor(long$arm_lab, levels = c("Bunking", "Debunking"))
  fit <- tryCatch(
    suppressMessages(lmerTest::lmer(belief ~ timepoint * arm_lab + (1 | pid),
                                    data = long)),
    error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  n_pid <- length(unique(long$pid))

  fe <- tryCatch(as.data.frame(stats::coef(summary(fit))), error = function(e) NULL)
  fe_rows <- NULL
  if (!is.null(fe) && nrow(fe)) {
    fe_rows <- tibble::tibble(
      outcome   = "belief_lmm",
      model     = study_lab,
      direction = NA_character_,
      term      = paste0("fe_", rownames(fe)),
      n         = n_pid,
      estimate  = fe[, "Estimate"],
      se        = fe[, "Std. Error"],
      statistic = fe[, "t value"],
      df_den    = fe[, "df"],
      p_value   = fe[, "Pr(>|t|)"],
      note      = sprintf("lmer(belief ~ timepoint*arm + (1|pid)) fixed effect; Satterthwaite df; n = %d participants, %d observations.", n_pid, nrow(long))
    )
  }

  emm_rows <- tryCatch({
    em <- as.data.frame(emmeans::emmeans(fit, ~ timepoint * arm_lab,
                                         lmer.df = "satterthwaite"))
    em <- em[is.finite(em$emmean), , drop = FALSE]
    if (!nrow(em)) NULL else tibble::tibble(
      outcome   = as.character(em$timepoint),
      model     = study_lab,
      direction = as.character(em$arm_lab),
      term      = paste0("emm_", as.character(em$timepoint)),
      n         = n_pid,
      estimate  = em$emmean,
      se        = em$SE,
      conf_low  = em$lower.CL,
      conf_high = em$upper.CL,
      statistic = match(as.character(em$timepoint), .bt_TPS),
      note      = "emmeans cell mean from the trajectory lmer; statistic = timepoint order; only estimable cells emitted."
    )
  }, error = function(e) NULL)

  dplyr::bind_rows(fe_rows, emm_rows)
}

compute_belief_trajectory <- function(core_objects) {
  si_require(c("dplyr", "tibble"))

  s13 <- core_objects$s13
  s13_levels <- c("Jailbroken", "Standard", "Truth-Constrained")
  s4_levels  <- c("Claude", "Gemini", "GPT-5.2", "Grok")
  arm_levels <- c("Bunking", "Debunking")

  # (sample id -> analytic frame). Study 4 is emitted PER MODEL (4 cells), never
  # as a single pooled "Study 4" row.
  jobs <- list(
    list(section = "S1-3",        sample = "full_sample",
         frame = .bt_s13_frame(s13),
         studies = s13_levels),
    list(section = "S1-3",        sample = "compliant",
         frame = .bt_s13_frame(s13[s13$compliant %in% TRUE, , drop = FALSE]),
         studies = s13_levels),
    list(section = "S4 + pooled", sample = "strict_n1272",
         frame = .bt_s4_frame_permodel(core_objects$s4$s4, s4_levels),
         studies = s4_levels),
    list(section = "S4 + pooled", sample = "compliant_n1073",
         frame = .bt_s4_frame_permodel(core_objects$s4$s4_compliant, s4_levels),
         studies = s4_levels)
  )

  traj_out <- list(); ecdf_out <- list(); lmm_out <- list()
  for (j in jobs) {
    rr <- .bt_frame_rows(j$frame, j$studies, arm_levels)
    if (length(rr$traj)) {
      traj_out[[length(traj_out) + 1]] <-
        std_row(dplyr::bind_rows(rr$traj), j$section, "belief_trajectory", j$sample)
    }
    if (length(rr$ecdf)) {
      ecdf_out[[length(ecdf_out) + 1]] <-
        std_row(dplyr::bind_rows(rr$ecdf), j$section, "belief_change_ecdf", j$sample)
    }
    if (requireNamespace("lmerTest", quietly = TRUE) &&
        requireNamespace("emmeans", quietly = TRUE)) {
      for (sl in j$studies) {
        lr <- .bt_lmm_rows(j$frame[j$frame$study_lab == sl, , drop = FALSE], sl)
        if (!is.null(lr) && nrow(lr)) {
          lmm_out[[length(lmm_out) + 1]] <-
            std_row(lr, j$section, "belief_trajectory_lmm", j$sample)
        }
      }
    }
  }

  out <- dplyr::bind_rows(c(traj_out, ecdf_out, lmm_out))
  if (!nrow(out)) {
    return(std_row(tibble::tibble(term = character(0)), "S1-3", "belief_trajectory", "full_sample"))
  }
  out
}
