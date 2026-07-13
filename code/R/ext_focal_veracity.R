# ext_focal_veracity.R ---------------------------------------------------------
# Veracity of the FOCAL CONSPIRACIES themselves (added 2026-07-04).
#
# Reads the frozen focal-statement veracity scores produced by the fresh
# provenance pipeline (code/provenance/python_api/focal_veracity/): one row per
# analytic participant, scoring the AI restatement (`conRestatement`) -- the
# declarative statement of the participant's focal conspiracy that anchored the
# belief ratings -- on a 0-100 veracity scale with a statement_type
# classification (conspiracy_claim / official_account / no_claim).
#
# Blocks:
#   * "focal_veracity"        per study + pooled: statement-type composition;
#                             among genuine conspiracy claims -- mean veracity,
#                             % scored false (<40), % scored true (>=60);
#                             sensitivity row folding in orientation-flipped
#                             denials (official_account scored as 100 - v).
#   * "focal_veracity_subset" bunk/debunk effects WITHIN the clearly-false
#                             conspiracy subset (and the complement), mirroring
#                             the primary spec: S1-3 additive ANCOVA
#                             (aligned change ~ condition + baseline, HC3);
#                             S4 equal-model-weighted direction contrast, HC3.
#
# FAIL-SOFT: if data/api_cached/focal_veracity/ is absent (scores not yet run),
# emits zero rows so the build and SI render cleanly without it.
#
# ENTRY POINT: compute_focal_veracity(core_objects) -> tibble (canonical schema).

.FV_FALSE_CUT <- 40
.FV_TRUE_CUT  <- 60

.fv_read <- function(repo_root) {
  p <- file.path(repo_root, "data", "api_cached", "focal_veracity",
                 "focal_statement_veracity_all_studies.csv")
  if (!file.exists(p)) return(NULL)
  out <- tryCatch(readr::read_csv(p, show_col_types = FALSE), error = function(e) NULL)
  if (is.null(out) || !nrow(out)) return(NULL)
  out$veracity_score <- suppressWarnings(as.numeric(out$veracity_score))
  out
}

.fv_study_lab <- c(study1 = "Jailbroken", study2 = "Standard",
                   study3 = "Truth-Constrained", study4 = "Study 4")

.fv_summary_rows <- function(d, lab) {
  n_all <- nrow(d)
  if (!n_all) return(NULL)
  rows <- list()
  mk <- function(term, est, n, note, outcome = "focal_veracity") {
    tibble::tibble(outcome = outcome, model = lab, direction = NA_character_,
                   term = term, n = n, estimate = est, note = note)
  }
  for (st in c("conspiracy_claim", "official_account", "no_claim")) {
    rows[[length(rows) + 1]] <- mk(paste0("share_", st),
                                   100 * mean(d$statement_type == st, na.rm = TRUE), n_all,
                                   "Statement-type composition of scored restatements (%).")
  }
  cc <- d[d$statement_type %in% "conspiracy_claim" & is.finite(d$veracity_score), , drop = FALSE]
  if (nrow(cc) >= 5) {
    rows[[length(rows) + 1]] <- mk("mean_veracity", mean(cc$veracity_score), nrow(cc),
      "Mean 0-100 veracity among genuine conspiracy-claim restatements.")
    rows[[length(rows) + 1]] <- mk("pct_false", 100 * mean(cc$veracity_score < .FV_FALSE_CUT), nrow(cc),
      sprintf("%% of conspiracy claims scored false (< %d).", .FV_FALSE_CUT))
    rows[[length(rows) + 1]] <- mk("pct_true", 100 * mean(cc$veracity_score >= .FV_TRUE_CUT), nrow(cc),
      sprintf("%% of conspiracy claims scored true (>= %d).", .FV_TRUE_CUT))
    rows[[length(rows) + 1]] <- mk("pct_uncertain",
      100 * mean(cc$veracity_score >= .FV_FALSE_CUT & cc$veracity_score < .FV_TRUE_CUT), nrow(cc),
      sprintf("%% of conspiracy claims in the mixed/uncertain band [%d, %d).", .FV_FALSE_CUT, .FV_TRUE_CUT))
  }
  # sensitivity: fold in denial-phrased restatements with flipped orientation
  oa <- d[d$statement_type %in% "official_account" & is.finite(d$veracity_score), , drop = FALSE]
  if (nrow(cc) >= 5) {
    v_sens <- c(cc$veracity_score, 100 - oa$veracity_score)
    rows[[length(rows) + 1]] <- mk("pct_false_sens_flipdenials",
      100 * mean(v_sens < .FV_FALSE_CUT), length(v_sens),
      "Sensitivity: conspiracy claims plus orientation-flipped official-account restatements (100 - v).")
  }
  dplyr::bind_rows(rows)
}

.fv_s13_subset_rows <- function(s13, fv) {
  # join veracity onto the S1-3 analytic frame; primary-spec ANCOVA within subsets
  key <- fv[fv$study %in% c("study1", "study2", "study3"),
            c("response_id", "statement_type", "veracity_score")]
  d <- s13
  d$statement_type <- key$statement_type[match(as.character(d$response_id), key$response_id)]
  d$focal_veracity <- key$veracity_score[match(as.character(d$response_id), key$response_id)]
  d$aligned <- suppressWarnings(as.numeric(d$aligned_belief_change))
  d$pre <- suppressWarnings(as.numeric(d$belief_rating_pre_rc))
  out <- list()
  subsets <- list(
    false_conspiracies = function(x) x$statement_type %in% "conspiracy_claim" & x$focal_veracity < .FV_FALSE_CUT,
    nonfalse_conspiracies = function(x) x$statement_type %in% "conspiracy_claim" & x$focal_veracity >= .FV_FALSE_CUT
  )
  for (sf in levels(d$study_factor)) {
    for (ss in names(subsets)) {
      sub <- d[d$study_factor == sf & subsets[[ss]](d) &
                 is.finite(d$aligned) & is.finite(d$pre), , drop = FALSE]
      if (nrow(sub) < 40 || length(unique(sub$direction)) < 2) next
      sub$dir <- factor(as.character(sub$direction), levels = c("debunk", "bunk"))
      fit <- stats::lm(aligned ~ dir + scale(pre, scale = FALSE), data = sub)
      tt <- hc3_tidy(fit)
      b <- tt[tt$term == "dirbunk", , drop = FALSE]
      for (dr in c("bunk", "debunk")) {
        x <- sub$aligned[as.character(sub$direction) == dr]
        ci <- mean_ci(x)
        out[[length(out) + 1]] <- tibble::tibble(
          outcome = ss, model = sf,
          direction = ifelse(dr == "bunk", "Bunking", "Debunking"),
          term = "aligned_change_mean", n = ci$n, estimate = ci$estimate,
          conf_low = ci$conf.low, conf_high = ci$conf.high,
          note = sprintf("Mean aligned change within the %s subset (focal veracity %s %d).",
                         ss, ifelse(ss == "false_conspiracies", "<", ">="), .FV_FALSE_CUT))
      }
      out[[length(out) + 1]] <- tibble::tibble(
        outcome = ss, model = sf, term = "bunk_minus_debunk_ancova",
        n = nrow(sub), estimate = b$estimate, se = b$std.error,
        conf_low = b$conf.low, conf_high = b$conf.high,
        statistic = b$statistic, p_value = b$p.value,
        note = "Primary-spec additive ANCOVA (aligned ~ condition + centered baseline), HC3, within subset.")
    }
  }
  if (!length(out)) return(NULL)
  dplyr::bind_rows(out)
}

.fv_s4_subset_rows <- function(s4, fv) {
  key <- fv[fv$study == "study4", c("response_id", "statement_type", "veracity_score")]
  d <- s4
  d$statement_type <- key$statement_type[match(as.character(d$ResponseId), key$response_id)]
  d$focal_veracity <- key$veracity_score[match(as.character(d$ResponseId), key$response_id)]
  d$aligned <- suppressWarnings(as.numeric(d$aligned_belief_change))
  out <- list()
  subsets <- list(
    false_conspiracies = function(x) x$statement_type %in% "conspiracy_claim" & x$focal_veracity < .FV_FALSE_CUT,
    nonfalse_conspiracies = function(x) x$statement_type %in% "conspiracy_claim" & x$focal_veracity >= .FV_FALSE_CUT
  )
  for (ss in names(subsets)) {
    sub <- d[subsets[[ss]](d) & is.finite(d$aligned), , drop = FALSE]
    sub <- sub[as.character(sub$direction) %in% c("bunk", "debunk"), , drop = FALSE]
    if (nrow(sub) < 80) next
    sub$direction <- factor(as.character(sub$direction), levels = c("bunk", "debunk"))
    sub$model_pooled <- droplevels(factor(as.character(sub$model_pooled)))
    if (length(levels(sub$model_pooled)) < 2) next
    fit <- stats::lm(aligned ~ direction * model_pooled, data = sub)
    vc <- sandwich::vcovHC(fit, type = "HC3")
    model_levels <- levels(sub$model_pooled)
    int_terms <- paste0("directiondebunk:model_pooled", setdiff(model_levels, model_levels[1]))
    int_terms <- int_terms[int_terms %in% names(stats::coef(fit))]
    w <- c("directiondebunk" = 1)
    if (length(int_terms)) w[int_terms] <- 1 / length(model_levels)
    ew <- coef_test_row(fit, vc, w, "ew")
    out[[length(out) + 1]] <- tibble::tibble(
      outcome = ss, model = "Study 4", term = "debunk_minus_bunk_equal_weighted",
      n = nrow(sub), estimate = ew$estimate, se = ew$se,
      conf_low = ew$ci_low, conf_high = ew$ci_high,
      statistic = ew$t, p_value = ew$p,
      note = "Equal-model-weighted debunk - bunk on aligned change, HC3, within subset.")
  }
  if (!length(out)) return(NULL)
  dplyr::bind_rows(out)
}

compute_focal_veracity <- function(core_objects) {
  si_require(c("dplyr", "tibble", "readr"))
  fv <- .fv_read(core_objects$pkg_root)
  if (is.null(fv)) {
    # scores not yet cached: contribute nothing (NULL is dropped by bind_rows;
    # a zero-row frame would carry logical-typed columns and break the bind)
    return(NULL)
  }

  rows <- list()
  for (st in names(.fv_study_lab)) {
    r <- .fv_summary_rows(fv[fv$study == st, , drop = FALSE], .fv_study_lab[[st]])
    if (!is.null(r)) rows[[length(rows) + 1]] <- r
  }
  r <- .fv_summary_rows(fv, "All studies")
  if (!is.null(r)) rows[[length(rows) + 1]] <- r
  summary_rows <- std_row(dplyr::bind_rows(rows), "Veracity", "focal_veracity", "analytic")

  sub13 <- .fv_s13_subset_rows(core_objects$s13, fv)
  sub4  <- .fv_s4_subset_rows(core_objects$s4$s4_with_compliance, fv)
  subset_rows <- NULL
  if (!is.null(sub13) || !is.null(sub4)) {
    subset_rows <- std_row(dplyr::bind_rows(sub13, sub4),
                           "Veracity", "focal_veracity_subset", "analytic")
  }
  dplyr::bind_rows(summary_rows, subset_rows)
}
