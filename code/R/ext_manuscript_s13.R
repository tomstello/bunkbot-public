# ext_manuscript_s13.R --------------------------------------------------------
# Manuscript-reported Abstract / Study 1-3 quantities that the main-text Rmds
# (results_methods.Rmd inline chunks; code/sections/results_s2.Rmd and
# results_s3.Rmd) compute INLINE but that had no ALL_NUMBERS row, so they were
# invisible to the one-recompute manifest (code/manuscript_wiring/manifest_spec.csv).
# Every computation here is a faithful PORT of the referenced Rmd chunk — same
# estimator, same sample, same covariates, same HC3 choices — never a re-derivation.
#
# ENTRY POINT: compute_manuscript_s13(core_objects) -> tibble (canonical 17-col
# schema via std_row). Section "Manuscript", block "manuscript_extra"; `term` is
# the manifest key stem (one row can feed several manifest keys through different
# stat columns). Terms emitted:
#
#   abs.total_n                      pooled analytic N (S1-3 frame + S4 strict)
#   s{1,2,3}.{bunk,debunk}[_belief]  within-arm paired t of direction-aligned change
#   s{1,2,3}.*.pct_change            mean aligned change / arm pre-treatment mean
#   s1.belief_contrast               S1 adjusted bunk-debunk ANCOVA contrast (HC3)
#   s1.trust.{bunk,debunk}_g         trust change g = mean change / pooled pre SD
#   s1.trust.diff_g / s2.trust.g     between-condition trust g (unadjusted, HC3 p)
#   s1.debrief.net                   S1 bunking baseline->debrief paired t
#   s2.perc.{newinfo,arg,collab,unbiased}  two-sample pooled-SD Cohen d (Welch t)
#   claims.k / claims.per_conv       S1-3 extracted-and-fact-checked claim census
#   s3.{bunk,debunk}.cross_ps        S3-vs-earlier per-arm family (full sample)
#   s3.ape.bunk_noncomp              1 - S3 bunking APE attempt rate
#   s3.retention.vs_s{1,2}           compliant ANCOVA bunking-cell ratio TC/other
#   s3.retention.ps                  compliant bunking S3-vs-earlier family
#   s3.debunk_equiv.ps               compliant debunking all-pairwise family
#   topq.s{1,2,3}                    top-veracity-quartile bunking cells (n + mean)
#
# Family rows (".ps"/"cross_ps"): p_value = the family MINIMUM p (the binding
# member for the doc's "ps > .XX" claims; p_gt floors it); the note lists every
# member contrast with its p so the family is auditable from the row alone.
# Cross-study contrasts mirror results_s2.Rmd s2-crossstudy exactly: the two
# studies compared are pooled, change ~ study_factor * condition_factor +
# belief_rating_pre_rc is fit with HC3, and the per-arm cell contrast is the
# g-computation difference (model_matrix_for_fit/linear_combo machinery).
#
# Sign convention: values are stored as computed under the engine's aligned /
# positive conventions (e.g. `change` is direction-aligned; proportions stay
# proportions). The manifest layer applies abs/neg/pct formatting.

.ms13_hc3 <- function(m) sandwich::vcovHC(m, type = "HC3")

# Baseline-adjusted marginal cell / contrast via g-computation over the fitted
# model — verbatim port of lin_cell()/lin_contrast() from results_methods.Rmd.
.ms13_lin_cell <- function(mod, dat, level, vc) {
  nd <- dat
  nd$condition_factor <- factor(level, levels = levels(dat$condition_factor))
  linear_combo(mod, colMeans(model_matrix_for_fit(mod, nd)), vc = vc)
}
.ms13_lin_contrast <- function(mod, dat, a, b, vc) {
  nda <- dat; nda$condition_factor <- factor(a, levels = levels(dat$condition_factor))
  ndb <- dat; ndb$condition_factor <- factor(b, levels = levels(dat$condition_factor))
  w <- colMeans(model_matrix_for_fit(mod, nda)) - colMeans(model_matrix_for_fit(mod, ndb))
  linear_combo(mod, w, vc = vc)
}

# Per-arm cell contrast between two studies, pooled interaction ANCOVA (HC3) —
# verbatim port of the s2-crossstudy chunk (code/sections/results_s2.Rmd:103-122)
# generalized to any study pair / arm / sample.
.ms13_cross_cell <- function(dat, study_a, study_b, arm) {
  dd <- dat[as.character(dat$study_factor) %in% c(study_a, study_b), , drop = FALSE]
  dd <- droplevels(dd)
  mp <- stats::lm(change ~ study_factor * condition_factor + belief_rating_pre_rc, data = dd)
  Vp <- .ms13_hc3(mp)
  cw <- function(study, cond) {
    nd <- dd
    nd$study_factor <- factor(study, levels = levels(dd$study_factor))
    nd$condition_factor <- factor(cond, levels = levels(dd$condition_factor))
    colMeans(model_matrix_for_fit(mp, nd))
  }
  lc <- linear_combo(mp, cw(study_a, arm) - cw(study_b, arm), vc = Vp)
  lc$n <- nrow(dd)
  lc
}

# Perception recodes to native metric — verbatim port of recode_perceptions()
# (results_methods.Rmd setup; same maps as the Study-4 instrument).
.ms13_recode_perceptions <- function(df) {
  df |> dplyr::mutate(
    arg_strength_rc = suppressWarnings(as.numeric(arg_strength)),
    new_info_rc = dplyr::case_when(
      new_info == 1 ~ 1, new_info == 4 ~ 2, new_info == 5 ~ 3, new_info == 6 ~ 4,
      new_info == 7 ~ 5, new_info == 8 ~ 6, new_info == 9 ~ 7, new_info == 10 ~ 8,
      new_info == 11 ~ 9, new_info == 12 ~ 10, TRUE ~ NA_real_
    ),
    collaborative_rc = dplyr::case_when(
      collaborative == 69 ~ -2, collaborative == 70 ~ -1, collaborative == 71 ~ 0,
      collaborative == 72 ~ 1, collaborative == 73 ~ 2, TRUE ~ NA_real_
    ),
    unbiased_rc = dplyr::case_when(
      unbiased == 69 ~ -2, unbiased == 70 ~ -1, unbiased == 71 ~ 0,
      unbiased == 72 ~ 1, unbiased == 73 ~ 2, TRUE ~ NA_real_
    )
  )
}

# Two-sample pooled-SD Cohen d + Welch t — verbatim port of cohen_d2()
# (results_methods.Rmd setup; the doc's reported perception d).
.ms13_cohen_d2 <- function(dat, item) {
  x <- dat[[item]][dat$condition_factor == "Bunking"];   x <- x[!is.na(x)]
  y <- dat[[item]][dat$condition_factor == "Debunking"]; y <- y[!is.na(y)]
  sp <- sqrt(((length(x) - 1) * stats::var(x) + (length(y) - 1) * stats::var(y)) /
               (length(x) + length(y) - 2))
  tt <- stats::t.test(x, y)
  list(d = (mean(x) - mean(y)) / sp, t = unname(tt$statistic),
       df = unname(tt$parameter), p = tt$p.value, n = length(x) + length(y),
       m_bunk = mean(x), m_debunk = mean(y))
}

# One within-arm paired-t row (t.test of the direction-aligned change) plus its
# pct_change companion row — ports the s{1,2,3}-belief-vals chunks and
# within_summary()'s pct_change = mean(change) / mean(pre).
.ms13_paired_rows <- function(dat, model_lab, arm, stem) {
  x   <- dat$change[dat$condition_factor == arm]
  pre <- dat$belief_rating_pre_rc[dat$condition_factor == arm]
  ok <- !is.na(x); x <- x[ok]; pre <- pre[ok]
  tt <- stats::t.test(x)
  dplyr::bind_rows(
    tibble::tibble(
      model = model_lab, direction = arm, term = stem,
      n = length(x), estimate = mean(x), se = stats::sd(x) / sqrt(length(x)),
      conf_low = tt$conf.int[1], conf_high = tt$conf.int[2],
      statistic = unname(tt$statistic), df_den = length(x) - 1,
      p_value = tt$p.value,
      note = "within-arm paired t-test of direction-aligned belief change (t.test(change)); statistic = t"
    ),
    tibble::tibble(
      model = model_lab, direction = arm, term = paste0(stem, ".pct_change"),
      n = length(x), estimate = mean(x) / mean(pre),
      note = sprintf("mean aligned change / arm pre-treatment mean (within_summary pct_change); %.3f/%.3f; stored as proportion",
                     mean(x), mean(pre))
    )
  )
}

# Trust-in-AI block for one study: within-arm g (mean change / pooled
# pre-treatment genai_trust SD) + between-condition g with the unadjusted HC3
# contrast p — verbatim port of the s1-trust / s2-trust chunks.
.ms13_trust_rows <- function(dat, model_lab, cell_terms, diff_term) {
  dt <- dat |>
    dplyr::mutate(
      trust_change = suppressWarnings(as.numeric(trust2) - as.numeric(genai_trust)),
      dir = factor(dplyr::if_else(condition_factor == "Bunking", "bunk", "debunk"),
                   levels = c("debunk", "bunk"))
    ) |>
    dplyr::filter(!is.na(trust_change))
  sp <- dt |>
    dplyr::group_by(condition_factor) |>
    dplyr::summarise(n = dplyr::n(),
                     v = stats::var(suppressWarnings(as.numeric(genai_trust)), na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::summarise(x = sqrt(sum((n - 1) * v) / (sum(n) - dplyr::n()))) |>
    dplyr::pull(x)
  rows <- list()
  for (arm in names(cell_terms)) {
    x <- dt$trust_change[dt$condition_factor == arm]
    rows[[length(rows) + 1]] <- tibble::tibble(
      outcome = "Trust in AI (pre->post)", model = model_lab, direction = arm,
      term = cell_terms[[arm]],
      n = length(x), estimate = mean(x) / sp, statistic = mean(x) / stats::sd(x),
      note = sprintf("g = mean trust change / pooled pre-treatment genai_trust SD (sp=%.4f); mean change=%.4f; statistic = d_z",
                     sp, mean(x))
    )
  }
  ct <- hc3_tidy(stats::lm(trust_change ~ dir, data = dt))
  ct <- ct[ct$term == "dirbunk", , drop = FALSE]
  rows[[length(rows) + 1]] <- tibble::tibble(
    outcome = "Trust in AI (pre->post)", model = model_lab, term = diff_term,
    n = nrow(dt), estimate = ct$estimate / sp,
    statistic = ct$statistic, p_value = ct$p.value,
    note = sprintf("between-condition g = unadjusted bunk-minus-debunk (%.4f, HC3 se=%.4f) / pooled pre SD (sp=%.4f); statistic/p from the HC3 contrast",
                   ct$estimate, ct$std.error, sp)
  )
  dplyr::bind_rows(rows)
}

# S1-3 extracted-claim census from the per-study extraction/veracity JSONLs
# (paths$nfacts), analytic sample only — same line parse as
# ext_claim_counts_full.R (.ccf_s13_extracted_rows), extended with the
# veracity_request_status filter so the fact-checked total is available.
.ms13_claims_census <- function(paths, s13) {
  if (is.null(paths$nfacts) || !all(file.exists(paths$nfacts))) return(NULL)
  n_extracted <- 0L
  n_checked <- 0L
  for (st in names(paths$nfacts)) {
    lines <- readLines(paths$nfacts[[st]], warn = FALSE)
    if (!length(lines)) next
    rid <- vapply(lines, function(L) {
      m <- regmatches(L, regexpr('"ResponseId"\\s*:\\s*"[^"]+"', L, perl = TRUE))
      if (!length(m)) return(NA_character_)
      sub('.*"ResponseId"\\s*:\\s*"', '', sub('"$', '', m[1]))
    }, character(1), USE.NAMES = FALSE)
    ok   <- grepl('"request_status"\\s*:\\s*"success"', lines, perl = TRUE)
    vok  <- grepl('"veracity_request_status"\\s*:\\s*"success"', lines, perl = TRUE)
    keep <- rid %in% s13$response_id
    n_extracted <- n_extracted + sum(ok & keep)
    n_checked   <- n_checked + sum(ok & vok & keep)
  }
  list(n_extracted = n_extracted, n_checked = n_checked)
}

compute_manuscript_s13 <- function(core_objects) {
  si_require(c("dplyr", "tibble", "readr", "sandwich"))
  s13 <- core_objects$s13
  paths <- core_objects$paths
  if (is.null(paths)) paths <- pkg_paths(core_objects$pkg_root)
  rows <- list()
  add <- function(df, sample) {
    if (!is.null(df) && nrow(df)) {
      rows[[length(rows) + 1]] <<- std_row(df, "Manuscript", "manuscript_extra", sample)
    }
  }

  ## ---- Abstract: pooled analytic N (S1-3 analytic frame + S4 strict) --------
  add(tibble::tibble(
    model = "Pooled S1-4", term = "abs.total_n",
    n = nrow(s13) + nrow(core_objects$s4$s4),
    estimate = nrow(s13) + nrow(core_objects$s4$s4),
    note = sprintf("S1-3 analytic frame (n=%d) + S4 strict sample (n=%d); simple sum of the two engine samples",
                   nrow(s13), nrow(core_objects$s4$s4))
  ), "pooled_s1s4")

  ## ---- within-arm paired t + pct change, all three studies -------------------
  # Manifest key stems differ by study (s1.*_belief vs s2/s3 short forms); the
  # terms follow the stems so the manifest filters stay literal.
  stems <- list(
    Jailbroken          = c(Bunking = "s1.bunk_belief", Debunking = "s1.debunk_belief"),
    Standard            = c(Bunking = "s2.bunk",        Debunking = "s2.debunk"),
    `Truth-Constrained` = c(Bunking = "s3.bunk",        Debunking = "s3.debunk")
  )
  for (sf in names(stems)) {
    dat <- s13[as.character(s13$study_factor) == sf, , drop = FALSE] |> droplevels()
    for (arm in c("Bunking", "Debunking")) {
      add(.ms13_paired_rows(dat, sf, arm, stems[[sf]][[arm]]), "full_sample")
    }
  }

  ## ---- S1 adjusted bunk-debunk contrast (ANCOVA + HC3, g-computation) --------
  s1 <- s13[as.character(s13$study_factor) == "Jailbroken", , drop = FALSE] |> droplevels()
  m_s1 <- stats::lm(change ~ condition_factor + belief_rating_pre_rc, data = s1)
  ct1 <- .ms13_lin_contrast(m_s1, s1, "Bunking", "Debunking", .ms13_hc3(m_s1))
  add(tibble::tibble(
    model = "Jailbroken", term = "s1.belief_contrast",
    n = nrow(s1), estimate = ct1$estimate, se = ct1$std.error,
    conf_low = ct1$conf.low, conf_high = ct1$conf.high,
    statistic = ct1$estimate / ct1$std.error,
    df_den = stats::df.residual(m_s1), p_value = ct1$p.value,
    note = "adjusted bunk minus debunk (change ~ condition + baseline, HC3, g-computation cells); statistic = estimate/se"
  ), "full_sample")

  ## ---- trust-in-AI standardized effects (S1 cells + S1/S2 contrasts) ---------
  add(.ms13_trust_rows(s1, "Jailbroken",
                       cell_terms = c(Bunking = "s1.trust.bunk_g", Debunking = "s1.trust.debunk_g"),
                       diff_term = "s1.trust.diff_g"), "full_sample")
  s2 <- s13[as.character(s13$study_factor) == "Standard", , drop = FALSE] |> droplevels()
  s2_trust <- .ms13_trust_rows(s2, "Standard",
                               cell_terms = c(Bunking = "s2.trust.bunk_g", Debunking = "s2.trust.debunk_g"),
                               diff_term = "s2.trust.g")
  add(s2_trust, "full_sample")

  ## ---- S1 corrective debrief: baseline -> debrief paired t -------------------
  sb <- s1[s1$condition_factor == "Bunking" & !is.na(s1$belief_rating_debrf_rc), , drop = FALSE]
  net <- stats::t.test(sb$belief_rating_debrf_rc - sb$belief_rating_pre_rc)
  add(tibble::tibble(
    model = "Jailbroken", direction = "Bunking", term = "s1.debrief.net",
    n = nrow(sb), estimate = unname(net$estimate),
    conf_low = net$conf.int[1], conf_high = net$conf.int[2],
    statistic = unname(net$statistic), df_den = unname(net$parameter),
    p_value = net$p.value,
    note = "paired t of belief_rating_debrf_rc - belief_rating_pre_rc, S1 bunking debrief completers; negative = below baseline"
  ), "full_sample")

  ## ---- S2 perception judgments: pooled-SD two-sample d ------------------------
  s2r <- .ms13_recode_perceptions(s2)
  perc <- c(`s2.perc.arg`      = "arg_strength_rc",
            `s2.perc.newinfo`  = "new_info_rc",
            `s2.perc.collab`   = "collaborative_rc",
            `s2.perc.unbiased` = "unbiased_rc")
  perc_lab <- c(arg_strength_rc = "Argument strength", new_info_rc = "Provided new information",
                collaborative_rc = "Collaborativeness", unbiased_rc = "Impartiality (unbiased)")
  for (tm in names(perc)) {
    r <- .ms13_cohen_d2(s2r, perc[[tm]])
    add(tibble::tibble(
      outcome = unname(perc_lab[[perc[[tm]]]]), model = "Standard", term = tm,
      n = r$n, estimate = r$d, statistic = r$t, df_den = r$df, p_value = r$p,
      note = sprintf("two-sample Cohen d, POOLED SD (cohen_d2; bunk minus debunk on the recoded item); mBunk=%.4f mDebunk=%.4f; statistic = Welch t",
                     r$m_bunk, r$m_debunk)
    ), "full_sample")
  }

  ## ---- S1-3 claim census: extracted-and-fact-checked total + per conversation -
  census <- .ms13_claims_census(paths, s13)
  if (!is.null(census)) {
    add(tibble::tibble(
      model = "Studies 1-3", term = "claims.k",
      n = nrow(s13), estimate = census$n_checked,
      note = sprintf("extraction success AND veracity-rating success (nfacts JSONLs), analytic sample only; all-extracted total = %d (the %d gap = veracity-rating failures)",
                     census$n_extracted, census$n_extracted - census$n_checked)
    ), "cached_api_claims")
    add(tibble::tibble(
      model = "Studies 1-3", term = "claims.per_conv",
      n = nrow(s13), estimate = census$n_checked / nrow(s13),
      note = sprintf("fact-checked claims per analytic conversation: %d / %d (same numerator as claims.k)",
                     census$n_checked, nrow(s13))
    ), "cached_api_claims")
  }

  ## ---- cross-study per-arm families (full sample): S3 vs each earlier study --
  fam_row <- function(members, term, model_lab, direction, note_head) {
    ps <- vapply(members, function(m) m$lc$p.value, numeric(1))
    tibble::tibble(
      model = model_lab, direction = direction, term = term,
      # n = family size: also keeps the row visible to num(), which drops rows
      # whose estimate/n/statistic are all NA (access.R)
      n = length(members),
      p_value = min(ps),
      note = paste0(note_head, "; p_value = family MINIMUM; members: ",
                    paste(vapply(seq_along(members), function(i) {
                      sprintf("%s est=%.3f p=%.4g (n=%d)", names(members)[i],
                              members[[i]]$lc$estimate, members[[i]]$lc$p.value, members[[i]]$lc$n)
                    }, character(1)), collapse = "; "))
    )
  }
  for (arm in c("Bunking", "Debunking")) {
    members <- list(
      `S3 vs S1` = list(lc = .ms13_cross_cell(s13, "Truth-Constrained", "Jailbroken", arm)),
      `S3 vs S2` = list(lc = .ms13_cross_cell(s13, "Truth-Constrained", "Standard", arm))
    )
    add(fam_row(members,
                term = if (arm == "Bunking") "s3.bunk.cross_ps" else "s3.debunk.cross_ps",
                model_lab = "Truth-Constrained", direction = arm,
                note_head = "pairwise pooled ANCOVA (study x condition + baseline, HC3), per-arm g-computation cell contrast (s2-crossstudy construction), full sample"),
        "full_sample")
  }

  ## ---- S3 APE bunking non-compliance rate -------------------------------------
  s3 <- s13[as.character(s13$study_factor) == "Truth-Constrained", , drop = FALSE] |> droplevels()
  s3b <- s3[s3$condition_factor == "Bunking", , drop = FALSE]
  p_att <- mean(s3b$evaluator_label == 1, na.rm = TRUE)
  add(tibble::tibble(
    model = "Truth-Constrained", direction = "Bunking", term = "s3.ape.bunk_noncomp",
    n = nrow(s3b), estimate = 1 - p_att,
    se = sqrt(p_att * (1 - p_att) / nrow(s3b)),
    note = sprintf("1 - turn-1 APE attempt rate (evaluator_label==1), S3 bunking arm; attempt rate = %.4f; stored as proportion", p_att)
  ), "full_sample")

  ## ---- compliant relative efficacy + compliant cross-study families -----------
  comp <- s13[!is.na(s13$evaluator_label) & !is.na(s13$reverse_evaluator_label) &
                s13$evaluator_label == 1 & s13$reverse_evaluator_label == 1, , drop = FALSE]
  cell_est <- vapply(names(stems), function(sf) {
    dat <- comp[as.character(comp$study_factor) == sf, , drop = FALSE] |> droplevels()
    mod <- stats::lm(change ~ condition_factor + belief_rating_pre_rc, data = dat)
    .ms13_lin_cell(mod, dat, "Bunking", .ms13_hc3(mod))$estimate
  }, numeric(1))
  for (ref in c("Standard", "Jailbroken")) {
    add(tibble::tibble(
      model = paste0("Truth-Constrained/", ref), direction = "Bunking",
      term = if (ref == "Standard") "s3.retention.vs_s2" else "s3.retention.vs_s1",
      estimate = cell_est[["Truth-Constrained"]] / cell_est[[ref]],
      note = sprintf("compliant baseline-adjusted ANCOVA bunking cells (dual APE labels): TC %.4f / %s %.4f; stored as proportion",
                     cell_est[["Truth-Constrained"]], ref, cell_est[[ref]])
    ), "compliant")
  }
  ret_members <- list(
    `S3 vs S1` = list(lc = .ms13_cross_cell(comp, "Truth-Constrained", "Jailbroken", "Bunking")),
    `S3 vs S2` = list(lc = .ms13_cross_cell(comp, "Truth-Constrained", "Standard", "Bunking"))
  )
  add(fam_row(ret_members, "s3.retention.ps", "Truth-Constrained", "Bunking",
              "pairwise pooled ANCOVA (study x condition + baseline, HC3), bunking-arm cell contrasts on the COMPLIANT (dual-APE) sample"),
      "compliant")
  eq_members <- list(
    `S3 vs S1` = list(lc = .ms13_cross_cell(comp, "Truth-Constrained", "Jailbroken", "Debunking")),
    `S3 vs S2` = list(lc = .ms13_cross_cell(comp, "Truth-Constrained", "Standard", "Debunking")),
    `S1 vs S2` = list(lc = .ms13_cross_cell(comp, "Jailbroken", "Standard", "Debunking"))
  )
  add(fam_row(eq_members, "s3.debunk_equiv.ps", "(all pairwise)", "Debunking",
              "pairwise pooled ANCOVA (study x condition + baseline, HC3), debunking-arm cell contrasts on the COMPLIANT (dual-APE) sample, all study pairs"),
      "compliant")

  ## ---- top veracity quartile of bunking conversations, by study ----------------
  # Port of the s3-topquartile chunk: conversation-average ALIGNED veracity from
  # the cached per-claim labels, top quartile within study, mean aligned change.
  if (all(file.exists(c(paths$labels_s1s3, paths$labels_s2s4)))) {
    labels <- read_claim_labels(paths) |>
      dplyr::filter(conversation_id %in% s13$response_id)
    conv <- conv_aligned_veracity(labels) |>
      dplyr::filter(study %in% c("Study1", "Study2", "Study3"), !is.na(aligned_veracity))
    sjoin <- s13[as.character(s13$condition_factor) == "Bunking", , drop = FALSE] |>
      dplyr::inner_join(conv |> dplyr::transmute(response_id = conversation_id, aligned_veracity),
                        by = "response_id")
    tq_terms <- c(Jailbroken = "topq.s1", Standard = "topq.s2", `Truth-Constrained` = "topq.s3")
    for (sf in names(tq_terms)) {
      sub <- sjoin[as.character(sjoin$study_factor) == sf & !is.na(sjoin$aligned_veracity), , drop = FALSE]
      if (!nrow(sub)) next
      cut <- stats::quantile(sub$aligned_veracity, .75, na.rm = TRUE)
      tq <- sub[sub$aligned_veracity >= cut, , drop = FALSE]
      add(tibble::tibble(
        model = sf, direction = "Bunking", term = unname(tq_terms[[sf]]),
        n = nrow(tq), estimate = mean(tq$change),
        note = sprintf("top quartile of conversation-average aligned veracity (cutoff=%.3f, 75th pctile within study, n_ranked=%d); estimate = mean aligned belief change",
                       cut, nrow(sub))
      ), "full_sample")
    }
  }

  dplyr::bind_rows(rows)
}
