# ext_s13.R
# =============================================================================
# Extension block-builders for the SI recompute engine. These add the
# Studies 1-3 quantities that were previously MISSING from all_numbers, plus a
# four-study (S1-3 + S4) multi-threshold large-shift robustness block.
#
# ENTRY POINT:  ext_s13_numbers(core_objects)  -> tibble in the canonical schema
#   (section, block, sample, outcome, model, direction, term, n, estimate, se,
#    conf_low, conf_high, statistic, df_num, df_den, p_value, note)
#
# Requires bunkbot_helpers.R already sourced (for hc3_tidy, mean_ci,
# model_matrix_for_fit, linear_combo) and R/tables_dynamic.R for std_cols/std_row.
# All inputs come from core_objects (built from raw + cached); no CSV reads.
#
# Conventions (canonical schema: std_cols in R/tables_dynamic.R):
#   * section = "S1-3" for S1-3-only blocks; "S1-3 + S4" for the pooled
#     large-shift block.
#   * model in {Jailbroken, Standard, Truth-Constrained} (S1-3) or
#     {Claude, Gemini, GPT-5.2, Grok} (S4).
#   * direction in {Bunking, Debunking} (S1-3) or {bunk, debunk} (S4); these
#     mirror the existing belief_change / aligned_* blocks respectively.
#   * S1-3 mediators recoded with the SAME maps as the S4 add_secondary_recodes()
#     (new_info 1/4..12 -> 1..10; collaborative/unbiased 69..73 -> -2..2) so the
#     two studies are directly comparable (do NOT treat them as different
#     instruments).
#   * Effects use HC3 robust SEs; within-condition means use mean_ci (1.96 SE).
# =============================================================================

# ---- shared local helpers ---------------------------------------------------

# Cohen's d_z for a one-sample (within-condition change) vector: mean / sd.
.d_z <- function(x) {
  x <- x[!is.na(x)]
  m <- mean(x); s <- stats::sd(x)
  if (is.na(s) || s == 0) return(NA_real_)
  m / s
}

# S4-consistent recodes for the S1-3 perception mediators.
.s13_recode_mediators <- function(df) {
  df |>
    dplyr::mutate(
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

# Row-mean GCBS score over the 7 paired items (1-11 scale).
.s13_add_gcbs <- function(df) {
  gp <- c("x2_gcbs_pre", "x4_gcbs_pre", "x6_gcbs_pre", "x8_gcbs_pre",
          "x10_gcbs_pre", "x12_gcbs_pre", "x14_gcbs_pre")
  go <- sub("_pre$", "_post", gp)
  pre_mat <- sapply(gp, function(c) suppressWarnings(as.numeric(df[[c]])))
  post_mat <- sapply(go, function(c) suppressWarnings(as.numeric(df[[c]])))
  df$GCBS_pre_score <- ifelse(rowSums(is.na(pre_mat)) == 0, rowMeans(pre_mat), NA_real_)
  df$GCBS_post_score <- ifelse(rowSums(is.na(post_mat)) == 0, rowMeans(post_mat), NA_real_)
  df$GCBS_change <- df$GCBS_post_score - df$GCBS_pre_score
  df
}

# Within-condition mean + HC3 bunk-minus-debunk contrast for one numeric change
# variable, looping over study_factor. Returns a list(within=..., contrast=...)
# of canonical-ready tibbles (pre-std_row). `dir_labels` selects how the
# direction column is named (defaults to Bunking/Debunking to match S1-3 blocks).
.s13_change_family <- function(s13, change_var, outcome_label,
                               dir_labels = c(Bunking = "Bunking", Debunking = "Debunking")) {
  within <- list()
  contrast <- list()
  for (sf in levels(s13$study_factor)) {
    dat <- s13 |>
      dplyr::filter(study_factor == sf, !is.na(.data[[change_var]])) |>
      droplevels()
    if (nrow(dat) < 5 || length(unique(dat$condition_factor)) < 2) next

    for (cl in levels(dat$condition_factor)) {
      sub <- dat |> dplyr::filter(condition_factor == cl)
      x <- sub[[change_var]]
      ci <- mean_ci(x)
      within[[length(within) + 1]] <- tibble::tibble(
        outcome = outcome_label,
        model = sf,
        direction = unname(dir_labels[[as.character(cl)]]),
        term = paste0(change_var, "_mean"),
        n = ci$n,
        estimate = ci$estimate,
        conf_low = ci$conf.low,
        conf_high = ci$conf.high,
        statistic = .d_z(x),
        note = sprintf("within-condition mean change; statistic col = Cohen d_z; sd=%.3f", stats::sd(x, na.rm = TRUE))
      )
    }

    dat <- dat |>
      dplyr::mutate(dir = factor(
        dplyr::if_else(condition_factor == "Bunking", "bunk", "debunk"),
        levels = c("debunk", "bunk")
      ))
    fit <- stats::lm(stats::as.formula(paste0(change_var, " ~ dir")), data = dat)
    tt <- hc3_tidy(fit)
    b <- tt[tt$term == "dirbunk", ]
    contrast[[length(contrast) + 1]] <- tibble::tibble(
      outcome = outcome_label,
      model = sf,
      term = "bunk_minus_debunk",
      n = nrow(dat),
      estimate = b$estimate,
      se = b$std.error,
      conf_low = b$conf.low,
      conf_high = b$conf.high,
      statistic = b$statistic,
      p_value = b$p.value,
      note = "bunk minus debunk in mean change; HC3 robust SE"
    )
  }
  list(within = dplyr::bind_rows(within), contrast = dplyr::bind_rows(contrast))
}

# Perception-mediator family (within-condition means + bunk-minus-debunk HC3
# contrast) computed on a given (already recoded) frame `dfm`. Returns a
# list(within=..., contrast=...) of pre-std_row tibbles. Used for BOTH the
# full-sample and compliant-sample perception_mediators emissions so the two
# samples are computed identically.
.s13_mediator_family <- function(dfm) {
  med_defs <- tibble::tribble(
    ~var,               ~label,
    "arg_strength_rc",  "Argument strength",
    "new_info_rc",      "Provided new information",
    "collaborative_rc", "Collaborativeness",
    "unbiased_rc",      "Impartiality (unbiased)"
  )
  med_within <- list()
  med_contrast <- list()
  for (i in seq_len(nrow(med_defs))) {
    v <- med_defs$var[[i]]
    lab <- med_defs$label[[i]]
    for (sf in levels(dfm$study_factor)) {
      dat <- dfm |>
        dplyr::filter(study_factor == sf, !is.na(.data[[v]])) |>
        droplevels()
      if (nrow(dat) < 5 || length(unique(dat$condition_factor)) < 2) next
      for (cl in levels(dat$condition_factor)) {
        sub <- dat |> dplyr::filter(condition_factor == cl)
        ci <- mean_ci(sub[[v]])
        med_within[[length(med_within) + 1]] <- tibble::tibble(
          outcome = lab, model = sf, direction = as.character(cl),
          term = paste0(v, "_mean"), n = ci$n, estimate = ci$estimate,
          conf_low = ci$conf.low, conf_high = ci$conf.high,
          note = "within-condition mean rating"
        )
      }
      dat <- dat |>
        dplyr::mutate(dir = factor(
          dplyr::if_else(condition_factor == "Bunking", "bunk", "debunk"),
          levels = c("debunk", "bunk")
        ))
      fit <- stats::lm(stats::as.formula(paste0(v, " ~ dir")), data = dat)
      tt <- hc3_tidy(fit)
      b <- tt[tt$term == "dirbunk", ]
      # pooled-SD Cohen's d (weighted within-group variances), matching
      # results_methods.Rmd's cohen_d2() — the manuscript's standardized
      # convention (design decision, 2026-07-06)
      xg <- dat[[v]][dat$dir == "bunk"];   xg <- xg[!is.na(xg)]
      yg <- dat[[v]][dat$dir == "debunk"]; yg <- yg[!is.na(yg)]
      pooled_sd <- sqrt(((length(xg) - 1) * stats::var(xg) +
                         (length(yg) - 1) * stats::var(yg)) /
                        (length(xg) + length(yg) - 2))
      med_contrast[[length(med_contrast) + 1]] <- tibble::tibble(
        outcome = lab, model = sf, term = "bunk_minus_debunk",
        n = nrow(dat), estimate = b$estimate, se = b$std.error,
        conf_low = b$conf.low, conf_high = b$conf.high,
        statistic = if (is.na(pooled_sd) || pooled_sd == 0) NA_real_ else b$estimate / pooled_sd,
        p_value = b$p.value,
        note = "bunk minus debunk; HC3 robust SE; statistic col = Cohen d (pooled SD, cohen_d2 convention)"
      )
    }
  }
  list(within = dplyr::bind_rows(med_within), contrast = dplyr::bind_rows(med_contrast))
}

# Post-to-debrief belief shift in the bunking arm only (mirrors the full-sample
# `debrief` block in tables_dynamic.R::compute_s13_numbers). Computed on a given
# frame so the compliant sample can re-use the identical computation. Cells with
# no non-NA debrief observations are skipped, so an empty result is possible.
.s13_debrief_family <- function(df) {
  deb_rows <- list()
  for (sf in levels(df$study_factor)) {
    sub <- df |>
      dplyr::filter(
        study_factor == sf,
        condition_factor == "Bunking",
        !is.na(belief_rating_debrf_rc)
      )
    if (nrow(sub) < 2) next
    shift <- sub$belief_rating_debrf_rc - sub$belief_rating_post_rc
    net <- sub$belief_rating_debrf_rc - sub$belief_rating_pre_rc
    if (length(stats::na.omit(shift)) < 2) next
    tt <- stats::t.test(shift)
    nt <- stats::t.test(net)
    deb_rows[[length(deb_rows) + 1]] <- tibble::tibble(
      model = sf,
      direction = "Bunking",
      term = "post_to_debrief_shift",
      n = length(shift),
      estimate = unname(tt$estimate),
      conf_low = tt$conf.int[1],
      conf_high = tt$conf.int[2],
      statistic = unname(tt$statistic),
      p_value = tt$p.value,
      note = sprintf("net baseline-to-debrief change=%.3f; p=%s", unname(nt$estimate), fmt_p(nt$p.value))
    )
  }
  dplyr::bind_rows(deb_rows)
}

# Two-sample KS distance D (bunk vs debunk) on direction-aligned belief change,
# plus the >=40-point chi-square in the statistic/note columns (mirrors the
# full-sample `distribution_ks` block in compute_s13_numbers). Computed on a
# given frame so the compliant sample can re-use the identical computation.
.s13_distribution_ks_family <- function(df) {
  dist_rows <- list()
  for (sf in levels(df$study_factor)) {
    dat <- df |>
      dplyr::filter(study_factor == sf) |>
      tidyr::drop_na(change, condition_factor)
    cb <- dat$change[dat$condition_factor == "Bunking"]
    cd <- dat$change[dat$condition_factor == "Debunking"]
    if (length(cb) < 1 || length(cd) < 1) next
    ks <- suppressWarnings(stats::ks.test(cb, cd))
    ge40 <- suppressWarnings(stats::chisq.test(table(dat$condition_factor, dat$change >= 40)))
    dist_rows[[length(dist_rows) + 1]] <- tibble::tibble(
      model = sf,
      term = "ks_D",
      estimate = unname(ks$statistic),
      statistic = unname(ge40$statistic),
      p_value = ks$p.value,
      note = sprintf(
        ">=40 points: bunk %.1f%% vs debunk %.1f%%; chi-square p=%s",
        100 * mean(cb >= 40), 100 * mean(cd >= 40), fmt_p(ge40$p.value)
      )
    )
  }
  dplyr::bind_rows(dist_rows)
}

# =============================================================================
# MAIN
# =============================================================================
ext_s13_numbers <- function(core_objects) {
  si_require(c("dplyr", "tidyr", "purrr", "sandwich", "tibble"))
  s13 <- core_objects$s13
  s4 <- core_objects$s4$s4
  rows <- list()

  # ---------------------------------------------------------------------------
  # 1a. GCBS general conspiracist beliefs (GCBS_post - GCBS_pre)
  # ---------------------------------------------------------------------------
  s13g <- .s13_add_gcbs(s13)
  gcbs <- .s13_change_family(s13g, "GCBS_change", "GCBS (general conspiracist beliefs)")
  rows[[length(rows) + 1]] <- gcbs$within |>
    std_row("S1-3", "gcbs_change", "full_sample")
  rows[[length(rows) + 1]] <- gcbs$contrast |>
    std_row("S1-3", "gcbs_change", "full_sample")

  # ---------------------------------------------------------------------------
  # 1b. Trust in AI: pre->post change and post->debrief "survives disclosure"
  #     genai_trust = pre, trust2 = post, trust_debrf = post-debrief (1-7 scale)
  # ---------------------------------------------------------------------------
  s13t <- s13 |>
    dplyr::mutate(
      genai_trust = suppressWarnings(as.numeric(genai_trust)),
      trust2 = suppressWarnings(as.numeric(trust2)),
      trust_debrf = suppressWarnings(as.numeric(trust_debrf)),
      trust_pre_post_change = trust2 - genai_trust,
      trust_post_debrief_change = trust_debrf - trust2
    )
  trust_pp <- .s13_change_family(s13t, "trust_pre_post_change", "Trust in AI (pre->post)")
  rows[[length(rows) + 1]] <- trust_pp$within |>
    std_row("S1-3", "trust_change", "full_sample")
  rows[[length(rows) + 1]] <- trust_pp$contrast |>
    std_row("S1-3", "trust_change", "full_sample")

  trust_pd <- .s13_change_family(s13t, "trust_post_debrief_change",
                                 "Trust in AI (post->debrief, survives disclosure)")
  # within-condition only: debrief trust collected mainly in bunking arm; keep
  # whatever cells have data, plus the contrast where both arms are present.
  rows[[length(rows) + 1]] <- trust_pd$within |>
    std_row("S1-3", "trust_change", "full_sample")
  if (nrow(trust_pd$contrast)) {
    rows[[length(rows) + 1]] <- trust_pd$contrast |>
      std_row("S1-3", "trust_change", "full_sample")
  }

  # ---------------------------------------------------------------------------
  # 1c. Four perception mediators: bunk-minus-debunk + within-condition means
  #     (recoded to match the S4 secondary_contrasts instrument exactly)
  # ---------------------------------------------------------------------------
  s13m <- .s13_recode_mediators(s13)
  med_full <- .s13_mediator_family(s13m)
  rows[[length(rows) + 1]] <- med_full$within |>
    std_row("S1-3", "perception_mediators", "full_sample")
  rows[[length(rows) + 1]] <- med_full$contrast |>
    std_row("S1-3", "perception_mediators", "full_sample")

  # ---------------------------------------------------------------------------
  # 2. COMPLIANT-subset belief effects (baseline-adjusted ANCOVA) + bunk-vs-debunk
  #    contrasts. compliant = evaluator_label==1 & reverse_evaluator_label==1.
  #    Mirrors the full-sample belief_change / belief_bunk_vs_debunk blocks.
  # ---------------------------------------------------------------------------
  s13c <- s13 |> dplyr::filter(compliant %in% TRUE) |> droplevels()
  belief_rows <- list()
  contrast_rows <- list()
  for (sf in levels(s13$study_factor)) {
    dat <- s13c |>
      dplyr::filter(study_factor == sf) |>
      droplevels()
    if (nrow(dat) < 5 || length(unique(dat$condition_factor)) < 2) next
    mod <- stats::lm(change ~ condition_factor + belief_rating_pre_rc, data = dat)
    vc <- sandwich::vcovHC(mod, type = "HC3")

    sp_pre <- dat |>
      dplyr::group_by(condition_factor) |>
      dplyr::summarise(n = dplyr::n(), v = stats::var(belief_rating_pre_rc), .groups = "drop") |>
      dplyr::summarise(x = sqrt(sum((n - 1) * v) / (sum(n) - dplyr::n()))) |>
      dplyr::pull(x)

    for (cl in levels(dat$condition_factor)) {
      sub <- dat |> dplyr::filter(condition_factor == cl)
      n <- nrow(sub)
      m <- mean(sub$change, na.rm = TRUE)
      s <- stats::sd(sub$change, na.rm = TRUE)
      se <- s / sqrt(n)
      crit <- stats::qt(.975, n - 1)
      dz <- m / s
      J <- 1 - 3 / (4 * (n - 1) - 1)
      gpre <- J * (m / sp_pre)
      nd <- dat
      nd$condition_factor <- factor(cl, levels = levels(dat$condition_factor))
      mm <- model_matrix_for_fit(mod, nd)
      lin <- linear_combo(mod, colMeans(mm), vc = vc)
      belief_rows[[length(belief_rows) + 1]] <- tibble::tibble(
        model = sf,
        direction = as.character(cl),
        n = n,
        estimate = lin$estimate,
        se = lin$std.error,
        conf_low = lin$conf.low,
        conf_high = lin$conf.high,
        statistic = dz,
        note = sprintf(
          "baseline-adjusted ANCOVA (change ~ condition + pre), HC3; raw_mean=%.3f; raw_ci=[%.3f, %.3f]; hedges_g_pre=%.3f",
          m, m - crit * se, m + crit * se, gpre
        )
      )
    }

    nd_b <- dat; nd_b$condition_factor <- factor("Bunking", levels = levels(dat$condition_factor))
    nd_d <- dat; nd_d$condition_factor <- factor("Debunking", levels = levels(dat$condition_factor))
    w <- colMeans(model_matrix_for_fit(mod, nd_b)) - colMeans(model_matrix_for_fit(mod, nd_d))
    ct <- linear_combo(mod, w, vc = vc)
    contrast_rows[[length(contrast_rows) + 1]] <- tibble::tibble(
      model = sf,
      term = "bunk_minus_debunk_lin",
      n = nrow(dat),
      estimate = ct$estimate,
      se = ct$std.error,
      conf_low = ct$conf.low,
      conf_high = ct$conf.high,
      p_value = ct$p.value,
      note = "compliant subset (evaluator_label==1 & reverse_evaluator_label==1); baseline-adjusted ANCOVA HC3"
    )
  }
  rows[[length(rows) + 1]] <- dplyr::bind_rows(belief_rows) |>
    std_row("S1-3", "belief_change_compliant", "compliant")
  rows[[length(rows) + 1]] <- dplyr::bind_rows(contrast_rows) |>
    std_row("S1-3", "belief_bunk_vs_debunk_compliant", "compliant")

  # ---------------------------------------------------------------------------
  # 2b. COMPLIANT-sample versions of the four blocks whose Study-4 analogues
  #     carry BOTH strict and compliant samples but whose S1-3 emissions above
  #     (and in compute_s13_numbers) were full_sample only. We re-run the SAME
  #     computations on the compliant subset (compliant column TRUE; the lone NA
  #     is treated as FALSE) and emit identical block names with sample =
  #     "compliant" so the three asymmetric figures/tables can plumb both
  #     samples for S1-3. Per-study compliant n: 1064 / 794 / 740. Same token
  #     ("compliant") as the existing belief_change_compliant block above.
  #       (1) perception_mediators -> fig_secondary
  #       (2) trust_change         -> tab:ra-trust-allstudies
  #       (3) debrief              -> fig_debrief_trajectory
  #       (4) distribution_ks      -> tab:ra-distribution-ks
  # ---------------------------------------------------------------------------
  s13_comp <- s13 |> dplyr::filter(compliant %in% TRUE) |> droplevels()

  # (1) perception mediators (recode first, then filter is equivalent; recode is
  #     row-wise so order does not matter — recode the compliant frame).
  s13m_comp <- .s13_recode_mediators(s13_comp)
  med_comp <- .s13_mediator_family(s13m_comp)
  rows[[length(rows) + 1]] <- med_comp$within |>
    std_row("S1-3", "perception_mediators", "compliant")
  rows[[length(rows) + 1]] <- med_comp$contrast |>
    std_row("S1-3", "perception_mediators", "compliant")

  # (2) trust pre->post bunk-minus-debunk contrast (+ within-condition means).
  s13t_comp <- s13_comp |>
    dplyr::mutate(
      genai_trust = suppressWarnings(as.numeric(genai_trust)),
      trust2 = suppressWarnings(as.numeric(trust2)),
      trust_pre_post_change = trust2 - genai_trust
    )
  trust_pp_comp <- .s13_change_family(s13t_comp, "trust_pre_post_change",
                                      "Trust in AI (pre->post)")
  rows[[length(rows) + 1]] <- trust_pp_comp$within |>
    std_row("S1-3", "trust_change", "compliant")
  rows[[length(rows) + 1]] <- trust_pp_comp$contrast |>
    std_row("S1-3", "trust_change", "compliant")

  # (3) debrief: post-to-debrief belief shift, bunking arm only (by design in
  #     S1-3). Emit compliant where the bunking-arm compliant cell is non-empty.
  deb_comp <- .s13_debrief_family(s13_comp)
  if (nrow(deb_comp)) {
    rows[[length(rows) + 1]] <- deb_comp |>
      std_row("S1-3", "debrief", "compliant")
  }

  # (4) distribution_ks: two-sample KS distance bunk-vs-debunk on aligned change.
  dist_comp <- .s13_distribution_ks_family(s13_comp)
  if (nrow(dist_comp)) {
    rows[[length(rows) + 1]] <- dist_comp |>
      std_row("S1-3", "distribution_ks", "compliant")
  }

  # ---------------------------------------------------------------------------
  # 3. Multi-threshold large-shift robustness, ALL FOUR studies.
  #    Share with direction-aligned shift >= T (T in 20/30/40/50), by
  #    study/model x direction, plus bunk-vs-debunk chi-square per threshold.
  #    S1-3 aligned shift = `change` (already direction-aligned in build_s1s3);
  #    S4 aligned shift = `aligned_belief_change`.
  # ---------------------------------------------------------------------------
  thresholds <- c(20, 30, 40, 50)

  # Long, harmonized frame: one row per participant with (group_section, model,
  # dir2 in {bunk,debunk}, aligned_shift).
  s13_long <- s13 |>
    dplyr::transmute(
      group_section = "S1-3",
      model = as.character(study_factor),
      dir2 = dplyr::if_else(condition_factor == "Bunking", "bunk", "debunk"),
      aligned_shift = change
    ) |>
    dplyr::filter(!is.na(aligned_shift), !is.na(dir2))
  s4_long <- s4 |>
    dplyr::transmute(
      group_section = "S4",
      model = as.character(model_pooled),
      dir2 = as.character(direction),
      aligned_shift = aligned_belief_change
    ) |>
    dplyr::filter(!is.na(aligned_shift), !is.na(dir2))
  shift_long <- dplyr::bind_rows(s13_long, s4_long)

  ls_rows <- list()
  for (sec in c("S1-3", "S4")) {
    sub_sec <- shift_long |> dplyr::filter(group_section == sec)
    models <- if (sec == "S1-3") {
      c("Jailbroken", "Standard", "Truth-Constrained")
    } else {
      c("Claude", "Gemini", "GPT-5.2", "Grok")
    }
    for (md in models) {
      sub <- sub_sec |> dplyr::filter(model == md)
      if (!nrow(sub)) next
      for (T in thresholds) {
        sub <- sub |> dplyr::mutate(big = aligned_shift >= T)
        # per-direction share
        for (dd in c("bunk", "debunk")) {
          cell <- sub |> dplyr::filter(dir2 == dd)
          if (!nrow(cell)) next
          ls_rows[[length(ls_rows) + 1]] <- tibble::tibble(
            section = sec, model = md, direction = dd,
            term = paste0("share_ge", T),
            n = nrow(cell), estimate = mean(cell$big),
            note = sprintf("share with aligned shift >= %d points", T)
          )
        }
        # bunk-vs-debunk chi-square at this threshold
        tb <- table(factor(sub$dir2, levels = c("bunk", "debunk")), sub$big)
        if (all(dim(tb) == c(2, 2)) && sum(tb) > 0) {
          cs <- suppressWarnings(stats::chisq.test(tb))
          ls_rows[[length(ls_rows) + 1]] <- tibble::tibble(
            section = sec, model = md, direction = NA_character_,
            term = paste0("bunk_vs_debunk_chisq_ge", T),
            n = sum(tb), estimate = unname(cs$statistic),
            statistic = unname(cs$statistic), df_num = unname(cs$parameter),
            p_value = cs$p.value,
            note = sprintf("Pearson chi-square, share aligned shift >= %d: bunk vs debunk", T)
          )
        }
      }
    }
    # pooled-across-models row per study section
    for (T in thresholds) {
      subp <- sub_sec |> dplyr::mutate(big = aligned_shift >= T)
      for (dd in c("bunk", "debunk")) {
        cell <- subp |> dplyr::filter(dir2 == dd)
        if (!nrow(cell)) next
        ls_rows[[length(ls_rows) + 1]] <- tibble::tibble(
          section = sec, model = "Pooled", direction = dd,
          term = paste0("share_ge", T),
          n = nrow(cell), estimate = mean(cell$big),
          note = sprintf("share with aligned shift >= %d points (pooled across models/studies in section)", T)
        )
      }
      tb <- table(factor(subp$dir2, levels = c("bunk", "debunk")), subp$big)
      if (all(dim(tb) == c(2, 2)) && sum(tb) > 0) {
        cs <- suppressWarnings(stats::chisq.test(tb))
        ls_rows[[length(ls_rows) + 1]] <- tibble::tibble(
          section = sec, model = "Pooled", direction = NA_character_,
          term = paste0("bunk_vs_debunk_chisq_ge", T),
          n = sum(tb), estimate = unname(cs$statistic),
          statistic = unname(cs$statistic), df_num = unname(cs$parameter),
          p_value = cs$p.value,
          note = sprintf("Pearson chi-square, share aligned shift >= %d: bunk vs debunk (pooled)", T)
        )
      }
    }
  }
  ls_df <- dplyr::bind_rows(ls_rows)
  # section column is per-row here; std_row overwrites it, so split-apply.
  ls_s13 <- ls_df |> dplyr::filter(section == "S1-3") |> dplyr::select(-section) |>
    std_row("S1-3", "large_shift_thresholds", "full_sample")
  ls_s4 <- ls_df |> dplyr::filter(section == "S4") |> dplyr::select(-section) |>
    std_row("S4 + pooled", "large_shift_thresholds", "strict_n1272")
  rows[[length(rows) + 1]] <- ls_s13
  rows[[length(rows) + 1]] <- ls_s4

  # std_row() fills absent columns with logical NA, so character cols (e.g.
  # outcome, direction, note) can disagree in type across blocks. Coerce the
  # character-typed canonical columns before binding.
  char_cols <- c("section", "block", "sample", "outcome", "model", "direction", "term", "note")
  rows <- lapply(rows, function(df) {
    for (cc in char_cols) if (cc %in% names(df)) df[[cc]] <- as.character(df[[cc]])
    num_cols <- c("n", "estimate", "se", "conf_low", "conf_high", "statistic",
                  "df_num", "df_den", "p_value")
    for (nc in num_cols) if (nc %in% names(df)) df[[nc]] <- as.numeric(df[[nc]])
    df
  })
  dplyr::bind_rows(rows)[, std_cols]
}
