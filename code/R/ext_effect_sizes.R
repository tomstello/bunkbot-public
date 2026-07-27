# ext_effect_sizes.R ---------------------------------------------------------------------
# Standardized within-condition EFFECT SIZES for every study x condition cell, on BOTH the
# observed-outcome and compliant samples of all four studies. For each cell we report, on the
# complete pre+post rows:
#   n_cell, pre_mean, pre_sd, post_mean, post_sd (raw reverse-coded 0-100 belief scale),
#   prepost_r = cor(pre, post), mean_change = mean(direction-ALIGNED change),
#   sd_change = sd(aligned change), and the within-condition standardized effect
#   d_z = mean_change / sd_change (Cohen's d for the paired/aligned change).
#
# Sign / alignment convention:
#   * S1-3: change = belief_rating_post_rc - belief_rating_pre_rc, then
#     aligned = ifelse(direction=="bunk", change, -change). (The stored `change`
#     column in s13 is NOT post-pre, so we recompute it from the raw rc scores; the
#     resulting aligned vector reproduces the stored aligned_belief_change exactly.)
#   * S4: aligned_belief_change is ALREADY direction-aligned (+ = movement toward the
#     assigned persuasion direction) -- we use it as-is and do NOT reflip.
#   pre_mean / post_mean are always the RAW reverse-coded scores (higher = more belief),
#   so positive aligned mean_change can correspond to either a rise (bunking) or a fall
#   (debunking) in the raw post mean.
#
# ENTRY POINT: compute_effect_sizes(core_objects) -> tibble in the canonical 17-col schema.
#   section "Cross-study"; block "effect_sizes".
#   One row per (cell x metric): term in
#     {n_cell, pre_mean, pre_sd, post_mean, post_sd, prepost_r, mean_change, sd_change, d_z}.
#   model     = study label (S1-3) OR S4 per-model OR "Study 4 (model-weighted)".
#   direction = "Bunking" / "Debunking".
#   sample    = "strict" / "compliant".
#   estimate  = the metric value; n = cell n on every row; statistic carries d_z on the
#               d_z row (== estimate) for convenience; note documents scale/alignment.
# The block is self-contained so the Table-S5 chunk is a PURE all_numbers reader.

# --- metric core ----------------------------------------------------------------------
# Given a cell data frame with columns pre, post, aligned (already direction-aligned),
# compute the nine standardized-effect metrics on the complete pre+post+aligned rows.
.es_metrics <- function(pre, post, aligned) {
  ok <- is.finite(pre) & is.finite(post) & is.finite(aligned)
  pre <- pre[ok]; post <- post[ok]; aligned <- aligned[ok]
  n <- length(pre)
  if (n < 3) return(NULL)
  sd_ch <- stats::sd(aligned)
  m_ch  <- mean(aligned)
  r <- if (stats::sd(pre) > 0 && stats::sd(post) > 0) stats::cor(pre, post) else NA_real_
  list(
    n_cell      = n,
    pre_mean    = mean(pre),
    pre_sd      = stats::sd(pre),
    post_mean   = mean(post),
    post_sd     = stats::sd(post),
    prepost_r   = r,
    mean_change = m_ch,
    sd_change   = sd_ch,
    d_z         = if (is.finite(sd_ch) && sd_ch > 0) m_ch / sd_ch else NA_real_
  )
}

# Emit one tibble (9 rows) for a single (model x direction x sample) cell from a metric list.
.es_cell_rows <- function(metr, model_lab, dir_lab, samp, note) {
  if (is.null(metr)) return(NULL)
  terms <- c("n_cell", "pre_mean", "pre_sd", "post_mean", "post_sd",
             "prepost_r", "mean_change", "sd_change", "d_z")
  tibble::tibble(
    model     = model_lab,
    direction = dir_lab,
    term      = terms,
    n         = metr$n_cell,
    estimate  = vapply(terms, function(t) as.numeric(metr[[t]]), numeric(1)),
    statistic = ifelse(terms == "d_z", as.numeric(metr$d_z), NA_real_),
    sample    = samp,
    note      = note
  )
}

# Build the per-cell metric rows for one analytic frame already carrying columns
# model_lab, direction (bunk/debunk), pre, post, aligned. `weighted` adds a
# model-equally-weighted aggregate row set per direction (S4 only).
.es_frame_rows <- function(df, samp, note, add_weighted = FALSE,
                           weighted_lab = "Study 4 (model-weighted)") {
  out <- list()
  models <- unique(as.character(df$model_lab))
  per_model_metrics <- list()  # for the weighted aggregate
  for (md in models) {
    for (dd in c("bunk", "debunk")) {
      sub <- df[as.character(df$model_lab) == md & as.character(df$direction) == dd, , drop = FALSE]
      metr <- .es_metrics(sub$pre, sub$post, sub$aligned)
      r <- .es_cell_rows(metr, md, if (dd == "bunk") "Bunking" else "Debunking", samp, note)
      if (!is.null(r)) out[[length(out) + 1]] <- r
      if (add_weighted && !is.null(metr)) per_model_metrics[[paste(md, dd)]] <- c(metr, direction = dd)
    }
  }
  # Model-equally-weighted aggregate: average each metric across the available models
  # within a direction (so a high-n model does not dominate); n = total cell n.
  if (add_weighted && length(per_model_metrics)) {
    for (dd in c("bunk", "debunk")) {
      keys <- names(per_model_metrics)[vapply(per_model_metrics, function(m) m$direction == dd, logical(1))]
      if (!length(keys)) next
      mlist <- per_model_metrics[keys]
      agg <- list(
        n_cell      = sum(vapply(mlist, function(m) m$n_cell, numeric(1))),
        pre_mean    = mean(vapply(mlist, function(m) m$pre_mean, numeric(1))),
        pre_sd      = mean(vapply(mlist, function(m) m$pre_sd, numeric(1))),
        post_mean   = mean(vapply(mlist, function(m) m$post_mean, numeric(1))),
        post_sd     = mean(vapply(mlist, function(m) m$post_sd, numeric(1))),
        prepost_r   = mean(vapply(mlist, function(m) m$prepost_r, numeric(1))),
        mean_change = mean(vapply(mlist, function(m) m$mean_change, numeric(1))),
        sd_change   = mean(vapply(mlist, function(m) m$sd_change, numeric(1))),
        d_z         = mean(vapply(mlist, function(m) m$d_z, numeric(1)))
      )
      wnote <- paste0(note, " Model-equally-weighted average across the ",
                      length(keys), " frontier models (each model counted once, not by n).")
      out[[length(out) + 1]] <- .es_cell_rows(agg, weighted_lab,
                                              if (dd == "bunk") "Bunking" else "Debunking", samp, wnote)
    }
  }
  dplyr::bind_rows(out)
}

compute_effect_sizes <- function(core_objects) {
  si_require(c("dplyr", "tibble"))

  out <- list()

  # --- Studies 1-3 -----------------------------------------------------------------
  s13 <- core_objects$s13
  study_lab <- c(Jailbroken = "Study 1 (Jailbroken)",
                 Standard = "Study 2 (Standard)",
                 `Truth-Constrained` = "Study 3 (Truth-Constrained)")
  for (sf in names(study_lab)) {
    base <- s13[as.character(s13$study_factor) == sf, , drop = FALSE]
    if (!nrow(base)) next
    pre  <- suppressWarnings(as.numeric(base$belief_rating_pre_rc))
    post <- suppressWarnings(as.numeric(base$belief_rating_post_rc))
    chg  <- post - pre
    aligned <- ifelse(as.character(base$direction) == "bunk", chg, -chg)
    frame <- tibble::tibble(model_lab = study_lab[[sf]],
                            direction = as.character(base$direction),
                            pre = pre, post = post, aligned = aligned,
                            compliant = base$compliant)
    note_s <- "Raw reverse-coded 0-100 belief scale (higher=more belief); aligned change = post-pre with debunking sign-flipped. d_z = mean/sd of aligned change."
    # strict = full observed-outcome analytic sample
    out[[length(out) + 1]] <- .es_frame_rows(frame, "strict", note_s)
    # compliant subsample
    comp <- frame[.es_truthy(frame$compliant), , drop = FALSE]
    if (nrow(comp)) out[[length(out) + 1]] <- .es_frame_rows(comp, "compliant", note_s)
  }

  # --- Study 4 ---------------------------------------------------------------------
  note_s4 <- "Raw reverse-coded 0-100 belief scale (higher=more belief); aligned_belief_change is already direction-aligned (not reflipped). d_z = mean/sd of aligned change."
  mk_s4_frame <- function(d) tibble::tibble(
    model_lab = as.character(d$model_pooled),
    direction = as.character(d$direction),
    pre  = suppressWarnings(as.numeric(d$belief_rating_pre_4)),
    post = suppressWarnings(as.numeric(d$belief_rating_post_4)),
    aligned = suppressWarnings(as.numeric(d$aligned_belief_change))
  )
  s4_strict <- mk_s4_frame(core_objects$s4$s4)
  s4_comp   <- mk_s4_frame(core_objects$s4$s4_compliant)
  out[[length(out) + 1]] <- .es_frame_rows(s4_strict, "strict",    note_s4, add_weighted = TRUE)
  out[[length(out) + 1]] <- .es_frame_rows(s4_comp,   "compliant", note_s4, add_weighted = TRUE)

  res <- dplyr::bind_rows(out)
  if (!nrow(res)) {
    return(std_row(tibble::tibble(term = character(0)), "Cross-study", "effect_sizes", "strict"))
  }
  std_row(res, "Cross-study", "effect_sizes")
}

# logical coalescing helper (compliant may carry NA); prefix-tagged to avoid collisions.
.es_truthy <- function(x) !is.na(x) & as.logical(x)
