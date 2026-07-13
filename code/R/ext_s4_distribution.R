# ext_s4_distribution.R
# =============================================================================
# Study-4 distributional (Kolmogorov-Smirnov) comparison of the direction-aligned
# belief-change distributions between the bunking and debunking arms, mirroring
# the S1-3 `distribution_ks` block (R/tables_dynamic.R::compute_s13_numbers).
#
# ENTRY POINT:  compute_s4_distribution(core_objects) -> tibble in the canonical
#   17-column schema (section, block, sample, outcome, model, direction, term, n,
#   estimate, se, conf_low, conf_high, statistic, df_num, df_den, p_value, note).
#
# BLOCK:    "s4_distribution_ks"   (section "S4 + pooled")
#
# For each SAMPLE (strict = core_objects$s4$s4, n=1272; compliant =
# core_objects$s4$s4_compliant, n=1,073 post-rescore — emitted under the
# compliant_n1056 sample id and renamed downstream) and each SCOPE (Overall pooled across
# models; and each of Claude / Gemini / GPT-5.2 / Grok) we:
#   * two-sample KS test of aligned_belief_change[bunk] vs aligned_belief_change[debunk]
#     stats::ks.test(cb, cd) -> term "ks_D" (estimate = D) and "ks_p" (estimate = p);
#   * the share with a direction-aligned shift >= 40 points in each arm, and a
#     bunk-vs-debunk chi-square of that large-shift indicator
#     -> term "large_shift_pct" (one row per direction; estimate = percent),
#        term "chisq_stat" (Pearson chi-square statistic, df in df_num),
#        term "chisq_p"   (chi-square p-value).
#
# Requires R/tables_dynamic.R (std_cols / std_row) and bunkbot_helpers.R sourced.
# All inputs come from core_objects (built from raw + cached); no CSV reads.
# =============================================================================

compute_s4_distribution <- function(core_objects) {
  si_require <- get0("si_require", ifnotfound = function(pkgs) {
    for (p in pkgs) suppressWarnings(suppressMessages(requireNamespace(p, quietly = TRUE)))
  })
  si_require(c("dplyr", "tidyr", "tibble"))

  samples <- list(
    strict_n1272   = core_objects$s4$s4,
    compliant_n1056 = core_objects$s4$s4_compliant
  )
  models <- c("Claude", "Gemini", "GPT-5.2", "Grok")

  # Compute the KS + large-shift rows for one (already-subset) frame.
  .ks_block <- function(dat, model_label) {
    dat <- dat |>
      dplyr::filter(!is.na(aligned_belief_change), !is.na(direction))
    cb <- dat$aligned_belief_change[dat$direction == "bunk"]
    cd <- dat$aligned_belief_change[dat$direction == "debunk"]
    if (length(cb) < 2 || length(cd) < 2) return(NULL)

    ks <- suppressWarnings(stats::ks.test(cb, cd))
    big <- dat$aligned_belief_change >= 40
    tb <- table(factor(dat$direction, levels = c("bunk", "debunk")), big)
    has_chisq <- all(dim(tb) == c(2, 2)) && sum(tb) > 0
    cs <- if (has_chisq) suppressWarnings(stats::chisq.test(tb)) else NULL

    pct_b <- 100 * mean(cb >= 40)
    pct_d <- 100 * mean(cd >= 40)
    note_ks <- sprintf(
      ">=40 points: bunk %.1f%% (n=%d) vs debunk %.1f%% (n=%d); chi-square p=%s",
      pct_b, length(cb), pct_d, length(cd),
      if (has_chisq) fmt_p(cs$p.value) else "NA"
    )

    out <- list(
      # KS D statistic
      tibble::tibble(
        model = model_label, direction = NA_character_, term = "ks_D",
        n = length(cb) + length(cd), estimate = unname(ks$statistic),
        statistic = unname(ks$statistic), p_value = ks$p.value, note = note_ks
      ),
      # KS p-value (carried as its own term for the per-term schema)
      tibble::tibble(
        model = model_label, direction = NA_character_, term = "ks_p",
        n = length(cb) + length(cd), estimate = ks$p.value,
        p_value = ks$p.value,
        note = "two-sample KS test p-value, bunk vs debunk aligned belief change"
      ),
      # large-shift share by direction
      tibble::tibble(
        model = model_label, direction = "bunk", term = "large_shift_pct",
        n = length(cb), estimate = pct_b,
        note = "percent with direction-aligned shift >= 40 points (bunking arm)"
      ),
      tibble::tibble(
        model = model_label, direction = "debunk", term = "large_shift_pct",
        n = length(cd), estimate = pct_d,
        note = "percent with direction-aligned shift >= 40 points (debunking arm)"
      )
    )
    if (has_chisq) {
      out[[length(out) + 1]] <- tibble::tibble(
        model = model_label, direction = NA_character_, term = "chisq_stat",
        n = sum(tb), estimate = unname(cs$statistic),
        statistic = unname(cs$statistic), df_num = unname(cs$parameter),
        p_value = cs$p.value,
        note = "Pearson chi-square of >=40-point large-shift share: bunk vs debunk"
      )
      out[[length(out) + 1]] <- tibble::tibble(
        model = model_label, direction = NA_character_, term = "chisq_p",
        n = sum(tb), estimate = cs$p.value, p_value = cs$p.value,
        df_num = unname(cs$parameter),
        note = "p-value, chi-square of >=40-point large-shift share: bunk vs debunk"
      )
    }
    dplyr::bind_rows(out)
  }

  all_rows <- list()
  for (smp in names(samples)) {
    dat <- samples[[smp]]
    scope_rows <- list()
    # Overall (pooled across models)
    scope_rows[[length(scope_rows) + 1]] <- .ks_block(dat, "Overall")
    # By model
    for (md in models) {
      sub <- dat |> dplyr::filter(as.character(model_pooled) == md)
      scope_rows[[length(scope_rows) + 1]] <- .ks_block(sub, md)
    }
    smp_df <- dplyr::bind_rows(scope_rows)
    all_rows[[length(all_rows) + 1]] <-
      std_row(smp_df, "S4 + pooled", "s4_distribution_ks", smp)
  }

  res <- dplyr::bind_rows(all_rows)

  # Coerce character-typed canonical columns (std_row fills absent cols with
  # logical NA) so downstream bind_rows does not error on type mismatch.
  char_cols <- c("section", "block", "sample", "outcome", "model", "direction", "term", "note")
  for (cc in char_cols) {
    if (cc %in% names(res)) res[[cc]] <- as.character(res[[cc]])
  }
  res[, std_cols]
}
