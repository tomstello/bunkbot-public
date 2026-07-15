# ext_volume_detail.R --------------------------------------------------------------------
# Detailed claim-VOLUME asymmetry: aligned belief change as a function of the number of
# fact-checkable aligned claims in the conversation, by condition. Bunking scales steeply
# with claim count; debunking is much flatter. This reproduces the per-conversation
# regression behind the "bunking scales with volume" figure, on BOTH the strict and the
# compliant Study-4 samples.
#
# ENTRY POINT: compute_volume_detail(core_objects) -> tibble (canonical schema).
# Source: core_objects$s4$claim_role (one row per Study-4 conversation) with
#   n_aligned = aligned_direct_n + aligned_indirect_n, aligned_belief_change, direction,
#   conversation_id; compliant = conversation_id in s4_compliant.
# block "claim_volume_detail" (section "S4 + pooled").
#   term "volume_slope"  : OLS slope of aligned_change on log(n_aligned) -- belief points
#                          per e-fold more aligned claims; intercept carried in statistic.
#   term "volume_r2"     : R^2 of that regression.
#   term "bin_<label>"   : binned mean aligned_change (outcome="binned_mean"); conf_low/high
#                          = 95% CI; statistic = mean log(n_aligned) in the bin (x position);
#                          n = bin size.

.vd_bins <- function(x) cut(x, breaks = c(0, 2, 5, 10, 20, 40, 1e4),
                            labels = c("1-2", "3-5", "6-10", "11-20", "21-40", "41+"))

.vd_cell <- function(d, samp, dir_lab) {
  d <- d[is.finite(d$logn) & is.finite(d$aligned_belief_change), , drop = FALSE]
  if (nrow(d) < 10) return(NULL)
  fit <- stats::lm(aligned_belief_change ~ logn, data = d)
  co <- stats::coef(fit)
  ci <- suppressWarnings(stats::confint(fit)["logn", ])
  rows <- list(tibble::tibble(
    outcome = NA_character_, model = NA, direction = dir_lab, term = "volume_slope",
    n = nrow(d), estimate = unname(co[["logn"]]),
    conf_low = ci[[1]], conf_high = ci[[2]],
    statistic = unname(co[["(Intercept)"]]),
    p_value = summary(fit)$coefficients["logn", 4],
    note = "OLS slope: aligned belief change per e-fold more aligned claims; intercept in statistic."))
  rows[[2]] <- tibble::tibble(
    outcome = NA_character_, model = NA, direction = dir_lab, term = "volume_r2",
    n = nrow(d), estimate = summary(fit)$r.squared,
    note = "R^2 of aligned belief change on log(aligned claim count).")
  # binned means
  d$cbin <- .vd_bins(d$n_aligned)
  bm <- d |>
    dplyr::group_by(cbin) |>
    dplyr::summarise(x = mean(logn), m = mean(aligned_belief_change),
                     se = stats::sd(aligned_belief_change) / sqrt(dplyr::n()),
                     nb = dplyr::n(), .groups = "drop") |>
    dplyr::filter(!is.na(cbin), nb >= 3)
  for (i in seq_len(nrow(bm))) {
    rows[[length(rows) + 1]] <- tibble::tibble(
      outcome = "binned_mean", model = NA, direction = dir_lab,
      term = paste0("bin_", as.character(bm$cbin[i])), n = bm$nb[i],
      estimate = bm$m[i], conf_low = bm$m[i] - 1.96 * bm$se[i],
      conf_high = bm$m[i] + 1.96 * bm$se[i], statistic = bm$x[i],
      note = "Binned mean aligned belief change; statistic = mean log(aligned claims) in bin.")
  }
  dplyr::bind_rows(rows)
}

compute_volume_detail <- function(core_objects) {
  si_require(c("dplyr", "tibble"))
  cr <- core_objects$s4$claim_role
  if (is.null(cr)) return(std_row(tibble::tibble(term = character(0)), "S4 + pooled", "claim_volume_detail", "strict_n1272"))
  cr <- cr |>
    dplyr::mutate(n_aligned = dplyr::coalesce(aligned_direct_n, 0) + dplyr::coalesce(aligned_indirect_n, 0),
                  logn = log(n_aligned)) |>
    dplyr::filter(n_aligned >= 1, !is.na(aligned_belief_change),
                  as.character(direction) %in% c("bunk", "debunk"),
                  as.character(model_pooled) %in% c("Claude", "Gemini", "GPT-5.2", "Grok"))  # Study-4 conversations
  strict_ids <- core_objects$s4$s4$ResponseId          # the strict observed-outcome sample
  comp_ids   <- core_objects$s4$s4_compliant$ResponseId
  # claim_role spans every conversation with extracted claims (S2 + S4, ~1,399 here);
  # restrict BOTH samples to their analytic ResponseId sets so the volume models run on
  # the same conversations as every other Study-4 estimate (strict overlap = 961).
  samples <- list(strict_n1272 = dplyr::filter(cr, conversation_id %in% strict_ids),
                  compliant_n1073 = dplyr::filter(cr, conversation_id %in% comp_ids))
  out <- list()
  for (samp in names(samples)) {
    d0 <- samples[[samp]]
    for (dd in c("bunk", "debunk")) {
      sub <- d0[as.character(d0$direction) == dd, , drop = FALSE]
      r <- .vd_cell(sub, samp, if (dd == "bunk") "Bunking" else "Debunking")
      if (!is.null(r)) out[[length(out) + 1]] <- std_row(r, "S4 + pooled", "claim_volume_detail", samp)
    }
  }
  if (!length(out)) return(std_row(tibble::tibble(term = character(0)), "S4 + pooled", "claim_volume_detail", "strict_n1272"))
  dplyr::bind_rows(out)
}
