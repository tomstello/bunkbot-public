# ext_nonattempt.R ----------------------------------------------------------------------
# Non-compliant cases that ACTIVELY ARGUED AGAINST the assigned direction.
# Generalizes the GPT-5.2-only gpt52_nonattempt_split to all four Study-4 models:
# among compliance-scored conversations whose first substantive turn did NOT attempt the
# assigned direction, split the non-attempts into (a) EXPLICIT REFUSALS and (b)
# COUNTER-ARGUMENTS (no refusal -- the model instead argued the opposite side), and report
# the direction-aligned belief movement of the counter-argument cases (which moves AGAINST
# the assigned direction). A negative aligned change among counter-arguments is a backfire:
# the model assigned to bunk in fact debunked, and belief fell.
#
# ENTRY POINT: compute_nonattempt_split(core_objects) -> tibble (canonical schema).
# Source: core_objects$s4$s4_with_compliance (attempt_binary, refusal_binary,
# compliance_scored, aligned_belief_change, model_pooled, direction). Requires
# R/tables_dynamic.R (std_row/std_cols).

# Mean + t 95% CI (returns c(mean, lo, hi, n)).
.na_meanci <- function(x) {
  x <- x[is.finite(x)]; n <- length(x)
  if (n < 1) return(c(NA_real_, NA_real_, NA_real_, 0))
  m <- mean(x)
  if (n < 2) return(c(m, NA_real_, NA_real_, n))
  se <- stats::sd(x) / sqrt(n); tt <- stats::qt(0.975, n - 1)
  c(m, m - tt * se, m + tt * se, n)
}

# One model x direction cell of non-attempts.
.nonattempt_cell <- function(df, model_lab, dir_lab) {
  na_n <- nrow(df)
  ref  <- sum(df$refusal_binary == 1, na.rm = TRUE)
  cnt  <- sum(df$refusal_binary == 0, na.rm = TRUE)
  ci   <- .na_meanci(df$aligned_belief_change[df$refusal_binary == 0])
  tibble::tibble(
    model = model_lab, direction = dir_lab,
    term = c("n_nonattempt", "n_explicit_refusal", "n_counterargument",
             "share_counterargument", "mean_aligned_change_counterargument"),
    n = c(na_n, na_n, na_n, na_n, ci[4]),
    estimate = c(na_n, ref, cnt, if (na_n > 0) cnt / na_n else NA_real_, ci[1]),
    conf_low  = c(NA_real_, NA_real_, NA_real_, NA_real_, ci[2]),
    conf_high = c(NA_real_, NA_real_, NA_real_, NA_real_, ci[3])
  )
}

compute_nonattempt_split <- function(core_objects) {
  si_require(c("dplyr", "tibble"))
  swc <- core_objects$s4$s4_with_compliance
  na <- swc |>
    dplyr::filter(compliance_scored, attempt_binary == 0,
                  model_pooled %in% c("Claude", "Gemini", "GPT-5.2", "Grok"),
                  direction %in% c("bunk", "debunk"))
  rows <- list()
  for (m in c("Claude", "Gemini", "GPT-5.2", "Grok")) {
    for (dir in c("bunk", "debunk")) {
      sub <- na[na$model_pooled == m & na$direction == dir, , drop = FALSE]
      if (nrow(sub) == 0) next
      rows[[length(rows) + 1]] <- .nonattempt_cell(sub, m, if (dir == "bunk") "Bunking" else "Debunking")
    }
  }
  for (dir in c("bunk", "debunk")) {
    sub <- na[na$direction == dir, , drop = FALSE]
    if (nrow(sub) == 0) next
    rows[[length(rows) + 1]] <- .nonattempt_cell(sub, "All models", if (dir == "bunk") "Bunking" else "Debunking")
  }
  note <- paste(
    "Among Study-4 compliance-scored conversations whose first turn did NOT attempt the assigned",
    "direction: explicit_refusal = the model refused; counterargument = no refusal, the model argued",
    "the opposite side. mean_aligned_change_counterargument is the direction-aligned belief change of",
    "the counter-argument cases; a negative value means the conversation moved belief AGAINST the",
    "assigned direction (a backfire).")
  dplyr::bind_rows(rows) |>
    dplyr::mutate(note = note) |>
    std_row("S4 + pooled", "nonattempt_split_by_model", "strict_n1272")
}
