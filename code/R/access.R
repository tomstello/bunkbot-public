# access.R — inline-number accessors over the live-recomputed all_numbers.
# The Rmd setup chunk builds (or loads) the table and exposes it as the global
# ALL_NUMBERS. Prose then pulls single values with num()/est()/fmt_* so text can
# never drift from the recomputed table.

num <- function(block, ..., data = NULL) {
  df  <- if (is.null(data)) get("ALL_NUMBERS", envir = globalenv()) else data
  out <- df[!is.na(df$block) & df$block == block, , drop = FALSE]
  flt <- list(...)
  for (k in names(flt)) {
    v <- flt[[k]]; if (is.null(v)) next
    if (!k %in% names(out)) stop(sprintf("num(): unknown filter column '%s'", k))
    if (length(v) == 1 && is.na(v)) out <- out[is.na(out[[k]]), , drop = FALSE]
    else out <- out[!is.na(out[[k]]) & as.character(out[[k]]) %in% as.character(v), , drop = FALSE]
  }
  out <- out[rowSums(!is.na(out[, c("estimate","n","statistic")])) > 0, , drop = FALSE]
  key <- sprintf("block=%s%s", block,
                 if (length(flt)) paste0(", ", paste(names(flt), unlist(flt), sep="=", collapse=", ")) else "")
  if (nrow(out) == 0) stop(sprintf("num(): NO match for %s", key))
  if (nrow(out) > 1)  stop(sprintf("num(): %d matches for %s — tighten filters", nrow(out), key))
  out[1, , drop = FALSE]
}
est  <- function(...) num(...)$estimate
lo   <- function(...) num(...)$conf_low
hi   <- function(...) num(...)$conf_high
pv   <- function(...) num(...)$p_value
nn   <- function(...) num(...)$n

# formatters (compatible with fmt_num/fmt_ci/fmt_p already in formatting.R)
fmt_n   <- function(x) formatC(round(as.numeric(x)), format = "d", big.mark = ",")
fmt_pct <- function(x, d = 1) paste0(formatC(100*as.numeric(x), format="f", digits=d), "%")
# estimate + CI + p in one go, en-dash minus
fmt_est_ci <- function(row, d = 1, signed = FALSE) {
  e <- as.numeric(row$estimate)
  s <- if (signed && e > 0) "+" else ""
  f <- function(z) sub("-", "−", formatC(as.numeric(z), format="f", digits=d))
  sprintf("%s%s [%s, %s]", s, f(e), f(row$conf_low), f(row$conf_high))
}
fmt_full <- function(row, d = 1, signed = FALSE) {
  p <- row$p_value
  pp <- if (is.na(p)) "" else if (p < .001) ", *p* < .001"
        else paste0(", *p* = ", sub("^0","", formatC(p, format="f", digits=3)))
  paste0(fmt_est_ci(row, d, signed), pp)
}
# p-value fragment alone ("< .001" / "= .045") for prose where the estimate and
# its unit must precede the p (avoids "…, p < .001 points" constructions)
fmt_p_eq <- function(row) {
  p <- if (is.list(row) || is.data.frame(row)) row$p_value else row
  if (is.na(p)) return("")
  if (p < .001) "< .001"
  else paste0("= ", sub("^0", "", formatC(p, format = "f", digits = 3)))
}
