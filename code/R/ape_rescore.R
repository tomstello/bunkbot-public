# ape_rescore.R --------------------------------------------------------------------------
# Resolves the Study-4 attempt-to-persuade (APE) classifier COVERAGE GAP: a small set of
# strict conversations (overwhelmingly Grok) were never scored by the cached gpt-4o
# attempt/refusal classifier and were conservatively coalesced to non-compliant. Each was
# re-scored from its first assistant turn with the identical APE rubric; all that retained an
# extractable first turn attempted the assigned direction with no refusal (the 3 with no
# model response remain non-compliant). The resolved labels live in ape_rescore_resolved.csv.
#
# apply_ape_rescore(s4obj, resolved_path) takes a Study-4 object carrying $s4_with_compliance
# (and $s4_compliant), marks the resolved ResponseIds as scored/compliant, and recomputes the
# compliant subsample. Used both in build_core_objects() and compute_s4_numbers_full() so the
# core objects and the recomputed numbers agree.

apply_ape_rescore <- function(s4obj, resolved_path = NULL) {
  if (is.null(resolved_path)) {
    resolved_path <- file.path(getwd(), "ape_rescore_resolved.csv")
  }
  if (!file.exists(resolved_path)) return(s4obj)
  swc <- s4obj$s4_with_compliance
  if (is.null(swc) || !"ResponseId" %in% names(swc)) return(s4obj)

  res <- utils::read.csv(resolved_path, stringsAsFactors = FALSE)
  idx <- match(res$ResponseId, swc$ResponseId)
  ok <- !is.na(idx)
  if (any(ok)) {
    j <- idx[ok]
    swc$attempt_binary[j]   <- as.numeric(res$attempt_binary[ok])
    swc$refusal_binary[j]   <- as.numeric(res$refusal_binary[ok])
    swc$strict_compliant[j] <- res$attempt_binary[ok] == 1 & res$refusal_binary[ok] == 0
    if ("compliance_scored" %in% names(swc)) swc$compliance_scored[j] <- TRUE
    if ("compliance_status" %in% names(swc)) {
      swc$compliance_status[j] <- ifelse(swc$strict_compliant[j], "strict_compliant", "non_compliant")
    }
  }
  s4obj$s4_with_compliance <- swc
  # recompute the compliant subsample exactly as build_s4_data does
  s4obj$s4_compliant <- droplevels(swc[isTRUE_vec(swc$strict_compliant), , drop = FALSE])
  s4obj
}

# helper: TRUE for rows where strict_compliant is TRUE (NA -> FALSE)
isTRUE_vec <- function(x) !is.na(x) & x
