# dev_qa.R — DEVELOPER-ONLY regression tripwire. NEVER part of the rendered document.
#
# Confirms the from-scratch recompute (`all_numbers`, built live from raw + cached
# data by build_all_numbers()) matches a set of audited canonical anchor values.
# Those anchors are an EXTERNAL cross-check ONLY: the rendered documents never
# read them — every reported number is recomputed live via num()/est(). This file
# exists to catch accidental drift, per the public-repo discipline.
#
# Run:  make test       (or)  Rscript code/dev_qa.R     [from the repo root]
# Fast: set BUNKBOT_DEVQA_RDS=output/_all_numbers.rds to reuse a prior build.

suppressWarnings(suppressMessages(library(dplyr)))

.bunkbot_root <- function() {
  for (cand in c(Sys.getenv("BUNKBOT_ROOT", unset = NA), ".", "..")) {
    if (!is.na(cand) && file.exists(file.path(cand, "code", "R", "build_all_numbers.R")))
      return(normalizePath(cand))
  }
  stop("dev_qa: cannot locate repo root (need code/R/build_all_numbers.R)")
}

REPO_ROOT <- .bunkbot_root()
.cached <- Sys.getenv("BUNKBOT_DEVQA_RDS", unset = "")
.obj <- if (nzchar(.cached) && file.exists(.cached)) {
  tryCatch(readRDS(.cached),                     # cache may be the bare table or the full build list
           error = function(e) { message("dev_qa: cache unreadable, rebuilding live."); NULL })
} else NULL
if (is.null(.obj)) {
  source(file.path(REPO_ROOT, "code", "R", "build_all_numbers.R"))
  .obj <- build_all_numbers(REPO_ROOT)
}
AN <- if (is.data.frame(.obj)) .obj else .obj$all_numbers

.get <- function(block, ...) {
  o <- AN %>% filter(block == !!block)
  f <- list(...); for (k in names(f)) { v <- f[[k]]
    o <- if (length(v) == 1 && is.na(v)) filter(o, is.na(.data[[k]])) else filter(o, .data[[k]] == v) }
  o <- o %>% filter(rowSums(!is.na(across(c(estimate, n, statistic)))) > 0)
  stopifnot(nrow(o) == 1); o
}
fails <- character(0)
chk <- function(label, got, expected, tol = 0.06) {
  ok <- !is.na(got) && abs(as.numeric(got) - expected) <= tol
  cat(sprintf("  [%s] %-40s %s vs %s\n", if (ok) "OK" else "XX", label, signif(got, 5), expected))
  if (!ok) fails[[length(fails) + 1]] <<- label
}

# Audited canonical anchors (no-duration S1-3 sample; post-APE-rescore compliant 1,073).
chk("S1 bunk belief change",      .get("belief_change", model = "Jailbroken",        direction = "Bunking")$estimate, 13.628)
chk("S3 bunk belief change",      .get("belief_change", model = "Truth-Constrained", direction = "Bunking")$estimate,  4.548)
chk("S3 bunk-vs-debunk contrast", .get("belief_bunk_vs_debunk", model = "Truth-Constrained")$estimate,                -6.43)
chk("Sharing asymmetry",          .get("prereg_registered_tests", outcome = "aligned_new_minus_old_weighted", term = "avg_debunk_minus_bunk")$estimate, 13.46, tol = 0.2)
chk("GPT-5.2 bunk belief",        .get("raw_aligned_means_cells", sample = "strict_n1272", model = "GPT-5.2", direction = "bunk", outcome = "aligned_belief_change", term = "raw_mean")$estimate, -13.98, tol = 0.2)
chk("Pooled S1+S2+S4 symmetry",   .get("pooled_symmetry_test", sample = "pooled_compliant", model = NA, direction = NA, term = "debunk_minus_bunk_equal_weighted")$estimate, -0.177, tol = 0.2)
chk("S3 counterfactual (n~740)",  .get("s3_counterfactual")$estimate, -3.09)

# --- newly added / corrected blocks (SI reconciliation, 2026-06-24) ----
chk("Simple slope contrast (S3 strict)", .get("simple_slopes", sample = "strict", model = "Truth-Constrained", term = "contrast_at_50")$estimate, -4.75, tol = 0.1)
chk("Compliance native unscored (n=20)", .get("compliance_coverage", term = "classifier_unscored")$estimate, 20, tol = 0.5)
chk("S4 claim census, analytic IDs",     .get("claim_counts_full", model = "Study4", direction = NA, term = "n_total_claims")$estimate, 60979, tol = 5)

# --- manuscript-wiring block (ext_manuscript_*, 2026-07-06) ---------------------------
chk("S1-3 fact-checked claim census",    .get("manuscript_extra", term = "claims.k")$estimate, 95707, tol = 5)
chk("S1 adjusted contrast t",            .get("manuscript_extra", term = "s1.belief_contrast")$statistic, 1.208, tol = 0.01)
chk("S4 non-mover sharing contrast",     .get("manuscript_extra", term = "s4.nonmover")$estimate, 9.726, tol = 0.06)

if (length(fails)) stop("dev_qa FAILED: ", paste(fails, collapse = ", "))
cat("\ndev_qa: all recompute anchors match the audited canonical values.\n")
