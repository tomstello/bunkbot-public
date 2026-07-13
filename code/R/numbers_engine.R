# numbers_engine.R
# Full canonical-schema Study-4 + pooled recompute for the SI engine.
# Rebuilds every Study-4 and pooled-cross-study quantity from raw + cached
# inputs (no pre-computed answer file is ever read), covering the full
# pooled-symmetry / GPT-5.2-decomposition / claim-volume / veracity-vs-persuasion
# family, in the canonical (section/block/sample/model/direction/term) schema.

compute_s4_numbers_full <- function(pkg_root, slim) {
  paths <- pkg_paths(pkg_root)            # FULL paths (incl. production_merged, screen_funnel)
  d_pre <- build_s4_data(paths)          # replication-format Study-4 object (self-validates anchors)
  # Apply the same APE-rescore coverage-gap resolution used in build_core_objects, so the
  # recomputed compliant-sample numbers are on the 1,073-row sample. compute_s4_numbers emits
  # the sample id "compliant_n1056"; renamed to "compliant_n1073" downstream.
  d <- apply_ape_rescore(d_pre, paths$ape_rescore)
  # Carry the PRE-rescore compliance snapshot + the rescore count so the
  # compliance_coverage block can report the gpt-4o classifier's NATIVE coverage
  # (1,252 scored / 20 unscored) plus the rescore resolution as separate terms,
  # rather than the post-rescore residual (which would mislabel the 17 manual
  # overrides as classifier output). Fixed at source -- see numbers_s4.R.
  d$s4_with_compliance_prerescore <- d_pre$s4_with_compliance
  d$n_rescored <- nrow(d$s4_compliant) - nrow(d_pre$s4_compliant)
  compute_s4_numbers(d, slim) |>
    dplyr::mutate(section = "S4 + pooled")
}
