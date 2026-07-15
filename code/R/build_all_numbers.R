# build_all_numbers.R ==========================================================
# Unified, STUDY-SYMMETRIC numbers assembly for the public Bunkbot reproduction
# (results_methods.Rmd + the Supplementary Information).
#
# Originally ported from a `targets` pipeline into a single plain function.
# It sources the data+inference engine (code/bunkbot_helpers.R, all four studies)
# and every numbers/extension module, then assembles ONE `all_numbers` table
# covering Studies 1-4 + pooled, recomputed from raw + cached data.
#
# Design note (study symmetry): there is NO Study-4-privileged engine file. The
# S4 number generator `compute_s4_numbers()` lives at code/R/numbers_s4.R as a
# PEER of the S1-3 generator `compute_s13_numbers()` (tables_dynamic.R), the
# veracity generator, the topic generator, and the ext_* modules. `build_all_numbers()`
# orchestrates them all uniformly.
#
# Discipline: NO `*_numbers.csv` is ever read as a source of values. Every row is
# computed live from data/ (raw + de-identified) and data/api_cached/ (frozen
# model annotations). The compliant S4 subsample is grown 1056 -> 1073 by the
# APE coverage-gap rescore (data/api_cached/compliance_ape/study4_compliance_ape_rescore_resolved.csv).
# ==============================================================================

build_all_numbers <- function(repo_root) {
  repo_root <- normalizePath(repo_root, mustWork = TRUE)
  R <- function(f) file.path(repo_root, "code", "R", f)

  ## ---- engine (data + inference, all studies) + numbers layer ----------------
  source(file.path(repo_root, "code", "bunkbot_helpers.R"))  # build_s1s3/build_s4_data + inference
  source(R("numbers_s4.R"))          # compute_s4_numbers() — PEER module, not a top-level S4 engine file
  source(R("formatting.R"))
  source(R("ape_rescore.R"))
  source(R("pipeline_core.R"))       # build_core_objects(): S1-3 + S4 (+ APE rescore -> 1073)
  source(R("tables_dynamic.R"))      # compute_s13_numbers / compute_veracity_numbers / compute_topic_numbers
  source(R("numbers_engine.R"))      # compute_s4_numbers_full(): wraps the peer compute_s4_numbers()
  source(R("materials_extractors.R"))
  ext_modules <- c(
    "ext_s13", "ext_attrition", "ext_s4_chat_started", "ext_missing", "ext_extras", "ext_funnel_s13",
    "ext_claim_prevalence", "ext_moderators", "ext_nonattempt", "ext_circumplex",
    "ext_s4_distribution", "ext_claim_counts_full", "ext_topic_contrasts",
    "ext_sm_pretreatment", "ext_perception_s4_bymodel", "ext_volume_detail", "ext_balance",
    # added in the SI reconciliation (2026-06-24): new SI tables/figures
    "ext_belief_trajectory", "ext_conv_length", "ext_demographics", "ext_effect_sizes",
    "ext_simple_slopes", "ext_topic_veracity", "ext_veracity_tail", "ext_paltering",
    # S4 trust change scores (added 2026-07-04; S4 does carry pre-treatment trust)
    "ext_s4_trust",
    # focal-statement veracity (added 2026-07-04; fail-soft until scores are cached)
    "ext_focal_veracity",
    # pooled (S1-4) causal-forest heterogeneity (twin of code/causal_forest_moderation.R)
    "ext_causal_forest",
    # manuscript-reported quantities the main-text Rmds computed inline (block
    # "manuscript_extra"; consumed by code/manuscript_numbers.R -> the Word-doc
    # wiring manifest). Added 2026-07-06.
    "ext_manuscript_s13", "ext_manuscript_s4meth",
    # evaluations-of-the-AI two-slope contrast: persuasion-tracking vs
    # truth-tracking (block "evaluation_slopes"; S1-3 + pooled S1-4).
    # Added 2026-07-07 (author-approved manuscript addition).
    "ext_evaluation_slopes"
  )
  for (m in ext_modules) source(R(paste0(m, ".R")))

  ## ---- core objects: S1-3 (no-duration canonical) + S4 strict/compliant ------
  core <- build_core_objects(repo_root)

  ## ---- base recompute: S1-3, S4 + pooled, veracity, topic (all peers) --------
  s13_numbers      <- compute_s13_numbers(core$s13)
  s4_numbers       <- compute_s4_numbers_full(core$pkg_root, core$slim)
  veracity_numbers <- compute_veracity_numbers(core$paths, core$s13, core$s4)
  topic_bundle     <- compute_topic_numbers(core$paths, core$s13, core$s4)
  all_numbers_core <- dplyr::bind_rows(s13_numbers, s4_numbers, veracity_numbers, topic_bundle$numbers)

  ## ---- comment-driven extensions (canonical schema, all recomputed) ----------
  ext <- dplyr::bind_rows(
    ext_s13_numbers(core),
    dplyr::filter(compute_ext_attrition_numbers(core),
                  !block %in% c("screening_funnel_s13", "attention_checks")),
    compute_s4_chat_started_sensitivity(core),
    ext_missing_numbers(core),
    compute_gold_coding_numbers(core$pkg_root),
    compute_cross_study_summary(all_numbers_core),
    compute_screening_funnel_s13_full(core, core$raw_s13_dir),
    compute_claim_prevalence(core),
    compute_moderator_numbers(core),
    compute_nonattempt_split(core),
    compute_circumplex(core),
    compute_s4_distribution(core),
    compute_claim_counts_full(core),
    compute_topic_contrasts(core),
    compute_topic_veracity(core),
    compute_sm_pretreatment(core),
    compute_perception_s4_by_model(core),
    compute_volume_detail(core),
    compute_balance(core),
    # added in the SI reconciliation: new SI tables/figures
    compute_belief_trajectory(core),
    compute_conversation_length(core),
    compute_demographics(core),
    compute_effect_sizes(core),
    compute_simple_slopes(core),
    compute_veracity_tail(core),
    compute_paltering(core),
    compute_s4_trust_change(core),
    compute_pooled_trust_change(core),
    compute_focal_veracity(core),
    compute_causal_forest_numbers(core),
    compute_manuscript_s13(core),
    compute_manuscript_s4meth(core, repo_root),
    compute_evaluation_slopes(core)
  )

  ## ---- bind + quarantine internal rows + 1056 -> 1073 rename -----------------
  all_numbers <- dplyr::bind_rows(all_numbers_core, ext) |>
    # drop internal, non-reported rows (derived N=1840 production cross-checks;
    # uncleaned admin/demographic summaries) so the reported set is exactly the
    # quantities recomputed from raw + cached data:
    dplyr::filter(
      !block %in% c("production_reference_contrasts", "production_reference_tests",
                    "admin", "admin_gender"),
      is.na(sample) | sample != "production_n1840"
    ) |>
    # the compliant subsample grows 1,056 -> 1,073 after the APE coverage-gap
    # rescore; rename the now-legacy sample id accordingly.
    dplyr::mutate(sample = ifelse(!is.na(sample) & sample == "compliant_n1056",
                                  "compliant_n1073", sample))

  ## ---- materials (QSF + prompt/codebook extracts for the SI Materials/Appendix)
  materials <- tryCatch(list(
    survey_materials   = materials_extract_surveys(repo_root),
    key_item_wording   = materials_key_item_wording(repo_root),
    api_prompt_strings = materials_read_api_strings(repo_root),
    persuader_prompts  = materials_read_persuader_prompts(repo_root),
    api_tables         = materials_read_api_tables(repo_root)
  ), error = function(e) { warning("materials extraction failed: ", conditionMessage(e)); NULL })

  list(
    all_numbers  = all_numbers,
    core         = core,
    topic_bundle = topic_bundle,
    materials    = materials,
    # lightweight causal-forest fit (data frames + per-row tau_hat; NO forest
    # objects) cached so SI figures need no refit on a cached-rds render.
    causal_forest = tryCatch(fit_causal_forest(core), error = function(e) NULL)
  )
}

# Guarded cache loader shared by every consumer (both SIs, methods.Rmd,
# make_figures.R, manuscript tooling). A truncated/corrupt cache — e.g. an
# interrupted saveRDS — falls back to a live rebuild instead of killing the
# render, and the refreshed cache is written atomically (temp file + rename)
# so an interrupted write can never leave a corrupt cache behind.
load_or_build_all_numbers <- function(repo_root,
                                      rds = file.path(repo_root, "output", "_all_numbers.rds")) {
  obj <- if (file.exists(rds)) {
    tryCatch(readRDS(rds), error = function(e) {
      message("Cache ", rds, " unreadable (", conditionMessage(e), ") - rebuilding live.")
      NULL
    })
  } else NULL
  rebuilt <- is.null(obj)
  if (rebuilt) {
    obj <- build_all_numbers(repo_root)
    tmp <- paste0(rds, ".tmp", Sys.getpid())
    saved <- tryCatch({ saveRDS(obj, tmp); file.rename(tmp, rds) },
                      error = function(e) FALSE)
    if (!isTRUE(saved)) unlink(tmp)
  }
  built <- if (is.data.frame(obj)) list(all_numbers = obj) else obj
  # small table-only cache (the numbers, not the ~GB core objects) for tooling
  # that needs values without the full build, e.g. code/manuscript_numbers.R
  tbl <- file.path(dirname(rds), "all_numbers_table.rds")
  if (rebuilt || !file.exists(tbl))
    try(saveRDS(built$all_numbers, tbl), silent = TRUE)
  built
}
