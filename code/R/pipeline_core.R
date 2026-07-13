# Core object builders for the dynamic SI.

source_replication_helpers <- function(repo_root) {
  pkg_root <- repo_root
  source(file.path(pkg_root, "code", "bunkbot_helpers.R"))
  invisible(pkg_root)
}

pkg_paths_dynamic <- function(repo_root) {
  pkg_root <- repo_root
  paths <- pkg_paths(pkg_root)
  generated_s4_keys <- grep(
    "^s4_(merged|screen|prereg|cell)",
    names(paths),
    value = TRUE
  )
  paths[generated_s4_keys] <- NULL
  paths
}

build_s4_data_dynamic <- function(paths) {
  post_score_status <- read_post_score_status(paths$s4_cached_post_scores)
  s4_raw <- read_s4_raw(paths$s4_raw) |>
    add_s4_flags() |>
    dplyr::left_join(post_score_status, by = "ResponseId") |>
    dplyr::mutate(
      cached_pre_direction_score_available = dplyr::coalesce(cached_pre_direction_score_available, FALSE),
      cached_post_direction_score_available = dplyr::coalesce(cached_post_direction_score_available, FALSE),
      cached_scores_available = dplyr::coalesce(cached_scores_available, FALSE)
    ) |>
    apply_restatement_orientation(paths$s4_orientation)

  s4 <- s4_raw |>
    dplyr::filter(valid_core_strict) |>
    merge_cached_post_scores(paths$s4_cached_post_scores) |>
    merge_stance_v2_scores(paths$s4_stance_v2) |>
    dplyr::mutate(
      direction = factor(direction, levels = c("bunk", "debunk")),
      modelName = factor(modelName),
      model_pooled = factor(model_pooled, levels = model_order_s4)
    )

  s4_master <- readr::read_csv(paths$s4_master, show_col_types = FALSE, progress = FALSE) |>
    dplyr::mutate(
      direction = factor(direction, levels = c("bunk", "debunk")),
      model_pooled = factor(model_pooled, levels = model_order_s4)
    )

  claim_role <- readr::read_csv(paths$claim_role, show_col_types = FALSE, progress = FALSE) |>
    dplyr::mutate(
      direction = factor(direction, levels = c("bunk", "debunk")),
      model_pooled = factor(model_pooled, levels = model_order_s4),
      truth_bin = factor(truth_bin, levels = c("False", "Mostly False", "True"))
    )

  compliance <- readr::read_csv(paths$compliance, show_col_types = FALSE, progress = FALSE) |>
    dplyr::mutate(
      attempt_binary = as.numeric(attempt_binary),
      refusal_binary = as.numeric(refusal_binary),
      strict_compliant = attempt_binary == 1 & refusal_binary == 0
    ) |>
    dplyr::transmute(
      conversation_id = human_id,
      model_pooled = factor(model_pooled, levels = model_order_s4),
      direction = factor(direction, levels = c("bunk", "debunk")),
      attempt_binary,
      refusal_binary,
      strict_compliant,
      persuasion_score_5 = as.numeric(persuasion_score_5),
      specificity_score_5 = as.numeric(specificity_score_5),
      compliance_status
    )

  s4_with_compliance <- s4 |>
    dplyr::left_join(
      compliance,
      by = c("ResponseId" = "conversation_id", "model_pooled", "direction")
    ) |>
    dplyr::mutate(
      compliance_scored = !is.na(attempt_binary) | !is.na(refusal_binary),
      strict_compliant = dplyr::coalesce(strict_compliant, FALSE)
    )

  s4_compliant <- s4_with_compliance |>
    dplyr::filter(strict_compliant) |>
    droplevels()

  list(
    paths = paths,
    s4_raw = s4_raw,
    s4 = s4,
    s4_with_compliance = s4_with_compliance,
    s4_compliant = s4_compliant,
    s4_master = s4_master,
    claim_role = claim_role,
    compliance = compliance
  )
}

validate_s4_data_dynamic <- function(d) {
  stopifnot(
    nrow(d$s4_raw) == 14399,
    nrow(d$s4) == 1272,
    nrow(d$s4_with_compliance) == 1272,
    nrow(d$s4_compliant) == 1056,
    sum(d$s4_raw$valid_core_strict, na.rm = TRUE) == 1272,
    sum(d$s4$chat_saved, na.rm = TRUE) == 1270,
    !any(duplicated(d$s4$rid_clean[!is.na(d$s4$rid_clean)])),
    !any(d$s4$restatement_orientation == "unaudited"),
    sum(d$s4$belief_scale_flipped) == 12,
    all(d$s4$belief_rating_pre_4 > 25 & d$s4$belief_rating_pre_4 < 75),
    !any(is.na(d$s4$pre_direction_score)),
    !any(is.na(d$s4$post_direction_score)),
    all(d$s4$pre_stance_n_raters >= 4, na.rm = FALSE),
    all(d$s4$post_stance_n_raters >= 4, na.rm = FALSE)
  )
  invisible(TRUE)
}

build_core_objects <- function(repo_root) {
  pkg_root <- source_replication_helpers(repo_root)
  paths <- pkg_paths_dynamic(repo_root)
  s13 <- build_s1s3(paths, duration_filter = FALSE)
  s13_duration_sensitivity <- build_s1s3(paths, duration_filter = TRUE)
  slim <- build_s1s3_slim(paths, duration_filter = FALSE)
  d <- build_s4_data_dynamic(paths)
  validate_s4_data_dynamic(d)  # base build: strict 1272, compliant 1056 (pre-rescore)
  # Resolve the gpt-4o APE classifier coverage gap: 17 strict conversations (16 Grok,
  # 1 Claude) were never scored and were re-scored from their first assistant turn with
  # the same attempt/refusal rubric (all compliant). This grows the compliant subsample
  # from 1,056 to 1,073. See R/ape_rescore.R.
  d <- apply_ape_rescore(d, paths$ape_rescore)
  list(
    repo_root = repo_root,
    pkg_root = pkg_root,
    paths = paths,
    raw_s13_dir = file.path(repo_root, "data", "raw_qualtrics"),  # S1-3 raw Qualtrics exports for the full screening funnel
    s13 = s13,
    s13_duration_sensitivity = s13_duration_sensitivity,
    slim = slim,
    s4 = d
  )
}
