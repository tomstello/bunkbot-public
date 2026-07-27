# ext_attrition.R
# ---------------------------------------------------------------------------
# Comprehensive ATTRITION / ENGAGEMENT / ATTENTION recompute for the Bunkbot SI,
# covering ALL FOUR studies, returning rows in the canonical all_numbers schema
# (section, block, sample, outcome, model, direction, term, n, estimate, se,
#  conf_low, conf_high, statistic, df_num, df_den, p_value, note).
#
# Three families of blocks are produced:
#
#   FAMILY 1  ATTRITION + screening
#     * screening_funnel_s13                 -- per-study S1-3 eligibility funnel
#     * dropout_logistic_s13                 -- differential attrition (dropout ~
#                                               condition) logistic, per study, +
#                                               an omnibus likelihood-ratio test
#     * attrition_substantive_vs_technical   -- ALL studies: separates confirmed
#                                               initial AI delivery from the
#                                               causally ambiguous no-callback
#                                               state, then distinguishes initial-
#                                               response-only, interactive-chat,
#                                               and chat-completed losses. Model x
#                                               date patterns identify an aggregate
#                                               Gemini endpoint-removal component;
#                                               individual no-callback rows are not
#                                               labeled technical vs voluntary.
#
#   FAMILY 2  ENGAGEMENT (block = "engagement")
#     * S1-3: mean user/assistant message counts, mean user/assistant whitespace
#       word counts, and % of conversations ending before the 10-turn max, by
#       study x direction, from the parsed content_user_* / content_assistant_*
#       columns of the analytic frame.
#     * S4: same metrics parsed from the JSON transcripts (chathistory0*). The
#       transcripts are chunked, double-escaped JSON; a robust reconstructor is
#       used (Hause Lin's chatlogr package join_str='""' convention). If the
#       chatlogr-style parse yields no usable transcript the per-row fallback is
#       NOTED.
#
#   FAMILY 3  ATTENTION / BOT items (block = "attention_checks")
#     * "nicks" instructed-response item (administered in every study)
#     * gender "favorite spice" trap (administered ONLY in S2/S3 to catch LLMs;
#       S4 uses the berry/strawberry trap instead -- reported in parallel)
#     * "if you are an LLM" self-disclosure item (administered S1-S4)
#     Pass rates are computed from the raw S1-3 clean files + S4 raw, per study.
#
# All functions take `core_objects` (built by build_all_numbers()) and assume
# bunkbot_helpers.R has been sourced (for nonempty(), make_screening_flow(),
# model_pooled_label(), model_order_s4, etc.). They reuse the canonical helpers;
# they do not reinvent screening/recoding.
# ---------------------------------------------------------------------------

# ============================================================================
# Small utilities
# ============================================================================

.ext_word_count <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  vapply(
    x,
    function(s) {
      s <- trimws(s)
      if (!nzchar(s)) return(0L)
      length(strsplit(s, "\\s+")[[1]])
    },
    integer(1),
    USE.NAMES = FALSE
  )
}

.ext_mean_ci <- function(v) {
  v <- v[!is.na(v)]
  n <- length(v)
  m <- if (n > 0) mean(v) else NA_real_
  s <- if (n > 1) stats::sd(v) else NA_real_
  se <- if (n > 1) s / sqrt(n) else NA_real_
  crit <- if (n > 1) stats::qt(.975, n - 1) else NA_real_
  list(
    n = n, estimate = m, se = se,
    conf_low = if (n > 1) m - crit * se else NA_real_,
    conf_high = if (n > 1) m + crit * se else NA_real_
  )
}

# Robust reconstruction of an S4 chunked JSON transcript. The chathistory0*
# fields are successive fragments of one JSON STRING literal (each fragment is
# itself surrounded by escaping quotes and joined with the chatlogr `""`
# convention). We strip each fragment's surrounding quotes, concatenate, then
# unescape once and parse. Returns the messages list or NULL.
.ext_parse_s4_transcript <- parse_s4_complete_transcript

# ============================================================================
# FAMILY 1a: S1-3 screening funnel
# ============================================================================

# Reconstructs, per Study, the eligibility funnel from the raw clean files at
# core_objects$paths$s1s3_clean toward the analytic frame produced by
# build_s1s3() (equivocal == TRUE, 25 < belief_pre_rc < 75, post present,
# condition assigned). The clean files already have the instructed-response
# attention check applied upstream (all surviving rows have nicks == 5), so the
# attention stage is reported with that caveat in the note. make_screening_flow()
# is S4-specific (different flag set), so the S1-3 funnel is computed directly
# from the documented build_s1s3() screen sequence.
compute_screening_funnel_s13 <- function(core_objects) {
  si_require(c("dplyr", "readr", "tibble"))
  paths <- core_objects$paths
  study_map <- c("Study 1" = "Jailbroken", "Study 2" = "Standard", "Study 3" = "Truth-Constrained")
  out <- list()
  for (st in names(study_map)) {
    d <- readr::read_csv(paths$s1s3_clean[[st]], show_col_types = FALSE, progress = FALSE)
    d <- d |>
      dplyr::mutate(
        .belief_pre   = suppressWarnings(as.numeric(belief_rating_pre_4)),
        .belief_post  = suppressWarnings(as.numeric(belief_rating_post_4)),
        .pre_rc  = dplyr::if_else(category == "denies", 100 - .belief_pre, .belief_pre),
        .post_rc = dplyr::if_else(category == "denies", 100 - .belief_post, .belief_post),
        .cond_ok = condition %in% c("treatment_mid_bunk", "treatment_mid_debunk")
      )
    n0 <- nrow(d)
    f_attn  <- d  # nicks == 5 already applied upstream in the clean file
    f_equiv <- f_attn  |> dplyr::filter(isEquivocal == TRUE)
    f_win   <- f_equiv |> dplyr::filter(.pre_rc > 25, .pre_rc < 75)
    f_post  <- f_win   |> dplyr::filter(!is.na(.post_rc))
    f_cond  <- f_post  |> dplyr::filter(.cond_ok)
    stages <- tibble::tibble(
      term = c(
        "Clean-file respondent rows (attention check already applied)",
        "Conspiracy classified equivocal (isEquivocal == TRUE)",
        "Baseline belief in equivocal window (25 < belief_rc < 75)",
        "Post-conversation belief present",
        "Assigned bunk/debunk condition (analytic N)"
      ),
      n = c(n0, nrow(f_equiv), nrow(f_win), nrow(f_post), nrow(f_cond))
    ) |>
      dplyr::mutate(
        estimate = n,
        statistic = dplyr::lag(n, default = NA_integer_) - n,   # excluded at this step
        note = paste0(
          "model=", study_map[[st]],
          "; pct_of_clean=", round(100 * n / n0, 1), "%",
          "; nicks==5 applied upstream so attention pass-rate is 100% by construction within this file"
        )
      )
    out[[st]] <- stages |>
      std_row("S1-3", "screening_funnel_s13", "full_sample") |>
      dplyr::mutate(model = study_map[[st]])
  }
  dplyr::bind_rows(out)
}

# ============================================================================
# FAMILY 1b: S1-3 differential-attrition logistic
# ============================================================================

# Among the eligible conversation pool in each study (equivocal == TRUE AND
# baseline belief in the 25-75 window AND a bunk/debunk condition assigned),
# models dropout = (post-conversation belief MISSING) as a function of condition.
# Reports the condition coefficient (log-odds) with HC-free model SE/CI/p plus an
# omnibus likelihood-ratio chi-square test of differential attrition by condition.
compute_dropout_logistic_s13 <- function(core_objects) {
  si_require(c("dplyr", "readr", "broom", "tibble"))
  paths <- core_objects$paths
  study_map <- c("Study 1" = "Jailbroken", "Study 2" = "Standard", "Study 3" = "Truth-Constrained")
  out <- list()
  for (st in names(study_map)) {
    d <- readr::read_csv(paths$s1s3_clean[[st]], show_col_types = FALSE, progress = FALSE) |>
      dplyr::mutate(
        .belief_pre  = suppressWarnings(as.numeric(belief_rating_pre_4)),
        .belief_post = suppressWarnings(as.numeric(belief_rating_post_4)),
        .pre_rc  = dplyr::if_else(category == "denies", 100 - .belief_pre, .belief_pre),
        .post_rc = dplyr::if_else(category == "denies", 100 - .belief_post, .belief_post)
      ) |>
      dplyr::filter(
        isEquivocal == TRUE, .pre_rc > 25, .pre_rc < 75,
        condition %in% c("treatment_mid_bunk", "treatment_mid_debunk")
      ) |>
      dplyr::mutate(
        dropout = as.integer(is.na(.post_rc)),
        condition_factor = factor(
          condition,
          levels = c("treatment_mid_bunk", "treatment_mid_debunk"),
          labels = c("Bunking", "Debunking")
        )
      )
    n_pool <- nrow(d)
    fit  <- stats::glm(dropout ~ condition_factor, data = d, family = stats::binomial())
    fit0 <- stats::glm(dropout ~ 1, data = d, family = stats::binomial())
    lr <- stats::anova(fit0, fit, test = "LRT")
    lr_stat <- lr$Deviance[2]
    lr_df   <- lr$Df[2]
    lr_p    <- lr$`Pr(>Chi)`[2]

    coef_rows <- broom::tidy(fit, conf.int = TRUE) |>
      dplyr::transmute(
        term = dplyr::recode(term,
          "(Intercept)" = "intercept_logit",
          "condition_factorDebunking" = "debunk_vs_bunk_logit"),
        n = n_pool,
        estimate, se = std.error, conf_low = conf.low, conf_high = conf.high,
        statistic, p_value = p.value,
        note = "logistic dropout(post belief missing) ~ condition; eligible pool = equivocal & 25-75 window & condition assigned; model-based SE"
      )
    omnibus <- tibble::tibble(
      term = "differential_attrition_LRT",
      n = n_pool, estimate = NA_real_, se = NA_real_,
      conf_low = NA_real_, conf_high = NA_real_,
      statistic = lr_stat, df_num = lr_df, p_value = lr_p,
      note = "likelihood-ratio chi-square: does dropout depend on condition?"
    )
    overall <- tibble::tibble(
      term = "overall_dropout_rate",
      n = n_pool, estimate = mean(d$dropout),
      note = "share of eligible pool with missing post-conversation belief"
    )
    out[[st]] <- dplyr::bind_rows(coef_rows, omnibus, overall) |>
      std_row("S1-3", "dropout_logistic_s13", "full_sample") |>
      dplyr::mutate(model = study_map[[st]])
  }
  dplyr::bind_rows(out)
}

# ============================================================================
# FAMILY 1c: stored delivery state and outcome attrition, ALL studies
# ============================================================================

# The partial-chat field is diagnostic of successful transcript callbacks, not
# of why a callback is absent. A no-callback record can reflect participant
# departure at the chatbot transition, iframe/client failure, or initial model
# non-delivery. `parse_partial_chat_state()` (defined in bunkbot_helpers.R) joins
# the Qualtrics chunks before parsing and excludes the hidden pre-chat `user`
# seed from the visible in-chat user-turn count.

compute_attrition_substantive_vs_technical <- function(core_objects) {
  si_require(c("dplyr", "readr", "tibble"))
  out <- list()

  # ---- S4: classify each non-completer from the partial-chat message log ----
  s4raw <- core_objects$s4$s4_raw
  .state_names <- names(parse_partial_chat_state(""))
  pool <- prepare_s4_delivery_pool(s4raw)

  .terms <- c(
    "pool", "completed", "no_successful_callback_n", "initial_ai_no_user_n",
    "interactive_chat_then_lost_n", "chat_completed_outcomes_missing_n",
    "user_no_initial_ai_n", "last_user_without_reply_n",
    "initial_ai_delivered_loss_n", "postdelivery_attrition_rate"
  )
  .att_note <- paste(
    "S4 pool = passed all strict screens through model assignment.",
    "Partial-chat chunks are joined before JSON parsing; the hidden pre-chat user seed is not counted as a visible chat turn.",
    "no_successful_callback means no nonempty assistant response was stored and cannot distinguish transition dropout from frontend/API/provider failure;",
    "initial_ai_no_user means an initial assistant response was stored but no new visible user turn followed;",
    "interactive_chat_then_lost means at least one new user turn was stored before an unfinished chat;",
    "chat_completed_outcomes_missing means nextSection was true or a complete main transcript was saved, but outcomes were missing.",
    "The requested google/gemini-3-pro-preview endpoint was removed from OpenRouter during fielding; the model/date-specific excess is identifiable only in aggregate.")
  .att_counts <- function(df) {
    cl <- df$loss_class
    cp <- sum(cl == "completed")
    nc <- sum(cl == "no_successful_callback")
    ia <- sum(cl == "initial_ai_no_user")
    ic <- sum(cl == "interactive_chat_then_lost")
    cc <- sum(cl == "chat_completed_outcomes_missing")
    un <- sum(cl == "user_no_initial_ai")
    lr <- sum(!df$completed & df$initial_ai == 1L & df$chat_user == 1L & df$last_user_replied == 0L)
    dl <- sum(!df$completed & df$initial_ai == 1L)
    c(nrow(df), cp, nc, ia, ic, cc, un, lr, dl,
      if (cp + dl > 0) dl / (cp + dl) else NA_real_)
  }
  s4_rows <- lapply(sort(unique(pool$model)), function(m)
    tibble::tibble(model = m, term = .terms, n = sum(pool$model == m),
                   estimate = .att_counts(pool[pool$model == m, ]), note = .att_note))
  s4_total <- tibble::tibble(model = NA_character_, term = .terms, n = nrow(pool),
                             estimate = .att_counts(pool), note = paste(.att_note, "(pooled across models)."))
  # Differential attrition by condition/model on the full S4 pool.
  pool <- pool |> dplyr::mutate(incomplete = as.integer(!completed))
  fit_dir <- stats::glm(incomplete ~ direction, data = pool, family = stats::binomial())
  lr_dir <- stats::anova(stats::glm(incomplete ~ 1, data = pool, family = stats::binomial()), fit_dir, test = "LRT")
  fit_mod <- stats::glm(incomplete ~ model, data = pool, family = stats::binomial())
  lr_mod <- stats::anova(stats::glm(incomplete ~ 1, data = pool, family = stats::binomial()), fit_mod, test = "LRT")
  s4_tests <- tibble::tibble(
    model = NA_character_,
    term = c("differential_attrition_by_direction_LRT", "differential_attrition_by_model_LRT"),
    n = nrow(pool),
    statistic = c(lr_dir$Deviance[2], lr_mod$Deviance[2]),
    df_num = c(lr_dir$Df[2], lr_mod$Df[2]),
    p_value = c(lr_dir$`Pr(>Chi)`[2], lr_mod$`Pr(>Chi)`[2]),
    note = "LRT on the S4 pool incomplete-outcome indicator. The by-model test includes the Gemini endpoint-removal period; a no-callback row is not individually attributable to technical failure or participant departure."
  )

  # The condition comparison that bears on selective post-treatment attrition is
  # restricted to confirmed delivery: all outcome completers plus noncompleters
  # with a stored initial assistant response. Pre-callback participants could not
  # know whether they had been assigned to bunking or debunking.
  delivered <- pool[pool$completed | pool$initial_ai == 1L, , drop = FALSE]
  delivered$post_dropout <- as.integer(!delivered$completed)
  sfit_dir <- stats::glm(post_dropout ~ direction, data = delivered, family = stats::binomial())
  slr_dir  <- stats::anova(stats::glm(post_dropout ~ 1, data = delivered, family = stats::binomial()), sfit_dir, test = "LRT")
  sfit_mod <- stats::glm(post_dropout ~ model, data = delivered, family = stats::binomial())
  slr_mod  <- stats::anova(stats::glm(post_dropout ~ 1, data = delivered, family = stats::binomial()), sfit_mod, test = "LRT")
  ft_dir <- stats::fisher.test(table(delivered$direction, delivered$post_dropout))
  s4_delivery_tests <- tibble::tibble(
    model = NA_character_,
    term = c(
      "postdelivery_attrition_by_direction_LRT",
      "postdelivery_attrition_by_model_LRT",
      "postdelivery_attrition_by_direction_Fisher"
    ),
    n = nrow(delivered),
    statistic = c(slr_dir$Deviance[2], slr_mod$Deviance[2], unname(ft_dir$estimate)),
    df_num = c(slr_dir$Df[2], slr_mod$Df[2], NA_real_),
    p_value = c(slr_dir$`Pr(>Chi)`[2], slr_mod$`Pr(>Chi)`[2], ft_dir$p.value),
    note = "Outcome attrition among participants with confirmed initial AI delivery. Direction was not visible before delivery; Fisher's exact test is included as a small-cell robustness check."
  )

  s4_delivery_rates <- delivered |>
    dplyr::group_by(direction) |>
    dplyr::summarise(n = dplyr::n(), estimate = mean(post_dropout), .groups = "drop") |>
    dplyr::transmute(
      model = NA_character_, direction = as.character(direction),
      term = "postdelivery_attrition_rate", n, estimate,
      note = "Missing primary outcomes among participants with a stored initial assistant response, by randomized direction."
    )

  # Operational diagnostics for the Gemini endpoint-removal period.
  pool$.field_date <- as.Date(substr(as.character(pool$StartDate), 1L, 10L))
  .endpoint <- if ("modelName_raw" %in% names(pool)) as.character(pool$modelName_raw) else as.character(pool$modelName)
  .blackout <- pool$model == "Gemini" &
    .endpoint == "google/gemini-3-pro-preview" &
    pool$.field_date >= as.Date("2026-03-28") & pool$.field_date <= as.Date("2026-03-30")
  .gem31 <- .endpoint == "google/gemini-3.1-pro-preview"
  .other <- pool$model != "Gemini"
  .other_nc_rate <- mean(pool$loss_class[.other] == "no_successful_callback")
  .gem <- pool$model == "Gemini"
  .gem_excess <- sum(pool$loss_class[.gem] == "no_successful_callback") - sum(.gem) * .other_nc_rate
  .no_callback <- pool$loss_class == "no_successful_callback"
  s4_operational <- tibble::tibble(
    model = NA_character_,
    term = c(
      "gemini3_blackout_pool_n", "gemini3_blackout_callback_n",
      "gemini3_blackout_completed_n", "gemini31_pool_n",
      "gemini31_completed_n", "gemini31_completion_rate",
      "gemini_excess_no_callback_vs_others_n",
      "no_callback_instruction_page_submitted_n",
      "no_callback_progress70_n"
    ),
    n = nrow(pool),
    estimate = c(
      sum(.blackout), sum(.blackout & pool$initial_ai == 1L),
      sum(.blackout & pool$completed), sum(.gem31),
      sum(.gem31 & pool$completed), mean(pool$completed[.gem31]), .gem_excess,
      sum(.no_callback & nonempty(pool[["Q89_Page Submit"]])),
      sum(.no_callback & suppressWarnings(as.numeric(pool$Progress)) == 70, na.rm = TRUE)
    ),
    note = "Gemini operational audit. The requested google/gemini-3-pro-preview endpoint was removed from OpenRouter during fielding; the survey switched to google/gemini-3.1-pro-preview. Excess no-callback loss applies the non-Gemini no-callback rate to the Gemini pool."
  )

  out[["S4"]] <- dplyr::bind_rows(
    s4_rows, list(s4_total), list(s4_tests), list(s4_delivery_tests),
    list(s4_delivery_rates), list(s4_operational)
  ) |>
    std_row("S4 + pooled", "attrition_substantive_vs_technical", "strict_n1272")

  .burden_rows <- list(
    `Study 4` = tibble::tibble(
      model = "Study 4",
      term = c("completed_total_duration_median_min", "completed_total_duration_p90_min"),
      n = sum(pool$completed),
      estimate = c(
        stats::median(suppressWarnings(as.numeric(pool[["Duration (in seconds)"]][pool$completed])), na.rm = TRUE) / 60,
        stats::quantile(suppressWarnings(as.numeric(pool[["Duration (in seconds)"]][pool$completed])), .9, na.rm = TRUE, names = FALSE) / 60
      ),
      note = "Total Qualtrics duration among participants with complete primary outcomes; descriptive evidence about study burden, not a causal decomposition of attrition."
    )
  )

  # ---- S1-3 ----
  paths <- core_objects$paths
  study_map <- c("Study 1" = "Jailbroken", "Study 2" = "Standard", "Study 3" = "Truth-Constrained")
  s13_rows <- list()
  # S1-3 loss is classified by the SAME rule as S4, from the SAME continuously-saved
  # partial-chat log (__js_chatPartialData1) -- NOT an API/LLM call. The clean files
  # do not retain a usable log, so we read the raw Qualtrics exports (same dir the
  # screening funnel uses). The 25-75 window is symmetric about 50, so filtering on
  # the raw pre-belief selects the identical pool as the reverse-coded window.
  raw_dir <- core_objects$raw_s13_dir
  if (is.null(raw_dir)) raw_dir <- file.path(core_objects$pkg_root, "data", "raw_qualtrics")
  raw_files <- c("Study 1" = "study1_jailbroken_raw.csv.gz",
                 "Study 2" = "study2_standard_raw.csv.gz",
                 "Study 3" = "study3_truth_constrained_raw.csv.gz")
  for (st in names(study_map)) {
    paper <- readr::read_csv(paths$s1s3_clean[[st]],
                             show_col_types = FALSE, progress = FALSE)
    .field_end <- max(as.POSIXct(paper$RecordedDate, tz = "UTC"), na.rm = TRUE)
    .analytic_ids <- core_objects$s13 |>
      dplyr::filter(as.character(.data$study_factor) == study_map[[st]]) |>
      dplyr::pull(.data$response_id)
    raw <- readr::read_csv(file.path(raw_dir, raw_files[[st]]),
                           show_col_types = FALSE, progress = FALSE, name_repair = "unique")
    raw <- raw[-c(1, 2), ]   # drop the two Qualtrics metadata header rows
    d <- raw |>
      dplyr::mutate(
        .bp  = suppressWarnings(as.numeric(belief_rating_pre_4)),
        .bpo = suppressWarnings(as.numeric(belief_rating_post_4)),
        .recorded = suppressWarnings(as.POSIXct(RecordedDate, tz = "UTC")),
        completed = !is.na(.bpo)
      ) |>
      dplyr::filter(
        as.logical(isEquivocal) == TRUE, .bp > 25, .bp < 75,
        condition %in% c("treatment_mid_bunk", "treatment_mid_debunk"),
        is.na(.recorded) | .recorded <= .field_end
      )
    pmat <- matrix(0L, nrow = nrow(d), ncol = length(.state_names),
                   dimnames = list(NULL, .state_names))
    inc <- which(!d$completed)
    if (length(inc)) {
      pmat[inc, ] <- t(vapply(
        d[["__js_chatPartialData1"]][inc], parse_partial_chat_state,
        integer(length(.state_names))
      ))
    }
    d <- dplyr::bind_cols(d, as.data.frame(pmat))
    chat_saved <- if ("chathistory01" %in% names(d)) nonempty(d$chathistory01) else rep(FALSE, nrow(d))
    cl <- dplyr::case_when(
      d$completed ~ "completed",
      chat_saved | d$next_section == 1L ~ "chat_completed_outcomes_missing",
      d$initial_ai == 1L & d$chat_user == 1L ~ "interactive_chat_then_lost",
      d$initial_ai == 1L ~ "initial_ai_no_user",
      d$chat_user == 1L ~ "user_no_initial_ai",
      TRUE ~ "no_successful_callback"
    )
    d$loss_class <- cl
    vals <- .att_counts(d)
    s13_rows[[st]] <- tibble::tibble(
      model = study_map[[st]],
      term = .terms,
      n = nrow(d),
      estimate = vals,
      note = paste(
        "S1-3 pool = equivocal + 25-75 baseline-belief window + bunk/debunk arm.",
        "The raw pool is capped at the last recorded timestamp in the corresponding paper analysis export, excluding later survey resumptions.",
        "Loss classified from the raw partial-chat log using the same joined-chunk and hidden-seed-aware rule as S4.",
        "A no-successful-callback record cannot distinguish participant transition dropout from technical non-delivery at the row level.")
    ) |>
      std_row("S1-3", "attrition_substantive_vs_technical", "full_sample")

    .dur_keep <- d$completed & d$ResponseId %in% .analytic_ids
    .dur <- suppressWarnings(as.numeric(d[["Duration (in seconds)"]][.dur_keep]))
    .burden_rows[[st]] <- tibble::tibble(
      model = study_map[[st]],
      term = c("completed_total_duration_median_min", "completed_total_duration_p90_min"),
      n = sum(.dur_keep),
      estimate = c(
        stats::median(.dur, na.rm = TRUE) / 60,
        stats::quantile(.dur, .9, na.rm = TRUE, names = FALSE) / 60
      ),
      note = "Total Qualtrics duration among participants with complete primary outcomes; descriptive evidence about study burden, not a causal decomposition of attrition."
    )
  }
  out[["S13"]] <- dplyr::bind_rows(s13_rows)
  out[["burden"]] <- dplyr::bind_rows(.burden_rows) |>
    std_row("S4 + pooled", "attrition_burden_context", "observed_outcomes")

  dplyr::bind_rows(out)
}

# ============================================================================
# FAMILY 2: engagement, all four studies
# ============================================================================

compute_engagement <- function(core_objects) {
  si_require(c("dplyr", "tidyr", "stringr", "tibble", "jsonlite"))

  max_turn_s13 <- 10L   # 10-turn max for the S1-3 conversation
  out <- list()

  # ---- S1-3 from parsed content_* columns of the analytic frame ----
  s13 <- core_objects$s13
  ucols <- grep("^content_user_[0-9]+$", names(s13), value = TRUE)
  acols <- grep("^content_assistant_[0-9]+$", names(s13), value = TRUE)

  nonempty_chr <- function(x) !is.na(x) & nzchar(trimws(as.character(x)))
  s13 <- s13 |>
    dplyr::mutate(
      .n_user = rowSums(dplyr::across(dplyr::all_of(ucols), nonempty_chr)),
      .n_asst = rowSums(dplyr::across(dplyr::all_of(acols), nonempty_chr)),
      .w_user = rowSums(dplyr::across(dplyr::all_of(ucols), ~ .ext_word_count(.x))),
      .w_asst = rowSums(dplyr::across(dplyr::all_of(acols), ~ .ext_word_count(.x))),
      .ended_early = .n_user < max_turn_s13
    )

  s13_eng <- s13 |>
    dplyr::group_by(model = as.character(study_factor), direction = as.character(condition_factor)) |>
    dplyr::group_modify(function(g, key) {
      metrics <- list(
        mean_user_messages = g$.n_user,
        mean_assistant_messages = g$.n_asst,
        mean_user_words = g$.w_user,
        mean_assistant_words = g$.w_asst
      )
      rows <- lapply(names(metrics), function(mn) {
        ci <- .ext_mean_ci(metrics[[mn]])
        tibble::tibble(term = mn, n = ci$n, estimate = ci$estimate, se = ci$se,
                       conf_low = ci$conf_low, conf_high = ci$conf_high)
      })
      early <- mean(g$.ended_early)
      rows[[length(rows) + 1]] <- tibble::tibble(
        term = "pct_ended_before_10turn_max", n = nrow(g), estimate = early
      )
      dplyr::bind_rows(rows)
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(note = "S1-3 engagement from parsed content_user_*/content_assistant_* of analytic frame; words = whitespace tokens; 10-turn max") |>
    std_row("S1-3", "engagement", "full_sample")
  out[["S13"]] <- s13_eng

  # ---- S4 from JSON transcripts ----
  s4 <- core_objects$s4$s4
  chat_cols <- paste0("chathistory0", 1:5)
  chat_cols <- intersect(chat_cols, names(s4))
  max_turn_s4 <- 10L

  per_conv <- vector("list", nrow(s4))
  n_parsed <- 0L
  for (i in seq_len(nrow(s4))) {
    chunks <- as.character(unlist(s4[i, chat_cols]))
    msgs <- .ext_parse_s4_transcript(chunks)
    if (is.null(msgs)) {
      per_conv[[i]] <- NULL
      next
    }
    roles <- vapply(msgs, function(m) if (!is.null(m$role)) m$role else NA_character_, character(1))
    contents <- vapply(msgs, function(m) if (!is.null(m$content)) as.character(m$content) else "", character(1))
    n_user <- sum(roles == "user", na.rm = TRUE)
    n_asst <- sum(roles == "assistant", na.rm = TRUE)
    w_user <- sum(.ext_word_count(contents[roles == "user"]))
    w_asst <- sum(.ext_word_count(contents[roles == "assistant"]))
    n_parsed <- n_parsed + 1L
    per_conv[[i]] <- tibble::tibble(
      model = model_pooled_label(s4$modelName[i]),
      direction = as.character(s4$direction[i]),
      n_user = n_user, n_asst = n_asst, w_user = w_user, w_asst = w_asst,
      ended_early = n_user < max_turn_s4
    )
  }
  parsed_df <- dplyr::bind_rows(per_conv)
  parse_rate <- n_parsed / nrow(s4)
  fallback_note <- if (parse_rate >= 0.9) {
    sprintf("S4 engagement from chatlogr-style reconstructed JSON transcripts (chathistory0*); transcript parse success=%.1f%% of strict N; words = whitespace tokens; 10-turn max", 100 * parse_rate)
  } else {
    sprintf("FALLBACK: chatlogr-style JSON parse succeeded for only %.1f%% of strict-N transcripts; engagement computed on parsed subset only", 100 * parse_rate)
  }

  s4_eng <- parsed_df |>
    dplyr::group_by(model, direction) |>
    dplyr::group_modify(function(g, key) {
      metrics <- list(
        mean_user_messages = g$n_user,
        mean_assistant_messages = g$n_asst,
        mean_user_words = g$w_user,
        mean_assistant_words = g$w_asst
      )
      rows <- lapply(names(metrics), function(mn) {
        ci <- .ext_mean_ci(metrics[[mn]])
        tibble::tibble(term = mn, n = ci$n, estimate = ci$estimate, se = ci$se,
                       conf_low = ci$conf_low, conf_high = ci$conf_high)
      })
      rows[[length(rows) + 1]] <- tibble::tibble(
        term = "pct_ended_before_10turn_max", n = nrow(g), estimate = mean(g$ended_early)
      )
      dplyr::bind_rows(rows)
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(note = fallback_note) |>
    std_row("S4 + pooled", "engagement", "strict_n1272")
  out[["S4"]] <- s4_eng

  dplyr::bind_rows(out)
}

# ============================================================================
# FAMILY 3: attention / bot items, by study
# ============================================================================

# Pass rates for the three trap items. Computed on the RAW S1-3 clean files +
# S4 raw. Note which studies administered which item.
compute_attention_checks <- function(core_objects) {
  si_require(c("dplyr", "readr", "stringr", "tibble"))
  out <- list()

  spice_re <- stringr::regex(
    "cinnamon|cumin|paprika|pepper|garlic|nutmeg|oregano|basil|thyme|cardamom|turmeric|saffron|ginger|clove|spice|cilantro|parsley|rosemary|chil(l)?i|cayenne|coriander|sage|dill|smoked paprika|smoked",
    ignore_case = TRUE
  )
  # "if you are an LLM, disclose it" -- a PASS of the trap is the human FAILING to
  # disclose (i.e., behaving like a human). We report the LLM self-disclosure RATE
  # (share whose free text admits being an AI/LLM), which is the diagnostic the
  # item was designed to surface; near-zero rates indicate human respondents.
  llm_disclose_re <- stringr::regex(
    "i am an? (llm|ai|language model|artificial)|i'?m an? (llm|ai|language model)|as an? (ai|llm|language model)|i am chatgpt|i am gpt|created by openai|am indeed an? (ai|llm|language model)|am an? (ai|llm) (language )?model",
    ignore_case = TRUE
  )

  paths <- core_objects$paths
  study_map <- c("Study 1" = "Jailbroken", "Study 2" = "Standard", "Study 3" = "Truth-Constrained")

  for (st in names(study_map)) {
    d <- readr::read_csv(paths$s1s3_clean[[st]], show_col_types = FALSE, progress = FALSE)
    n0 <- nrow(d)

    # nicks instructed-response item (administered every study). In the clean
    # files the screen is already applied, so report observed pass-rate (== 1)
    # with that caveat.
    nicks_num <- suppressWarnings(as.numeric(d$nicks))
    nicks_pass <- mean(nicks_num == 5, na.rm = TRUE)
    n_nicks <- sum(!is.na(nicks_num))

    # spice trap lives in the free-text `Gender` field; present only S2/S3
    gender_txt <- if ("Gender" %in% names(d)) d$Gender else rep(NA_character_, n0)
    spice_hits <- sum(stringr::str_detect(dplyr::coalesce(gender_txt, ""), spice_re), na.rm = TRUE)
    spice_present <- spice_hits > 0 || st %in% c("Study 2", "Study 3")
    spice_caught_rate <- if (spice_present) spice_hits / n0 else NA_real_

    # LLM self-disclosure item ("LLM agent" free text)
    llm_txt <- if ("LLM agent" %in% names(d)) d[["LLM agent"]] else rep(NA_character_, n0)
    llm_answered <- sum(nonempty_or_na <- !is.na(llm_txt) & nzchar(trimws(llm_txt)))
    llm_disclose <- sum(stringr::str_detect(dplyr::coalesce(llm_txt, ""), llm_disclose_re), na.rm = TRUE)
    llm_disclose_rate <- if (llm_answered > 0) llm_disclose / llm_answered else NA_real_

    rows <- tibble::tibble(
      term = c("nicks_instructed_response_pass_rate",
               "spice_trap_bot_response_rate",
               "llm_self_disclosure_rate"),
      n = c(n_nicks, n0, llm_answered),
      estimate = c(nicks_pass, spice_caught_rate, llm_disclose_rate),
      statistic = c(NA_real_, spice_hits, llm_disclose),
      note = c(
        "instructed-response 'nicks'==5 item; administered all studies; clean file already screened so observed pass-rate is ~1 within this file",
        if (spice_present) "gender 'favorite spice' trap (free-text Gender); administered S2/S3 only; estimate = share giving a spice answer (bot-like)" else "spice trap NOT administered in this study (S1); estimate=NA",
        "'if you are an LLM' self-disclosure item ('LLM agent' free text); administered S1-S4; estimate = share of answered who disclosed being an AI/LLM"
      )
    ) |>
      std_row("S1-3", "attention_checks", "full_sample") |>
      dplyr::mutate(model = study_map[[st]])
    out[[st]] <- rows
  }

  # ---- S4 ----
  s4raw <- core_objects$s4$s4_raw
  n_attn <- sum(!is.na(s4raw$attention_pass))
  nicks_pass_s4 <- mean(s4raw$attention_pass, na.rm = TRUE)
  n_berry <- sum(!is.na(s4raw$berry_pass))
  berry_pass_s4 <- mean(s4raw$berry_pass, na.rm = TRUE)
  llm_txt4 <- if ("LLM agent" %in% names(s4raw)) s4raw[["LLM agent"]] else rep(NA_character_, nrow(s4raw))
  llm_answered4 <- sum(!is.na(llm_txt4) & nzchar(trimws(llm_txt4)))
  llm_disclose4 <- sum(stringr::str_detect(dplyr::coalesce(llm_txt4, ""), llm_disclose_re), na.rm = TRUE)
  # spice trap not used in S4; berry/strawberry trap used instead
  s4_rows <- tibble::tibble(
    term = c("nicks_instructed_response_pass_rate",
             "berry_strawberry_trap_pass_rate",
             "llm_self_disclosure_rate"),
    n = c(n_attn, n_berry, llm_answered4),
    estimate = c(nicks_pass_s4, berry_pass_s4, if (llm_answered4 > 0) llm_disclose4 / llm_answered4 else NA_real_),
    statistic = c(NA_real_, NA_real_, llm_disclose4),
    note = c(
      "instructed-response 'nicks'==5 item; administered all studies; pass-rate over all raw S4 rows where item present",
      "S4 used the berry/strawberry trap in place of the S2/S3 gender spice trap; estimate = pass-rate (gave a berry answer)",
      "'if you are an LLM' self-disclosure item; administered S1-S4; estimate = share of answered who disclosed being an AI/LLM"
    )
  ) |>
    std_row("S4 + pooled", "attention_checks", "strict_n1272") |>
    dplyr::mutate(model = NA_character_)
  out[["S4"]] <- s4_rows

  dplyr::bind_rows(out)
}

# ============================================================================
# Top-level convenience: bind every family into one canonical table
# ============================================================================

compute_ext_attrition_numbers <- function(core_objects) {
  dplyr::bind_rows(
    compute_screening_funnel_s13(core_objects),
    compute_dropout_logistic_s13(core_objects),
    compute_attrition_substantive_vs_technical(core_objects),
    compute_engagement(core_objects),
    compute_attention_checks(core_objects)
  )
}
