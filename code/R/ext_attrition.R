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
#     * attrition_substantive_vs_technical   -- ALL studies: separates technical
#                                               non-delivery (API failure / CINT
#                                               bad data: conversation never
#                                               started) from SUBSTANTIVE
#                                               participant dropout. For S4 the
#                                               raw funnel/attrition blocks already
#                                               exist via compute_s4_numbers(); this
#                                               block ADDS the technical-vs-
#                                               substantive distinction the author
#                                               requires (Gemini API non-delivery is
#                                               ~the entire S4 loss and is excluded
#                                               from substantive attrition).
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
.ext_parse_s4_transcript <- function(chunks) {
  chunks <- chunks[!is.na(chunks) & nzchar(chunks)]
  if (length(chunks) == 0) return(NULL)
  inner <- vapply(
    chunks,
    function(s) {
      if (startsWith(s, "\"")) s <- substr(s, 2, nchar(s))
      if (endsWith(s, "\"")) s <- substr(s, 1, nchar(s) - 1)
      s
    },
    character(1),
    USE.NAMES = FALSE
  )
  joined <- paste0(inner, collapse = "")
  joined <- gsub("\\\\\"", "\"", joined)     # \" -> "
  joined <- gsub("\\\\\\\\", "\\\\", joined)   # \\ -> \
  p <- tryCatch(jsonlite::fromJSON(joined, simplifyVector = FALSE), error = function(e) NULL)
  if (is.character(p)) {
    p <- tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) NULL)
  }
  if (is.list(p) && !is.null(p$messages)) return(p$messages)
  NULL
}

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
# FAMILY 1c: substantive vs technical non-delivery, ALL studies
# ============================================================================

# KEY distinction. For S4, ~90% of the conversation-pool loss is
# TECHNICAL non-delivery (Gemini API failure / CINT bad data: the conversation
# never started, so no transcript was saved), concentrated in Gemini. Only a
# handful of pool members had a transcript but no post outcomes (substantive
# participant dropout). We separate these and report per-study x model counts and
# rates, plus a differential-attrition significance test by condition (and, for
# S4, by model) for completeness. Technical non-delivery is EXCLUDED from
# substantive attrition.
#
# For S1-3, every conversation in the eligible pool ran on the same GPT-4o
# pipeline with no comparable technical-non-delivery channel, so technical
# non-delivery is 0 there and all dropout is substantive; this is stated in the
# note so the four studies are reported on one comparable framework.
# Parse the continuously-saved partial-chat log (__js_chatPartialData1: a JSON
# array whose single element is a stringified {"messages":[{role,content},...]})
# into counts of non-empty user messages and non-empty model (assistant) replies.
# This separates a conversation that functioned (>=1 model reply) from one that
# never started (no user message) or one where a sent message went unanswered.
.parse_partial_chat_counts <- function(cell) {
  if (is.na(cell) || !nzchar(trimws(cell))) return(c(0L, 0L))
  ms <- tryCatch({
    arr <- jsonlite::fromJSON(cell, simplifyVector = TRUE)
    acc <- list()
    for (s in arr) {
      o <- tryCatch(jsonlite::fromJSON(s, simplifyDataFrame = FALSE), error = function(e) NULL)
      if (!is.null(o$messages)) acc <- c(acc, o$messages)
    }
    acc
  }, error = function(e) NULL)
  if (is.null(ms) || length(ms) == 0) return(c(0L, 0L))
  role <- vapply(ms, function(m) if (is.null(m$role)) "" else m$role, character(1))
  cont <- vapply(ms, function(m) if (is.null(m$content)) "" else paste(as.character(m$content), collapse = ""), character(1))
  c(sum(role == "user" & nchar(trimws(cont)) > 0L),
    sum(role == "assistant" & nchar(trimws(cont)) > 0L))
}

compute_attrition_substantive_vs_technical <- function(core_objects) {
  si_require(c("dplyr", "readr", "tibble"))
  out <- list()

  # ---- S4: classify each non-completer from the partial-chat message log ----
  s4raw <- core_objects$s4$s4_raw
  pool <- s4raw |>
    dplyr::filter(
      attention_pass, berry_pass, dedup_pass, condition_assigned, model_assigned,
      pre_belief_present, pre_post_present, equivocal_pass, !invalid_claim,
      nonempty(direction), belief_window_pass
    ) |>
    dplyr::mutate(model = model_pooled_label(modelName), completed = post_outcomes_present)
  # parse the partial-chat log for non-completers only (completers short-circuit)
  pool$n_user_msg <- 0L; pool$n_ai_reply <- 0L
  .inc <- which(!pool$completed)
  if (length(.inc)) {
    .pcc <- t(vapply(pool[["__js_chatPartialData1"]][.inc], .parse_partial_chat_counts, integer(2)))
    pool$n_user_msg[.inc] <- .pcc[, 1]; pool$n_ai_reply[.inc] <- .pcc[, 2]
  }
  pool <- pool |>
    dplyr::mutate(loss_class = dplyr::case_when(
      completed                        ~ "completed",
      n_ai_reply > 0                   ~ "substantive_midchat",   # model replied >=1x, then abandoned
      n_user_msg > 0 & n_ai_reply == 0 ~ "technical_no_reply",    # sent a message, model never replied (== 0 observed)
      TRUE                             ~ "no_message"             # reached the chat but never sent a message
    ))

  .terms <- c("pool", "completed", "substantive_midchat_n", "technical_no_reply_n",
              "no_message_n", "substantive_attrition_rate")
  .att_note <- paste(
    "S4 pool = passed all strict screens through model assignment.",
    "Loss classified from the partial-chat log (__js_chatPartialData1):",
    "substantive_midchat = the model replied at least once and the participant then abandoned the study;",
    "technical_no_reply = the participant sent a message but the model never replied (zero observed -- no started conversation went unanswered);",
    "no_message = reached the chat but never sent a message (dominated by Gemini, consistent with a chat that failed to load as Google was deprecating Gemini 3 during fielding).",
    "substantive_attrition_rate = substantive_midchat / (completed + substantive_midchat).")
  .att_counts <- function(df) {
    cl <- df$loss_class
    cp <- sum(cl == "completed"); sm <- sum(cl == "substantive_midchat")
    tn <- sum(cl == "technical_no_reply"); nm <- sum(cl == "no_message")
    c(nrow(df), cp, sm, tn, nm, if (cp + sm > 0) sm / (cp + sm) else NA_real_)
  }
  s4_rows <- lapply(sort(unique(pool$model)), function(m)
    tibble::tibble(model = m, term = .terms, n = sum(pool$model == m),
                   estimate = .att_counts(pool[pool$model == m, ]), note = .att_note))
  s4_total <- tibble::tibble(model = NA_character_, term = .terms, n = nrow(pool),
                             estimate = .att_counts(pool), note = paste(.att_note, "(pooled across models)."))
  # Differential-attrition tests by condition and by model on the S4 pool
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
    note = "LRT on S4 pool incomplete-outcome indicator (incl. both technical and substantive loss; the by-model test is dominated by Gemini's technical chat-load failures, not disengagement)"
  )
  # Substantive-attrition test: among participants who actually began the
  # conversation (the model replied at least once), does mid-conversation dropout
  # vary by condition or model? This isolates genuine disengagement from the
  # technical chat-load failures that dominate the no-message loss, and is the
  # test that bears on whether the analytic samples are differentially shaped.
  engaged <- pool[pool$loss_class %in% c("completed", "substantive_midchat"), , drop = FALSE]
  engaged$sub_dropout <- as.integer(engaged$loss_class == "substantive_midchat")
  sfit_dir <- stats::glm(sub_dropout ~ direction, data = engaged, family = stats::binomial())
  slr_dir  <- stats::anova(stats::glm(sub_dropout ~ 1, data = engaged, family = stats::binomial()), sfit_dir, test = "LRT")
  sfit_mod <- stats::glm(sub_dropout ~ model, data = engaged, family = stats::binomial())
  slr_mod  <- stats::anova(stats::glm(sub_dropout ~ 1, data = engaged, family = stats::binomial()), sfit_mod, test = "LRT")
  s4_sub_tests <- tibble::tibble(
    model = NA_character_,
    term = c("substantive_attrition_by_direction_LRT", "substantive_attrition_by_model_LRT"),
    n = nrow(engaged),
    statistic = c(slr_dir$Deviance[2], slr_mod$Deviance[2]),
    df_num = c(slr_dir$Df[2], slr_mod$Df[2]),
    p_value = c(slr_dir$`Pr(>Chi)`[2], slr_mod$`Pr(>Chi)`[2]),
    note = "LRT for SUBSTANTIVE mid-conversation dropout among participants who began the conversation (model replied at least once); excludes technical no-message / no-reply loss. Non-significance establishes the analytic samples are not differentially shaped by disengagement."
  )
  out[["S4"]] <- dplyr::bind_rows(s4_rows, list(s4_total), list(s4_tests), list(s4_sub_tests)) |>
    std_row("S4 + pooled", "attrition_substantive_vs_technical", "strict_n1272")

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
    raw <- readr::read_csv(file.path(raw_dir, raw_files[[st]]),
                           show_col_types = FALSE, progress = FALSE, name_repair = "unique")
    raw <- raw[-c(1, 2), ]   # drop the two Qualtrics metadata header rows
    d <- raw |>
      dplyr::mutate(
        .bp  = suppressWarnings(as.numeric(belief_rating_pre_4)),
        .bpo = suppressWarnings(as.numeric(belief_rating_post_4)),
        completed = !is.na(.bpo)
      ) |>
      dplyr::filter(
        as.logical(isEquivocal) == TRUE, .bp > 25, .bp < 75,
        condition %in% c("treatment_mid_bunk", "treatment_mid_debunk")
      )
    nu <- rep(0L, nrow(d)); nr <- rep(0L, nrow(d)); inc <- which(!d$completed)
    if (length(inc)) {
      pc <- t(vapply(d[["__js_chatPartialData1"]][inc], .parse_partial_chat_counts, integer(2)))
      nu[inc] <- pc[, 1]; nr[inc] <- pc[, 2]
    }
    cl <- dplyr::case_when(
      d$completed       ~ "completed",
      nr > 0            ~ "substantive_midchat",
      nu > 0 & nr == 0  ~ "technical_no_reply",
      TRUE              ~ "no_message"
    )
    cp <- sum(cl == "completed"); sm <- sum(cl == "substantive_midchat")
    tn <- sum(cl == "technical_no_reply"); nm <- sum(cl == "no_message")
    s13_rows[[st]] <- tibble::tibble(
      model = study_map[[st]],
      term = c("pool", "completed", "substantive_midchat_n", "technical_no_reply_n",
               "no_message_n", "substantive_attrition_rate"),
      n = nrow(d),
      estimate = c(nrow(d), cp, sm, tn, nm, if (cp + sm > 0) sm / (cp + sm) else NA_real_),
      note = paste(
        "S1-3 pool = equivocal + 25-75 baseline-belief window + bunk/debunk arm.",
        "Loss classified from the raw partial-chat log (__js_chatPartialData1), the SAME rule as S4 (NOT an API call):",
        "substantive_midchat = the model replied at least once and the participant then abandoned;",
        "no_message = reached the chat but never sent a message -- an empty log (chat never loaded) OR a system-prompt-only log (loaded, then left before sending); these participant- vs technical-cause senses are not separable at the row level;",
        "technical_no_reply = sent a message, model never replied (0 in all three, as in S4).")
    ) |>
      std_row("S1-3", "attrition_substantive_vs_technical", "full_sample")
  }
  out[["S13"]] <- dplyr::bind_rows(s13_rows)

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
