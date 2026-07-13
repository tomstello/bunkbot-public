# ext_sm_pretreatment.R
# =============================================================================
# Study-4 PRE-TREATMENT social-media descriptives. Before any AI dialogue,
# Study-4 participants answered three self-report items about their social-media
# behaviour (verbatim Qualtrics wording carried in `note`):
#
#   sm_platform     "If you were most likely to post about a controversial topic
#                    online, which platform would you be most likely to use?"
#                    -- MULTI-SELECT (comma-separated atomic codes 1..13; ~36%
#                    of respondents selected more than one).
#   sm_account      "When you post on social media, which best describes the
#                    audience for the account you use most often for posting?"
#                    -- single-select (codes 1..4).
#   sm_posting_freq "How often do you post content on social media?"
#                    -- ordinal posting-frequency Likert (1..7).
#
# ENTRY POINT:  compute_sm_pretreatment(core_objects) -> tibble in the canonical
#   17-column schema (section, block, sample, outcome, model, direction, term, n,
#   estimate, se, conf_low, conf_high, statistic, df_num, df_den, p_value, note).
#
# BLOCK:    "sm_pretreatment"   (section "S4 + pooled")
#
# These are pre-treatment descriptives pooled across the whole Study-4 sample, so
# model = NA and direction = NA on every row. SAMPLE = "full_sample_n1272"
# (core_objects$s4$s4, n=1272). Each item's denominator is the number of
# participants who answered that item (NA responses dropped per item).
#
# Rows emitted, all term = item/category, estimate = share (proportion):
#   * outcome "sm_platform"     : one row per atomic platform code -> proportion
#       of answering participants who SELECTED that platform (multi-select, so
#       shares sum to > 1). n = answered the item; conf_low/high = Wilson 95% CI.
#   * outcome "sm_account"      : one row per audience code -> share of answering
#       participants choosing it (single-select; shares sum to ~1). Wilson CI.
#   * outcome "sm_posting_freq" : one row per frequency code 1..7 -> share of
#       answering participants in that category (shares sum to ~1). Wilson CI.
#
# NOTE ON LABELS: these three items were added in Qualtrics after the archived
# .qsf was exported, so the data CSV carries only numeric choice codes. `term`
# is the human-readable label from the author-supplied label maps below (e.g.
# sm_platform 11 = "Other", 13 = "Truth Social"); the verbatim question text is
# carried in `note`.
#
# Requires R/tables_dynamic.R (std_cols / std_row) and bunkbot_helpers.R sourced.
# All inputs come from core_objects (built from raw + cached); no CSV reads.
# =============================================================================

compute_sm_pretreatment <- function(core_objects) {
  si_require <- get0("si_require", ifnotfound = function(pkgs) {
    for (p in pkgs) suppressWarnings(suppressMessages(requireNamespace(p, quietly = TRUE)))
  })
  si_require(c("dplyr", "tibble"))

  # The SM pre-treatment items live in the raw Qualtrics export, not the cleaned
  # analytic frame; restrict the raw rows to the strict analytic ResponseIds.
  s4 <- core_objects$s4$s4_raw
  s4 <- s4[s4$ResponseId %in% core_objects$s4$s4$ResponseId, , drop = FALSE]

  # Deployed Qualtrics choice labels (author-provided).
  .lab_platform <- c("1"="X / Twitter", "2"="Facebook", "3"="Instagram", "4"="TikTok",
                     "5"="Reddit", "6"="YouTube", "7"="Threads", "8"="Snapchat",
                     "9"="Discord", "10"="LinkedIn", "11"="Other", "12"="Bluesky",
                     "13"="Truth Social")
  .lab_account <- c("1"="Mostly private", "2"="Mixed private/public",
                    "3"="Mostly public", "4"="I do not usually post")
  .lab_freq <- c("1"="Never", "2"="Less than once a month", "3"="1-3 times a month",
                 "4"="1-2 times a week", "5"="3-6 times a week", "6"="About once a day",
                 "7"="Multiple times a day")
  .lab <- function(code, map) { l <- map[[as.character(code)]]; if (is.null(l)) as.character(code) else l }

  # Wilson (score) 95% CI for a proportion k/n; matches prop.test default.
  .wilson <- function(k, n) {
    if (is.na(n) || n <= 0) return(c(NA_real_, NA_real_))
    ci <- suppressWarnings(stats::prop.test(k, n, correct = FALSE)$conf.int)
    c(ci[1], ci[2])
  }

  rows <- list()

  # --- sm_platform : MULTI-SELECT --------------------------------------------
  q_platform <- paste(
    "If you were most likely to post about a controversial topic online,",
    "which platform would you be most likely to use?"
  )
  if ("sm_platform" %in% names(s4)) {
    v <- s4$sm_platform
    v <- v[!is.na(v) & nzchar(trimws(v))]
    n_ans <- length(v)
    # atomic codes, ordered numerically
    atoms <- strsplit(v, ",")
    all_codes <- sort(unique(suppressWarnings(as.integer(trimws(unlist(atoms))))))
    all_codes <- all_codes[!is.na(all_codes)]
    for (code in all_codes) {
      sel <- vapply(atoms, function(z) code %in% suppressWarnings(as.integer(trimws(z))), logical(1))
      k <- sum(sel)
      ci <- .wilson(k, n_ans)
      rows[[length(rows) + 1]] <- tibble::tibble(
        outcome = "sm_platform", model = NA_character_, direction = NA_character_,
        term = .lab(code, .lab_platform), n = n_ans, estimate = k / n_ans,
        conf_low = ci[1], conf_high = ci[2],
        note = sprintf(
          "%s | multi-select platform '%s' (code %s) selected by %d of %d answering participants (shares sum to >1)",
          q_platform, .lab(code, .lab_platform), code, k, n_ans
        )
      )
    }
  }

  # --- sm_account : single-select audience ------------------------------------
  q_account <- paste(
    "When you post on social media, which best describes the audience for the",
    "account you use most often for posting?"
  )
  if ("sm_account" %in% names(s4)) {
    v <- s4$sm_account
    v <- v[!is.na(v) & nzchar(trimws(v))]
    n_ans <- length(v)
    codes <- sort(unique(suppressWarnings(as.integer(trimws(v)))))
    codes <- codes[!is.na(codes)]
    for (code in codes) {
      k <- sum(suppressWarnings(as.integer(trimws(v))) == code)
      ci <- .wilson(k, n_ans)
      rows[[length(rows) + 1]] <- tibble::tibble(
        outcome = "sm_account", model = NA_character_, direction = NA_character_,
        term = .lab(code, .lab_account), n = n_ans, estimate = k / n_ans,
        conf_low = ci[1], conf_high = ci[2],
        note = sprintf(
          "%s | audience '%s' (code %s) chosen by %d of %d answering participants",
          q_account, .lab(code, .lab_account), code, k, n_ans
        )
      )
    }
  }

  # --- sm_posting_freq : ordinal posting-frequency Likert ---------------------
  q_freq <- "How often do you post content on social media?"
  if ("sm_posting_freq" %in% names(s4)) {
    v <- s4$sm_posting_freq
    v <- v[!is.na(v)]
    n_ans <- length(v)
    codes <- sort(unique(suppressWarnings(as.integer(v))))
    codes <- codes[!is.na(codes)]
    for (code in codes) {
      k <- sum(suppressWarnings(as.integer(v)) == code)
      ci <- .wilson(k, n_ans)
      rows[[length(rows) + 1]] <- tibble::tibble(
        outcome = "sm_posting_freq", model = NA_character_, direction = NA_character_,
        term = .lab(code, .lab_freq), n = n_ans, estimate = k / n_ans,
        conf_low = ci[1], conf_high = ci[2],
        note = sprintf(
          "%s | posting frequency '%s' (code %s): %d of %d answering participants",
          q_freq, .lab(code, .lab_freq), code, k, n_ans
        )
      )
    }
  }

  # --- AI-study experience: howmanystudiesclear / howmanystudiestotal ----------
  # Self-reported count of prior studies involving an AI/LLM. Report the
  # distribution (n, mean, quartiles, max) "to see the range".
  exp_rows <- list()
  exp_items <- c(
    howmanystudiesclear = "How many studies have you completed that clearly say you're interacting with an AI (LLM like ChatGPT)?",
    howmanystudiestotal = "How many studies have you completed that you think likely had an AI/LLM as part of the experience (even if AI was not mentioned)?"
  )
  for (it in names(exp_items)) {
    if (!it %in% names(s4)) next
    x <- suppressWarnings(as.numeric(as.character(s4[[it]])))
    x <- x[is.finite(x) & x >= 0]
    if (!length(x)) next
    qs <- stats::quantile(x, c(.25, .5, .75), na.rm = TRUE)
    stat <- c(n_answered = length(x), mean = mean(x), p25 = qs[[1]], median = qs[[2]],
              p75 = qs[[3]], max = max(x), min = min(x))
    exp_rows[[length(exp_rows) + 1]] <- tibble::tibble(
      outcome = it, model = NA_character_, direction = NA_character_,
      term = names(stat), n = length(x), estimate = as.numeric(stat),
      note = sprintf("%s | self-reported count of prior AI/LLM studies (pre-treatment).", exp_items[[it]])
    )
  }

  if (length(rows) == 0L) {
    # No social-media pre-treatment items found: return empty canonical tibble.
    return(std_row(
      tibble::tibble(term = character(0)),
      "S4 + pooled", "sm_pretreatment", "full_sample_n1272"
    ))
  }

  res <- std_row(
    dplyr::bind_rows(rows),
    "S4 + pooled", "sm_pretreatment", "full_sample_n1272"
  )
  if (length(exp_rows)) {
    res <- dplyr::bind_rows(res, std_row(
      dplyr::bind_rows(exp_rows),
      "S4 + pooled", "ai_study_experience", "full_sample_n1272"))
  }

  char_cols <- c("section", "block", "sample", "outcome", "model", "direction", "term", "note")
  for (cc in char_cols) if (cc %in% names(res)) res[[cc]] <- as.character(res[[cc]])
  res[, std_cols]
}
