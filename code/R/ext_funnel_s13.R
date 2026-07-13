# ext_funnel_s13.R
# Full Studies 1-3 screening / attrition funnel, reconstructed from the RAW
# entrant-level Qualtrics exports (the cleaned per-study files begin only at the
# equivocality classifier, so the upstream attention/bot screens are invisible to
# them). Survey-flow gates (identical across studies; from the .qsf SurveyFlow):
#   QID55 `response1` "respond -1" screener  -> EndSurvey if Contains "0"
#   QID115 `nicks` instructed-response       -> EndSurvey if "Somewhat disagree" (choice 8, recodes to 5) not selected
#   isEquivocal == INVALID                   -> EndSurvey (after one correction)
# Order: consent -> response1 -> nicks (Demographics) -> condition -> equivocality
#   classifier -> 25-75 baseline window -> conversation -> analytic sample.
# Upstream exclusions are computed DISJOINT from the reached-classifier set so the
# funnel reconciles exactly to the clean-file row counts and the analytic Ns.

RAW_S13_DIR_DEFAULT <- "data/raw_qualtrics"
.RAW_S13_FILES <- c(
  "study1_jailbroken_raw.csv.gz",
  "study2_standard_raw.csv.gz",
  "study3_truth_constrained_raw.csv.gz"
)
.S13_LABELS <- c("Jailbroken", "Standard", "Truth-Constrained")  # match the model labels used by every other S1-3 block

.funnel_cols <- c("section","block","sample","outcome","model","direction","term","n",
                  "estimate","se","conf_low","conf_high","statistic","df_num","df_den","p_value","note")
.funnel_row <- function(block, model, term, n, stage, note, section = "S1-3") {
  r <- list(section = section, block = block, sample = "raw_entrants", outcome = NA, model = model,
            direction = NA, term = term, n = n, estimate = n, se = NA, conf_low = NA, conf_high = NA,
            statistic = stage, df_num = NA, df_den = NA, p_value = NA, note = note)
  tibble::as_tibble(r)[.funnel_cols]
}

compute_screening_funnel_s13_full <- function(core_objects, raw_dir = RAW_S13_DIR_DEFAULT) {
  out <- list()
  for (i in 1:3) {
    f <- file.path(raw_dir, .RAW_S13_FILES[i]); lab <- .S13_LABELS[i]
    d <- suppressWarnings(readr::read_csv(f, show_col_types = FALSE, progress = FALSE, name_repair = "unique"))
    d <- d[-c(1, 2), ]  # drop the two Qualtrics metadata header rows

    real    <- d[(is.na(d$DistributionChannel) | d$DistributionChannel != "preview") &
                 (is.na(d$Status) | d$Status == "0"), ]
    n_raw   <- nrow(d); n_real <- nrow(real)
    reached <- !is.na(real$isEquivocal)              # reached the equivocality classifier (anchor)
    nr      <- real[!reached, ]                      # did NOT reach it: decompose upstream losses (disjoint)
    r1      <- as.character(nr$response1)
    f_resp1 <- !is.na(r1) & grepl("0", r1)           # QID55 Contains "0"
    nr2     <- nr[!f_resp1, ]
    nk      <- suppressWarnings(as.numeric(nr2$nicks))
    f_nicks <- !is.na(nk) & nk != 5                  # QID115 not "Somewhat disagree"
    n_resp1_fail <- sum(f_resp1); n_nicks_fail <- sum(f_nicks)
    n_abandon    <- nrow(nr) - n_resp1_fail - n_nicks_fail   # started but quit before the classifier
    n_reached    <- sum(reached)

    eq        <- real$isEquivocal[reached]
    n_invalid <- sum(eq == "INVALID", na.rm = TRUE)
    n_false   <- sum(eq == "FALSE",   na.rm = TRUE)
    n_true    <- sum(eq == "TRUE",    na.rm = TRUE)

    # window + analytic (anchor the terminal stage to the engine's analytic sample)
    rid_analytic <- core_objects$s13$response_id[core_objects$s13$study_factor ==
                       c("Jailbroken","Standard","Truth-Constrained")[i]]
    pre <- suppressWarnings(as.numeric(real$belief_rating_pre_4))
    in_window_true <- real$isEquivocal == "TRUE" & !is.na(pre) & pre > 25 & pre < 75
    n_window   <- sum(in_window_true, na.rm = TRUE)
    n_analytic <- length(rid_analytic)   # the engine's analytic sample (1,092/814/818) anchors the terminal stage

    rows <- dplyr::bind_rows(
      .funnel_row("screening_funnel_s13", lab, "raw_responses",            n_raw,    1, "All recorded survey responses (raw Qualtrics export)."),
      .funnel_row("screening_funnel_s13", lab, "real_non_preview",         n_real,   2, sprintf("Excluded %d survey-preview/test rows.", n_raw - n_real)),
      .funnel_row("screening_funnel_s13", lab, "passed_respond_minus1",    n_real - n_resp1_fail, 3, sprintf("Excluded %d who failed the 'respond -1' chatbot screener (QID55 contains '0'; EndSurvey).", n_resp1_fail)),
      .funnel_row("screening_funnel_s13", lab, "passed_attention_nicks",   n_real - n_resp1_fail - n_nicks_fail, 4, sprintf("Excluded %d who failed the instructed-response attention check (QID115 'Somewhat disagree' not selected; EndSurvey).", n_nicks_fail)),
      .funnel_row("screening_funnel_s13", lab, "reached_equivocality",     n_reached, 5, sprintf("Excluded %d who abandoned the survey before the conspiracy-screening stage.", n_abandon)),
      .funnel_row("screening_funnel_s13", lab, "valid_conspiracy",         n_reached - n_invalid, 6, sprintf("Excluded %d whose description was classified INVALID/off-task after one correction (EndSurvey).", n_invalid)),
      .funnel_row("screening_funnel_s13", lab, "equivocal_belief",         n_true,   7, sprintf("Excluded %d who expressed firm belief or firm disbelief (equivocality classifier = FALSE).", n_false)),
      .funnel_row("screening_funnel_s13", lab, "within_2575_window",       n_window, 8, sprintf("Excluded %d whose baseline belief fell outside the 25-75 uncertainty window.", n_true - n_window)),
      .funnel_row("screening_funnel_s13", lab, "analytic_sample",          n_analytic, 9, sprintf("Excluded %d for incomplete conversation, missing post-belief, or unusable persuasion direction; final analytic sample.", n_window - n_analytic))
    )

    # attention / bot-detection items: real pass/fail counts among those who reached each item
    att <- function(term, n, est, note) .funnel_row("attention_checks", lab, term, n, NA, note)
    att_rows <- dplyr::bind_rows(
      .funnel_row("attention_checks", lab, "respond_minus1_failed", n_resp1_fail, NA, "QID55 'respond -1' chatbot screener: responses containing '0' (EndSurvey)."),
      .funnel_row("attention_checks", lab, "nicks_failed",          n_nicks_fail, NA, "QID115 instructed-response: did not select 'Somewhat disagree' (EndSurvey)."),
      {
        gcol <- grep("^Gender", names(real), value = TRUE)[1]
        spice <- if (i %in% c(2,3) && !is.na(gcol)) {
          gv <- tolower(as.character(real[[gcol]]))
          sum(!is.na(gv) & !grepl("male|female|man|woman|non.?binary|trans|prefer|fluid|f$|m$", gv))
        } else NA_integer_
        .funnel_row("attention_checks", lab, "gender_spice_trap_failed", spice, NA,
                    if (i %in% c(2,3)) "Gender item carried an LLM 'favorite spice' trap in Studies 2-3: responses that are not a recognizable gender." else "Gender 'favorite spice' trap not administered in Study 1 (plain gender question).")
      },
      {
        lv <- tolower(as.character(real[["LLM agent"]])); reached_llm <- sum(!is.na(lv))
        llm_fail <- sum(!is.na(lv) & grepl("i am an llm|yes.*llm|i am a (language|large)|^llm", lv))
        .funnel_row("attention_checks", lab, "llm_self_disclosure_flagged", llm_fail, NA,
                    sprintf("'If you are an LLM, say so' bot screen: self-disclosed LLM responses (of %d reaching the item).", reached_llm))
      }
    )
    out[[i]] <- dplyr::bind_rows(rows, att_rows)
  }
  dplyr::bind_rows(out)
}
