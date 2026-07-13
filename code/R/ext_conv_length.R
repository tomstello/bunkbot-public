# ext_conv_length.R --------------------------------------------------------------------
# Conversation-LENGTH distribution (Fig S11). Per analytic cell (study/model x direction x
# sample) we report the full empirical distribution of the per-conversation USER message
# count, plus the cell mean and median. This makes Fig S11 a pure all_numbers reader: the
# figure plots share (=fraction of conversations with exactly k user messages) against k,
# faceted by study/model and coloured by arm. The per-conversation cap is 10 user
# messages; 93-99% of conversations end before reaching it (block "engagement",
# term pct_ended_before_10turn_max), so distributions concentrate at low counts
# with only a small terminal spike at the cap.
#
# ENTRY POINT: compute_conversation_length(core_objects) -> tibble (canonical schema).
#
# S1-3: per-conversation user count = number of content_user_<n> columns (regex
#   ^content_user_[0-9]+$) that are non-empty (!is.na & nzchar(trimws(.))), read off the
#   analytic frame core_objects$s13. Cells = study_factor x direction; BOTH samples = the
#   full analytic frame (full_sample) and the compliant subframe (compliant flag).
# S4: per-conversation user count parsed from the chunked JSON transcript (chathistory0*)
#   with the canonical chatlogr-style reconstructor .ext_parse_s4_transcript() defined in
#   ext_attrition.R (sourced into the same global env at pipeline time). Cells = model_pooled
#   x direction; BOTH samples = strict (s4) and compliant (s4_compliant) via ResponseId.
#
# block "conversation_length" (sections "S1-3" and "S4 + pooled").
#   one row per observed count value k in the cell:
#       term = paste0("count_", k); estimate = share = (# conv with exactly k user msgs)/cell_n;
#       statistic = k (x position); outcome = "count_share"; n = cell_n.
#   plus per-cell summaries:
#       term = "mean_user_msgs"   : estimate = mean user messages; statistic = NA; n = cell_n.
#       term = "median_user_msgs" : estimate = median user messages; statistic = NA; n = cell_n.

# Tabulate the user-count distribution for one cell into canonical rows.
.cv_dist_rows <- function(counts, model_lab, dir_lab) {
  counts <- counts[is.finite(counts)]
  cell_n <- length(counts)
  if (cell_n < 1) return(NULL)
  tab <- as.data.frame(table(k = counts), stringsAsFactors = FALSE)
  tab$k <- as.integer(as.character(tab$k))
  tab <- tab[order(tab$k), , drop = FALSE]
  share_rows <- tibble::tibble(
    outcome   = "count_share",
    model     = model_lab,
    direction = dir_lab,
    term      = paste0("count_", tab$k),
    n         = cell_n,
    estimate  = tab$Freq / cell_n,
    statistic = as.numeric(tab$k),
    note      = "Share of conversations in the cell with exactly k user messages; statistic = k (x position)."
  )
  summ_rows <- tibble::tibble(
    outcome   = "count_summary",
    model     = model_lab,
    direction = dir_lab,
    term      = c("mean_user_msgs", "median_user_msgs"),
    n         = cell_n,
    estimate  = c(mean(counts), stats::median(counts)),
    statistic = NA_real_,
    note      = "Cell mean / median per-conversation user message count."
  )
  dplyr::bind_rows(share_rows, summ_rows)
}

# S1-3: non-empty content_user_<n> count per conversation row.
.cv_user_count_s13 <- function(d, ucols) {
  ne <- function(x) !is.na(x) & nzchar(trimws(as.character(x)))
  as.integer(rowSums(vapply(ucols, function(cc) ne(d[[cc]]), logical(nrow(d)))))
}

# S4: per-ResponseId user-message count from the reconstructed JSON transcript. Reuses the
# canonical .ext_parse_s4_transcript() parser (ext_attrition.R) so the count is identical to
# the engagement block's user-message count. Returns a named integer vector keyed by ResponseId
# (un-parsable transcripts dropped).
.cv_user_count_s4 <- function(s4) {
  chat_cols <- intersect(paste0("chathistory0", 1:5), names(s4))
  ids <- as.character(s4$ResponseId)
  out <- integer(nrow(s4))
  keep <- logical(nrow(s4))
  for (i in seq_len(nrow(s4))) {
    msgs <- .ext_parse_s4_transcript(as.character(unlist(s4[i, chat_cols])))
    if (is.null(msgs)) { keep[i] <- FALSE; next }
    keep[i] <- TRUE
    roles <- vapply(msgs, function(m) if (!is.null(m$role)) m$role else NA_character_, character(1))
    contents <- vapply(msgs, function(m) if (!is.null(m$content)) paste(as.character(m$content), collapse = "") else "", character(1))
    out[i] <- sum(roles == "user" & nchar(trimws(contents)) > 0L, na.rm = TRUE)
  }
  stats::setNames(out[keep], ids[keep])
}

compute_conversation_length <- function(core_objects) {
  si_require(c("dplyr", "tibble"))
  out <- list()

  # ---------------------------------------------------------------- S1-3 ----
  s13 <- core_objects$s13
  ucols <- grep("^content_user_[0-9]+$", names(s13), value = TRUE)
  s13$.cv_nuser <- .cv_user_count_s13(s13, ucols)

  s13_samples <- list(
    full_sample      = s13,
    compliant_sample = s13[!is.na(s13$compliant) & s13$compliant, , drop = FALSE]
  )
  for (samp in names(s13_samples)) {
    ds <- s13_samples[[samp]]
    for (sf in c("Jailbroken", "Standard", "Truth-Constrained")) {
      for (dd in c("bunk", "debunk")) {
        sub <- ds[as.character(ds$study_factor) == sf & as.character(ds$direction) == dd, , drop = FALSE]
        r <- .cv_dist_rows(sub$.cv_nuser, sf, if (dd == "bunk") "Bunking" else "Debunking")
        if (!is.null(r)) out[[length(out) + 1]] <-
          std_row(r, "S1-3", "conversation_length", samp)
      }
    }
  }

  # ----------------------------------------------------------------- S4 ----
  s4 <- core_objects$s4$s4
  s4_uc <- .cv_user_count_s4(s4)                      # named by ResponseId
  comp_ids <- as.character(core_objects$s4$s4_compliant$ResponseId)

  s4tab <- tibble::tibble(
    ResponseId = as.character(s4$ResponseId),
    model      = as.character(s4$model_pooled),
    direction  = as.character(s4$direction)
  )
  s4tab$nuser <- s4_uc[s4tab$ResponseId]
  s4tab <- s4tab[!is.na(s4tab$nuser), , drop = FALSE]

  s4_samples <- list(
    strict_n1272    = s4tab,
    compliant_n1073 = s4tab[s4tab$ResponseId %in% comp_ids, , drop = FALSE]
  )
  s4_models <- c("Claude", "Gemini", "GPT-5.2", "Grok")
  for (samp in names(s4_samples)) {
    ds <- s4_samples[[samp]]
    for (mm in s4_models) {
      for (dd in c("bunk", "debunk")) {
        sub <- ds[ds$model == mm & ds$direction == dd, , drop = FALSE]
        r <- .cv_dist_rows(sub$nuser, mm, if (dd == "bunk") "Bunking" else "Debunking")
        if (!is.null(r)) out[[length(out) + 1]] <-
          std_row(r, "S4 + pooled", "conversation_length", samp)
      }
    }
  }

  if (!length(out)) {
    return(std_row(tibble::tibble(term = character(0)), "S1-3", "conversation_length", "full_sample"))
  }
  dplyr::bind_rows(out)
}
