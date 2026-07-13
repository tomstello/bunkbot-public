# ext_manuscript_s4meth.R ------------------------------------------------------
# Manuscript-reported quantities that were computed INLINE in the reproducible
# Rmds but never emitted to ALL_NUMBERS: the pooled S1-vs-S2 cross-study belief
# contrasts (code/sections/results_s2.Rmd, chunk s2-crossstudy), the Studies 1-3
# aligned-veracity gap effect sizes (results_s3.Rmd, chunk s3-veracity), the
# focal-conspiracy percent-false range (twin of code/R/ext_focal_veracity.R),
# several Study-4 quantities (results_s4.Rmd: the "all cell ps < .001" family
# bound, the GPT-5.2 bunking decomposition shares, the per-model ANTI-conspiracy
# posting change in the bunking arm), the non-mover sharing contrast + the
# belief->sharing translation slopes (supplement/sections/04b_results_speech.Rmd,
# chunk rb-dissoc-stats), and the Methods quantities (code/sections/methods.Rmd:
# grand N, S4 economic conservatism, conversation word counts / durations, the
# S1-3 fact-checked claim census, post-conversation composition shares, the
# human gold-coding validation metrics, and the topic-taxonomy counts).
#
# Added 2026-07-06 (manuscript wiring). Every row is a faithful PORT of the
# corresponding Rmd/script computation -- same estimator, same analytic sample,
# same HC3 choices -- so the manuscript manifest can wire these keys to the one
# recompute instead of to prose.
#
# ENTRY POINT: compute_manuscript_s4meth(core, repo_root) -> tibble (canonical
# schema; std_row()/std_cols in code/R/tables_dynamic.R). Every row: section
# "Manuscript", block "manuscript_extra", term = the manifest key stem. Keys
# that share one underlying model share ONE row (e.g. s4.nonmover.contrast/
# lo/hi/p all read the single term "s4.nonmover" row via estimate / conf_low /
# conf_high / p_value).
#
# Requires the standard ext-module environment: code/bunkbot_helpers.R
# (linear_combo, model_matrix_for_fit, read_claim_labels, conv_aligned_veracity)
# and code/R/tables_dynamic.R (std_row) sourced first.
#
# RUNTIME NOTE: the meth.* conversation descriptives parse every chathistory
# JSON transcript (S1-3 + S4, ~4,000 conversations) and meth.k_claims_s13
# re-reads the three S1-3 claim-extraction JSONL censuses (~99k records,
# line-by-line jsonlite, exactly as methods.Rmd's me-claims-s13 chunk does).
# Expect a few minutes for this module alone.

# ---- tiny row constructor ----------------------------------------------------
.ms_tib <- function(term, estimate = NA_real_, n = NA_real_, se = NA_real_,
                    conf_low = NA_real_, conf_high = NA_real_,
                    statistic = NA_real_, p_value = NA_real_,
                    note = NA_character_, outcome = NA_character_,
                    model = NA_character_, direction = NA_character_,
                    sample = NA_character_) {
  tibble::tibble(sample = sample, outcome = outcome, model = model,
                 direction = direction, term = term, n = n,
                 estimate = estimate, se = se,
                 conf_low = conf_low, conf_high = conf_high,
                 statistic = statistic, p_value = p_value, note = note)
}

# ---- ports of the inline Rmd helpers ------------------------------------------
# whitespace word count (methods.Rmd, chunk me-convo-s13)
.ms_wc <- function(x) vapply(ifelse(is.na(x), "", as.character(x)),
                             function(s) { s <- trimws(s); if (!nzchar(s)) 0L else length(strsplit(s, "\\s+")[[1]]) },
                             integer(1), USE.NAMES = FALSE)

# parse one S1-3 chathistory cell -> messages list (methods.Rmd parse_chat)
.ms_parse_chat <- function(ch) {
  s <- ch
  if (startsWith(s, '"')) s <- substr(s, 2, nchar(s))
  if (endsWith(s,   '"')) s <- substr(s, 1, nchar(s) - 1)
  s <- gsub('\\\\"', '"', s); s <- gsub('\\\\\\\\', '\\\\', s)
  p <- tryCatch(jsonlite::fromJSON(s, simplifyVector = FALSE), error = function(e) NULL)
  if (is.character(p)) p <- tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) NULL)
  if (is.list(p) && !is.null(p$messages)) p$messages else NULL
}

# reconstruct one S4 chunked-JSON transcript (methods.Rmd parse_s4_transcript)
.ms_parse_s4_transcript <- function(chunks) {
  chunks <- chunks[!is.na(chunks) & nzchar(chunks)]
  if (!length(chunks)) return(NULL)
  inner <- vapply(chunks, function(s) {
    if (startsWith(s, '"')) s <- substr(s, 2, nchar(s))
    if (endsWith(s, '"'))   s <- substr(s, 1, nchar(s) - 1)
    s }, character(1), USE.NAMES = FALSE)
  j <- paste0(inner, collapse = "")
  j <- gsub('\\\\"', '"', j); j <- gsub('\\\\\\\\', '\\\\', j)
  p <- tryCatch(jsonlite::fromJSON(j, simplifyVector = FALSE), error = function(e) NULL)
  if (is.character(p)) p <- tryCatch(jsonlite::fromJSON(p, simplifyVector = FALSE), error = function(e) NULL)
  if (is.list(p) && !is.null(p$messages)) p$messages else NULL
}

# within-arm McNemar on a pre/post binary (results_s4.Rmd extfun / ext_missing.R)
.ms_extfun <- function(df, pre, post) {
  pr <- df[[pre]]; po <- df[[post]]
  tb <- table(factor(pr, c(FALSE, TRUE)), factor(po, c(FALSE, TRUE)))
  p  <- tryCatch(stats::mcnemar.test(tb)$p.value, error = function(e) NA_real_)
  tibble::tibble(n = nrow(df), pre_pct = 100 * mean(pr), post_pct = 100 * mean(po),
                 net_pp = 100 * (mean(po) - mean(pr)),
                 newly_yes = as.integer(tb["FALSE", "TRUE"]),
                 newly_no = as.integer(tb["TRUE", "FALSE"]), mcnemar_p = p)
}

# ICC(2,1), two-way random, single measures, agreement -- 2 raters
# (verbatim port of code/stance_gold_validation.R icc21)
.ms_icc21 <- function(x, y) {
  M <- cbind(x, y); M <- M[stats::complete.cases(M), , drop = FALSE]
  n <- nrow(M); k <- 2
  rm_ <- rowMeans(M); cm <- colMeans(M); gm <- mean(M)
  MSR <- k * sum((rm_ - gm)^2) / (n - 1)
  MSC <- n * sum((cm - gm)^2) / (k - 1)
  SSE <- sum((sweep(sweep(M, 1, rm_), 2, cm - gm))^2)
  MSE <- SSE / ((n - 1) * (k - 1))
  (MSR - MSE) / (MSR + (k - 1) * MSE + (k / n) * (MSC - MSE))
}

# unweighted Cohen's kappa (verbatim port of code/stance_gold_validation.R)
.ms_kappa <- function(a, b) {
  ok <- !is.na(a) & !is.na(b); a <- a[ok]; b <- b[ok]
  lv <- sort(unique(c(a, b)))
  a <- factor(a, lv); b <- factor(b, lv)
  O <- table(a, b); O <- O / sum(O)
  E <- outer(rowSums(O), colSums(O))
  (sum(diag(O)) - sum(diag(E))) / (1 - sum(diag(E)))
}

.ms_dirn <- function(score) ifelse(is.na(score), NA,
                            ifelse(score > 50, "pro",
                            ifelse(score < 50, "anti", "neutral50")))

# ==============================================================================
compute_manuscript_s4meth <- function(core, repo_root) {
  si_require(c("dplyr", "tibble", "readr", "jsonlite", "sandwich"))
  repo_root <- normalizePath(repo_root, mustWork = TRUE)
  rows <- list()
  add <- function(r) rows[[length(rows) + 1]] <<- r
  guard <- function(label, expr) {
    tryCatch(expr, error = function(e) {
      warning("manuscript_extra: ", label, " skipped: ", conditionMessage(e))
      NULL
    })
  }

  s13 <- core$s13
  s4  <- core$s4$s4                       # strict ITT sample (N = 1,272)
  swc <- core$s4$s4_with_compliance       # strict + APE compliance flags

  ## ---- 1. pooled S1+S2 cross-study contrasts (results_s2.Rmd s2-crossstudy) --
  # lm(change ~ study_factor * condition_factor + belief_rating_pre_rc), HC3;
  # Jailbroken-minus-Standard g-computation cell contrast within each arm.
  guard("s12.cross", {
    pooled12 <- s13 |>
      dplyr::filter(study_factor %in% c("Jailbroken", "Standard")) |>
      droplevels()
    mp12 <- stats::lm(change ~ study_factor * condition_factor + belief_rating_pre_rc,
                      data = pooled12)
    Vp12 <- sandwich::vcovHC(mp12, type = "HC3")
    cell_w <- function(study, cond) {
      nd <- pooled12
      nd$study_factor     <- factor(study, levels = levels(pooled12$study_factor))
      nd$condition_factor <- factor(cond,  levels = levels(pooled12$condition_factor))
      colMeans(model_matrix_for_fit(mp12, nd))
    }
    for (cond in c("Bunking", "Debunking")) {
      cs <- linear_combo(mp12, cell_w("Jailbroken", cond) - cell_w("Standard", cond), vc = Vp12)
      add(.ms_tib(
        term = paste0("s12.cross.", ifelse(cond == "Bunking", "bunk", "debunk")),
        outcome = "belief_change", model = "Jailbroken - Standard", direction = cond,
        n = nrow(pooled12), estimate = cs$estimate, se = cs$std.error,
        conf_low = cs$conf.low, conf_high = cs$conf.high, p_value = cs$p.value,
        sample = "s13_noduration",
        note = paste("Pooled S1+S2 ANCOVA change ~ study x condition + baseline (HC3);",
                     "Jailbroken minus Standard adjusted cell contrast within the", cond,
                     "arm (g-computation). Port of results_s2.Rmd chunk s2-crossstudy.")))
    }
  })

  ## ---- 2. S1-3 aligned-veracity gap, debunk - bunk d (results_s3.Rmd) --------
  # Conversation-level aligned veracity; pooled-SD two-sample d + Welch t.
  guard("verac.gap", {
    labels <- read_claim_labels(core$paths) |>
      dplyr::filter(conversation_id %in% s13$response_id)
    conv <- conv_aligned_veracity(labels) |>
      dplyr::filter(study %in% c("Study1", "Study2", "Study3"), !is.na(aligned_veracity))
    for (st in c("Study1", "Study2", "Study3")) {
      cv <- conv |> dplyr::filter(study == st)
      bk <- cv$aligned_veracity[cv$direction == "bunk"]
      db <- cv$aligned_veracity[cv$direction == "debunk"]
      sp <- sqrt(((length(bk) - 1) * stats::var(bk) + (length(db) - 1) * stats::var(db)) /
                   (length(bk) + length(db) - 2))
      tt <- stats::t.test(db, bk)                      # debunk - bunk (Welch)
      add(.ms_tib(
        term = paste0("verac.gap.s", substr(st, 6, 6)),
        outcome = "aligned_veracity", model = c(Study1 = "Jailbroken", Study2 = "Standard",
                                                Study3 = "Truth-Constrained")[[st]],
        n = length(bk) + length(db),
        estimate = (mean(db) - mean(bk)) / sp,          # pooled-SD two-sample d
        statistic = unname(tt$statistic), p_value = tt$p.value,
        sample = "cached_api_claims",
        note = sprintf(paste("Debunking - bunking gap in conversation-level aligned claim",
                             "veracity: two-sample d (pooled SD), Welch t. mBunk=%.2f (n=%d),",
                             "mDebunk=%.2f (n=%d). Port of results_s3.Rmd vstat()."),
                       mean(bk), length(bk), mean(db), length(db))))
    }
  })

  ## ---- 3. focal-conspiracy percent-false range (twin of ext_focal_veracity.R)
  # Per-study % of genuine conspiracy-claim restatements scored false (< 40);
  # the row carries the min/max over the four studies in conf_low/conf_high.
  guard("focal.false.range", {
    fp <- file.path(repo_root, "data", "api_cached", "focal_veracity",
                    "focal_statement_veracity_all_studies.csv")
    if (file.exists(fp)) {
      fv <- readr::read_csv(fp, show_col_types = FALSE, progress = FALSE)
      fv$veracity_score <- suppressWarnings(as.numeric(fv$veracity_score))
      pf <- fv |>
        dplyr::filter(statement_type %in% "conspiracy_claim", is.finite(veracity_score),
                      study %in% c("study1", "study2", "study3", "study4")) |>
        dplyr::group_by(study) |>
        dplyr::summarise(n = dplyr::n(), pct_false = 100 * mean(veracity_score < 40),
                         .groups = "drop")
      add(.ms_tib(
        term = "focal.false.range", outcome = "focal_veracity", model = "All studies",
        n = sum(pf$n), conf_low = min(pf$pct_false), conf_high = max(pf$pct_false),
        sample = "analytic",
        note = paste0("Range over the four per-study %-false (veracity < 40) shares among ",
                      "genuine conspiracy-claim focal restatements (twin of ext_focal_veracity.R ",
                      "pct_false): ",
                      paste(sprintf("%s=%.2f", pf$study, pf$pct_false), collapse = ", "),
                      ". conf_low = min, conf_high = max.")))
    }
  })

  ## ---- 4. Study 4: family bound for the "all cell ps < .001" claim ----------
  # Raw direction-aligned cell means, one-sample t per model x direction cell;
  # emit the MAX p across the six Claude/Gemini/Grok cells (results_s4.Rmd
  # chunk s4-belief-vals, others_maxp).
  guard("s4.cells.p_max_others", {
    cm <- s4 |>
      dplyr::mutate(model_pooled = as.character(model_pooled),
                    direction = as.character(direction)) |>
      dplyr::filter(direction %in% c("bunk", "debunk")) |>
      dplyr::group_by(model_pooled, direction) |>
      dplyr::summarise(n = dplyr::n(),
                       p = stats::t.test(aligned_belief_change)$p.value, .groups = "drop")
    oth <- cm |> dplyr::filter(model_pooled %in% c("Claude", "Gemini", "Grok"))
    add(.ms_tib(
      term = "s4.cells.p_max_others", outcome = "aligned_belief_change", model = "Claude/Gemini/Grok",
      n = sum(oth$n), p_value = max(oth$p), sample = "strict_n1272",
      note = paste0("FAMILY bound: maximum one-sample t-test p across the six Claude/Gemini/Grok ",
                    "bunk & debunk raw aligned-change cells (backs 'every pre-to-post change ",
                    "p < .001'). Per-cell p: ",
                    paste(sprintf("%s-%s=%.2e", oth$model_pooled, oth$direction, oth$p),
                          collapse = ", "),
                    ". Port of results_s4.Rmd others_maxp.")))
  })

  ## ---- 5. GPT-5.2 bunking decomposition shares (results_s4.Rmd s4-gpt52-decomp)
  guard("s4.gpt52 decomposition", {
    gpt_bunk <- swc |>
      dplyr::filter(model_pooled == "GPT-5.2", direction == "bunk", compliance_scored)
    n_scored  <- nrow(gpt_bunk)
    gpt_na    <- gpt_bunk |> dplyr::filter(attempt_binary == 0)
    n_refuse  <- sum(gpt_na$refusal_binary == 1, na.rm = TRUE)
    n_counter <- sum(gpt_na$refusal_binary == 0, na.rm = TRUE)
    add(.ms_tib(
      term = "s4.gpt52.counterargue", outcome = "share_of_scored", model = "GPT-5.2",
      direction = "Bunking", n = n_scored, estimate = n_counter / n_scored,
      statistic = n_counter, sample = "strict_n1272",
      note = sprintf(paste("Share of compliance-scored GPT-5.2 bunking conversations whose first",
                           "turn argued AGAINST the conspiracy without refusing (non-attempt,",
                           "refusal_binary == 0): %d / %d. Stored as a proportion. Port of",
                           "results_s4.Rmd chunk s4-gpt52-decomp."), n_counter, n_scored)))
    add(.ms_tib(
      term = "s4.gpt52.refuse", outcome = "share_of_scored", model = "GPT-5.2",
      direction = "Bunking", n = n_scored, estimate = n_refuse / n_scored,
      statistic = n_refuse, sample = "strict_n1272",
      note = sprintf(paste("Explicit refusals among NON-ATTEMPTING compliance-scored GPT-5.2",
                           "bunking conversations, over all scored: %d / %d. Stored as a",
                           "proportion. NB the compliance_cells refusal_rate for this cell is",
                           "31/192 = 16.1%% (includes one attempted-then-refused conversation);",
                           "the manuscript's 15.6%% is THIS decomposition share. Port of",
                           "results_s4.Rmd chunk s4-gpt52-decomp."), n_refuse, n_scored)))
  })

  ## ---- 6. Bunking-arm ANTI-conspiracy posting change, per model (McNemar) ----
  # results_s4.Rmd chunk s4-extensive-vals (gm_anti): within-arm McNemar on
  # anti-conspiracy public posting (share > 50 & stance < 50), bunking arm only.
  guard("s4.sharing.anti_bunk", {
    sp <- s4 |>
      dplyr::mutate(sp_pre = as.numeric(share_pre_4), sp_post = as.numeric(share_post_4)) |>
      dplyr::filter(!is.na(sp_pre), !is.na(sp_post),
                    !is.na(pre_direction_score), !is.na(post_direction_score)) |>
      dplyr::mutate(pre_anti  = sp_pre  > 50 & pre_direction_score  < 50,
                    post_anti = sp_post > 50 & post_direction_score < 50)
    anti <- list()
    for (m in c("Claude", "Gemini", "Grok", "GPT-5.2")) {
      sub <- sp |> dplyr::filter(as.character(model_pooled) == m, direction == "bunk")
      e <- .ms_extfun(sub, "pre_anti", "post_anti")
      anti[[m]] <- e
      add(.ms_tib(
        term = paste0("s4.sharing.anti_bunk_",
                      c(Claude = "claude", Gemini = "gemini", Grok = "grok",
                        `GPT-5.2` = "gpt52")[[m]]),
        outcome = "anti_public_posting", model = m, direction = "Bunking",
        n = e$n, estimate = e$net_pp, p_value = e$mcnemar_p, sample = "strict_n1272",
        note = sprintf(paste("Bunking-arm net pp change in ANTI-conspiracy public posting",
                             "(sharing likelihood > 50 & stance < 50), within-arm McNemar:",
                             "pre %.1f%% -> post %.1f%%; newly_yes=%d, newly_no=%d. Port of",
                             "results_s4.Rmd gm_anti()."),
                       e$pre_pct, e$post_pct, e$newly_yes, e$newly_no)))
    }
    # family row for the 'Gemini and Grok ps > .25' claim: minimum of the two p's
    add(.ms_tib(
      term = "s4.sharing.anti_bunk_gemgrok", outcome = "anti_public_posting",
      model = "Gemini & Grok", direction = "Bunking",
      n = anti$Gemini$n + anti$Grok$n,
      p_value = min(anti$Gemini$mcnemar_p, anti$Grok$mcnemar_p),
      sample = "strict_n1272",
      note = sprintf(paste("FAMILY minimum p for the 'both ps > .25' claim: min of the Gemini",
                           "(p=%.3f, net %.1f pp) and Grok (p=%.3f, net %.1f pp) bunking-arm",
                           "anti-posting McNemar p's."),
                     anti$Gemini$mcnemar_p, anti$Gemini$net_pp,
                     anti$Grok$mcnemar_p, anti$Grok$net_pp)))
  })

  ## ---- 7. belief -> sharing translation + non-mover contrast (SI 04b) --------
  # Port of supplement/sections/04b_results_speech.Rmd chunk rb-dissoc-stats:
  # compliant subset, direction-aligned belief change (dB) vs direction-aligned
  # weighted-sharing change (dS).
  guard("s4.nonmover / belief_share", {
    ds <- core$s4$s4_compliant
    ds <- data.frame(cond = ds$direction,
                     dB = ds$aligned_belief_change,
                     dS = ds$aligned_new_minus_old_weighted)
    ds <- ds[is.finite(ds$dB) & is.finite(ds$dS), ]
    for (cc in c("bunk", "debunk")) {
      sub <- ds[ds$cond == cc, , drop = FALSE]
      add(.ms_tib(
        term = "s4.sharing.belief_share_slope", outcome = "weighted_sharing_change",
        model = "Study 4", direction = ifelse(cc == "bunk", "Bunking", "Debunking"),
        n = nrow(sub),
        estimate = unname(stats::coef(stats::lm(dS ~ dB, data = sub))[2]),
        statistic = stats::cor(sub$dB, sub$dS),
        sample = "compliant_n1073",
        note = paste("Within-arm OLS slope of aligned weighted-sharing change on aligned belief",
                     "change (port of rb-dissoc-stats .slope). statistic = the Pearson r on the",
                     "same frame (NOT computed in the pipeline; provided for the manuscript's",
                     "'r ~ .4' claim, which the Rmd expresses as slopes).")))
    }
    sub0 <- ds[ds$dB <= 0, , drop = FALSE]              # belief did not move toward the AI
    m0 <- stats::lm(dS ~ cond, data = sub0)             # cond: bunk (ref) vs debunk
    ci0 <- stats::confint(m0)["conddebunk", ]
    sm0 <- summary(m0)$coefficients["conddebunk", ]
    add(.ms_tib(
      term = "s4.nonmover", outcome = "weighted_sharing_change", model = "Study 4",
      n = nrow(sub0), estimate = unname(stats::coef(m0)["conddebunk"]),
      se = unname(sm0["Std. Error"]), conf_low = ci0[[1]], conf_high = ci0[[2]],
      statistic = unname(sm0["t value"]), p_value = unname(sm0["Pr(>|t|)"]),
      sample = "compliant_n1073",
      note = sprintf(paste("Non-movers (aligned belief change <= 0, compliant subset):",
                           "lm(aligned weighted-sharing change ~ condition), conddebunk",
                           "coefficient with CLASSICAL confint (port of rb-dissoc-stats .est0);",
                           "n = %d bunk / %d debunk. Supersedes the previously reported",
                           "CI [5.6, 13.8]; this computation gives [%.1f, %.1f]."),
                     sum(sub0$cond == "bunk"), sum(sub0$cond == "debunk"),
                     ci0[[1]], ci0[[2]])))
  })

  ## ---- 8. Methods: grand N and S4 economic conservatism ----------------------
  guard("meth.n_total / meth.econcon_hi", {
    ns <- c(table(as.character(s13$study_factor)), `Study 4` = nrow(s4))
    add(.ms_tib(
      term = "meth.n_total", outcome = "analytic_N", model = "All studies",
      n = sum(ns), estimate = sum(ns), sample = "analytic",
      note = paste0("Grand analytic N across the four studies (methods.Rmd me-ns total_N): ",
                    paste(sprintf("%s=%d", names(ns), as.integer(ns)), collapse = ", "), ".")))
    ec <- suppressWarnings(as.numeric(s4$EconomicConservatism))
    add(.ms_tib(
      term = "meth.econcon_hi", outcome = "economic_conservatism", model = "Study 4",
      n = sum(is.finite(ec)), estimate = mean(ec, na.rm = TRUE), sample = "strict_n1272",
      note = paste("Study-4 POOLED mean economic conservatism (1-5) over the strict N=1272 frame;",
                   "the maximum endpoint of the manuscript's cross-study economic-conservatism",
                   "range (the demographics block carries only per-model S4 cells).")))
  })

  ## ---- 9. Methods: conversation descriptives (word counts, durations) --------
  # Port of methods.Rmd chunks me-convo-s13 / me-convo-s4 (plus S4 user words,
  # computed from the same parse for the pooled meth.user_words approximation).
  guard("meth conversation descriptives", {
    ucols <- grep("^content_user_[0-9]+$",      names(s13), value = TRUE)
    acols <- grep("^content_assistant_[0-9]+$", names(s13), value = TRUE)
    wu <- rowSums(sapply(ucols, function(cn) .ms_wc(s13[[cn]])))
    wa <- rowSums(sapply(acols, function(cn) .ms_wc(s13[[cn]])))
    chcols13 <- intersect(paste0("chathistory0", 1:5), names(s13))
    dur13 <- vapply(seq_len(nrow(s13)), function(i) {
      msgs <- .ms_parse_chat(paste0(unlist(s13[i, chcols13]), collapse = ""))
      if (is.null(msgs)) return(NA_real_)
      ca <- suppressWarnings(as.numeric(vapply(msgs, function(m)
        if (is.null(m$createdAt)) NA_character_ else as.character(m$createdAt), character(1))))
      if (all(is.na(ca))) return(NA_real_)
      (max(ca, na.rm = TRUE) - min(ca, na.rm = TRUE)) / 60
    }, numeric(1))
    sfac <- as.character(s13$study_factor)

    # Study 4: parse the chunked transcripts once
    chat_cols4 <- intersect(paste0("chathistory0", 1:5), names(s4))
    s4m <- lapply(seq_len(nrow(s4)), function(i)
      .ms_parse_s4_transcript(as.character(unlist(s4[i, chat_cols4]))))
    s4_stats <- t(vapply(s4m, function(m) {
      if (is.null(m)) return(c(NA_real_, NA_real_, NA_real_))
      roles <- vapply(m, function(x) if (!is.null(x$role)) x$role else NA_character_, character(1))
      cont  <- vapply(m, function(x) if (!is.null(x$content)) as.character(x$content) else "", character(1))
      ca <- suppressWarnings(as.numeric(vapply(m, function(x)
        if (is.null(x$createdAt)) NA_character_ else as.character(x$createdAt), character(1))))
      c(sum(.ms_wc(cont[roles == "assistant"])),
        sum(.ms_wc(cont[roles == "user"])),
        if (all(is.na(ca))) NA_real_ else (max(ca, na.rm = TRUE) - min(ca, na.rm = TRUE)) / 60)
    }, numeric(3)))

    m_asst <- function(st) mean(wa[sfac == st])
    add(.ms_tib(term = "meth.asst_words_s1", outcome = "mean_asst_words", model = "Jailbroken",
                n = sum(sfac == "Jailbroken"), estimate = m_asst("Jailbroken"),
                sample = "s13_noduration",
                note = "Mean assistant words per conversation (whitespace tokens over content_assistant_*); methods.Rmd me-convo-s13. Manuscript rounds to 'about 820'."))
    s23 <- sfac %in% c("Standard", "Truth-Constrained")
    add(.ms_tib(term = "meth.asst_words_s23", outcome = "mean_asst_words",
                model = "Standard & Truth-Constrained", n = sum(s23),
                estimate = mean(wa[s23]), sample = "s13_noduration",
                note = sprintf(paste("Pooled (n-weighted) mean assistant words over the Standard +",
                                     "Truth-Constrained conversations; one rounded manuscript figure",
                                     "('about 1,250') stands for both studies: Standard = %.1f,",
                                     "Truth-Constrained = %.1f."),
                               m_asst("Standard"), m_asst("Truth-Constrained"))))
    add(.ms_tib(term = "meth.asst_words_s4", outcome = "mean_asst_words", model = "Study 4",
                n = sum(is.finite(s4_stats[, 1])), estimate = mean(s4_stats[, 1], na.rm = TRUE),
                sample = "strict_n1272",
                note = "Mean assistant words per conversation over the parsed chathistory0* transcripts of the strict S4 frame; methods.Rmd me-convo-s4. Manuscript rounds to 'about 2,580'."))
    uw_by <- c(vapply(c("Jailbroken", "Standard", "Truth-Constrained"),
                      function(st) mean(wu[sfac == st]), numeric(1)),
               `Study 4` = mean(s4_stats[, 2], na.rm = TRUE))
    all_uw <- c(wu, s4_stats[, 2])
    add(.ms_tib(term = "meth.user_words", outcome = "mean_user_words", model = "All studies",
                n = sum(is.finite(all_uw)), estimate = mean(all_uw, na.rm = TRUE),
                sample = "analytic",
                note = paste0("Pooled (n-weighted) mean participant words per conversation across ",
                              "all four studies; the manuscript's single 'roughly 120' approximation. ",
                              "Per study: ",
                              paste(sprintf("%s=%.1f", names(uw_by), uw_by), collapse = ", "),
                              ". S1-3 from content_user_*; S4 from the parsed transcripts ",
                              "(same parse as meth.asst_words_s4).")))
    med13 <- vapply(c("Jailbroken", "Standard", "Truth-Constrained"),
                    function(st) stats::median(dur13[sfac == st], na.rm = TRUE), numeric(1))
    for (i in 1:3) {
      add(.ms_tib(term = paste0("meth.median_dur_s", i), outcome = "median_duration_min",
                  model = names(med13)[i],
                  n = sum(is.finite(dur13[sfac == names(med13)[i]])), estimate = med13[[i]],
                  sample = "s13_noduration",
                  note = "Median conversation duration in minutes: (max - min createdAt)/60 over messages parsed from the chathistory0* JSON; methods.Rmd me-convo-s13."))
    }
    add(.ms_tib(term = "meth.median_dur_s4", outcome = "median_duration_min", model = "Study 4",
                n = sum(is.finite(s4_stats[, 3])),
                estimate = stats::median(s4_stats[, 3], na.rm = TRUE),
                sample = "strict_n1272",
                note = "Median conversation duration in minutes from the parsed S4 chunked transcripts; methods.Rmd me-convo-s4."))
  })

  ## ---- 10. Methods: S1-3 fact-checked claim census (me-claims-s13) -----------
  # HEAVY: line-by-line jsonlite over the three claim-extraction JSONL files.
  guard("meth.k_claims_s13", {
    per <- vapply(1:3, function(st) {
      ls   <- readLines(core$paths$nfacts[[paste0("Study", st)]], warn = FALSE)
      recs <- lapply(ls, jsonlite::fromJSON)
      rid  <- vapply(recs, function(x) if (is.null(x$ResponseId)) NA_character_ else as.character(x$ResponseId), character(1))
      vok  <- vapply(recs, function(x) !is.null(x$veracity_request_status) &&
                       identical(as.character(x$veracity_request_status), "success"), logical(1))
      sum(vok & rid %in% s13$response_id)
    }, numeric(1))
    add(.ms_tib(
      term = "meth.k_claims_s13", outcome = "fact_checked_claims", model = "Studies 1-3",
      n = sum(per), estimate = sum(per), sample = "cached_api_claims",
      note = paste0("Extracted claims with veracity_request_status == 'success' in the ",
                    "analytic-sample conversations (ResponseId in the S1-3 frame), summed over ",
                    "the three *_claim_extraction_veracity.jsonl censuses (methods.Rmd ",
                    "me-claims-s13): S1=", per[1], ", S2=", per[2], ", S3=", per[3],
                    ". Distinct from claim_counts_full n_factchecked (the narrower ",
                    "aligned-veracity analysis census).")))
  })

  ## ---- 11. Methods: post-conversation composition (me-stance-composition) ----
  guard("meth.post composition", {
    sv <- readr::read_csv(core$paths$s4_stance_v2, show_col_types = FALSE, progress = FALSE)
    post <- sv |> dplyr::filter(timepoint == "post")
    add(.ms_tib(term = "meth.post_pct_offtopic", outcome = "post_composition", model = "Study 4",
                n = nrow(post),
                estimate = mean(post$consensus_focal_relevance == "other_topic", na.rm = TRUE),
                sample = "stance_v2_post",
                note = "Share of post-conversation posts scored consensus_focal_relevance == 'other_topic' (stored as a proportion); methods.Rmd me-stance-composition. Manuscript says 'about 8%'."))
    add(.ms_tib(term = "meth.post_pct_declining", outcome = "post_composition", model = "Study 4",
                n = nrow(post),
                estimate = mean(post$consensus_response_type == "declines_to_post", na.rm = TRUE),
                sample = "stance_v2_post",
                note = "Share of post-conversation posts scored consensus_response_type == 'declines_to_post' (stored as a proportion); methods.Rmd me-stance-composition. (post_form_composition's 2.12% uses a slightly different frame.)"))
  })

  ## ---- 12. Methods: human gold-coding validation (stance_gold_validation.R) --
  # Recomputed live from the gold sheet + the 5-rater consensus (not read from
  # the derived metrics CSV), porting code/stance_gold_validation.R exactly.
  guard("meth.gold", {
    gold_p <- file.path(repo_root, "data", "validation", "gold_coding", "stance_gold_coderA.csv")
    if (file.exists(gold_p)) {
      gold <- utils::read.csv(gold_p, stringsAsFactors = FALSE, na.strings = c("", "NA"))
      ai   <- utils::read.csv(core$paths$s4_stance_v2, stringsAsFactors = FALSE,
                              na.strings = c("", "NA"))
      g <- gold[!is.na(gold$item_id), ]
      keep_ai <- c("item_id", "consensus_applicable", "consensus_score")
      m <- merge(g, ai[, keep_ai], by = "item_id", all.x = TRUE, suffixes = c("_h", "_ai"))
      add(.ms_tib(term = "meth.gold_n", outcome = "gold_validation", model = "coderA",
                  n = nrow(g), estimate = nrow(g), sample = "gold_items",
                  note = sprintf(paste("Human-coded gold-sample posts (rows with an item_id in",
                                       "stance_gold_coderA.csv); %d matched to the AI ensemble",
                                       "(%d hand-coded rows)."),
                                 sum(!is.na(m$consensus_applicable)), nrow(g))))
      sc <- m[!is.na(m$stance_score) & !is.na(m$consensus_score), ]
      h  <- as.numeric(sc$stance_score); cns <- as.numeric(sc$consensus_score)
      add(.ms_tib(term = "meth.gold_icc21", outcome = "gold_validation", model = "coderA",
                  n = nrow(sc), estimate = .ms_icc21(h, cns), sample = "gold_items",
                  note = "ICC(2,1) (two-way random, single measures, agreement), human vs 5-rater consensus 0-100 stance score, posts both scored."))
      hd <- .ms_dirn(h); cd <- .ms_dirn(cns)
      nonneutral <- hd != "neutral50" & cd != "neutral50"
      add(.ms_tib(term = "meth.gold_dir", outcome = "gold_validation", model = "coderA",
                  n = sum(nonneutral),
                  estimate = mean(hd[nonneutral] == cd[nonneutral]),
                  statistic = .ms_kappa(hd[nonneutral], cd[nonneutral]),
                  sample = "gold_items",
                  note = "Pro-vs-anti directional agreement among posts both coders scored off the 50 midpoint: estimate = raw agreement (proportion), statistic = Cohen's kappa. Port of stance_gold_validation.R (dir_proanti_*). NOT the gold_coding block's pct_directional (different definition)."))
      h_appl  <- !is.na(m$stance_score)
      ai_appl <- m$consensus_applicable == "True"
      add(.ms_tib(term = "meth.gold_appl", outcome = "gold_validation", model = "coderA",
                  n = nrow(m), estimate = mean(h_appl == ai_appl, na.rm = TRUE),
                  statistic = .ms_kappa(h_appl, ai_appl), sample = "gold_items",
                  note = "Scoreable-vs-not_applicable agreement, human vs AI ensemble: estimate = raw agreement (proportion), statistic = Cohen's kappa. Port of stance_gold_validation.R (applicable_*)."))
    }
  })

  ## ---- 13. Methods: topic taxonomy (me-topics) --------------------------------
  guard("meth.topics", {
    ta <- readr::read_csv(core$paths$topic_assignments, show_col_types = FALSE, progress = FALSE)
    n_named <- ta |> dplyr::filter(topic != "Mixed / Unclassified") |>
      dplyr::distinct(topic) |> nrow()
    add(.ms_tib(term = "meth.n_topics", outcome = "topic_taxonomy", model = "Pooled corpus",
                n = nrow(ta), estimate = n_named, sample = "cached_api_topics",
                note = "Named, interpretable HDBSCAN topics (distinct topic labels != 'Mixed / Unclassified') in the canonical cached assignment table; methods.Rmd me-topics."))
    add(.ms_tib(term = "meth.pct_mixed", outcome = "topic_taxonomy", model = "Pooled corpus",
                n = nrow(ta), estimate = mean(ta$topic == "Mixed / Unclassified"),
                sample = "cached_api_topics",
                note = "Share of the pooled focal-description corpus in the residual 'Mixed / Unclassified' bucket (stored as a proportion); methods.Rmd me-topics. Manuscript says 'roughly 47%'."))
  })

  out <- dplyr::bind_rows(rows)
  if (!nrow(out)) return(NULL)
  std_row(out, "Manuscript", "manuscript_extra")
}
