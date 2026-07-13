# Dynamic table/number builders for the Bunkbot SI.
# These functions compute from live objects, raw/de-facto-raw data, and cached API
# outputs. They do not read legacy summary tables or pre-rendered analysis files.

std_cols <- c(
  "section", "block", "sample", "outcome", "model", "direction", "term",
  "n", "estimate", "se", "conf_low", "conf_high", "statistic",
  "df_num", "df_den", "p_value", "note"
)

std_row <- function(df, section, block, sample = NA_character_) {
  out <- df
  out$section <- section
  out$block <- block
  if (!"sample" %in% names(out)) out$sample <- sample
  for (nm in std_cols) {
    if (!nm %in% names(out)) out[[nm]] <- NA
  }
  out[, std_cols]
}

compute_s13_numbers <- function(s13) {
  si_require(c("dplyr", "tidyr", "purrr", "sandwich", "lmtest", "tibble"))
  rows <- list()

  # Screening and sample size.
  rows[[length(rows) + 1]] <- s13 |>
    dplyr::count(study_factor, name = "n") |>
    dplyr::transmute(
      model = as.character(study_factor),
      term = "analytic_N",
      estimate = n,
      n = n,
      note = "Full equivocal + 25-75 sample; no post-treatment duration screen"
    ) |>
    std_row("S1-3", "screening_counts", "full_sample")

  belief_rows <- list()
  contrast_rows <- list()
  for (sf in levels(s13$study_factor)) {
    dat <- s13 |>
      dplyr::filter(study_factor == sf) |>
      droplevels()
    mod <- stats::lm(change ~ condition_factor + belief_rating_pre_rc, data = dat)
    vc <- sandwich::vcovHC(mod, type = "HC3")

    sp_pre <- dat |>
      dplyr::group_by(condition_factor) |>
      dplyr::summarise(n = dplyr::n(), v = stats::var(belief_rating_pre_rc), .groups = "drop") |>
      dplyr::summarise(x = sqrt(sum((n - 1) * v) / (sum(n) - dplyr::n()))) |>
      dplyr::pull(x)

    for (cl in levels(dat$condition_factor)) {
      sub <- dat |> dplyr::filter(condition_factor == cl)
      n <- nrow(sub)
      m <- mean(sub$change, na.rm = TRUE)
      s <- stats::sd(sub$change, na.rm = TRUE)
      se <- s / sqrt(n)
      crit <- stats::qt(.975, n - 1)
      dz <- m / s
      J <- 1 - 3 / (4 * (n - 1) - 1)
      gpre <- J * (m / sp_pre)

      nd <- dat
      nd$condition_factor <- factor(cl, levels = levels(dat$condition_factor))
      mm <- model_matrix_for_fit(mod, nd)
      lin <- linear_combo(mod, colMeans(mm), vc = vc)
      belief_rows[[length(belief_rows) + 1]] <- tibble::tibble(
        model = sf,
        direction = cl,
        n = n,
        estimate = lin$estimate,
        se = lin$std.error,
        conf_low = lin$conf.low,
        conf_high = lin$conf.high,
        statistic = dz,
        note = sprintf(
          "raw_mean=%.3f; raw_ci=[%.3f, %.3f]; hedges_g_pre=%.3f",
          m, m - crit * se, m + crit * se, gpre
        )
      )
    }

    nd_b <- dat
    nd_b$condition_factor <- factor("Bunking", levels = levels(dat$condition_factor))
    nd_d <- dat
    nd_d$condition_factor <- factor("Debunking", levels = levels(dat$condition_factor))
    w <- colMeans(model_matrix_for_fit(mod, nd_b)) - colMeans(model_matrix_for_fit(mod, nd_d))
    ct <- linear_combo(mod, w, vc = vc)
    contrast_rows[[length(contrast_rows) + 1]] <- tibble::tibble(
      model = sf,
      term = "bunk_minus_debunk_lin",
      n = nrow(dat),
      estimate = ct$estimate,
      se = ct$std.error,
      conf_low = ct$conf.low,
      conf_high = ct$conf.high,
      p_value = ct$p.value
    )
  }
  rows[[length(rows) + 1]] <- dplyr::bind_rows(belief_rows) |>
    std_row("S1-3", "belief_change", "full_sample")
  rows[[length(rows) + 1]] <- dplyr::bind_rows(contrast_rows) |>
    std_row("S1-3", "belief_bunk_vs_debunk", "full_sample")

  dist_rows <- list()
  for (sf in levels(s13$study_factor)) {
    dat <- s13 |>
      dplyr::filter(study_factor == sf) |>
      tidyr::drop_na(change, condition_factor)
    cb <- dat$change[dat$condition_factor == "Bunking"]
    cd <- dat$change[dat$condition_factor == "Debunking"]
    ks <- suppressWarnings(stats::ks.test(cb, cd))
    ge40 <- suppressWarnings(stats::chisq.test(table(dat$condition_factor, dat$change >= 40)))
    dist_rows[[length(dist_rows) + 1]] <- tibble::tibble(
      model = sf,
      term = "ks_D",
      estimate = unname(ks$statistic),
      statistic = unname(ge40$statistic),
      p_value = ks$p.value,
      note = sprintf(
        ">=40 points: bunk %.1f%% vs debunk %.1f%%; chi-square p=%s",
        100 * mean(cb >= 40), 100 * mean(cd >= 40), fmt_p(ge40$p.value)
      )
    )
  }
  rows[[length(rows) + 1]] <- dplyr::bind_rows(dist_rows) |>
    std_row("S1-3", "distribution_ks", "full_sample")

  deb_rows <- list()
  for (sf in levels(s13$study_factor)) {
    sub <- s13 |>
      dplyr::filter(
        study_factor == sf,
        condition_factor == "Bunking",
        !is.na(belief_rating_debrf_rc)
      )
    shift <- sub$belief_rating_debrf_rc - sub$belief_rating_post_rc
    net <- sub$belief_rating_debrf_rc - sub$belief_rating_pre_rc
    tt <- stats::t.test(shift)
    nt <- stats::t.test(net)
    deb_rows[[length(deb_rows) + 1]] <- tibble::tibble(
      model = sf,
      direction = "Bunking",
      term = "post_to_debrief_shift",
      n = length(shift),
      estimate = unname(tt$estimate),
      conf_low = tt$conf.int[1],
      conf_high = tt$conf.int[2],
      statistic = unname(tt$statistic),
      p_value = tt$p.value,
      note = sprintf("net baseline-to-debrief change=%.3f; p=%s", unname(nt$estimate), fmt_p(nt$p.value))
    )
  }
  rows[[length(rows) + 1]] <- dplyr::bind_rows(deb_rows) |>
    std_row("S1-3", "debrief", "full_sample")

  comp <- s13 |>
    dplyr::group_by(study_factor, condition_factor) |>
    dplyr::summarise(
      n = dplyr::n(),
      attempt_rate = mean(evaluator_label == 1, na.rm = TRUE),
      paired_attempt_rate = mean(evaluator_label == 1 & reverse_evaluator_label == 1, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::transmute(
      model = as.character(study_factor),
      direction = as.character(condition_factor),
      term = "ape_attempt_rate",
      n = n,
      estimate = attempt_rate,
      statistic = paired_attempt_rate,
      note = "statistic column = actual and counterfactual paired compliance rate"
    )
  rows[[length(rows) + 1]] <- comp |> std_row("S1-3", "compliance", "full_sample")

  dplyr::bind_rows(rows)
}

compute_veracity_numbers <- function(paths, s13, d) {
  si_require(c("dplyr", "tidyr", "readr", "stringr", "tibble"))
  labels <- read_claim_labels(paths)
  # Restrict claims to the ANALYTIC samples so no excluded-participant claims bleed
  # in. The cached labels are already analytic-only, but we enforce it in code so the
  # guarantee does not depend on the upstream extraction set.
  analytic_ids <- unique(c(s13$response_id, d$s4$ResponseId))
  labels <- labels |> dplyr::filter(conversation_id %in% analytic_ids)
  conv <- conv_aligned_veracity(labels)
  vtab <- conv |>
    dplyr::filter(!is.na(aligned_veracity)) |>
    dplyr::group_by(study, model, direction) |>
    dplyr::summarise(
      n_conv = dplyr::n(),
      # se BEFORE the mean overwrites `aligned_veracity` with a scalar
      se = stats::sd(aligned_veracity, na.rm = TRUE) / sqrt(n_conv),
      aligned_veracity = mean(aligned_veracity, na.rm = TRUE),
      conf_low = aligned_veracity - stats::qt(.975, pmax(n_conv - 1, 1)) * se,
      conf_high = aligned_veracity + stats::qt(.975, pmax(n_conv - 1, 1)) * se,
      .groups = "drop"
    )

  rows <- list()
  rows[[length(rows) + 1]] <- vtab |>
    dplyr::transmute(
      model = ifelse(study == "Study4", as.character(model),
                     sub("^Study(\\d)$", "Study \\1", as.character(study))),
      direction = as.character(direction),
      term = "aligned_veracity",
      n = n_conv,
      estimate = aligned_veracity,
      se,
      conf_low,
      conf_high,
      note = "Substantive aligned direct+indirect claims; cached API fact-check scores; n = conversations with >=1 aligned claim (veracity aggregated per conversation first)"
    ) |>
    std_row("Veracity", "table_aligned_veracity", "cached_api_claims")

  counts <- labels |>
    dplyr::mutate(is_aligned = aligned_flag(stance_to_focal, directness_to_focal, direction)) |>
    dplyr::group_by(study_source) |>
    dplyr::summarise(
      substantive = dplyr::n(),
      aligned = sum(is_aligned, na.rm = TRUE),
      fact_checked = sum(!is.na(veracity_score), na.rm = TRUE),
      .groups = "drop"
    )
  rows[[length(rows) + 1]] <- counts |>
    dplyr::transmute(
      model = as.character(study_source),
      term = "claim_counts",
      n = substantive,
      estimate = aligned,
      statistic = fact_checked,
      note = "n=substantive claims; estimate=aligned claims; statistic=fact-checked claims"
    ) |>
    std_row("Veracity", "claim_counts", "cached_api_claims")

  dplyr::bind_rows(rows)
}

compute_topic_numbers <- function(paths, s13, d) {
  si_require(c("dplyr", "tidyr", "tibble"))
  mk_text <- function(a, b) dplyr::case_when(
    !is.na(a) & nchar(trimws(a)) > 5 ~ trimws(a),
    !is.na(b) & nchar(trimws(b)) > 5 ~ trimws(b),
    TRUE ~ NA_character_
  )
  corpus <- dplyr::bind_rows(
    s13 |>
      dplyr::transmute(
        response_id,
        study = paste0("S", as.integer(study_factor)),
        variant = as.character(study_factor),
        condition = as.character(condition_factor),
        belief_change = change,
        embed_text = mk_text(con_restatement, con_summary)
      ),
    d$s4 |>
      dplyr::transmute(
        response_id = ResponseId,
        study = "S4",
        variant = as.character(model_pooled),
        condition = ifelse(direction == "bunk", "Bunking", "Debunking"),
        belief_change = aligned_belief_change,
        embed_text = mk_text(conRestatement, conSummary)
      )
  ) |>
    dplyr::filter(!is.na(embed_text))

  tc <- run_topic_clustering(corpus, paths$embed_cache)
  list(
    corpus = corpus,
    topic_solution = tc,
    numbers = tc$topic_eff |>
      dplyr::transmute(
        model = topic,
        direction = condition,
        n = n,
        estimate = mean_change,
        term = "topic_mean_belief_change",
        note = paste0("minPts=", tc$best$minPts, "; retained_clusters=", tc$best$retained_clusters)
      ) |>
      std_row("Topic", "topic_effects", "cached_embeddings")
  )
}

wide_number_table <- function(numbers, section_filter = NULL, block_filter = NULL) {
  out <- numbers
  if (!is.null(section_filter)) out <- dplyr::filter(out, section %in% section_filter)
  if (!is.null(block_filter)) out <- dplyr::filter(out, block %in% block_filter)
  out
}
