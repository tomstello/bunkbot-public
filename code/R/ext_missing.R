# ext_missing.R — two analyses the comments require that compute_s4_numbers does not emit:
#   (1) S3 truth-constraint counterfactual matched-compliance contrast (+ noncompliant S3 bunk cell)
#   (2) Study-4 extensive-margin "would actually post" (within-arm McNemar + between-arm LPM, +/- GPT-5.2)
# Both recomputed from raw+cached via bunkbot_helpers.R; emitted in the canonical 17-column schema.

.em_cols <- c("section","block","sample","outcome","model","direction","term","n",
              "estimate","se","conf_low","conf_high","statistic","df_num","df_den","p_value","note")
.em_row <- function(...) {
  r <- list(...); for (c in setdiff(.em_cols, names(r))) r[[c]] <- NA
  tibble::as_tibble(r)[.em_cols]
}

# (1) ----------------------------------------------------------------------------------
ext_s3_counterfactual <- function(core_objects) {
  s3 <- dplyr::filter(core_objects$s13, study_factor == "Truth-Constrained")
  A <- dplyr::filter(s3, condition == "treatment_mid_bunk",  evaluator_label == 1, reverse_evaluator_label == 1)
  B <- dplyr::filter(s3, condition == "treatment_mid_debunk", evaluator_label == 1, reverse_evaluator_label == 1)
  matched <- dplyr::bind_rows(A, B)
  matched$direction <- factor(matched$direction, levels = c("debunk", "bunk"))
  m  <- stats::lm(change ~ direction + belief_rating_pre_rc, data = matched)
  v  <- sandwich::vcovHC(m, type = "HC3")
  cf <- lmtest::coeftest(m, vcov. = v)["directionbunk", ]
  cfr <- .em_row(section = "S1-3", block = "s3_counterfactual", model = "Truth-Constrained",
                 direction = "bunk_minus_debunk", n = nrow(matched),
                 estimate = cf[["Estimate"]], se = cf[["Std. Error"]],
                 conf_low = cf[["Estimate"]] - stats::qt(0.975, m$df.residual) * cf[["Std. Error"]],
                 conf_high = cf[["Estimate"]] + stats::qt(0.975, m$df.residual) * cf[["Std. Error"]],
                 statistic = cf[["t value"]], p_value = cf[["Pr(>|t|)"]],
                 note = "S3 symmetric matched-compliance counterfactual: both arms restricted to evaluator_label==1 & reverse_evaluator_label==1 (the same dual-compliance rule as the pooled symmetry test); aligned belief change, baseline-adjusted (+ pre), HC3")
  nc  <- dplyr::filter(s3, condition == "treatment_mid_bunk", evaluator_label == 0)
  tt  <- stats::t.test(nc$change)
  ncr <- .em_row(section = "S1-3", block = "noncompliant_s3_bunk", model = "Truth-Constrained",
                 direction = "Bunking", n = nrow(nc), estimate = unname(tt$estimate),
                 conf_low = tt$conf.int[1], conf_high = tt$conf.int[2],
                 statistic = unname(tt$statistic), p_value = tt$p.value,
                 note = "S3 bunking conversations the model did not comply with (first-turn evaluator_label==0); aligned belief change")
  dplyr::bind_rows(cfr, ncr)
}

# (2) ----------------------------------------------------------------------------------
ext_extensive_margin <- function(pkg_root) {
  paths <- pkg_paths(pkg_root)
  d <- build_s4_data(paths)
  sp <- d$s4 |>
    dplyr::mutate(sp_pre = as.numeric(share_pre_4), sp_post = as.numeric(share_post_4),
                  Condition = ifelse(direction == "bunk", "Bunking", "Debunking")) |>
    dplyr::filter(!is.na(sp_pre), !is.na(sp_post), !is.na(pre_direction_score), !is.na(post_direction_score)) |>
    dplyr::mutate(pre_pro = sp_pre > 50 & pre_direction_score > 50, post_pro = sp_post > 50 & post_direction_score > 50,
                  pre_anti = sp_pre > 50 & pre_direction_score < 50, post_anti = sp_post > 50 & post_direction_score < 50,
                  pre_al = ifelse(direction == "bunk", pre_pro, pre_anti),
                  post_al = ifelse(direction == "bunk", post_pro, post_anti))
  extfun <- function(df, pre, post) {
    pr <- df[[pre]]; po <- df[[post]]; tb <- table(factor(pr, c(FALSE, TRUE)), factor(po, c(FALSE, TRUE)))
    p <- tryCatch(stats::mcnemar.test(tb)$p.value, error = function(e) NA_real_)
    tibble::tibble(n = nrow(df), pre_pct = 100 * mean(pr), post_pct = 100 * mean(po),
                   net_pp = 100 * (mean(po) - mean(pr)),
                   newly_yes = as.integer(tb["FALSE", "TRUE"]), newly_no = as.integer(tb["TRUE", "FALSE"]), mcnemar_p = p)
  }
  ext <- dplyr::bind_rows(
    sp |> dplyr::group_by(Condition) |> dplyr::group_modify(~ extfun(.x, "pre_pro", "post_pro")) |> dplyr::mutate(outcome = "pro_public_posting"),
    sp |> dplyr::group_by(Condition) |> dplyr::group_modify(~ extfun(.x, "pre_anti", "post_anti")) |> dplyr::mutate(outcome = "anti_public_posting"),
    sp |> dplyr::group_by(Condition) |> dplyr::group_modify(~ extfun(.x, "pre_al", "post_al")) |> dplyr::mutate(outcome = "aligned_public_posting")
  ) |> dplyr::ungroup()
  rows_within <- ext |> dplyr::transmute(
    section = "Public-posting", block = "extensive_margin", sample = "strict_n1272", outcome,
    model = NA, direction = Condition, term = "net_pp_change", n,
    estimate = net_pp, se = NA, conf_low = NA, conf_high = NA, statistic = NA, df_num = NA, df_den = NA,
    p_value = mcnemar_p,
    note = sprintf("share>50 & stance-aligned; within-arm McNemar; pre=%.1f%% post=%.1f%%; newly_yes=%d newly_no=%d", pre_pct, post_pct, newly_yes, newly_no))
  # per-model within-arm McNemar (direction-aligned posting), broken out by Study-4 model
  ext_by_model <- sp |>
    dplyr::group_by(model_pooled = as.character(model_pooled), Condition) |>
    dplyr::group_modify(~ extfun(.x, "pre_al", "post_al")) |>
    dplyr::ungroup() |>
    dplyr::mutate(outcome = "aligned_public_posting")
  rows_within_model <- ext_by_model |> dplyr::transmute(
    section = "Public-posting", block = "extensive_margin", sample = "strict_n1272", outcome,
    model = model_pooled, direction = Condition, term = "net_pp_change", n,
    estimate = net_pp, se = NA, conf_low = NA, conf_high = NA, statistic = NA, df_num = NA, df_den = NA,
    p_value = mcnemar_p,
    note = sprintf("Per-model direction-aligned posting; within-arm McNemar; pre=%.1f%% post=%.1f%%; newly_yes=%d newly_no=%d", pre_pct, post_pct, newly_yes, newly_no))
  # between-arm LPM (equal model weights, HC3), all four models and excluding GPT-5.2
  sp2 <- sp |> dplyr::mutate(d_pro = as.numeric(post_pro) - as.numeric(pre_pro),
                             d_anti = as.numeric(post_anti) - as.numeric(pre_anti),
                             direction = factor(direction, levels = c("debunk", "bunk")),
                             model_pooled = factor(model_pooled, levels = model_order_s4))
  lpm_row <- function(dat, oc, models, term, note) {
    m <- stats::lm(stats::reformulate("direction * model_pooled", oc), data = dat)
    lc <- direction_contrast_equal_weighted(m, models, vc = fit_vcov(m))
    .em_row(section = "Public-posting", block = "extensive_margin_contrast", sample = "strict_n1272",
            outcome = oc, term = term, n = nrow(dat),
            estimate = 100 * lc$estimate, conf_low = 100 * lc$conf.low, conf_high = 100 * lc$conf.high,
            p_value = lc$p.value, note = note)
  }
  sp_x <- sp2 |> dplyr::filter(model_pooled != "GPT-5.2") |> dplyr::mutate(model_pooled = droplevels(model_pooled))
  rows_contrast <- dplyr::bind_rows(
    lpm_row(sp2, "d_pro",  model_order_s4, "bunk_minus_debunk_pp", "LPM of delta(would post pro-conspiracy); equal model weights, HC3, all four models"),
    lpm_row(sp2, "d_anti", model_order_s4, "bunk_minus_debunk_pp_anti", "LPM of delta(would post anti-conspiracy); equal model weights, HC3, all four models"),
    lpm_row(sp_x, "d_pro", setdiff(model_order_s4, "GPT-5.2"), "bunk_minus_debunk_pp_excl_gpt52", "LPM of delta(would post pro-conspiracy); equal weights, HC3, excluding GPT-5.2 (its bunking arm argued against the conspiracy)")
  )
  # per-model between-arm LPM contrast on direction-aligned posting (bunk minus debunk)
  sp2 <- sp2 |> dplyr::mutate(d_al = as.numeric(post_al) - as.numeric(pre_al))
  per_model_lpm <- function(mdl) {
    dat <- sp2 |> dplyr::filter(model_pooled == mdl)
    m <- stats::lm(d_al ~ direction, data = dat)
    vc <- sandwich::vcovHC(m, type = "HC3")
    cf <- lmtest::coeftest(m, vcov. = vc)["directionbunk", ]
    .em_row(section = "Public-posting", block = "extensive_margin_contrast", sample = "strict_n1272",
            outcome = "aligned_public_posting", model = mdl, term = "bunk_minus_debunk_pp", n = nrow(dat),
            estimate = 100 * cf[["Estimate"]],
            conf_low = 100 * (cf[["Estimate"]] - 1.96 * cf[["Std. Error"]]),
            conf_high = 100 * (cf[["Estimate"]] + 1.96 * cf[["Std. Error"]]),
            statistic = cf[["t value"]], p_value = cf[["Pr(>|t|)"]],
            note = sprintf("Per-model LPM of delta(direction-aligned posting); bunk minus debunk, HC3 (%s)", mdl))
  }
  rows_contrast_model <- dplyr::bind_rows(lapply(levels(sp2$model_pooled), per_model_lpm))
  dplyr::bind_rows(rows_within, rows_within_model, rows_contrast, rows_contrast_model)
}

ext_missing_numbers <- function(core_objects) {
  dplyr::bind_rows(
    ext_s3_counterfactual(core_objects),
    ext_extensive_margin(core_objects$pkg_root)
  )
}
