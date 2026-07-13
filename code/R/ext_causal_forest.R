# ext_causal_forest.R
# =============================================================================
# Pooled (Studies 1-4) CAUSAL-FOREST heterogeneity / moderation, wired into the
# live build_all_numbers() engine so the SI prose, tables, and figures all derive
# from one recompute. This is the engine-resident twin of the standalone
# code/causal_forest_moderation.R (same estimand, moderators, seed, and tuning);
# keep the two in sync.
#
# ENTRY POINTS
#   fit_causal_forest(core_objects)        -> rich list (memoized per session):
#       dat (per-row tau_hat), ate, calibration, autoc(+toc), quartiles,
#       by_stratum, blp_person, blp_context, importance, perarm, robustness.
#       Consumed by compute_causal_forest_numbers() AND by the fig_cf_* figures.
#   compute_causal_forest_numbers(core)    -> tibble in the canonical 17-col schema
#       (section = "Causal forest"). Registered in build_all_numbers().
#
# ESTIMAND. No study has a no-persuasion control arm, so the causal contrast is
#   bunk (W=1) vs debunk (W=0); outcome Y = raw reverse-coded change toward the
#   conspiracy (= aligned_belief_change * sign(arm)). The CATE is the bunk-debunk
#   "swing" = a person's total persuadability; its ATE ~ the pooled bunk-debunk
#   gap. (Aligned change cannot be Y: it already encodes the arm in its sign.)
#   Moderators are harmonized + z-scored within study/model; GCBS is S1-3 only and
#   handled by grf's native missing-attribute splitting. Study/model strata enter
#   as one-hot context covariates. See the causal-forest section of the SI
#   (supplement/sections/05_causal_forest.Rmd).
# =============================================================================

CF_SEED    <- 20260629L
CF_N_TREES <- 4000L
CF_SAMPLE  <- "compliant_pooled"   # pooled S1-4 compliant; sample id for num()

# ---- small local helpers (cf-prefixed to avoid clashing with ext_moderators) -
.cf_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
.cf_z <- function(x) {
  x <- .cf_num(x); s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}
.cf_clean_age <- function(x) { a <- .cf_num(x); a[!is.finite(a) | a < 13 | a > 100] <- NA_real_; a }
.cf_gcbs_pre <- function(df) {
  gp <- c("x2_gcbs_pre","x4_gcbs_pre","x6_gcbs_pre","x8_gcbs_pre",
          "x10_gcbs_pre","x12_gcbs_pre","x14_gcbs_pre")
  m <- sapply(gp, function(c) .cf_num(df[[c]]))
  ifelse(rowSums(is.na(m)) == 0, rowMeans(m), NA_real_)
}
# race multi-select -> White selected? (single-digit categories 1-6; S4 "1,3", S1-3 "13")
.cf_parse_white <- function(race) {
  d <- gsub("[^0-9]", "", as.character(race))
  ifelse(is.na(race) | d == "", NA_integer_, as.integer(grepl("1", d)))
}
.cf_to_female <- function(g) { g <- .cf_num(g); ifelse(g == 2, 1L, ifelse(g == 1, 0L, NA_integer_)) }
.cf_tidy_ct <- function(m) {                         # grf coeftest -> tidy df (+95% CI)
  m <- unclass(m)
  data.frame(term = rownames(m), estimate = m[, 1], se = m[, 2],
             statistic = m[, 3], p_value = m[, 4],
             conf_low = m[, 1] - 1.96 * m[, 2], conf_high = m[, 1] + 1.96 * m[, 2],
             row.names = NULL, stringsAsFactors = FALSE)
}
.cf_ate <- function(a) c(estimate = unname(a["estimate"]), se = unname(a["std.err"]))

CF_MODS_CONT <- c("baseline_belief","ai_trust","ai_familiarity","ai_use",
                  "pol_demrep","social_conservatism","economic_conservatism",
                  "education","age","religiosity")
CF_MODS_BIN  <- c("female","white")

# pretty labels for figures / tables
cf_mod_labels <- c(
  baseline_belief = "Baseline belief", ai_trust = "AI trust",
  ai_familiarity = "AI familiarity", ai_use = "AI use",
  pol_demrep = "Party (Dem→Rep)", social_conservatism = "Social ideology (Cons→Lib)",
  economic_conservatism = "Economic ideology (Cons→Lib)", education = "Education",
  age = "Age", religiosity = "Religiosity", female = "Female (vs male)",
  white = "White (vs other)", gcbs = "Conspiracy mentality (GCBS)"
)
cf_strata <- c("Jailbroken","Standard","Truth-Constrained","Claude","Gemini","GPT-5.2","Grok")

# conspiracy topic (canonical 18-topic assignments; joined by response_id). Topics
# with >= CF_TOPIC_MIN_X conversations enter the forest as one-hot covariates; the
# rest (and the "Mixed / Unclassified" catch-all) are lumped. Topics with
# >= CF_TOPIC_MIN_REPORT conversations are reported in the by-topic tables.
CF_TOPIC_MIN_X      <- 40L
CF_TOPIC_MIN_REPORT <- 60L
CF_TOPIC_DROP       <- c("Unclassified", "Other")   # not substantive topics

# ---- memoized fit -----------------------------------------------------------
.cf_memo <- new.env(parent = emptyenv())

fit_causal_forest <- function(core_objects = NULL, force = FALSE) {
  if (!force && !is.null(.cf_memo$fit)) return(.cf_memo$fit)
  if (!requireNamespace("grf", quietly = TRUE))
    stop("Package 'grf' is required for the causal-forest module.", call. = FALSE)
  if (is.null(core_objects)) core_objects <- get("core_objects", envir = globalenv())

  s13 <- dplyr::filter(core_objects$s13, .data$compliant %in% TRUE)
  s4  <- core_objects$s4$s4_compliant

  mk <- function(df, rid, study_model, gcbs, baseline, demrep, soc, econ, edu, age, relig, gender, race) tibble::tibble(
    response_id = as.character(rid),
    study_model = as.character(study_model), direction = as.character(df$direction),
    aligned = .cf_num(df$aligned_belief_change), baseline_belief = .cf_num(baseline),
    ai_trust = .cf_num(df$genai_trust), ai_familiarity = .cf_num(df$genai_fam_1),
    ai_use = .cf_num(df$genai_use_1), pol_demrep = .cf_num(demrep),
    social_conservatism = .cf_num(soc), economic_conservatism = .cf_num(econ),
    education = .cf_num(edu), age = .cf_clean_age(age), religiosity = .cf_num(relig),
    female = .cf_to_female(gender), white = .cf_parse_white(race), gcbs = gcbs
  )
  s13m <- mk(s13, s13$response_id, s13$study_factor, .cf_gcbs_pre(s13), s13$belief_rating_pre_rc,
             s13$dem_rep_c, s13$social_conservatism, s13$economic_conservatism,
             s13$education, s13$age_2, s13$religion, s13$gender_2, s13$race)
  s4m  <- mk(s4, s4$ResponseId, s4$model_pooled, NA_real_, s4$belief_rating_pre_4,
             s4$DemRep_C, s4$SocialConservatism, s4$EconomicConservatism,
             s4$Education, s4$age, s4$religion, s4$gender, s4$Race)

  dat <- dplyr::bind_rows(s13m, s4m)
  dat <- dat[dat$direction %in% c("bunk","debunk") & !is.na(dat$aligned), , drop = FALSE]
  dat$study_model <- factor(dat$study_model, levels = cf_strata)
  dat$W <- as.integer(dat$direction == "bunk")
  dat$Y <- dat$aligned * ifelse(dat$direction == "bunk", 1, -1)

  # ---- conspiracy topic (join by response_id; lump small/unclassified) --------
  repo <- if (!is.null(core_objects$repo_root)) core_objects$repo_root else "."
  tpath <- file.path(repo, "data", "api_cached", "topic_modeling", "topic_assignments.csv")
  ta <- tryCatch(if (file.exists(tpath)) readr::read_csv(tpath, show_col_types = FALSE) else NULL,
                 error = function(e) NULL)
  dat$topic_raw <- if (!is.null(ta)) ta$topic[match(dat$response_id, ta$response_id)] else NA_character_
  dat$topic_raw[is.na(dat$topic_raw) | dat$topic_raw == "Mixed / Unclassified"] <- "Unclassified"
  tcnt <- table(dat$topic_raw)
  keep_t <- setdiff(names(tcnt)[tcnt >= CF_TOPIC_MIN_X], "Unclassified")
  dat$topic_f <- ifelse(dat$topic_raw %in% keep_t, dat$topic_raw,
                        ifelse(dat$topic_raw == "Unclassified", "Unclassified", "Other"))
  dat$topic_f <- factor(dat$topic_f)

  # z within study/model
  for (m in c(CF_MODS_CONT)) {
    dat[[paste0(m, "_z")]] <- ave(dat[[m]], dat$study_model, FUN = .cf_z)
  }
  zc <- paste0(CF_MODS_CONT, "_z")
  dat[zc] <- lapply(dat[zc], function(v) ifelse(is.na(v), 0, v))           # residual NA -> 0 (= mean)
  for (b in CF_MODS_BIN) dat[[b]] <- ifelse(is.na(dat[[b]]), mean(dat[[b]], na.rm = TRUE), dat[[b]])
  gcbs_z <- ave(dat$gcbs, dat$study_model, FUN = .cf_z)                    # NA for S4 (kept NA)

  person_cols <- c(zc, CF_MODS_BIN)
  X_person <- as.matrix(cbind(dat[person_cols], gcbs_z = gcbs_z))
  sm_oh <- stats::model.matrix(~ study_model - 1, data = dat)
  colnames(sm_oh) <- sub("^study_model", "ctx_", colnames(sm_oh))
  topic_oh <- stats::model.matrix(~ topic_f - 1, data = dat)
  colnames(topic_oh) <- paste0("topic_", sub("^topic_f", "", colnames(topic_oh)))
  X_full <- cbind(X_person, sm_oh, topic_oh)        # traits + study/model + topic
  X_common <- as.matrix(dat[person_cols])
  Y <- dat$Y; W <- dat$W
  What <- as.numeric(ave(W, dat$study_model, FUN = mean))

  set.seed(CF_SEED)
  cf <- grf::causal_forest(X_full, Y, W, W.hat = What, num.trees = CF_N_TREES,
                           honesty = TRUE, seed = CF_SEED)
  dat$tau_hat <- as.numeric(cf$predictions)

  # ---- per-person DIRECTION FAVORED (per-arm regression forests) --------------
  # Predict each person's ALIGNED change under bunking (from a forest trained on the
  # bunk arm) and under debunking (from the debunk arm), then compare. This is the
  # per-person analogue of the observed arm-aligned means and is robust in small/
  # outlier strata (e.g. GPT-5.2, where bunking backfires) -- unlike reconstructing
  # potential outcomes from the pooled causal forest, whose marginal nuisance is
  # smoothed toward the pool. DESCRIPTIVE (predictive), not a within-person causal
  # contrast. dir_margin > 0 -> bunking is the stronger lever for that person.
  set.seed(CF_SEED)
  rf_b <- grf::regression_forest(X_full[W == 1, , drop = FALSE], dat$aligned[W == 1],
                                 num.trees = CF_N_TREES, seed = CF_SEED)
  rf_d <- grf::regression_forest(X_full[W == 0, , drop = FALSE], dat$aligned[W == 0],
                                 num.trees = CF_N_TREES, seed = CF_SEED)
  dat$bunk_aligned   <- as.numeric(predict(rf_b, X_full)$predictions)
  dat$debunk_aligned <- as.numeric(predict(rf_d, X_full)$predictions)
  dat$dir_margin     <- dat$bunk_aligned - dat$debunk_aligned
  dat$dir_favored    <- ifelse(dat$dir_margin > 0, "Bunking", "Debunking")

  ate <- .cf_ate(grf::average_treatment_effect(cf, target.sample = "all"))
  simple_gap <- mean(Y[W == 1]) - mean(Y[W == 0])
  m_al_b <- mean(dat$aligned[W == 1]); m_al_d <- mean(dat$aligned[W == 0])
  calibration <- .cf_tidy_ct(grf::test_calibration(cf))

  # honest-split AUTOC
  autoc <- tryCatch({
    set.seed(CF_SEED); n <- nrow(X_full); tr <- sample(n, floor(n/2)); ev <- setdiff(seq_len(n), tr)
    cft <- grf::causal_forest(X_full[tr, ], Y[tr], W[tr], W.hat = What[tr], num.trees = CF_N_TREES, seed = CF_SEED)
    tev <- predict(cft, X_full[ev, ])$predictions
    cfe <- grf::causal_forest(X_full[ev, ], Y[ev], W[ev], W.hat = What[ev], num.trees = CF_N_TREES, seed = CF_SEED)
    r <- grf::rank_average_treatment_effect(cfe, tev, target = "AUTOC")
    list(estimate = r$estimate, se = r$std.err, toc = as.data.frame(r$TOC))
  }, error = function(e) { warning("CF AUTOC failed: ", conditionMessage(e)); NULL })

  # AIPW swing by CATE quartile
  qs <- cut(dat$tau_hat, stats::quantile(dat$tau_hat, 0:4/4), include.lowest = TRUE, labels = paste0("Q", 1:4))
  quartiles <- do.call(rbind, lapply(levels(qs), function(g) {
    a <- .cf_ate(grf::average_treatment_effect(cf, subset = qs == g))
    data.frame(quartile = g, n = sum(qs == g), mean_tau = mean(dat$tau_hat[qs == g]),
               estimate = a["estimate"], se = a["se"], row.names = NULL)
  }))

  # AIPW swing by study/model (+ per-person share favoring bunking)
  by_stratum <- do.call(rbind, lapply(levels(dat$study_model), function(g) {
    idx <- dat$study_model == g
    if (sum(idx) < 20 || length(unique(W[idx])) < 2) return(NULL)
    a <- .cf_ate(grf::average_treatment_effect(cf, subset = idx))
    data.frame(study_model = g, n = sum(idx),
               mean_aligned_bunk = mean(dat$aligned[idx & W == 1]),
               mean_aligned_debunk = mean(dat$aligned[idx & W == 0]),
               pct_favor_bunk = mean(dat$dir_favored[idx] == "Bunking"),
               estimate = a["estimate"], se = a["se"], row.names = NULL)
  }))

  # AIPW swing by conspiracy topic (substantive topics only) + direction
  rep_topics <- setdiff(levels(dat$topic_f), CF_TOPIC_DROP)
  by_topic <- do.call(rbind, lapply(rep_topics, function(g) {
    idx <- dat$topic_f == g
    if (sum(idx) < CF_TOPIC_MIN_REPORT || length(unique(W[idx])) < 2) return(NULL)
    a <- .cf_ate(grf::average_treatment_effect(cf, subset = idx))
    data.frame(topic = g, n = sum(idx),
               bunk_aligned = mean(dat$aligned[idx & W == 1]),     # observed arm means
               debunk_aligned = mean(dat$aligned[idx & W == 0]),
               pct_favor_bunk = mean(dat$dir_favored[idx] == "Bunking"),  # per-person share
               estimate = a["estimate"], se = a["se"], row.names = NULL)
  }))

  # per-person direction-favored, overall + by study/model. Arm-aligned columns are
  # OBSERVED arm means (accurate at group level); pct_favor_bunk is the per-person
  # share for whom the bunk-arm forest predicts a larger aligned effect than debunk.
  dir_grp <- function(idx) data.frame(
    n = sum(idx), pct_favor_bunk = mean(dat$dir_favored[idx] == "Bunking"),
    bunk_aligned = mean(dat$aligned[idx & W == 1]),
    debunk_aligned = mean(dat$aligned[idx & W == 0]))
  direction <- rbind(
    cbind(group = "Pooled", dir_grp(rep(TRUE, nrow(dat)))),
    do.call(rbind, lapply(levels(dat$study_model), function(g)
      cbind(group = g, dir_grp(dat$study_model == g))))
  )

  # BLP: person (common), context (+ design dummies), and S1-3 incl gcbs
  blp_person <- .cf_tidy_ct(grf::best_linear_projection(cf, A = X_person[, person_cols, drop = FALSE]))
  A_design <- cbind(X_person[, person_cols, drop = FALSE],
                    sm_oh[, setdiff(colnames(sm_oh), "ctx_Standard"), drop = FALSE])
  blp_context <- .cf_tidy_ct(grf::best_linear_projection(cf, A = A_design))
  s13_idx <- which(!is.na(gcbs_z))
  blp_s13 <- .cf_tidy_ct(grf::best_linear_projection(
    cf, A = cbind(X_person[, person_cols, drop = FALSE], gcbs_z = gcbs_z)[s13_idx, , drop = FALSE],
    subset = s13_idx))

  .imp_kind <- function(v) ifelse(grepl("^ctx_", v), "context",
                                  ifelse(grepl("^topic_", v), "topic", "person"))
  importance <- data.frame(variable = colnames(X_full),
                           importance = as.numeric(grf::variable_importance(cf)),
                           kind = .imp_kind(colnames(X_full)),
                           row.names = NULL)
  importance <- importance[order(-importance$importance), ]

  # per-arm descriptive OLS of aligned change (HC3 if sandwich present)
  perarm <- do.call(rbind, lapply(c("bunk","debunk"), function(arm) {
    idx <- dat$direction == arm
    d_arm <- as.data.frame(cbind(aligned = dat$aligned[idx], dat[idx, person_cols]))
    fit <- stats::lm(aligned ~ ., data = d_arm)
    vc <- if (requireNamespace("sandwich", quietly = TRUE)) sandwich::vcovHC(fit, type = "HC3") else stats::vcov(fit)
    co <- summary(fit)$coefficients; se <- sqrt(diag(vc))
    data.frame(arm = arm, term = rownames(co), estimate = co[, 1], se = se[rownames(co)],
               statistic = co[, 1] / se[rownames(co)],
               p_value = 2 * stats::pnorm(-abs(co[, 1] / se[rownames(co)])), row.names = NULL)
  }))

  # robustness: common covariates only (no gcbs, no context)
  set.seed(CF_SEED)
  cfc <- grf::causal_forest(X_common, Y, W, W.hat = What, num.trees = CF_N_TREES, honesty = TRUE, seed = CF_SEED)
  robustness <- list(ate = .cf_ate(grf::average_treatment_effect(cfc)),
                     calibration = .cf_tidy_ct(grf::test_calibration(cfc)))

  # ---- separate forests by study group ---------------------------------------
  # Refit WITHIN Studies 1-3 (GPT-4o; GCBS fully observed -> no missingness/study
  # confound) and WITHIN Study 4 (frontier models; no GCBS). Clarifies which
  # moderators hold inside each group. Reuses the within-stratum z-scores.
  .cf_subgroup <- function(rows) {
    d <- dat[rows, , drop = FALSE]; g <- gcbs_z[rows]
    sm <- droplevels(d$study_model); tf <- droplevels(d$topic_f)
    has_gcbs <- !all(is.na(g)); g[is.na(g)] <- 0
    Pm <- as.matrix(d[person_cols]); pc <- person_cols
    if (has_gcbs) { Pm <- cbind(Pm, gcbs_z = g); pc <- c(person_cols, "gcbs_z") }
    ctx <- stats::model.matrix(~ sm - 1); colnames(ctx) <- sub("^sm", "ctx_", colnames(ctx))
    top <- stats::model.matrix(~ tf - 1); colnames(top) <- paste0("topic_", sub("^tf", "", colnames(top)))
    X <- cbind(Pm, ctx, top)
    Wh <- as.numeric(ave(d$W, sm, FUN = mean))
    set.seed(CF_SEED)
    cfx <- grf::causal_forest(X, d$Y, d$W, W.hat = Wh, num.trees = CF_N_TREES, honesty = TRUE, seed = CF_SEED)
    ref <- if ("ctx_Standard" %in% colnames(ctx)) "ctx_Standard" else colnames(ctx)[1]
    ctx_keep <- setdiff(colnames(ctx), ref)
    cb <- .cf_tidy_ct(grf::best_linear_projection(cfx, A = cbind(Pm[, pc, drop = FALSE], ctx[, ctx_keep, drop = FALSE])))
    cb <- cb[grepl("^ctx_", cb$term), , drop = FALSE]; cb$term <- sub("^ctx_", "", ctx_keep)  # canonical, positional
    imp <- data.frame(variable = colnames(X), importance = as.numeric(grf::variable_importance(cfx)),
                      kind = .imp_kind(colnames(X)), row.names = NULL)
    list(n = nrow(d),
         ate = .cf_ate(grf::average_treatment_effect(cfx, target.sample = "all")),
         calibration = .cf_tidy_ct(grf::test_calibration(cfx)),
         blp_person = .cf_tidy_ct(grf::best_linear_projection(cfx, A = Pm[, pc, drop = FALSE])),
         blp_context = cb, importance = imp[order(-imp$importance), ],
         ref_stratum = sub("^ctx_", "", ref))
  }
  subgroups <- list(s1s3 = .cf_subgroup(dat$study_model %in% cf_strata[1:3]),
                    s4   = .cf_subgroup(dat$study_model %in% cf_strata[4:7]))

  out <- list(
    dat = dat, n = nrow(dat),
    n_s13 = sum(dat$study_model %in% cf_strata[1:3]),
    n_s4 = sum(dat$study_model %in% cf_strata[4:7]),
    n_bunk = sum(W == 1), n_debunk = sum(W == 0),
    ate = ate, simple_gap = simple_gap, mean_aligned_bunk = m_al_b, mean_aligned_debunk = m_al_d,
    calibration = calibration, autoc = autoc, quartiles = quartiles, by_stratum = by_stratum,
    by_topic = by_topic, direction = direction,
    pct_favor_bunk = mean(dat$dir_favored == "Bunking"),
    blp_person = blp_person, blp_context = blp_context, blp_s13 = blp_s13,
    importance = importance, perarm = perarm, robustness = robustness,
    subgroups = subgroups
  )
  .cf_memo$fit <- out
  out
}

# ---- canonical-schema numbers ----------------------------------------------
compute_causal_forest_numbers <- function(core_objects) {
  S <- "Causal forest"
  empty <- function() { e <- as.data.frame(matrix(nrow = 0, ncol = length(std_cols))); names(e) <- std_cols; e }
  f <- tryCatch(fit_causal_forest(core_objects), error = function(e) {
    warning("compute_causal_forest_numbers skipped: ", conditionMessage(e)); NULL })
  if (is.null(f)) return(empty())

  rows <- list()
  add <- function(df, block, ...) {
    extra <- list(...)
    for (k in names(extra)) df[[k]] <- extra[[k]]
    rows[[length(rows) + 1]] <<- std_row(df, S, block, CF_SAMPLE)
  }

  # 0. sample sizes (queryable counts)
  add(data.frame(term = c("n_total","n_s13","n_s4","n_bunk","n_debunk"),
                 estimate = c(f$n, f$n_s13, f$n_s4, f$n_bunk, f$n_debunk),
                 n = c(f$n, f$n_s13, f$n_s4, f$n_bunk, f$n_debunk)),
      "cf_sample")
  # 1. overall ATE (swing) + descriptive anchors
  add(data.frame(term = c("ATE","simple_gap","mean_aligned_bunk","mean_aligned_debunk"),
                 estimate = c(f$ate["estimate"], f$simple_gap, f$mean_aligned_bunk, f$mean_aligned_debunk),
                 se = c(f$ate["se"], NA, NA, NA), n = f$n),
      "cf_ate", outcome = "swing")
  # 2. calibration
  add(f$calibration, "cf_calibration", outcome = "swing")
  # 3. AUTOC
  if (!is.null(f$autoc))
    add(data.frame(term = "AUTOC", estimate = f$autoc$estimate, se = f$autoc$se,
                   statistic = f$autoc$estimate / f$autoc$se), "cf_autoc", outcome = "swing")
  # 4. CATE quartiles
  add(data.frame(term = f$quartiles$quartile, estimate = f$quartiles$estimate, se = f$quartiles$se,
                 n = f$quartiles$n, statistic = f$quartiles$mean_tau,
                 conf_low = f$quartiles$estimate - 1.96*f$quartiles$se,
                 conf_high = f$quartiles$estimate + 1.96*f$quartiles$se),
      "cf_cate_quartile", outcome = "swing")
  # 5. BLP person (common moderators) + gcbs row from the S1-3 BLP
  bp <- f$blp_person[f$blp_person$term != "(Intercept)", ]
  gc <- f$blp_s13[f$blp_s13$term == "gcbs_z", ]; if (nrow(gc)) gc$term <- "gcbs"
  bp <- rbind(bp, gc)
  bp$note <- ifelse(bp$term == "gcbs", "S1-3 only (grf MIA)", NA_character_)
  bp$term <- sub("_z$", "", bp$term)
  add(bp, "cf_blp_person", outcome = "swing")
  # 6. BLP context (study/model, ref = Standard). lm() inside best_linear_projection
  # sanitizes "Truth-Constrained"/"GPT-5.2" to dotted names; the ctx_ rows are in
  # factor-level order, so restore canonical labels positionally.
  bc <- f$blp_context[grepl("^ctx_", f$blp_context$term), ]
  bc$term <- setdiff(cf_strata, "Standard")
  add(bc, "cf_blp_context", outcome = "swing", note = "ref = Standard GPT-4o")
  # 7. variable importance
  add(data.frame(term = sub("^ctx_", "", sub("_z$", "", f$importance$variable)),
                 estimate = f$importance$importance, note = f$importance$kind),
      "cf_importance", outcome = "swing")
  # 8. AIPW swing by study/model
  add(data.frame(model = f$by_stratum$study_model, term = "swing", estimate = f$by_stratum$estimate,
                 se = f$by_stratum$se, n = f$by_stratum$n,
                 conf_low = f$by_stratum$estimate - 1.96*f$by_stratum$se,
                 conf_high = f$by_stratum$estimate + 1.96*f$by_stratum$se,
                 note = sprintf("aligned bunk=%.1f, debunk=%.1f", f$by_stratum$mean_aligned_bunk,
                                f$by_stratum$mean_aligned_debunk)),
      "cf_swing_by_stratum", outcome = "swing")
  # 9. per-arm descriptive OLS (aligned change)
  pa <- f$perarm[f$perarm$term != "(Intercept)", ]
  pa$direction <- ifelse(pa$arm == "bunk", "Bunking", "Debunking")
  pa$term <- sub("_z$", "", pa$term)
  pa$conf_low <- pa$estimate - 1.96*pa$se; pa$conf_high <- pa$estimate + 1.96*pa$se
  add(pa[, c("term","direction","estimate","se","statistic","p_value","conf_low","conf_high")],
      "cf_perarm", outcome = "aligned_change")
  # 10. robustness (common covariates only)
  rb <- rbind(data.frame(term = "ATE", estimate = f$robustness$ate["estimate"], se = f$robustness$ate["se"],
                         statistic = NA, p_value = NA, conf_low = NA, conf_high = NA),
              f$robustness$calibration[, c("term","estimate","se","statistic","p_value","conf_low","conf_high")])
  add(rb, "cf_robustness", outcome = "swing")
  # 11. direction favored (per person, overall + by study/model): predicted aligned
  #     effect of each arm and the share of people for whom bunking is the stronger lever
  dirdf <- do.call(rbind, lapply(seq_len(nrow(f$direction)), function(i) {
    r <- f$direction[i, ]
    data.frame(model = r$group, n = r$n,
               term = c("pct_favor_bunk","bunk_aligned","debunk_aligned"),
               estimate = c(r$pct_favor_bunk, r$bunk_aligned, r$debunk_aligned))
  }))
  add(dirdf, "cf_direction", outcome = "swing")
  # 12. swing + direction by conspiracy topic
  if (!is.null(f$by_topic) && nrow(f$by_topic)) {
    bt <- do.call(rbind, lapply(seq_len(nrow(f$by_topic)), function(i) {
      r <- f$by_topic[i, ]
      data.frame(model = r$topic, n = r$n,
                 term = c("swing","pct_favor_bunk","bunk_aligned","debunk_aligned"),
                 estimate = c(r$estimate, r$pct_favor_bunk, r$bunk_aligned, r$debunk_aligned),
                 se = c(r$se, NA, NA, NA),
                 conf_low = c(r$estimate - 1.96*r$se, NA, NA, NA),
                 conf_high = c(r$estimate + 1.96*r$se, NA, NA, NA))
    }))
    add(bt, "cf_swing_by_topic", outcome = "swing")
  }
  # 13. separate forests by study group (S1-3 fully observes GCBS; S4 has no GCBS)
  for (sg in names(f$subgroups)) {
    s <- f$subgroups[[sg]]
    samp <- if (sg == "s1s3") "compliant_s1s3" else "compliant_s4"
    add(data.frame(term = "ATE", estimate = s$ate["estimate"], se = s$ate["se"], n = s$n,
                   sample = samp), "cf_sub_ate", outcome = "swing")
    cal <- s$calibration; cal$sample <- samp
    add(cal, "cf_sub_calibration", outcome = "swing")
    bp <- s$blp_person[s$blp_person$term != "(Intercept)", ]
    bp$term <- sub("_z$", "", bp$term); bp$sample <- samp
    bp$note <- ifelse(bp$term == "gcbs", "observed within Studies 1-3", NA_character_)
    add(bp, "cf_sub_blp_person", outcome = "swing")
    bc <- s$blp_context; bc$sample <- samp
    add(bc, "cf_sub_blp_context", outcome = "swing", note = paste0("ref = ", s$ref_stratum))
    imp <- utils::head(s$importance, 12)
    add(data.frame(term = sub("^ctx_", "", sub("^topic_", "", sub("_z$", "", imp$variable))),
                   estimate = imp$importance, note = imp$kind, sample = samp),
        "cf_sub_importance", outcome = "swing")
  }

  out <- dplyr::bind_rows(rows)
  out[, std_cols]
}
