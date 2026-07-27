# ext_s4_chat_started.R =======================================================
# Study 4 missing-outcome robustness conditional on confirmed chatbot delivery.
#
# Estimand: the 1,353 eligible records with a complete outcome page OR a stored
# initial assistant message (1,272 + 81). This is not assignment-level ITT.
# Four approaches are emitted in separate structured blocks:
#   available_outcomes, zero_change, mi_mar, and support_bounds.
# ============================================================================

.s4cs_hc3 <- function(fit) sandwich::vcovHC(fit, type = "HC3")

.s4cs_num_complete <- function(x) {
  z <- suppressWarnings(as.numeric(as.character(x)))
  good <- is.finite(z)
  fill <- if (any(good)) stats::median(z[good]) else 0
  z[!good] <- fill
  z
}

.s4cs_fac_complete <- function(x) {
  z <- trimws(as.character(x))
  z[is.na(z) | !nzchar(z)] <- "Missing"
  factor(z)
}

.s4cs_grid <- function(fit) {
  g <- tidyr::expand_grid(
    model = factor(model_order_s4, levels = model_order_s4),
    direction = factor(c("bunk", "debunk"), levels = c("bunk", "debunk"))
  )
  x <- stats::model.matrix(stats::delete.response(stats::terms(fit)), g,
                           contrasts.arg = fit$contrasts)
  list(grid = g, x = x)
}

.s4cs_scalar <- function(fit, w, scale = 1) {
  b <- stats::coef(fit)
  v <- .s4cs_hc3(fit)
  w <- w[names(b)]
  est <- drop(sum(w * b)) * scale
  var <- drop(t(w) %*% v %*% w) * scale^2
  se <- sqrt(var)
  df <- stats::df.residual(fit)
  crit <- stats::qt(.975, df)
  tibble::tibble(
    estimate = est, se = se, conf_low = est - crit * se,
    conf_high = est + crit * se, statistic = est / se,
    df_den = df, p_value = 2 * stats::pt(abs(est / se), df, lower.tail = FALSE)
  )
}

.s4cs_pool_scalar <- function(q, u, n, k = 1L, scale = 1) {
  ps <- mice::pool.scalar(as.numeric(q), as.numeric(u), n = n, k = k)
  est <- ps$qbar * scale
  se <- sqrt(ps$t) * scale
  crit <- stats::qt(.975, ps$df)
  tibble::tibble(
    estimate = est, se = se, conf_low = est - crit * se,
    conf_high = est + crit * se, statistic = est / se,
    df_den = ps$df,
    p_value = 2 * stats::pt(abs(est / se), ps$df, lower.tail = FALSE),
    fmi = ps$fmi
  )
}

.s4cs_pool_fit_weight <- function(fits, weight_fun, n, k = 1L, scale = 1) {
  q <- vapply(fits, function(f) {
    w <- weight_fun(f); sum(w[names(stats::coef(f))] * stats::coef(f))
  }, numeric(1))
  u <- vapply(fits, function(f) {
    w <- weight_fun(f)[names(stats::coef(f))]
    drop(t(w) %*% .s4cs_hc3(f) %*% w)
  }, numeric(1))
  .s4cs_pool_scalar(q, u, n = n, k = k, scale = scale)
}

.s4cs_mcnemar <- function(pre, post) {
  ok <- is.finite(pre) & is.finite(post)
  tb <- table(factor(pre[ok], c(0, 1)), factor(post[ok], c(0, 1)))
  tryCatch(stats::mcnemar.test(tb)$p.value, error = function(e) NA_real_)
}

.s4cs_derive <- function(z) {
  signed <- ifelse(z$direction == "bunk", 1, -1)
  pre_weighted <- (z$stance_pre - 50) * z$share_pre / 100
  post_weighted <- (z$stance_post - 50) * z$share_revised_post / 100
  original_now <- (z$stance_pre - 50) * z$share_original_post / 100
  z$aligned_belief_change <- (z$belief_post - z$belief_pre) * signed
  z$aligned_stance_change <- (z$stance_post - z$stance_pre) * signed
  z$aligned_new_minus_old_weighted <- (post_weighted - pre_weighted) * signed
  z$aligned_original_post_share_change <- (original_now - pre_weighted) * signed
  z$pre_pro <- as.numeric(z$share_pre > 50 & z$stance_pre > 50)
  z$pre_anti <- as.numeric(z$share_pre > 50 & z$stance_pre < 50)
  z$post_pro <- as.numeric(z$share_revised_post > 50 & z$stance_post > 50)
  z$post_anti <- as.numeric(z$share_revised_post > 50 & z$stance_post < 50)
  z$pre_aligned_public <- ifelse(z$direction == "bunk", z$pre_pro, z$pre_anti)
  z$post_aligned_public <- ifelse(z$direction == "bunk", z$post_pro, z$post_anti)
  z$pro_public_posting_change <- z$post_pro - z$pre_pro
  z$anti_public_posting_change <- z$post_anti - z$pre_anti
  z$aligned_public_posting_change <- z$post_aligned_public - z$pre_aligned_public
  z
}

.s4cs_point_blocks <- function(dat, approach, outcomes) {
  cells <- contrasts <- tests <- list()
  note <- paste0(
    "Confirmed-chat-start ", approach,
    "; aligned effects are positive in the assigned direction; HC3 inference."
  )
  for (outcome in outcomes) {
    z <- dat[is.finite(dat[[outcome]]), , drop = FALSE]
    z$.y <- z[[outcome]]
    fit <- stats::lm(.y ~ direction * model, data = z)
    gx <- .s4cs_grid(fit)

    for (i in seq_len(nrow(gx$grid))) {
      g <- gx$grid[i, ]
      one <- .s4cs_scalar(fit, gx$x[i, ])
      cells[[length(cells) + 1L]] <- dplyr::bind_cols(
        tibble::tibble(
          outcome = outcome, model = as.character(g$model),
          direction = as.character(g$direction), term = approach,
          n = sum(z$model == g$model & z$direction == g$direction), note = note
        ), one
      )
    }

    bunk_rows <- which(gx$grid$direction == "bunk")
    debunk_rows <- which(gx$grid$direction == "debunk")
    for (model in model_order_s4) {
      ib <- bunk_rows[gx$grid$model[bunk_rows] == model]
      id <- debunk_rows[gx$grid$model[debunk_rows] == model]
      one <- .s4cs_scalar(fit, gx$x[id, ] - gx$x[ib, ])
      contrasts[[length(contrasts) + 1L]] <- dplyr::bind_cols(
        tibble::tibble(
          outcome = outcome, model = model, direction = "debunk-minus-bunk",
          term = approach, n = sum(z$model == model),
          note = paste(note, "Positive contrasts favor debunking.")
        ), one
      )
    }

    avg_w <- colMeans(gx$x[debunk_rows, , drop = FALSE]) -
      colMeans(gx$x[bunk_rows, , drop = FALSE])
    avg <- .s4cs_scalar(fit, avg_w)
    tests[[length(tests) + 1L]] <- dplyr::bind_cols(
      tibble::tibble(
        outcome = outcome, model = NA_character_, direction = "debunk-minus-bunk",
        term = paste0(approach, "_equal_model_average"), n = nrow(z),
        note = paste(note, "Equal weight for each of the four model families.")
      ), avg
    )

    ints <- grep("^directiondebunk:model", names(stats::coef(fit)), value = TRUE)
    L <- matrix(0, nrow = length(ints), ncol = length(stats::coef(fit)),
                dimnames = list(ints, names(stats::coef(fit))))
    L[cbind(seq_along(ints), match(ints, colnames(L)))] <- 1
    omni <- car::linearHypothesis(fit, L, vcov. = .s4cs_hc3(fit), test = "F")
    tests[[length(tests) + 1L]] <- tibble::tibble(
      outcome = outcome, model = NA_character_, direction = "direction-x-model",
      term = paste0(approach, "_omnibus"), n = nrow(z),
      statistic = omni$F[2], df_num = omni$Df[2], df_den = omni$Res.Df[2],
      p_value = omni$`Pr(>F)`[2],
      note = paste(note, "HC3 multivariate F test of the direction-by-model interaction.")
    )
  }
  list(cells = dplyr::bind_rows(cells), contrasts = dplyr::bind_rows(contrasts),
       tests = dplyr::bind_rows(tests))
}

.s4cs_mi_blocks <- function(sets, outcomes, m) {
  cells <- contrasts <- tests <- list()
  note <- paste0(
    m, " PMM imputations (20 iterations), jointly imputing the four raw post-treatment outcomes; ",
    "Rubin pooling with HC3 within-imputation variances."
  )
  for (outcome in outcomes) {
    fits <- lapply(sets, function(z) {
      z$.y <- z[[outcome]]
      stats::lm(.y ~ direction * model, data = z)
    })
    gx <- .s4cs_grid(fits[[1]])
    for (i in seq_len(nrow(gx$grid))) {
      g <- gx$grid[i, ]
      one <- .s4cs_pool_fit_weight(
        fits, function(f) .s4cs_grid(f)$x[i, ], n = nrow(sets[[1]]), k = 8L
      )
      cells[[length(cells) + 1L]] <- dplyr::bind_cols(
        tibble::tibble(
          outcome = outcome, model = as.character(g$model),
          direction = as.character(g$direction), term = "mi_mar",
          n = sum(sets[[1]]$model == g$model & sets[[1]]$direction == g$direction),
          note = note
        ), one
      )
    }
    bunk_rows <- which(gx$grid$direction == "bunk")
    debunk_rows <- which(gx$grid$direction == "debunk")
    for (model in model_order_s4) {
      ib <- bunk_rows[gx$grid$model[bunk_rows] == model]
      id <- debunk_rows[gx$grid$model[debunk_rows] == model]
      one <- .s4cs_pool_fit_weight(
        fits,
        function(f) {
          x <- .s4cs_grid(f)$x
          x[id, ] - x[ib, ]
        },
        n = nrow(sets[[1]]), k = 8L
      )
      contrasts[[length(contrasts) + 1L]] <- dplyr::bind_cols(
        tibble::tibble(
          outcome = outcome, model = model, direction = "debunk-minus-bunk",
          term = "mi_mar", n = sum(sets[[1]]$model == model),
          note = paste(note, "Positive contrasts favor debunking.")
        ), one
      )
    }
    avg <- .s4cs_pool_fit_weight(
      fits,
      function(f) {
        x <- .s4cs_grid(f)$x
        colMeans(x[debunk_rows, , drop = FALSE]) - colMeans(x[bunk_rows, , drop = FALSE])
      },
      n = nrow(sets[[1]]), k = 8L
    )
    tests[[length(tests) + 1L]] <- dplyr::bind_cols(
      tibble::tibble(
        outcome = outcome, model = NA_character_, direction = "debunk-minus-bunk",
        term = "mi_mar_equal_model_average", n = nrow(sets[[1]]),
        note = paste(note, "Equal weight for each model family.")
      ), avg
    )

    ints <- grep("^directiondebunk:model", names(stats::coef(fits[[1]])), value = TRUE)
    q <- t(vapply(fits, function(f) stats::coef(f)[ints], numeric(length(ints))))
    u <- lapply(fits, function(f) .s4cs_hc3(f)[ints, ints, drop = FALSE])
    qbar <- colMeans(q)
    ubar <- Reduce(`+`, u) / length(u)
    between <- stats::cov(q)
    total <- ubar + (1 + 1 / length(u)) * between
    wald <- drop(t(qbar) %*% solve(total, qbar))
    tests[[length(tests) + 1L]] <- tibble::tibble(
      outcome = outcome, model = NA_character_, direction = "direction-x-model",
      term = "mi_mar_omnibus", n = nrow(sets[[1]]), statistic = wald,
      df_num = length(ints), p_value = stats::pchisq(wald, length(ints), lower.tail = FALSE),
      note = paste(note, "Rubin total-covariance multivariate Wald chi-square for the interaction.")
    )
  }
  list(cells = dplyr::bind_rows(cells), contrasts = dplyr::bind_rows(contrasts),
       tests = dplyr::bind_rows(tests))
}

.s4cs_bounds <- function(d, outcomes) {
  signed <- ifelse(d$direction == "bunk", 1, -1)
  pre_weighted <- (d$stance_pre - 50) * d$share_pre / 100
  cpre <- d$stance_pre - 50
  orig_min <- pmin(0, cpre) - pre_weighted
  orig_max <- pmax(0, cpre) - pre_weighted

  proposed <- list(
    aligned_belief_change = cbind(
      pmin((0 - d$belief_pre) * signed, (100 - d$belief_pre) * signed),
      pmax((0 - d$belief_pre) * signed, (100 - d$belief_pre) * signed)
    ),
    aligned_stance_change = cbind(
      pmin((0 - d$stance_pre) * signed, (100 - d$stance_pre) * signed),
      pmax((0 - d$stance_pre) * signed, (100 - d$stance_pre) * signed)
    ),
    aligned_new_minus_old_weighted = cbind(
      pmin((-50 - pre_weighted) * signed, (50 - pre_weighted) * signed),
      pmax((-50 - pre_weighted) * signed, (50 - pre_weighted) * signed)
    ),
    aligned_original_post_share_change = cbind(
      pmin(orig_min * signed, orig_max * signed),
      pmax(orig_min * signed, orig_max * signed)
    )
  )

  # Unit-test the endpoint algebra participant by participant.
  stopifnot(
    all.equal(proposed$aligned_belief_change[, 1],
              pmin((0 - d$belief_pre) * signed, (100 - d$belief_pre) * signed)) == TRUE,
    all.equal(proposed$aligned_stance_change[, 2],
              pmax((0 - d$stance_pre) * signed, (100 - d$stance_pre) * signed)) == TRUE,
    all.equal(proposed$aligned_new_minus_old_weighted[, 1],
              pmin((-50 - pre_weighted) * signed, (50 - pre_weighted) * signed)) == TRUE,
    all.equal(proposed$aligned_original_post_share_change[, 2],
              pmax(((0 * cpre) - pre_weighted) * signed,
                   ((1 * cpre) - pre_weighted) * signed)) == TRUE
  )

  cells <- contrasts <- tests <- list()
  for (outcome in outcomes) {
    obs <- d[[outcome]]
    lo <- ifelse(is.finite(obs), obs, proposed[[outcome]][, 1])
    hi <- ifelse(is.finite(obs), obs, proposed[[outcome]][, 2])
    for (model in model_order_s4) for (direction in c("bunk", "debunk")) {
      take <- d$model == model & d$direction == direction
      cells[[length(cells) + 1L]] <- tibble::tibble(
        outcome = outcome, model = model, direction = direction,
        term = "support_bounds", n = sum(take), estimate = NA_real_,
        conf_low = mean(lo[take]), conf_high = mean(hi[take]),
        note = "Exact feasible-value identification interval; endpoints are not confidence limits."
      )
    }
    per_model <- list()
    for (model in model_order_s4) {
      b <- d$model == model & d$direction == "bunk"
      e <- d$model == model & d$direction == "debunk"
      low <- mean(lo[e]) - mean(hi[b])
      high <- mean(hi[e]) - mean(lo[b])
      per_model[[model]] <- c(low, high)
      contrasts[[length(contrasts) + 1L]] <- tibble::tibble(
        outcome = outcome, model = model, direction = "debunk-minus-bunk",
        term = "support_bounds", n = sum(b | e), estimate = NA_real_,
        conf_low = low, conf_high = high,
        note = "Exact feasible-value contrast interval; endpoints are not confidence limits."
      )
    }
    mat <- do.call(rbind, per_model)
    tests[[length(tests) + 1L]] <- tibble::tibble(
      outcome = outcome, model = NA_character_, direction = "debunk-minus-bunk",
      term = "support_bounds_equal_model_average", n = nrow(d), estimate = NA_real_,
      conf_low = mean(mat[, 1]), conf_high = mean(mat[, 2]),
      note = "Equal-model average of exact feasible-value contrast intervals; endpoints are not confidence limits."
    )
  }
  list(cells = dplyr::bind_rows(cells), contrasts = dplyr::bind_rows(contrasts),
       tests = dplyr::bind_rows(tests))
}

.s4cs_extensive_point <- function(dat, approach) {
  rows <- list()
  add_mean <- function(z, outcome, model, direction, term, pre, post, note) {
    y <- z[[outcome]]
    ok <- is.finite(y)
    fit <- stats::lm(y[ok] ~ 1)
    one <- .s4cs_scalar(fit, c("(Intercept)" = 1), scale = 100)
    one$p_value <- .s4cs_mcnemar(z[[pre]], z[[post]])
    dplyr::bind_cols(tibble::tibble(
      outcome = outcome, model = model, direction = direction, term = term,
      n = sum(ok), note = paste(note, "Estimate is percentage-point change; p is McNemar for available/zero-change pairs.")
    ), one)
  }
  # Pooled debunking pro and anti margins.
  z <- dat[dat$direction == "debunk", , drop = FALSE]
  rows[[length(rows) + 1L]] <- add_mean(z, "pro_public_posting_change", NA_character_, "debunk",
                                        approach, "pre_pro", "post_pro", "Pooled across models.")
  rows[[length(rows) + 1L]] <- add_mean(z, "anti_public_posting_change", NA_character_, "debunk",
                                        approach, "pre_anti", "post_anti", "Pooled across models.")
  # Bunking changes on both margins, per model.
  for (model in model_order_s4) {
    z <- dat[dat$model == model & dat$direction == "bunk", , drop = FALSE]
    rows[[length(rows) + 1L]] <- add_mean(z, "pro_public_posting_change", model, "bunk",
                                          approach, "pre_pro", "post_pro", "Per-model bunking margin.")
    rows[[length(rows) + 1L]] <- add_mean(z, "anti_public_posting_change", model, "bunk",
                                          approach, "pre_anti", "post_anti", "Per-model bunking margin.")
  }
  # Per-model aligned changes in both directions.
  for (model in model_order_s4) for (direction in c("bunk", "debunk")) {
    z <- dat[dat$model == model & dat$direction == direction, , drop = FALSE]
    rows[[length(rows) + 1L]] <- add_mean(
      z, "aligned_public_posting_change", model, direction, approach,
      "pre_aligned_public", "post_aligned_public", "Per-model direction-aligned margin."
    )
  }
  # Per-model bunk-versus-debunk contrasts in aligned posting change.
  for (model in model_order_s4) {
    z <- dat[dat$model == model & is.finite(dat$aligned_public_posting_change), , drop = FALSE]
    fit <- stats::lm(aligned_public_posting_change ~ direction, data = z)
    one <- .s4cs_scalar(fit, c("(Intercept)" = 0, "directiondebunk" = -1), scale = 100)
    rows[[length(rows) + 1L]] <- dplyr::bind_cols(tibble::tibble(
      outcome = "aligned_public_posting_change", model = model,
      direction = "bunk-minus-debunk", term = approach, n = nrow(z),
      note = "Per-model bunk-minus-debunk contrast in direction-aligned public posting; HC3."
    ), one)
  }
  dplyr::bind_rows(rows)
}

.s4cs_extensive_mi <- function(sets, m) {
  rows <- list()
  pool_mean <- function(sub_fun, outcome, model, direction, note) {
    fits <- lapply(sets, function(z) {
      q <- sub_fun(z)
      q$.y <- q[[outcome]]
      stats::lm(.y ~ 1, data = q)
    })
    sub_n <- nrow(sub_fun(sets[[1]]))
    one <- .s4cs_pool_fit_weight(
      fits, function(f) c("(Intercept)" = 1), n = sub_n, k = 1L, scale = 100
    )
    dplyr::bind_cols(tibble::tibble(
      outcome = outcome, model = model, direction = direction, term = "mi_mar",
      n = sub_n,
      note = paste0(m, "-imputation Rubin-pooled percentage-point change; HC3. ", note)
    ), one)
  }
  rows[[length(rows) + 1L]] <- pool_mean(
    function(z) z[z$direction == "debunk", ], "pro_public_posting_change",
    NA_character_, "debunk", "Pooled across models."
  )
  rows[[length(rows) + 1L]] <- pool_mean(
    function(z) z[z$direction == "debunk", ], "anti_public_posting_change",
    NA_character_, "debunk", "Pooled across models."
  )
  for (model in model_order_s4) {
    rows[[length(rows) + 1L]] <- pool_mean(
      function(z) z[z$model == model & z$direction == "bunk", ],
      "pro_public_posting_change", model, "bunk", "Per-model bunking margin."
    )
    rows[[length(rows) + 1L]] <- pool_mean(
      function(z) z[z$model == model & z$direction == "bunk", ],
      "anti_public_posting_change", model, "bunk", "Per-model bunking margin."
    )
    for (direction in c("bunk", "debunk")) {
      rows[[length(rows) + 1L]] <- pool_mean(
        function(z) z[z$model == model & z$direction == direction, ],
        "aligned_public_posting_change", model, direction,
        "Per-model direction-aligned margin."
      )
    }
    fits <- lapply(sets, function(z) {
      q <- z[z$model == model, ]
      stats::lm(aligned_public_posting_change ~ direction, data = q)
    })
    one <- .s4cs_pool_fit_weight(
      fits, function(f) c("(Intercept)" = 0, "directiondebunk" = -1),
      n = sum(sets[[1]]$model == model), k = 2L, scale = 100
    )
    rows[[length(rows) + 1L]] <- dplyr::bind_cols(tibble::tibble(
      outcome = "aligned_public_posting_change", model = model,
      direction = "bunk-minus-debunk", term = "mi_mar",
      n = sum(sets[[1]]$model == model),
      note = paste0(m, "-imputation Rubin-pooled bunk-minus-debunk contrast; HC3.")
    ), one)
  }
  dplyr::bind_rows(rows)
}

.s4cs_extensive_bounds <- function(d) {
  rows <- list()
  make_bounds <- function(outcome, pre_name) {
    obs <- d[[outcome]]
    pre <- d[[pre_name]]
    cbind(ifelse(is.finite(obs), obs, -pre), ifelse(is.finite(obs), obs, 1 - pre))
  }
  bb <- list(
    pro_public_posting_change = make_bounds("pro_public_posting_change", "pre_pro"),
    anti_public_posting_change = make_bounds("anti_public_posting_change", "pre_anti"),
    aligned_public_posting_change = make_bounds("aligned_public_posting_change", "pre_aligned_public")
  )
  # Brute-force endpoint check for 0/1 post-treatment support.
  stopifnot(all(bb$pro_public_posting_change[, 1] <= bb$pro_public_posting_change[, 2]),
            all(bb$anti_public_posting_change[, 1] <= bb$anti_public_posting_change[, 2]),
            all(bb$aligned_public_posting_change[, 1] <= bb$aligned_public_posting_change[, 2]))
  add_group <- function(take, outcome, model, direction, note) {
    tibble::tibble(
      outcome = outcome, model = model, direction = direction,
      term = "support_bounds", n = sum(take), estimate = NA_real_,
      conf_low = 100 * mean(bb[[outcome]][take, 1]),
      conf_high = 100 * mean(bb[[outcome]][take, 2]),
      note = paste(note, "Exact binary-support interval; endpoints are not confidence limits.")
    )
  }
  deb <- d$direction == "debunk"
  rows[[length(rows) + 1L]] <- add_group(deb, "pro_public_posting_change", NA_character_, "debunk", "Pooled across models.")
  rows[[length(rows) + 1L]] <- add_group(deb, "anti_public_posting_change", NA_character_, "debunk", "Pooled across models.")
  for (model in model_order_s4) {
    bunk <- d$model == model & d$direction == "bunk"
    rows[[length(rows) + 1L]] <- add_group(bunk, "pro_public_posting_change", model, "bunk", "Per-model bunking margin.")
    rows[[length(rows) + 1L]] <- add_group(bunk, "anti_public_posting_change", model, "bunk", "Per-model bunking margin.")
    for (direction in c("bunk", "debunk")) {
      take <- d$model == model & d$direction == direction
      rows[[length(rows) + 1L]] <- add_group(take, "aligned_public_posting_change", model, direction,
                                             "Per-model direction-aligned margin.")
    }
    b <- d$model == model & d$direction == "bunk"
    e <- d$model == model & d$direction == "debunk"
    rows[[length(rows) + 1L]] <- tibble::tibble(
      outcome = "aligned_public_posting_change", model = model,
      direction = "bunk-minus-debunk", term = "support_bounds", n = sum(b | e),
      estimate = NA_real_,
      conf_low = 100 * (mean(bb$aligned_public_posting_change[b, 1]) -
                          mean(bb$aligned_public_posting_change[e, 2])),
      conf_high = 100 * (mean(bb$aligned_public_posting_change[b, 2]) -
                           mean(bb$aligned_public_posting_change[e, 1])),
      note = "Per-model bunk-minus-debunk exact binary-support interval; endpoints are not confidence limits."
    )
  }
  dplyr::bind_rows(rows)
}

compute_s4_chat_started_sensitivity <- function(core_objects, m = 50L,
                                                maxit = 20L, seed = 20260714L) {
  si_require(c("dplyr", "tibble", "tidyr", "readr", "mice", "sandwich", "car"))
  paths <- core_objects$paths
  fresh_paths <- pkg_paths(core_objects$pkg_root)
  paths$s4_orientation_gap <- fresh_paths$s4_orientation_gap
  paths$s4_stance_v2_gap <- fresh_paths$s4_stance_v2_gap
  paths$s4_gap_manifest <- fresh_paths$s4_gap_manifest
  required <- c(paths$s4_orientation_gap, paths$s4_stance_v2_gap, paths$s4_gap_manifest)
  if (!all(file.exists(required))) {
    stop("Missing shipped Study-4 chat-starter gap classification asset(s): ",
         paste(required[!file.exists(required)], collapse = ", "), call. = FALSE)
  }

  pool <- build_s4_chat_started_pool(core_objects$s4$s4_raw)
  stopifnot(nrow(pool) == 1353L, sum(!pool$completed) == 81L)

  orientation <- dplyr::bind_rows(
    readr::read_csv(paths$s4_orientation, show_col_types = FALSE),
    readr::read_csv(paths$s4_orientation_gap, show_col_types = FALSE)
  ) |>
    dplyr::distinct(.data$ResponseId, .keep_all = TRUE) |>
    dplyr::select(.data$ResponseId, orientation = .data$orientation_consensus,
                  orientation_votes = .data$n_votes)
  stopifnot(nrow(orientation) >= 1353L)

  stance_score <- function(path) {
    readr::read_csv(path, show_col_types = FALSE) |>
      dplyr::filter(.data$timepoint == "pre") |>
      dplyr::transmute(
        ResponseId = .data$ResponseId,
        stance_pre = dplyr::if_else(
          as.character(.data$consensus_applicable) %in% c("True", "TRUE"),
          suppressWarnings(as.numeric(.data$consensus_score)), 50
        ),
        stance_n_raters = suppressWarnings(as.integer(.data$n_raters_ok)),
        stance_dispersion = as.character(.data$dispersion_flag) %in% c("True", "TRUE")
      )
  }
  stance <- dplyr::bind_rows(
    stance_score(paths$s4_stance_v2), stance_score(paths$s4_stance_v2_gap)
  ) |>
    dplyr::distinct(.data$ResponseId, .keep_all = TRUE)

  strict <- core_objects$s4$s4 |>
    dplyr::transmute(
      ResponseId = as.character(.data$ResponseId),
      strict_belief_change = .data$aligned_belief_change,
      stance_post = as.numeric(.data$post_direction_score),
      share_revised_post = as.numeric(.data$share_post_4),
      share_original_post = as.numeric(.data$share_original_post_now_4)
    )

  field_date <- as.Date(substr(as.character(pool$StartDate), 1L, 10L))
  d <- pool |>
    dplyr::left_join(orientation, by = "ResponseId") |>
    dplyr::left_join(stance, by = "ResponseId") |>
    dplyr::left_join(strict, by = "ResponseId") |>
    dplyr::transmute(
      ResponseId = as.character(.data$ResponseId),
      completed = .data$completed,
      direction = factor(as.character(.data$direction), levels = c("bunk", "debunk")),
      model = factor(as.character(.data$model), levels = model_order_s4),
      cell = factor(paste(as.character(.data$model), as.character(.data$direction), sep = "__")),
      field_regime = factor(dplyr::case_when(
        field_date < as.Date("2026-03-28") ~ "before_endpoint_removal",
        field_date <= as.Date("2026-03-30") ~ "endpoint_removal",
        TRUE ~ "after_model_switch"
      )),
      orientation = .data$orientation,
      orientation_votes = .data$orientation_votes,
      stance_n_raters = .data$stance_n_raters,
      stance_dispersion = .data$stance_dispersion,
      belief_pre_orig = suppressWarnings(as.numeric(.data$belief_rating_pre_4_orig)),
      belief_post_orig = suppressWarnings(as.numeric(.data$belief_rating_post_4_orig)),
      belief_pre = ifelse(.data$orientation == "denies", 100 - .data$belief_pre_orig, .data$belief_pre_orig),
      belief_post = ifelse(.data$orientation == "denies", 100 - .data$belief_post_orig, .data$belief_post_orig),
      stance_pre = .data$stance_pre,
      stance_post = .data$stance_post,
      share_pre = suppressWarnings(as.numeric(.data$share_pre_4)),
      share_revised_post = .data$share_revised_post,
      share_original_post = .data$share_original_post,
      strict_belief_change = .data$strict_belief_change,
      pre_confidence = .s4cs_num_complete(.data$pre_confidence_4),
      importance = .s4cs_num_complete(.data$Importance),
      age = .s4cs_num_complete(.data$age),
      gender = .s4cs_fac_complete(.data$gender),
      education = .s4cs_num_complete(.data$Education),
      party = .s4cs_fac_complete(.data$PartyAffil),
      social_conservatism = .s4cs_num_complete(.data$SocialConservatism),
      economic_conservatism = .s4cs_num_complete(.data$EconomicConservatism),
      genai_familiarity = .s4cs_num_complete(.data$genai_fam_1),
      genai_use = .s4cs_num_complete(.data$genai_use_1),
      genai_trust = .s4cs_num_complete(.data$genai_trust),
      chat_visible_user_n = .s4cs_num_complete(.data$chat_visible_user_n),
      chat_assistant_n = .s4cs_num_complete(.data$chat_assistant_n),
      chat_visible_user_words = .s4cs_num_complete(.data$chat_visible_user_words),
      chat_assistant_words = .s4cs_num_complete(.data$chat_assistant_words)
    )

  stopifnot(
    !anyNA(d$orientation), all(d$orientation_votes == 3L),
    !anyNA(d$stance_pre), all(d$stance_n_raters[d$completed] >= 4L),
    all(d$stance_n_raters[!d$completed] == 5L),
    sum(is.finite(d$belief_post)) == 1278L,
    sum(is.finite(d$stance_post)) == 1272L,
    sum(is.finite(d$share_revised_post)) == 1272L,
    sum(is.finite(d$share_original_post)) == 1272L
  )
  available <- .s4cs_derive(d)
  stopifnot(sum(!d$completed & is.finite(d$belief_post)) == 6L)
  chk <- available$completed & is.finite(available$strict_belief_change)
  stopifnot(max(abs(available$aligned_belief_change[chk] -
                      available$strict_belief_change[chk])) < 1e-10)

  outcomes <- c(
    "aligned_belief_change", "aligned_new_minus_old_weighted",
    "aligned_stance_change", "aligned_original_post_share_change"
  )
  available_blocks <- .s4cs_point_blocks(available, "available_outcomes", outcomes)

  zero <- available
  for (v in outcomes) zero[[v]][!is.finite(zero[[v]])] <- 0
  for (v in c("post_pro", "post_anti", "post_aligned_public")) {
    pre_v <- sub("^post", "pre", v)
    zero[[v]][!is.finite(zero[[v]])] <- zero[[pre_v]][!is.finite(zero[[v]])]
  }
  zero$pro_public_posting_change[!is.finite(zero$pro_public_posting_change)] <- 0
  zero$anti_public_posting_change[!is.finite(zero$anti_public_posting_change)] <- 0
  zero$aligned_public_posting_change[!is.finite(zero$aligned_public_posting_change)] <- 0
  zero_blocks <- .s4cs_point_blocks(zero, "zero_change", outcomes)

  impdat <- d |>
    dplyr::select(
      belief_post, stance_post, share_revised_post, share_original_post,
      cell, field_regime, belief_pre, stance_pre, share_pre, pre_confidence,
      importance, age, gender, education, party, social_conservatism,
      economic_conservatism, genai_familiarity, genai_use, genai_trust,
      chat_visible_user_n, chat_assistant_n, chat_visible_user_words,
      chat_assistant_words
    )
  methods <- mice::make.method(impdat); methods[] <- ""
  imputed_vars <- c("belief_post", "stance_post", "share_revised_post", "share_original_post")
  methods[imputed_vars] <- "pmm"
  predictors <- mice::make.predictorMatrix(impdat); predictors[,] <- 0
  for (v in imputed_vars) predictors[v, setdiff(names(impdat), v)] <- 1
  set.seed(seed)
  imp <- mice::mice(
    impdat, m = as.integer(m), maxit = as.integer(maxit), method = methods,
    predictorMatrix = predictors, seed = seed, printFlag = FALSE,
    remove.collinear = TRUE, remove.constant = TRUE
  )
  completed_raw <- mice::complete(imp, action = "all")
  convergence <- mice::convergence(imp)
  convergence_last <- convergence[convergence$.it == max(convergence$.it), , drop = FALSE]
  finite_max <- function(x) {
    x <- abs(x[is.finite(x)])
    if (length(x)) max(x) else NA_real_
  }
  mi_logged_events <- if (is.null(imp$loggedEvents)) 0L else nrow(imp$loggedEvents)
  sets <- lapply(completed_raw, function(z) {
    z$ResponseId <- d$ResponseId
    z$direction <- d$direction
    z$model <- d$model
    .s4cs_derive(z)
  })
  for (z in completed_raw) for (v in imputed_vars) {
    stopifnot(all(is.finite(z[[v]])), min(z[[v]]) >= 0, max(z[[v]]) <= 100)
  }
  mi_blocks <- .s4cs_mi_blocks(sets, outcomes, m = m)
  bound_blocks <- .s4cs_bounds(available, outcomes)

  extensive_available <- .s4cs_extensive_point(available, "available_outcomes")
  extensive_zero <- .s4cs_extensive_point(zero, "zero_change")
  extensive_mi <- .s4cs_extensive_mi(sets, m = m)
  extensive_bounds <- .s4cs_extensive_bounds(available)

  cohort_diag <- available |>
    dplyr::group_by(.data$model, .data$direction) |>
    dplyr::summarise(
      group_size = dplyr::n(), cohort_n = dplyr::n(), incomplete_n = sum(!.data$completed),
      belief_observed_n = sum(is.finite(.data$belief_post)),
      social_observed_n = sum(is.finite(.data$stance_post)), .groups = "drop"
    ) |>
    tidyr::pivot_longer(c("cohort_n", "incomplete_n", "belief_observed_n", "social_observed_n"),
                        names_to = "term", values_to = "estimate") |>
    dplyr::transmute(
      model = as.character(.data$model), direction = as.character(.data$direction),
      term = .data$term, n = as.integer(.data$group_size), estimate = .data$estimate,
      note = "Shared confirmed-chat-start cohort and outcome-availability audit."
    )
  diag_total <- tibble::tibble(
    model = NA_character_, direction = NA_character_,
    term = c("cohort_n", "incomplete_n", "belief_observed_n", "social_observed_n",
             "orientation_gap_n", "stance_gap_n", "mi_m", "mi_maxit",
             "mi_logged_events_n", "mi_last_iteration_max_abs_autocorrelation",
             "mi_last_iteration_max_psrf"),
    n = nrow(available),
    estimate = c(nrow(available), sum(!available$completed),
                 sum(is.finite(available$belief_post)), sum(is.finite(available$stance_post)),
                 81, 81, m, maxit, mi_logged_events,
                 finite_max(convergence_last$ac), finite_max(convergence_last$psrf)),
    note = "Shared confirmed-chat-start cohort; gap audits require 3 orientation votes and 5 stance raters per record."
  )

  dplyr::bind_rows(
    dplyr::bind_rows(available_blocks$cells, zero_blocks$cells,
                     mi_blocks$cells, bound_blocks$cells) |>
      std_row("S4 + pooled", "s4_chat_started_cell_effects", "chat_started_n1353"),
    dplyr::bind_rows(available_blocks$contrasts, zero_blocks$contrasts,
                     mi_blocks$contrasts, bound_blocks$contrasts) |>
      std_row("S4 + pooled", "s4_chat_started_model_contrasts", "chat_started_n1353"),
    dplyr::bind_rows(available_blocks$tests, zero_blocks$tests,
                     mi_blocks$tests, bound_blocks$tests) |>
      std_row("S4 + pooled", "s4_chat_started_registered_tests", "chat_started_n1353"),
    dplyr::bind_rows(extensive_available, extensive_zero, extensive_mi, extensive_bounds) |>
      std_row("Public-posting", "s4_chat_started_extensive_margin", "chat_started_n1353"),
    dplyr::bind_rows(cohort_diag, diag_total) |>
      std_row("S4 + pooled", "s4_chat_started_diagnostics", "chat_started_n1353")
  )
}
