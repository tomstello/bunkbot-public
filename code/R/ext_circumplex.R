# ext_circumplex.R ----------------------------------------------------------------------
# Circumplex-aware affect analysis for Studies 1-3.
#
# The affect circumplex is a 2D self-report grid (valence on x, arousal on y) on a 0-800
# scale centered at 400. In core_objects$s13 the pre-treatment baseline is
# circumplex_pre_1_x/y, the post-treatment measure is circumplex2_1_x/y, and the
# post-debrief measure is circumplex_postdb_1_x/y. Because the change vector requires a
# pre-treatment baseline (which was administered ONLY in Studies 1-3 -- Study 4 collected a
# post measure only), every quantity here is S1-3 only.
#
# Centering: val_pre = circumplex_pre_1_x - 400; aro_pre = circumplex_pre_1_y - 400; etc.
# Change vector: delta_val = val_post - val_pre = circumplex2_1_x - circumplex_pre_1_x;
#                delta_aro = circumplex2_1_y - circumplex_pre_1_y.
# (The -400 centering cancels in the difference, but is irrelevant to the change vector.)
#
# The analysis treats (valence, arousal) JOINTLY rather than testing each margin alone:
#   (1) ONE-SAMPLE Hotelling T^2 per study x condition: does the mean 2D change vector
#       differ from (0,0)? Emits term = "hotelling_T2_vs_zero" (F, df, p) plus the
#       marginal mean_delta_valence and mean_delta_arousal (estimate + 95% CI).
#   (2) TWO-SAMPLE Hotelling T^2 per study (improvement over the reference): does the mean
#       change vector DIFFER between Bunking and Debunking within a study? This is a
#       one-way MANOVA on cbind(delta_val, delta_aro) ~ condition. Emits
#       term = "hotelling_T2_bunk_vs_debunk" (F, df, p).
#
# model = study_factor; direction = Bunking/Debunking (NA for the two-sample test).
# ENTRY POINT: compute_circumplex(core_objects) -> tibble (canonical schema).
# Requires R/tables_dynamic.R (std_row/std_cols).

# Mean + t 95% CI for a single margin (returns c(mean, lo, hi)).
.cplx_meanci <- function(x) {
  x <- x[is.finite(x)]; n <- length(x)
  if (n < 1) return(c(NA_real_, NA_real_, NA_real_))
  m <- mean(x)
  if (n < 2) return(c(m, NA_real_, NA_real_))
  se <- stats::sd(x) / sqrt(n); tt <- stats::qt(0.975, n - 1)
  c(m, m - tt * se, m + tt * se)
}

# Direct one-sample Hotelling T^2 vs mu=(0,0) (fallback if DescTools is unavailable).
# Returns list(F, df1, df2, p, n). p = 2.
.hotelling_one_direct <- function(M) {
  M <- M[stats::complete.cases(M), , drop = FALSE]
  n <- nrow(M); p <- ncol(M)
  if (n <= p) return(list(F = NA_real_, df1 = p, df2 = NA_real_, p = NA_real_, n = n))
  mbar <- colMeans(M)
  S <- stats::cov(M)
  T2 <- as.numeric(n * t(mbar) %*% solve(S) %*% mbar)
  Fstat <- (n - p) / (p * (n - 1)) * T2
  df1 <- p; df2 <- n - p
  pval <- stats::pf(Fstat, df1, df2, lower.tail = FALSE)
  list(F = Fstat, df1 = df1, df2 = df2, p = pval, n = n)
}

# One-sample Hotelling T^2 vs (0,0): prefer DescTools, else direct formula.
.hotelling_one <- function(M) {
  M <- M[stats::complete.cases(M), , drop = FALSE]
  n <- nrow(M); p <- ncol(M)
  if (n <= p) return(list(F = NA_real_, df1 = p, df2 = NA_real_, p = NA_real_, n = n))
  if (requireNamespace("DescTools", quietly = TRUE)) {
    ht <- tryCatch(
      DescTools::HotellingsT2Test(M, mu = c(0, 0)),
      error = function(e) NULL
    )
    if (!is.null(ht)) {
      return(list(
        F = unname(ht$statistic),
        df1 = unname(ht$parameter[1]),
        df2 = unname(ht$parameter[2]),
        p = unname(ht$p.value),
        n = n
      ))
    }
  }
  .hotelling_one_direct(M)
}

# Two-sample Hotelling T^2 (== one-way MANOVA, 2 groups) for delta vectors by condition.
# Prefer DescTools::HotellingsT2Test(X, Y); else MANOVA (Hotelling-Lawley) via stats::manova.
# Returns list(F, df1, df2, p, n_total, n1, n2).
.hotelling_two <- function(M, grp) {
  ok <- stats::complete.cases(M) & !is.na(grp)
  M <- M[ok, , drop = FALSE]; grp <- droplevels(factor(grp[ok]))
  if (nlevels(grp) != 2) return(NULL)
  lv <- levels(grp)
  X <- M[grp == lv[1], , drop = FALSE]; Y <- M[grp == lv[2], , drop = FALSE]
  n1 <- nrow(X); n2 <- nrow(Y); p <- ncol(M)
  if (n1 + n2 - 2 <= p) return(NULL)
  if (requireNamespace("DescTools", quietly = TRUE)) {
    ht <- tryCatch(DescTools::HotellingsT2Test(X, Y), error = function(e) NULL)
    if (!is.null(ht)) {
      return(list(
        F = unname(ht$statistic),
        df1 = unname(ht$parameter[1]),
        df2 = unname(ht$parameter[2]),
        p = unname(ht$p.value),
        n_total = n1 + n2, n1 = n1, n2 = n2
      ))
    }
  }
  # Fallback: one-way MANOVA, exact F from the Hotelling-Lawley trace (2 groups => exact).
  man <- stats::manova(M ~ grp)
  s <- summary(man, test = "Hotelling-Lawley")$stats
  list(
    F = unname(s["grp", "approx F"]),
    df1 = unname(s["grp", "num Df"]),
    df2 = unname(s["grp", "den Df"]),
    p = unname(s["grp", "Pr(>F)"]),
    n_total = n1 + n2, n1 = n1, n2 = n2
  )
}

# k-means clustering of S1-3 affect-change vectors (delta_val, delta_aro), with a
# bootstrap stability check and a per-cluster directional interpretation. Returns
# a list(assign = per-row cluster id, summary = centroid table). k fixed at 5
# (matching the authors' reference analysis).
.cplx_cluster <- function(M, k = 5L) {
  M <- M[stats::complete.cases(M), , drop = FALSE]
  if (nrow(M) < k * 3) return(NULL)
  Z <- scale(M)
  set.seed(42)
  km <- stats::kmeans(Z, centers = k, nstart = 50)
  # bootstrap cluster stability (mean Jaccard per cluster) if fpc is available
  jac <- rep(NA_real_, k)
  if (requireNamespace("fpc", quietly = TRUE)) {
    cb <- tryCatch(
      fpc::clusterboot(Z, B = 100, bootmethod = "boot",
                       clustermethod = fpc::kmeansCBI, krange = k,
                       seed = 123, count = FALSE),
      error = function(e) NULL)
    if (!is.null(cb)) jac <- cb$bootmean
  }
  # interpret each cluster by its centroid direction in the RAW (unscaled) space
  ctr <- t(vapply(seq_len(k), function(g) colMeans(M[km$cluster == g, , drop = FALSE]),
                  numeric(2)))
  colnames(ctr) <- c("dv", "da")
  interp <- apply(ctr, 1, function(r) {
    dv <- r[["dv"]]; da <- r[["da"]]
    vh <- abs(dv) >= 20; ah <- abs(da) >= 20
    if (!vh && !ah) "Little change"
    else if (dv > 0 && da > 0) "More positive + aroused"
    else if (dv > 0 && da < 0) "More positive + calmer"
    else if (dv < 0 && da > 0) "More negative + aroused"
    else if (dv < 0 && da < 0) "More negative + calmer"
    else if (vh && dv > 0) "More positive (valence)"
    else if (vh && dv < 0) "More negative (valence)"
    else if (ah && da > 0) "More aroused"
    else "Calmer"
  })
  list(assign = km$cluster, dv = ctr[, "dv"], da = ctr[, "da"],
       interp = interp, jaccard = jac,
       size = as.integer(table(factor(km$cluster, levels = seq_len(k)))))
}

compute_circumplex <- function(core_objects) {
  if (!requireNamespace("tibble", quietly = TRUE)) stop("tibble required")
  s13 <- core_objects$s13

  note_scope <- paste(
    "Affect circumplex (valence x, arousal y; 0-800 grid centered at 400).",
    "Change vector = post (circumplex2) minus pre-treatment baseline (circumplex_pre).",
    "S1-3 ONLY: the pre-treatment baseline was administered only in Studies 1-3;",
    "Study 4 collected a post-treatment measure only, so no change vector exists for S4."
  )

  rows <- list()

  for (sf in levels(s13$study_factor)) {
    study_dat <- s13 |>
      dplyr::filter(study_factor == sf) |>
      dplyr::mutate(
        delta_val = circumplex2_1_x - circumplex_pre_1_x,
        delta_aro = circumplex2_1_y - circumplex_pre_1_y
      )

    # ---- (1) one-sample tests + marginal means, per condition ----
    for (cl in levels(study_dat$condition_factor)) {
      sub <- study_dat |> dplyr::filter(condition_factor == cl)
      M <- cbind(delta_val = sub$delta_val, delta_aro = sub$delta_aro)
      M <- M[stats::complete.cases(M), , drop = FALSE]
      n <- nrow(M)

      ht <- .hotelling_one(M)
      ci_v <- .cplx_meanci(M[, "delta_val"])
      ci_a <- .cplx_meanci(M[, "delta_aro"])

      rows[[length(rows) + 1]] <- tibble::tibble(
        model = sf,
        direction = cl,
        term = c("hotelling_T2_vs_zero", "mean_delta_valence", "mean_delta_arousal"),
        n = c(ht$n, n, n),
        estimate = c(NA_real_, ci_v[1], ci_a[1]),
        conf_low = c(NA_real_, ci_v[2], ci_a[2]),
        conf_high = c(NA_real_, ci_v[3], ci_a[3]),
        statistic = c(ht$F, NA_real_, NA_real_),
        df_num = c(ht$df1, NA_real_, NA_real_),
        df_den = c(ht$df2, NA_real_, NA_real_),
        p_value = c(ht$p, NA_real_, NA_real_),
        note = c(
          paste0(
            "One-sample Hotelling T2: mean 2D affect-change vector vs (0,0). ",
            note_scope
          ),
          paste0("Marginal mean valence change (post-pre). ", note_scope),
          paste0("Marginal mean arousal change (post-pre). ", note_scope)
        )
      )
    }

    # ---- (2) two-sample test: bunk vs debunk change vector ----
    M2 <- cbind(delta_val = study_dat$delta_val, delta_aro = study_dat$delta_aro)
    ht2 <- .hotelling_two(M2, study_dat$condition_factor)
    if (!is.null(ht2)) {
      rows[[length(rows) + 1]] <- tibble::tibble(
        model = sf,
        direction = NA_character_,
        term = "hotelling_T2_bunk_vs_debunk",
        n = ht2$n_total,
        estimate = NA_real_,
        statistic = ht2$F,
        df_num = ht2$df1,
        df_den = ht2$df2,
        p_value = ht2$p,
        note = paste0(
          "Two-sample Hotelling T2 / one-way MANOVA: does the 2D affect-change vector ",
          "differ between Bunking (n=", ht2$n1, ") and Debunking (n=", ht2$n2, ")? ",
          note_scope
        )
      )
    }
  }

  change_rows <- dplyr::bind_rows(rows) |>
    std_row("S1-3", "affect_circumplex", "full_sample")

  # ---- (3) clustering of S1-3 change vectors (pooled across studies) ----------
  cl_dat <- s13 |>
    dplyr::mutate(delta_val = circumplex2_1_x - circumplex_pre_1_x,
                  delta_aro = circumplex2_1_y - circumplex_pre_1_y)
  Mall <- cbind(delta_val = cl_dat$delta_val, delta_aro = cl_dat$delta_aro)
  keep <- stats::complete.cases(Mall)
  cl <- .cplx_cluster(Mall[keep, , drop = FALSE], k = 5L)
  cluster_rows_out <- NULL
  if (!is.null(cl)) {
    cl_dat2 <- cl_dat[keep, , drop = FALSE]
    cl_dat2$cluster <- cl$assign
    # cluster centroids + size + stability
    cen_rows <- tibble::tibble(
      model = paste0("Cluster ", seq_along(cl$interp), " (", cl$interp, ")"),
      direction = NA_character_,
      term = "cluster_centroid",
      n = cl$size,
      estimate = cl$dv, statistic = cl$da,
      conf_low = cl$jaccard,   # carry bootstrap Jaccard stability in conf_low
      note = "k-means (k=5) cluster of S1-3 affect-change vectors; estimate=mean delta-valence, statistic=mean delta-arousal, conf_low=bootstrap Jaccard stability, n=cluster size.")
    # cluster prevalence by study x condition + chi-square (cluster x condition) per study
    prev_rows <- list(); chi_rows <- list()
    for (sf in levels(s13$study_factor)) {
      sd <- cl_dat2[cl_dat2$study_factor == sf, , drop = FALSE]
      if (!nrow(sd)) next
      tab <- table(sd$cluster, sd$condition_factor)
      for (g in rownames(tab)) for (cc in colnames(tab)) {
        denom <- sum(tab[, cc])
        prev_rows[[length(prev_rows) + 1]] <- tibble::tibble(
          model = sf, direction = cc, term = paste0("prevalence_cluster", g),
          n = denom, estimate = if (denom > 0) tab[g, cc] / denom else NA_real_,
          note = "Share of the study x condition cell assigned to this affect-change cluster.")
      }
      chi <- suppressWarnings(stats::chisq.test(tab))
      chi_rows[[length(chi_rows) + 1]] <- tibble::tibble(
        model = sf, direction = NA_character_, term = "cluster_by_condition_chisq",
        n = sum(tab), statistic = unname(chi$statistic),
        df_num = unname(chi$parameter), p_value = chi$p.value,
        note = "Chi-square of affect-change cluster x condition within study (does emotional-response type depend on bunk vs debunk?).")
    }
    cluster_rows_out <- dplyr::bind_rows(cen_rows, dplyr::bind_rows(prev_rows),
                                         dplyr::bind_rows(chi_rows)) |>
      std_row("S1-3", "affect_circumplex_clusters", "full_sample")
  }

  # ---- (4) Study 4: POST-treatment affect comparison (no pre baseline) --------
  # S4 has only the post measure (circumplex2_1_x/y), so compare the POST affect
  # coordinates between bunk and debunk (two-sample Hotelling T2), overall and per
  # model, plus the marginal post means by condition.
  s4 <- core_objects$s4$s4
  s4post_rows <- NULL
  if (!is.null(s4) && all(c("circumplex2_1_x", "circumplex2_1_y") %in% names(s4))) {
    s4 <- s4 |>
      dplyr::mutate(post_val = as.numeric(circumplex2_1_x),
                    post_aro = as.numeric(circumplex2_1_y),
                    direction = as.character(direction))
    s4_note <- paste(
      "Study 4 affect circumplex: POST-treatment coordinates only (no pre-treatment",
      "baseline was collected in Study 4), so this is a between-arm comparison of",
      "post-conversation affect rather than a change-vector analysis.")
    .s4_post_cell <- function(df, scope) {
      M <- cbind(post_val = df$post_val, post_aro = df$post_aro)
      grp <- df$direction
      ht <- .hotelling_two(M, grp)
      out <- list()
      if (!is.null(ht)) out[[1]] <- tibble::tibble(
        model = scope, direction = NA_character_, term = "hotelling_T2_bunk_vs_debunk_post",
        n = ht$n_total, statistic = ht$F, df_num = ht$df1, df_den = ht$df2,
        p_value = ht$p, note = paste0(s4_note, " Two-sample Hotelling T2 on post (valence, arousal)."))
      for (d in c("bunk", "debunk")) {
        sub <- df[df$direction == d, , drop = FALSE]
        cv <- .cplx_meanci(sub$post_val); ca <- .cplx_meanci(sub$post_aro)
        out[[length(out) + 1]] <- tibble::tibble(
          model = scope, direction = if (d == "bunk") "Bunking" else "Debunking",
          term = c("post_mean_valence", "post_mean_arousal"),
          n = sum(stats::complete.cases(cbind(sub$post_val, sub$post_aro))),
          estimate = c(cv[1], ca[1]), conf_low = c(cv[2], ca[2]), conf_high = c(cv[3], ca[3]),
          note = paste0(s4_note, " Marginal post-treatment mean (0-800 grid, uncentered)."))
      }
      dplyr::bind_rows(out)
    }
    blocks <- list(.s4_post_cell(s4, "All models"))
    for (m in levels(factor(s4$model_pooled))) {
      sm <- s4[as.character(s4$model_pooled) == m, , drop = FALSE]
      if (nrow(sm) > 10) blocks[[length(blocks) + 1]] <- .s4_post_cell(sm, m)
    }
    s4post_rows <- dplyr::bind_rows(blocks) |>
      std_row("S4 + pooled", "affect_circumplex_s4_post", "strict_n1272")
  }

  dplyr::bind_rows(change_rows, cluster_rows_out, s4post_rows)
}
