# ext_moderators.R
# =============================================================================
# Classic pre-treatment MODERATION of the treatment effect, BY CONDITION and
# STUDY, for the SI recompute engine. This module replaces an earlier
# predictive-ML section: instead of an opaque feature-importance
# model, it asks the textbook question -- "for whom, and in which direction, is
# AI persuasion strongest?" -- one pre-treatment moderator at a time.
#
# ENTRY POINT:  compute_moderator_numbers(core_objects) -> tibble in the
#   canonical 17-col schema (section, block, sample, outcome, model, direction,
#   term, n, estimate, se, conf_low, conf_high, statistic, df_num, df_den,
#   p_value, note).
#
# Requires bunkbot_helpers.R already sourced (hc3_tidy, nonempty) and
# R/tables_dynamic.R for std_cols/std_row. All inputs come from core_objects
# (built from raw + cached); no CSV reads.
#
# DESIGN
#   Outcome = direction-aligned belief change (`change` in S1-3, which equals
#   aligned_belief_change there; `aligned_belief_change` in S4).
#
#   Strata = the three S1-3 GPT-4o variants kept separate (Jailbroken, Standard,
#   Truth-Constrained) PLUS the four S4 frontier models EACH kept as its own
#   stratum (Claude, Gemini, GPT-5.2, Grok). We NEVER emit a pooled / equal-
#   model-weighted Study-4 estimate.
#
#   For each stratum x CONDITION (bunk/debunk) x moderator, fit an HC3 OLS of
#   aligned change on the z-scored (within-stratum) moderator, adjusting for
#   z-scored baseline belief UNLESS the moderator IS baseline belief. The
#   reported standardized slope means: "within <condition> in <stratum>, a 1-SD
#   higher <moderator> is associated with <b> points more direction-aligned
#   change."  block = "moderators".
#
#   For each stratum x moderator, ALSO fit the pooled-condition model
#   change ~ direction * z_mod (+ z_baseline) and report the
#   direction-by-moderator INTERACTION (debunk-vs-bunk * moderator): does the
#   moderator move the bunk-vs-debunk treatment-effect gap?  block =
#   "moderator_interactions".
#
#   section = "Moderators" throughout.  Robust SEs via sandwich::vcovHC(HC3).
#
# MODERATORS (only where present/clean in a given stratum)
#   * baseline_belief : belief_rating_pre_rc (S1-3) / belief_rating_pre_4 (S4)
#   * ai_trust        : genai_trust (1-7)
#   * gcbs            : S1-3 ONLY -- rowMeans of x2_gcbs_pre..x14_gcbs_pre (1-11)
#   * ideology        : S4   -- DemRep_C (1-6, higher = more Republican) and
#                              SocialConservatism (1-5; QSF coding runs 1 = very
#                              conservative ... 5 = very liberal, so higher = more
#                              LIBERAL)
#                       S1-3 -- dem_rep_c (1-6, "Strongly Democratic" ...
#                              "Strongly Republican"; higher = more Republican),
#                              the same partisanship item as S4. (The categorical
#                              party_affil item -- 1 = Republican, 2 = Democrat,
#                              3 = Independent -- is not ordinal and is not used
#                              as a moderator.)
#   * education       : 1-8 (S1-3) / numeric (S4)
#   * age             : S1-3 -- the RAW `age` field is malformed (it is mostly
#                              year-of-birth, plus stray dates / free text /
#                              impossible values like "1050" or "7161972"); the
#                              dataset's clean derived age lives in `age_2`
#                              (year_of_birth + age_2 ~ the 2025 collection year),
#                              so we use age_2 and drop ages outside [13,100].
#                       S4   -- `age` is already a clean integer age (13-90);
#                              parsed numerically and bounded to [13,100].
# =============================================================================

# ---- small local helpers ----------------------------------------------------

# z-score within a vector (NA-safe); constant/empty vectors -> all NA.
.z <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  m <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - m) / s
}

# Clean an age field to plausible human ages (S1-3 `age_2` derived field or the
# S4 numeric `age`): parse numerically and bound to [13, 100].
# Implausible (<13 or >100) and unparseable values become NA.
.clean_age <- function(x) {
  a <- suppressWarnings(as.numeric(as.character(x)))
  a[!is.finite(a) | a < 13 | a > 100] <- NA_real_
  a
}

# Row-mean GCBS (7 paired pre items), 1-11 scale; NA if any item missing.
.gcbs_pre <- function(df) {
  gp <- c("x2_gcbs_pre", "x4_gcbs_pre", "x6_gcbs_pre", "x8_gcbs_pre",
          "x10_gcbs_pre", "x12_gcbs_pre", "x14_gcbs_pre")
  m <- sapply(gp, function(c) suppressWarnings(as.numeric(df[[c]])))
  ifelse(rowSums(is.na(m)) == 0, rowMeans(m), NA_real_)
}

# S1-3 partisanship: dem_rep_c (1 = Strongly Democratic ... 6 = Strongly
# Republican). Higher = more Republican; out-of-range values -> NA.
.s13_ideology <- function(x) {
  v <- suppressWarnings(as.numeric(as.character(x)))
  ifelse(v %in% 1:6, v, NA_real_)
}

# One per-condition standardized slope (HC3) for a single moderator.
# `adjust_baseline` adds z_baseline as a covariate (skipped when the moderator
# IS baseline). Returns a one-row pre-canonical tibble, or NULL if too sparse.
.cond_slope_row <- function(dat, term_label, adjust_baseline) {
  d <- dat[!is.na(dat$change) & !is.na(dat$z_mod), , drop = FALSE]
  if (adjust_baseline) d <- d[!is.na(d$z_base), , drop = FALSE]
  n <- nrow(d)
  if (n < 10 || stats::sd(d$z_mod) == 0) return(NULL)
  fit <- if (adjust_baseline) {
    stats::lm(change ~ z_mod + z_base, data = d)
  } else {
    stats::lm(change ~ z_mod, data = d)
  }
  td <- hc3_tidy(fit)
  row <- td[td$term == "z_mod", , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  tibble::tibble(
    term = term_label, n = n,
    estimate = row$estimate, se = row$std.error,
    conf_low = row$conf.low, conf_high = row$conf.high,
    statistic = row$statistic, p_value = row$p.value,
    note = if (adjust_baseline) "HC3; std slope (z-mod), baseline-belief adjusted"
           else "HC3; std slope (z-mod); moderator IS baseline belief"
  )
}

# Direction-by-moderator interaction (HC3): debunk-vs-bunk gap * z_mod.
# Returns a one-row pre-canonical tibble or NULL.
.interaction_row <- function(dat, term_label, adjust_baseline) {
  d <- dat[!is.na(dat$change) & !is.na(dat$z_mod), , drop = FALSE]
  if (adjust_baseline) d <- d[!is.na(d$z_base), , drop = FALSE]
  d$direction <- factor(as.character(d$direction), levels = c("bunk", "debunk"))
  d <- d[!is.na(d$direction), , drop = FALSE]
  if (length(unique(d$direction)) < 2) return(NULL)
  n <- nrow(d)
  fit <- if (adjust_baseline) {
    stats::lm(change ~ direction * z_mod + z_base, data = d)
  } else {
    stats::lm(change ~ direction * z_mod, data = d)
  }
  td <- hc3_tidy(fit)
  row <- td[td$term == "directiondebunk:z_mod", , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  tibble::tibble(
    term = term_label, n = n,
    estimate = row$estimate, se = row$std.error,
    conf_low = row$conf.low, conf_high = row$conf.high,
    statistic = row$statistic, p_value = row$p.value,
    note = paste0("HC3; debunk-vs-bunk x moderator interaction",
                  if (adjust_baseline) "; baseline-belief adjusted" else
                    "; moderator IS baseline belief")
  )
}

# Build a tidy long table of (stratum) moderator values for one analytic frame.
# `mod_specs` is a named list: name -> numeric raw moderator vector (pre-z).
# `is_baseline` names which moderator should NOT be baseline-adjusted.
# Returns canonical-ready rows for both blocks for this single stratum.
.stratum_moderator_rows <- function(change, direction, mod_specs,
                                     baseline_raw, stratum_label,
                                     sample_label) {
  z_base <- .z(baseline_raw)
  slope_rows <- list()
  inter_rows <- list()

  for (mname in names(mod_specs)) {
    raw <- mod_specs[[mname]]
    is_base <- identical(mname, "baseline_belief")
    z_mod <- .z(raw)
    if (all(is.na(z_mod))) next

    base_dat <- tibble::tibble(
      change = change, direction = direction, z_mod = z_mod, z_base = z_base
    )

    # per-condition slopes
    for (cond in c("bunk", "debunk")) {
      sub <- base_dat[base_dat$direction == cond, , drop = FALSE]
      r <- .cond_slope_row(sub, term_label = mname, adjust_baseline = !is_base)
      if (!is.null(r)) {
        r$model <- stratum_label
        r$direction <- if (cond == "bunk") "Bunking" else "Debunking"
        slope_rows[[length(slope_rows) + 1]] <- r
      }
    }

    # condition-by-moderator interaction
    ri <- .interaction_row(base_dat, term_label = mname, adjust_baseline = !is_base)
    if (!is.null(ri)) {
      ri$model <- stratum_label
      ri$direction <- NA_character_
      inter_rows[[length(inter_rows) + 1]] <- ri
    }
  }

  slopes <- if (length(slope_rows)) dplyr::bind_rows(slope_rows) else NULL
  inters <- if (length(inter_rows)) dplyr::bind_rows(inter_rows) else NULL

  out <- list()
  if (!is.null(slopes)) {
    out[[length(out) + 1]] <- std_row(slopes, "Moderators", "moderators", sample_label)
  }
  if (!is.null(inters)) {
    out[[length(out) + 1]] <- std_row(inters, "Moderators",
                                      "moderator_interactions", sample_label)
  }
  if (length(out)) dplyr::bind_rows(out) else NULL
}

# ---- entry point ------------------------------------------------------------

compute_moderator_numbers <- function(core_objects) {
  s13 <- core_objects$s13
  s4  <- core_objects$s4$s4

  all_out <- list()

  # ---- Studies 1-3: each GPT-4o variant kept as its own stratum --------------
  for (sf in c("Jailbroken", "Standard", "Truth-Constrained")) {
    d <- s13[s13$study_factor == sf, , drop = FALSE]
    if (!nrow(d)) next

    baseline_raw <- suppressWarnings(as.numeric(d$belief_rating_pre_rc))
    mod_specs <- list(
      baseline_belief = baseline_raw,
      ai_trust        = suppressWarnings(as.numeric(d$genai_trust)),
      gcbs            = .gcbs_pre(d),
      ideology        = .s13_ideology(d$dem_rep_c),
      education       = suppressWarnings(as.numeric(d$education)),
      age             = .clean_age(d$age_2)
    )

    rows <- .stratum_moderator_rows(
      change       = suppressWarnings(as.numeric(d$change)),
      direction    = as.character(d$direction),
      mod_specs    = mod_specs,
      baseline_raw = baseline_raw,
      stratum_label = sf,
      sample_label  = "full_sample"
    )
    if (!is.null(rows)) all_out[[length(all_out) + 1]] <- rows
  }

  # ---- Study 4: each frontier model kept as its OWN stratum ------------------
  # STANDING RULE: never emit a pooled / equal-model-weighted S4 estimate. We
  # split S4 into its four models (Claude, Gemini, GPT-5.2, Grok) and run the
  # identical standardized-slope computation on each model's strict subset.
  # Per-model n is ~75-200/condition; the smallest model (Gemini, ~74-77/cond)
  # can yield noisy standardized slopes -- emitted but flagged in `note`.
  for (mdl in c("Claude", "Gemini", "GPT-5.2", "Grok")) {
    d4 <- s4[as.character(s4$model_pooled) == mdl, , drop = FALSE]
    if (!nrow(d4)) next

    baseline_raw4 <- suppressWarnings(as.numeric(d4$belief_rating_pre_4))
    mod_specs4 <- list(
      baseline_belief    = baseline_raw4,
      ai_trust           = suppressWarnings(as.numeric(d4$genai_trust)),
      ideology_demrep    = suppressWarnings(as.numeric(d4$DemRep_C)),
      ideology_socialcon = suppressWarnings(as.numeric(d4$SocialConservatism)),
      education          = suppressWarnings(as.numeric(d4$Education)),  # canonical field (1,271); lower-case `education` is half-missing
      age                = .clean_age(d4$age)
    )

    rows4 <- .stratum_moderator_rows(
      change       = suppressWarnings(as.numeric(d4$aligned_belief_change)),
      direction    = as.character(d4$direction),
      mod_specs    = mod_specs4,
      baseline_raw = baseline_raw4,
      stratum_label = mdl,
      sample_label  = "strict_n1272"
    )

    # Per-model stability flag: standardized slopes from the smallest cells can
    # be unstable. Annotate any |estimate| that is large relative to its SE-
    # implied scale OR where the per-cell n is small. We flag (do not drop).
    if (!is.null(rows4)) {
      unstable <- is.finite(rows4$n) & rows4$n < 90
      if (any(unstable)) {
        rows4$note[unstable] <- paste0(rows4$note[unstable],
                                       "; SMALL-CELL (n<90): std slope may be unstable")
      }
      all_out[[length(all_out) + 1]] <- rows4
    }
  }

  out <- dplyr::bind_rows(all_out)
  out[, std_cols]
}
