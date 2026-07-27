# ext_demographics.R ---------------------------------------------------------------------
# Sample-composition / demographics descriptives that document randomization balance. For
# every study (S1, S2, S3, and Study 4) crossed with condition (Bunking / Debunking /
# Overall) and crossed with sample (observed outcomes / compliant), this reports the cell n, age
# mean & SD, percent female / male, mean education (1-8 scale), and a party/ideology
# summary. Because random assignment to arm is the experimental manipulation, these
# descriptives should be near-identical across Bunking and Debunking within each study; the
# Overall rows give the study-level composition.
#
# Age:  S1-3 use the CLEAN age_2 column (NEVER the raw `age`); S4 uses as.numeric(age)
#       clamped to [13, 90].
# Sex:  pct_female = 100 * mean(gender == 2); pct_male = 100 * mean(gender == 1)  -- S1-3
#       gender_2, S4 gender; only the two coded levels enter the denominator.
# Educ: S1-3 `education` (1-8); S4 the canonical `Education` (1-8, ~fully populated) -- the
#       lower-case S4 `education` is half-missing and is deliberately NOT used.
# Party/ideology: S1-3 report the percent in each party_affil category (Republican=1,
#       Democrat=2, Independent=3; the dem_rep_c crosstab confirms 1=Rep / 2=Dem) as
#       term party_pct_<label>; S4 has no categorical party, so it reports DemRep_C mean
#       (1=strong Dem ... 6=strong Rep) as term party_demrep_mean.
#
# ENTRY POINT: compute_demographics(core_objects) -> tibble (canonical schema).
# block "demographics".
#   section "S1-3"          for Studies 1-3; section "S4 + pooled" for Study 4.
#   model    = study label ("Study 1 (Jailbroken)" etc.).
#   direction= "Bunking" / "Debunking" / NA (Overall).
#   sample   = "strict" / "compliant".
#   term in {n_cell, age_mean, age_sd, pct_female, pct_male, education_mean,
#            party_pct_<label> (S1-3), party_demrep_mean (S4)};
#     estimate carries the value; for age_mean we also stash the SD in `se` for convenience.

# ---- helpers (module tag .dg_) ---------------------------------------------------------

.dg_num <- function(x) suppressWarnings(as.numeric(x))

# one descriptive row
.dg_row <- function(term, estimate, n, note, se = NA_real_, statistic = NA_real_) {
  tibble::tibble(
    outcome = NA_character_, model = NA_character_, direction = NA_character_,
    term = term, n = as.integer(n), estimate = as.numeric(estimate),
    se = as.numeric(se), conf_low = NA_real_, conf_high = NA_real_,
    statistic = as.numeric(statistic), df_num = NA_real_, df_den = NA_real_,
    p_value = NA_real_, note = note)
}

# Build every descriptive term for one cell (a data frame already subset to study x arm x
# sample). `is_s4` switches the column conventions and the party summary.
.dg_cell <- function(d, is_s4) {
  rows <- list()
  ncell <- nrow(d)
  rows[[length(rows) + 1]] <- .dg_row("n_cell", ncell, ncell, "Cell size (respondents).")

  # age ----------------------------------------------------------------------------------
  if (is_s4) {
    a <- .dg_num(d$age); a <- ifelse(a >= 13 & a <= 90, a, NA_real_)   # clamp [13,90]
  } else {
    a <- .dg_num(d$age_2)                                              # clean S1-3 age
  }
  a <- a[is.finite(a)]
  rows[[length(rows) + 1]] <- .dg_row(
    "age_mean", if (length(a)) mean(a) else NA_real_, length(a),
    "Mean age (years).", se = if (length(a) > 1) stats::sd(a) else NA_real_)
  rows[[length(rows) + 1]] <- .dg_row(
    "age_sd", if (length(a) > 1) stats::sd(a) else NA_real_, length(a),
    "SD of age (years).")

  # gender -------------------------------------------------------------------------------
  g <- .dg_num(if (is_s4) d$gender else d$gender_2)
  g <- g[g %in% c(1, 2)]
  pf <- if (length(g)) 100 * mean(g == 2) else NA_real_
  pm <- if (length(g)) 100 * mean(g == 1) else NA_real_
  rows[[length(rows) + 1]] <- .dg_row("pct_female", pf, length(g),
                                      "Percent female (gender code 2); denominator = coded respondents.")
  rows[[length(rows) + 1]] <- .dg_row("pct_male", pm, length(g),
                                      "Percent male (gender code 1); denominator = coded respondents.")

  # education ----------------------------------------------------------------------------
  e <- .dg_num(if (is_s4) d$Education else d$education)
  e <- e[is.finite(e) & e >= 1 & e <= 8]
  rows[[length(rows) + 1]] <- .dg_row(
    "education_mean", if (length(e)) mean(e) else NA_real_, length(e),
    "Mean education (1=less than high school ... 8=doctorate).")

  # race (multi-select) -------------------------------------------------------------------
  # S1-3 store the multi-select as concatenated single-digit codes ("146"); S4 as
  # comma-separated ("1,4,6"). Strip non-digits and split to a uniform per-respondent
  # category set. DEFINITIONAL CHOICE (made explicit 2026-07-04): pct_white counts
  # "White ALONE" (race category 1 selected and nothing else); respondents selecting
  # White plus another category count under pct_multiracial instead. Any manuscript
  # or SI text quoting %White must use this White-alone definition.
  rraw <- as.character(if (is_s4) d$Race else d$race)
  rraw <- rraw[!is.na(rraw) & nzchar(trimws(rraw))]
  cats <- lapply(rraw, function(s) strsplit(gsub("[^0-9]", "", s), "")[[1]])
  white_only  <- vapply(cats, function(z) length(z) == 1L && identical(z[1], "1"), logical(1))
  multiracial <- vapply(cats, function(z) length(z) > 1L, logical(1))
  rows[[length(rows) + 1]] <- .dg_row(
    "pct_white", if (length(rraw)) 100 * mean(white_only) else NA_real_, length(rraw),
    "Percent White only (race category 1 selected alone).")
  rows[[length(rows) + 1]] <- .dg_row(
    "pct_multiracial", if (length(rraw)) 100 * mean(multiracial) else NA_real_, length(rraw),
    "Percent multiracial (more than one race category selected).")

  # social / economic conservatism (5-point; collected identically in all four studies) --
  sc <- .dg_num(if (is_s4) d$SocialConservatism else d$social_conservatism); sc <- sc[is.finite(sc)]
  ec <- .dg_num(if (is_s4) d$EconomicConservatism else d$economic_conservatism); ec <- ec[is.finite(ec)]
  rows[[length(rows) + 1]] <- .dg_row(
    "social_conservatism_mean", if (length(sc)) mean(sc) else NA_real_, length(sc),
    "Mean of the 5-point social ideology item (1 = very conservative ... 5 = very liberal).", se = if (length(sc) > 1) stats::sd(sc) else NA_real_)
  rows[[length(rows) + 1]] <- .dg_row(
    "economic_conservatism_mean", if (length(ec)) mean(ec) else NA_real_, length(ec),
    "Mean of the 5-point economic ideology item (1 = very conservative ... 5 = very liberal).", se = if (length(ec) > 1) stats::sd(ec) else NA_real_)

  # party / ideology ---------------------------------------------------------------------
  if (is_s4) {
    dr <- .dg_num(d$DemRep_C); dr <- dr[is.finite(dr)]
    rows[[length(rows) + 1]] <- .dg_row(
      "party_demrep_mean", if (length(dr)) mean(dr) else NA_real_, length(dr),
      "Mean Dem-Rep ideology (1=strong Democrat ... higher=more Republican).",
      se = if (length(dr) > 1) stats::sd(dr) else NA_real_)
  } else {
    p <- .dg_num(d$party_affil)
    labs <- c(`1` = "Republican", `2` = "Democrat", `3` = "Independent")
    pv <- p[p %in% c(1, 2, 3)]
    denom <- length(pv)
    for (k in names(labs)) {
      pct <- if (denom) 100 * mean(pv == as.numeric(k)) else NA_real_
      rows[[length(rows) + 1]] <- .dg_row(
        paste0("party_pct_", labs[[k]]), pct, denom,
        sprintf("Percent %s (party_affil==%s); denominator = R/D/I respondents.", labs[[k]], k))
    }
  }
  dplyr::bind_rows(rows)
}

# Emit Bunking / Debunking / Overall rows for one study frame, one sample.
.dg_study_sample <- function(d, study_label, section, sample_label, is_s4) {
  out <- list()
  dir_raw <- as.character(d$direction)
  cells <- list(
    Bunking   = d[dir_raw == "bunk", , drop = FALSE],
    Debunking = d[dir_raw == "debunk", , drop = FALSE],
    Overall   = d)
  for (lab in names(cells)) {
    sub <- cells[[lab]]
    if (!nrow(sub)) next
    r <- .dg_cell(sub, is_s4)
    r$model <- study_label
    r$direction <- if (lab == "Overall") NA_character_ else lab
    out[[length(out) + 1]] <- std_row(r, section, "demographics", sample_label)
  }
  dplyr::bind_rows(out)
}

# ---- entry point -----------------------------------------------------------------------

compute_demographics <- function(core_objects) {
  si_require(c("dplyr", "tibble"))
  out <- list()

  # Studies 1-3 ---------------------------------------------------------------------------
  s13 <- core_objects$s13
  s13_labels <- c(Jailbroken = "Study 1 (Jailbroken)",
                  Standard = "Study 2 (Standard)",
                  `Truth-Constrained` = "Study 3 (Truth-Constrained)")
  for (sf in names(s13_labels)) {
    d_all <- s13[as.character(s13$study_factor) == sf, , drop = FALSE]
    if (!nrow(d_all)) next
    # strict = full analytic sample for the study; compliant = compliant flag TRUE
    d_comp <- d_all[!is.na(d_all$compliant) & d_all$compliant %in% c(TRUE, 1, "TRUE"), , drop = FALSE]
    out[[length(out) + 1]] <- .dg_study_sample(d_all, s13_labels[[sf]], "S1-3", "strict", FALSE)
    if (nrow(d_comp)) out[[length(out) + 1]] <- .dg_study_sample(d_comp, s13_labels[[sf]], "S1-3", "compliant", FALSE)
  }

  # Study 4 -------------------------------------------------------------------------------
  # Break Study 4 into its four frontier models (NEVER a pooled Study-4 row): one set of
  # Bunking/Debunking/Overall demographic cells per model, for both samples.
  s4_strict <- core_objects$s4$s4
  s4_comp <- core_objects$s4$s4_compliant
  for (mdl in c("Claude", "Gemini", "GPT-5.2", "Grok")) {
    lab <- sprintf("Study 4 (%s)", mdl)
    if (!is.null(s4_strict) && nrow(s4_strict)) {
      dm <- s4_strict[as.character(s4_strict$model_pooled) == mdl, , drop = FALSE]
      if (nrow(dm)) out[[length(out) + 1]] <- .dg_study_sample(dm, lab, "S4 + pooled", "strict", TRUE)
    }
    if (!is.null(s4_comp) && nrow(s4_comp)) {
      dc <- s4_comp[as.character(s4_comp$model_pooled) == mdl, , drop = FALSE]
      if (nrow(dc)) out[[length(out) + 1]] <- .dg_study_sample(dc, lab, "S4 + pooled", "compliant", TRUE)
    }
  }

  if (!length(out))
    return(std_row(tibble::tibble(term = character(0)), "S1-3", "demographics", "strict"))
  dplyr::bind_rows(out)
}
