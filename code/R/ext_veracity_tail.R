# ext_veracity_tail.R --------------------------------------------------------------------
# Table S14: LOW-VERACITY TAIL RATES of the persuader's fact-checkable claims.
#
# For every study (S1-4) x direction (bunk/debunk) x claim-set, this reports how often the
# persuader's claims fall into low-veracity tails. The veracity score is the per-claim 0-100
# fact-check rating from read_claim_labels(); we clip to [0,100] and drop the single 875
# out-of-range value before computing any rate.
#
# Two claim sets (outcome column):
#   * "aligned"     = the HEADLINE set: substantive claims arguing the ASSIGNED side
#                     (aligned_flag(stance, directness, direction) == TRUE). This is the
#                     denominator behind the package's veracity measure.
#   * "substantive" = ALL fact-checked claim rows in the label file (the file IS the
#                     substantive/all-extracted set), regardless of stance/directness.
#
# Per cell metrics (term column), all over non-NA, in-range veracity:
#   * n_claims      = number of fact-checked claims in the cell
#   * mean_veracity = mean veracity score
#   * p_lt10        = mean(v < 10) * 100   (% of claims scoring below 10)
#   * p_lt20        = mean(v < 20) * 100
#   * p_lt40        = mean(v < 40) * 100   (low-veracity cutoff = 40; chosen as a generous
#                     "more-false-than-true" tail; see note)
#
# S4 STUDY-LEVEL cells (model = "Study4") are EQUAL-MODEL-WEIGHTED: each metric is computed
# per model_pooled (Claude/Gemini/GPT-5.2/Grok) and then averaged across the 4 models, so the
# study cell is not dominated by Grok (which contributes ~13.5k of 27k S4 claims; claim-pooled
# mean veracity ~30 vs equal-weighted ~44). For S4 we ALSO emit per-model rows
# (model = "Study4_Claude" etc.). For S1-3 there is a single persuader model, so the study cell
# is just the claim-pooled cell.
#
# BOTH SAMPLES: "strict" = all fact-checked claims (PRIMARY); "compliant" = claims restricted to
# conversations whose ResponseId is in the per-study compliant analytic sample (cheap analogue).
#
# ENTRY POINT: compute_veracity_tail(core_objects) -> tibble (canonical 17-col schema).
#   section "Veracity"; block "veracity_tail".
#   outcome = claim set (aligned/substantive); model = study label (Study1..Study4 + per-model
#   Study4_<Model>); direction = bunk/debunk; sample = strict_claims / compliant_claims.

.VT_CUTOFF <- 40L                                   # low-veracity tail cutoff (more-false-than-true)
.VT_MODELS_S4 <- c("Claude", "Gemini", "GPT-5.2", "Grok")

# Clip + drop out-of-range / NA veracity. Returns numeric vector of in-range scores.
.vt_clean_v <- function(v) {
  v <- suppressWarnings(as.numeric(v))
  v <- v[is.finite(v) & v >= 0 & v <= 100]
  v
}

# Five tail-metric rows for one already-subset claim frame (single model / pooled).
# `vvec` = cleaned veracity vector. Returns a tibble of 5 rows (term-keyed) or NULL if empty.
.vt_metric_rows <- function(vvec, claim_set, model_lab, dir_lab, note) {
  n <- length(vvec)
  if (n == 0L) return(NULL)
  vals <- c(
    n_claims      = n,
    mean_veracity = mean(vvec),
    p_lt10        = mean(vvec < 10) * 100,
    p_lt20        = mean(vvec < 20) * 100,
    p_lt40        = mean(vvec < .VT_CUTOFF) * 100
  )
  tibble::tibble(
    outcome   = claim_set,
    model     = model_lab,
    direction = dir_lab,
    term      = names(vals),
    n         = n,
    estimate  = unname(vals),
    note      = note
  )
}

# Equal-MODEL-weighted metric rows for an S4 cell: compute each metric per model_pooled, then
# average across the models actually present. n (claim count) is SUMMED across models (a true
# claim count), but mean_veracity / p_lt* are the simple mean of the per-model values.
.vt_s4_equalwt_rows <- function(df, claim_set, dir_lab, note) {
  per <- lapply(.VT_MODELS_S4, function(m) {
    vv <- .vt_clean_v(df$veracity_score[df$model_pooled == m])
    if (!length(vv)) return(NULL)
    data.frame(
      model = m, n = length(vv),
      mean_veracity = mean(vv),
      p_lt10 = mean(vv < 10) * 100,
      p_lt20 = mean(vv < 20) * 100,
      p_lt40 = mean(vv < .VT_CUTOFF) * 100
    )
  })
  per <- do.call(rbind, per)
  if (is.null(per) || !nrow(per)) return(NULL)
  vals <- c(
    n_claims      = sum(per$n),                       # true total claim count
    mean_veracity = mean(per$mean_veracity),          # equal-model average
    p_lt10        = mean(per$p_lt10),
    p_lt20        = mean(per$p_lt20),
    p_lt40        = mean(per$p_lt40)
  )
  tibble::tibble(
    outcome   = claim_set,
    model     = "Study4",
    direction = dir_lab,
    term      = names(vals),
    n         = sum(per$n),
    estimate  = unname(vals),
    note      = sprintf("%s Equal-MODEL-weighted across %d frontier models (%s); n=summed claim count.",
                        note, nrow(per), paste(per$model, collapse = "/"))
  )
}

# All rows for one study x sample, over both claim sets and both directions.
# `lab` is the study tag (Study1..Study4). `is_s4` toggles equal-model weighting + per-model rows.
.vt_study_rows <- function(d, lab, is_s4, samp_note) {
  out <- list()
  d <- d[as.character(d$direction) %in% c("bunk", "debunk"), , drop = FALSE]
  # mark substantive vs aligned
  d$.aligned <- aligned_flag(d$stance_to_focal, d$directness_to_focal, d$direction)
  for (cs in c("aligned", "substantive")) {
    dcs <- if (cs == "aligned") d[d$.aligned %in% TRUE, , drop = FALSE] else d
    for (dd in c("bunk", "debunk")) {
      sub <- dcs[as.character(dcs$direction) == dd, , drop = FALSE]
      if (!nrow(sub)) next
      note <- sprintf("%s claims; veracity clipped to [0,100]; low-veracity cutoff=%d. %s",
                      cs, .VT_CUTOFF, samp_note)
      if (is_s4) {
        out[[length(out) + 1]] <- .vt_s4_equalwt_rows(sub, cs, dd, note)
        # per-model rows
        for (m in .VT_MODELS_S4) {
          vv <- .vt_clean_v(sub$veracity_score[sub$model_pooled == m])
          out[[length(out) + 1]] <- .vt_metric_rows(
            vv, cs, paste0("Study4_", m), dd,
            sprintf("%s claims for S4 model %s; veracity clipped to [0,100]; cutoff=%d. %s",
                    cs, m, .VT_CUTOFF, samp_note))
        }
      } else {
        vv <- .vt_clean_v(sub$veracity_score)
        out[[length(out) + 1]] <- .vt_metric_rows(vv, cs, lab, dd, note)
      }
    }
  }
  dplyr::bind_rows(out)
}

compute_veracity_tail <- function(core_objects) {
  si_require(c("dplyr", "tibble"))

  labels <- read_claim_labels(core_objects$paths)

  # Per-study compliant ResponseId sets (cheap compliant analogue: restrict claims to
  # conversations in the compliant analytic sample).
  s13 <- core_objects$s13
  comp_ids_s13 <- s13$response_id[s13$compliant %in% c(1L, TRUE)]
  comp_ids_s4  <- core_objects$s4$s4_compliant$ResponseId
  comp_ids_all <- unique(c(comp_ids_s13, comp_ids_s4))

  study_map <- c(Study1 = "Study1", Study2 = "Study2", Study3 = "Study3", Study4 = "Study4")

  build_sample <- function(lab_df, samp, samp_note) {
    out <- list()
    for (src in names(study_map)) {
      d <- lab_df[lab_df$study_source == src, , drop = FALSE]
      if (!nrow(d)) next
      out[[length(out) + 1]] <- .vt_study_rows(
        d, study_map[[src]], is_s4 = (src == "Study4"), samp_note = samp_note)
    }
    res <- dplyr::bind_rows(out)
    if (!nrow(res)) return(NULL)
    std_row(res, "Veracity", "veracity_tail", samp)
  }

  strict <- build_sample(labels, "strict_claims",
                         "Strict (all fact-checked claims; PRIMARY).")
  comp_lab <- labels[labels$conversation_id %in% comp_ids_all, , drop = FALSE]
  compliant <- build_sample(comp_lab, "compliant_claims",
                            "Compliant analogue: claims restricted to conversations in the compliant analytic sample.")

  res <- dplyr::bind_rows(strict, compliant)
  if (is.null(res) || !nrow(res)) {
    return(std_row(tibble::tibble(term = character(0)), "Veracity", "veracity_tail", "strict_claims"))
  }
  res
}
