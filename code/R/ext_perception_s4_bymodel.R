# ext_perception_s4_bymodel.R ------------------------------------------------------------
# Study-4 perception mediators BY MODEL (separates the pooled perception-contrast
# figure by model). The pooled, equal-model-weighted contrasts live in
# block "secondary_contrasts"; this module reports the per-model bunk-minus-debunk contrast
# on each of the four perception items, HC3, on the strict and compliant samples.
#
# ENTRY POINT: compute_perception_s4_by_model(core_objects) -> tibble (canonical schema).
# Items (raw response scales): ArgStrength (1-5 weak->strong), new_info (1-10), Unbiased
# (1-5 biased->unbiased), Collaborative (1-5 adversarial->collaborative). The contrast is
# bunk-minus-debunk so a positive value means the item is rated higher in the bunking arm.
# block "perception_s4_by_model" (section "S4 + pooled").

.psm_items <- c(ArgStrength = "Argument strength", new_info = "Provided new information",
                Unbiased = "Impartiality", Collaborative = "Collaborativeness")

.psm_contrast <- function(df, item) {
  d <- df[!is.na(df[[item]]) & df$direction %in% c("bunk", "debunk"), , drop = FALSE]
  d$y <- suppressWarnings(as.numeric(d[[item]]))
  d$dir <- factor(as.character(d$direction), levels = c("debunk", "bunk"))  # bunk vs debunk
  d <- d[!is.na(d$y) & !is.na(d$dir), , drop = FALSE]
  if (nrow(d) < 10 || length(unique(d$dir)) < 2) return(NULL)
  fit <- stats::lm(y ~ dir, data = d)
  td <- hc3_tidy(fit)
  row <- td[td$term == "dirbunk", , drop = FALSE]
  if (!nrow(row)) return(NULL)
  tibble::tibble(
    outcome = item, model = NA, direction = NA_character_,
    term = .psm_items[[item]], n = nrow(d),
    estimate = row$estimate, se = row$std.error,
    conf_low = row$conf.low, conf_high = row$conf.high,
    statistic = row$statistic, p_value = row$p.value,
    note = "Study-4 per-model perception contrast: bunk minus debunk on the raw item scale, HC3.")
}

compute_perception_s4_by_model <- function(core_objects) {
  si_require(c("dplyr", "tibble"))
  base <- list(strict_n1272 = core_objects$s4$s4_with_compliance,
               compliant_n1073 = core_objects$s4$s4_compliant)
  out <- list()
  for (samp in names(base)) {
    df <- base[[samp]]
    if (is.null(df)) next
    for (mdl in c("Claude", "Gemini", "GPT-5.2", "Grok")) {
      sub <- df[as.character(df$model_pooled) == mdl, , drop = FALSE]
      for (it in names(.psm_items)) {
        r <- .psm_contrast(sub, it)
        if (!is.null(r)) { r$model <- mdl; out[[length(out) + 1]] <- std_row(r, "S4 + pooled", "perception_s4_by_model", samp) }
      }
    }
  }
  if (!length(out)) return(std_row(tibble::tibble(term = character(0)), "S4 + pooled", "perception_s4_by_model", "strict_n1272"))
  dplyr::bind_rows(out)
}
