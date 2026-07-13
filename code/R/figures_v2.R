# figures_v2.R — ALL dynamic ggplot figures for the SI.
# Every function reads the live-recomputed `all_numbers` (default arg `numbers`
# falls back to the global) and returns a ggplot. NO embedded images;
# everything is rebuilt from the canonical (section/block/sample/outcome/model/
# direction/term) schema defined by `std_cols` in code/R/tables_dynamic.R.
#
# Theme + palette (theme_si(), bb_colors) are defined here so the supplement is
# visually consistent across every figure.

# ---- theme + palette (copied from CODEX/R/figures_core.R) -------------------

bb_colors <- c("Bunking" = "#D81B60", "Debunking" = "#1E88E5", "bunk" = "#D81B60", "debunk" = "#1E88E5")

theme_si <- function(base_size = 9) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# ---- small helpers ----------------------------------------------------------

.an <- function(numbers) {
  if (!is.null(numbers)) return(numbers)
  get("all_numbers", envir = globalenv())
}

# normalise raw direction tokens to display labels
.dir_factor <- function(x) {
  x <- as.character(x)
  x <- dplyr::recode(x, bunk = "Bunking", debunk = "Debunking")
  factor(x, levels = c("Bunking", "Debunking"))
}

.model_levels_s4 <- c("Claude", "Gemini", "GPT-5.2", "Grok")

# =============================================================================
# 1. fig_belief_4study  — belief change, all four studies on one forest
#    blocks: belief_change (S1-3) + raw_aligned_means_cells (S4, strict_n1272,
#            outcome=aligned_belief_change)
# =============================================================================
fig_belief_4study <- function(numbers = NULL) {
  an <- .an(numbers)

  # Studies 1-3 have a single analytic sample; label it "Strict (ITT)" so it
  # aligns with the Study-4 strict cells on the shared shape/linetype scale.
  s13 <- an |>
    dplyr::filter(.data$section == "S1-3", .data$block == "belief_change") |>
    dplyr::transmute(
      study = "Studies 1-3 (GPT-4o)",
      Sample = "Strict (ITT)",
      model = .data$model,
      direction = .data$direction,
      estimate = .data$estimate, conf_low = .data$conf_low, conf_high = .data$conf_high
    )

  # Studies 1-3 also carry a compliant subset (evaluator_label==1 &
  # reverse_evaluator_label==1); plot it so S1-3 mirror the S4 strict+compliant
  # cells on the shared Sample shape/linetype scale.
  s13_comp <- an |>
    dplyr::filter(.data$section == "S1-3", .data$block == "belief_change_compliant") |>
    dplyr::transmute(
      study = "Studies 1-3 (GPT-4o)",
      Sample = "Compliant",
      model = .data$model,
      direction = .data$direction,
      estimate = .data$estimate, conf_low = .data$conf_low, conf_high = .data$conf_high
    )

  # Study 4: show BOTH the strict (ITT) and compliant cells.
  s4 <- an |>
    dplyr::filter(
      .data$block == "raw_aligned_means_cells",
      .data$sample %in% c("strict_n1272", "compliant_n1073"),
      .data$outcome == "aligned_belief_change"
    ) |>
    dplyr::transmute(
      study = "Study 4 (frontier models)",
      Sample = dplyr::recode(.data$sample,
                             strict_n1272 = "Strict (ITT)", compliant_n1073 = "Compliant"),
      model = .data$model,
      direction = .data$direction,
      estimate = .data$estimate, conf_low = .data$conf_low, conf_high = .data$conf_high
    )

  dat <- dplyr::bind_rows(s13, s13_comp, s4) |>
    dplyr::mutate(
      direction = .dir_factor(.data$direction),
      Sample = factor(.data$Sample, levels = c("Strict (ITT)", "Compliant")),
      model = factor(.data$model,
                     levels = c("Jailbroken", "Standard", "Truth-Constrained", .model_levels_s4)),
      study = factor(.data$study, levels = c("Studies 1-3 (GPT-4o)", "Study 4 (frontier models)"))
    )

  ggplot2::ggplot(dat, ggplot2::aes(x = .data$estimate, y = .data$model,
                                    color = .data$direction,
                                    shape = .data$Sample, linetype = .data$Sample,
                                    group = interaction(.data$direction, .data$Sample))) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high),
                            height = .2, position = ggplot2::position_dodge(width = .6)) +
    ggplot2::geom_point(size = 2.2, position = ggplot2::position_dodge(width = .6),
                        fill = "white") +
    ggplot2::facet_grid(study ~ ., scales = "free_y", space = "free_y") +
    ggplot2::scale_color_manual(values = bb_colors) +
    ggplot2::scale_shape_manual(values = c("Strict (ITT)" = 16, "Compliant" = 21)) +
    ggplot2::scale_linetype_manual(values = c("Strict (ITT)" = "solid", "Compliant" = "22")) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::labs(
      title = "Direction-aligned belief change across all four studies",
      x = "Belief change toward the assigned AI position (0-100 points; 95% CI)",
      y = NULL, color = NULL, shape = "Sample", linetype = "Sample"
    ) +
    theme_si()
}

# =============================================================================
# 2. fig_compliance_4study — first-turn APE attempt rate by study/model x dir
#    blocks: compliance (the dedicated S1-3 attempt-rate generator) +
#            compliance_cells (S4 strict_n1272, term=attempt_rate)
# =============================================================================
fig_compliance_4study <- function(numbers = NULL) {
  an <- .an(numbers)

  s13 <- an |>
    dplyr::filter(.data$block == "compliance", .data$term == "ape_attempt_rate") |>
    dplyr::transmute(
      study = "Studies 1-3",
      model = dplyr::recode(.data$model,
                            "Jailbroken" = "Study 1", "Standard" = "Study 2",
                            "Truth-Constrained" = "Study 3"),
      direction = dplyr::recode(tolower(.data$direction),
                                "bunking" = "bunk", "debunking" = "debunk"),
      estimate = .data$estimate
    )

  s4 <- an |>
    dplyr::filter(
      .data$block == "compliance_cells",
      .data$sample == "strict_n1272",
      .data$term == "attempt_rate"
    ) |>
    dplyr::transmute(
      study = "Study 4", model = .data$model,
      direction = .data$direction, estimate = .data$estimate
    )

  dat <- dplyr::bind_rows(s13, s4) |>
    dplyr::mutate(
      direction = .dir_factor(.data$direction),
      model = factor(.data$model,
                     levels = c("Study 1", "Study 2", "Study 3", .model_levels_s4))
    )

  ggplot2::ggplot(dat, ggplot2::aes(x = .data$model, y = .data$estimate, fill = .data$direction)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = .75), width = .65) +
    ggplot2::facet_grid(~ study, scales = "free_x", space = "free_x") +
    ggplot2::scale_fill_manual(values = bb_colors) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::labs(
      title = "First-turn attempt-to-persuade rate by study, model, and direction",
      x = NULL, y = "Share attempting assigned direction", fill = NULL
    ) +
    theme_si() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
}

# =============================================================================
# 3. fig_distribution_thresholds — share with aligned shift >= T (20/30/40/50)
#    block: large_shift_thresholds (terms share_ge20/30/40/50), S1-3 + S4
# =============================================================================
fig_distribution_thresholds <- function(numbers = NULL) {
  an <- .an(numbers)

  dat <- an |>
    dplyr::filter(.data$block == "large_shift_thresholds",
                  grepl("^share_ge", .data$term)) |>
    dplyr::filter(!is.na(.data$model), !is.na(.data$direction)) |>
    dplyr::mutate(
      threshold = as.integer(sub("share_ge", "", .data$term)),
      threshold = factor(paste0("≥", .data$threshold, " pts"),
                         levels = paste0("≥", c(20, 30, 40, 50), " pts")),
      direction = .dir_factor(.data$direction),
      model = factor(.data$model,
                     levels = c("Jailbroken", "Standard", "Truth-Constrained",
                                "Pooled", .model_levels_s4))
    ) |>
    dplyr::filter(.data$model != "Pooled")  # keep per-model cells; Pooled is redundant

  ggplot2::ggplot(dat, ggplot2::aes(x = .data$threshold, y = .data$estimate,
                                    color = .data$direction, group = .data$direction)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::facet_wrap(~ model, nrow = 2) +
    ggplot2::scale_color_manual(values = bb_colors) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(
      title = "Tail of the belief-change distribution: share with large aligned shifts",
      x = "Aligned shift threshold", y = "Share of participants", color = NULL
    ) +
    theme_si(base_size = 8) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

# =============================================================================
# 4. fig_veracity — aligned veracity 0-100 by study/model x direction (CI)
#    block: table_aligned_veracity (Veracity, term=aligned_veracity)
# =============================================================================
fig_veracity <- function(numbers = NULL) {
  an <- .an(numbers)

  dat <- an |>
    dplyr::filter(.data$block == "table_aligned_veracity",
                  .data$term == "aligned_veracity") |>
    dplyr::mutate(
      direction = .dir_factor(.data$direction),
      model = factor(.data$model,
                     levels = c("Study 1", "Study 2", "Study 3", .model_levels_s4))
    )

  ggplot2::ggplot(dat, ggplot2::aes(x = .data$estimate, y = .data$model, color = .data$direction)) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high),
                            height = .2, position = ggplot2::position_dodge(width = .5)) +
    ggplot2::geom_point(size = 2.2, position = ggplot2::position_dodge(width = .5)) +
    ggplot2::scale_color_manual(values = bb_colors) +
    ggplot2::scale_x_continuous(limits = c(0, 100)) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::labs(
      title = "Veracity of direction-aligned claims by study, model, and direction",
      x = "Mean aligned-claim veracity (0 = false, 100 = true; 95% CI)",
      y = NULL, color = NULL
    ) +
    theme_si()
}

# =============================================================================
# 5. fig_veracity_persuasion — per-cell aligned veracity (x) vs belief change (y)
#    blocks: accuracy_panel_metrics (term=Intended-direction veracity) for x,
#            accuracy_panel_scatter (term=belief_cell_estimate) for y.
#    One point per study/model x direction cell (strict_n1272).
# =============================================================================
fig_veracity_persuasion <- function(numbers = NULL) {
  an <- .an(numbers)

  .samp_lab <- c(strict_n1272 = "Strict (ITT)", compliant_n1073 = "Compliant")

  # Read BOTH samples for veracity (x) and belief change (y); join within sample.
  ver <- an |>
    dplyr::filter(.data$block == "accuracy_panel_metrics",
                  .data$sample %in% names(.samp_lab),
                  .data$term == "Intended-direction veracity") |>
    dplyr::transmute(sample = .data$sample, model = .data$model,
                     direction = .data$direction, veracity = .data$estimate)

  bel <- an |>
    dplyr::filter(.data$block == "accuracy_panel_scatter",
                  .data$sample %in% names(.samp_lab),
                  .data$term == "belief_cell_estimate") |>
    dplyr::transmute(sample = .data$sample, model = .data$model,
                     direction = .data$direction,
                     belief = .data$estimate,
                     belief_lo = .data$conf_low, belief_hi = .data$conf_high)

  dat <- dplyr::inner_join(ver, bel, by = c("sample", "model", "direction")) |>
    dplyr::mutate(
      Sample = factor(.samp_lab[.data$sample],
                      levels = c("Strict (ITT)", "Compliant")),
      direction = .dir_factor(.data$direction),
      model = factor(.data$model, levels = .model_levels_s4)
    )

  ggplot2::ggplot(dat, ggplot2::aes(x = .data$veracity, y = .data$belief,
                                    color = .data$direction, shape = .data$model)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::geom_linerange(ggplot2::aes(ymin = .data$belief_lo, ymax = .data$belief_hi),
                            alpha = .5) +
    ggplot2::geom_point(size = 3) +
    ggplot2::facet_wrap(~ Sample) +
    ggplot2::scale_color_manual(values = bb_colors) +
    ggplot2::scale_x_continuous(limits = c(0, 100)) +
    ggplot2::labs(
      title = "Aligned-claim veracity does not track persuasive impact",
      x = "Mean aligned-claim veracity (0-100)",
      y = "Direction-aligned belief change (points; 95% CI)",
      color = NULL, shape = NULL
    ) +
    theme_si()
}

# =============================================================================
# 6. fig_volume_asymmetry — aligned-claim volume vs belief change, bunk vs debunk
#    blocks: claim_role_by_direction (term=mean_aligned_direct_n) for volume;
#            raw_aligned_means_cells (aligned_belief_change) pooled-by-direction
#            for the matching belief change. (claim_volume block is study-level
#            descriptive only with no direction split, so the per-direction
#            aligned-volume asymmetry is read from claim_role_by_direction.)
# =============================================================================
fig_volume_asymmetry <- function(numbers = NULL) {
  an <- .an(numbers)
  .samp_lab <- c(strict_n1272 = "Strict (ITT)", compliant_n1073 = "Compliant")

  vd <- an |> dplyr::filter(.data$block == "claim_volume_detail")
  # binned means (points + CI), x = mean log(aligned claims)
  bins <- vd |>
    dplyr::filter(.data$outcome == "binned_mean") |>
    dplyr::transmute(
      Sample = factor(.samp_lab[.data$sample], levels = .samp_lab),
      direction = factor(.data$direction, levels = c("Bunking", "Debunking")),
      x = .data$statistic, m = .data$estimate,
      lo = .data$conf_low, hi = .data$conf_high, nb = .data$n)
  # OLS slope lines: y = intercept (statistic) + slope (estimate) * x, over the bin x-range
  slopes <- vd |> dplyr::filter(.data$term == "volume_slope")
  segs <- do.call(rbind, lapply(seq_len(nrow(slopes)), function(i) {
    s <- slopes[i, ]
    xr <- range(bins$x[bins$Sample == .samp_lab[s$sample] &
                        bins$direction == s$direction], na.rm = TRUE)
    if (!all(is.finite(xr))) return(NULL)
    data.frame(Sample = factor(unname(.samp_lab[s$sample]), levels = .samp_lab),
               direction = factor(s$direction, levels = c("Bunking", "Debunking")),
               x = xr, y = as.vector(s$statistic + s$estimate * xr),
               row.names = NULL)
  }))
  lab <- slopes |>
    dplyr::transmute(
      Sample = factor(.samp_lab[.data$sample], levels = .samp_lab),
      direction = factor(.data$direction, levels = c("Bunking", "Debunking")),
      txt = paste0(.data$direction, ": ", sprintf("%+.1f", .data$estimate), " pts / e-fold"))
  lab <- lab |> dplyr::group_by(.data$Sample) |>
    dplyr::summarise(txt = paste(.data$txt, collapse = "\n"), .groups = "drop")

  xb <- log(c(2, 5, 10, 20, 40, 80))
  ggplot2::ggplot(bins, ggplot2::aes(color = .data$direction)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey75", linewidth = .3) +
    ggplot2::geom_line(data = segs, ggplot2::aes(x = .data$x, y = .data$y, color = .data$direction),
                       linewidth = 1) +
    ggplot2::geom_errorbar(ggplot2::aes(x = .data$x, ymin = .data$lo, ymax = .data$hi),
                           width = 0, linewidth = .4) +
    ggplot2::geom_point(ggplot2::aes(x = .data$x, y = .data$m, size = .data$nb),
                        shape = 21, fill = "white", stroke = .8) +
    ggplot2::geom_text(data = lab, ggplot2::aes(x = min(xb), y = -8, label = .data$txt),
                       inherit.aes = FALSE, hjust = 0, vjust = 1, size = 2.6,
                       color = "grey25", fontface = "italic") +
    ggplot2::facet_wrap(~ Sample) +
    ggplot2::scale_color_manual(values = bb_colors) +
    ggplot2::scale_size_continuous(range = c(2, 7), guide = "none") +
    ggplot2::scale_x_continuous(breaks = xb, labels = c(2, 5, 10, 20, 40, 80)) +
    ggplot2::labs(
      title = "Bunking scales with claim volume; debunking does not",
      subtitle = "Aligned belief change vs number of fact-checkable aligned claims (binned means, 95% CI; lines = OLS)",
      x = "Aligned claims in the conversation (log scale)",
      y = "Aligned belief change (points)", color = NULL) +
    theme_si()
}

# =============================================================================
# 7. fig_topic_effects — per-topic mean direction-aligned belief change,
#    bunk vs debunk; topics with adequate n, ordered by bunk-minus-debunk gap.
#    block: topic_effects (Topic, term=topic_mean_belief_change)
# =============================================================================
fig_topic_effects <- function(numbers = NULL) {
  an <- .an(numbers)

  raw <- an |>
    dplyr::filter(.data$block == "topic_effects",
                  .data$term == "topic_mean_belief_change",
                  .data$model != "Mixed / Unclassified")

  # adequate-n topics: require both bunk and debunk cells and total n >= 30
  keep <- raw |>
    dplyr::group_by(topic = .data$model) |>
    dplyr::summarise(total_n = sum(.data$n, na.rm = TRUE),
                     k = dplyr::n_distinct(.data$direction), .groups = "drop") |>
    dplyr::filter(.data$total_n >= 30, .data$k == 2)

  gap <- raw |>
    dplyr::filter(.data$model %in% keep$topic) |>
    dplyr::select(topic = .data$model, .data$direction, .data$estimate) |>
    tidyr::pivot_wider(names_from = "direction", values_from = "estimate") |>
    dplyr::mutate(gap = .data$Bunking - .data$Debunking)

  dat <- raw |>
    dplyr::filter(.data$model %in% keep$topic) |>
    dplyr::mutate(direction = factor(.data$direction, levels = c("Bunking", "Debunking"))) |>
    dplyr::left_join(dplyr::select(gap, "topic", "gap"),
                     by = c("model" = "topic")) |>
    dplyr::mutate(topic = stats::reorder(.data$model, .data$gap))

  ggplot2::ggplot(dat, ggplot2::aes(x = .data$estimate, y = .data$topic, color = .data$direction)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::geom_line(ggplot2::aes(group = .data$topic), color = "grey75") +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_manual(values = bb_colors) +
    ggplot2::labs(
      title = "Topic-specific belief change (ordered by bunk-minus-debunk gap)",
      x = "Mean direction-aligned belief change (points)", y = NULL, color = NULL
    ) +
    theme_si(base_size = 8)
}

# =============================================================================
# 7b. fig_topic_contrasts — per-topic HC3 bunk-minus-debunk REGRESSION contrast
#     with 95% CIs (block: topic_contrasts, term=bunk_vs_debunk_contrast).
#     Replaces the raw-cell-mean topic figure with inferential contrasts.
# =============================================================================
fig_topic_contrasts <- function(numbers = NULL) {
  an <- .an(numbers)

  dat <- an |>
    dplyr::filter(.data$block == "topic_contrasts",
                  .data$term == "bunk_vs_debunk_contrast") |>
    dplyr::transmute(topic = .data$model,
                     Sample = factor(dplyr::recode(.data$sample,
                                                   strict = "Strict (ITT)",
                                                   compliant = "Compliant"),
                                     levels = c("Strict (ITT)", "Compliant")),
                     estimate = .data$estimate,
                     conf_low = .data$conf_low, conf_high = .data$conf_high)
  ord <- dat |> dplyr::filter(.data$Sample == "Strict (ITT)") |>
    dplyr::arrange(.data$estimate) |> dplyr::pull(.data$topic)
  ord <- c(ord, setdiff(unique(dat$topic), ord))
  dat <- dat |> dplyr::mutate(topic = factor(.data$topic, levels = ord))

  dodge <- ggplot2::position_dodge(width = 0.55)
  ggplot2::ggplot(dat, ggplot2::aes(x = .data$estimate, y = .data$topic, color = .data$Sample)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high),
      orientation = "y", width = 0, position = dodge) +
    ggplot2::geom_point(size = 2, position = dodge) +
    ggplot2::scale_color_manual(values = c("Strict (ITT)" = "#5E35B1", "Compliant" = "#00897B")) +
    ggplot2::labs(
      title = "Topic-specific bunk-minus-debunk belief-change contrast (HC3, 95% CI)",
      x = "Bunking − debunking aligned belief change (points)", y = NULL, color = NULL
    ) +
    theme_si(base_size = 8)
}

# =============================================================================
# 8. fig_public_speech — Study-4 public speech, coherent two-panel:
#    Panel A: private belief change by model x direction
#             (raw_aligned_means_cells, aligned_belief_change)
#    Panel B: weighted public sharing by model x direction
#             (raw_aligned_means_cells, aligned_new_minus_old_weighted)
# =============================================================================
fig_public_speech <- function(numbers = NULL) {
  an <- .an(numbers)

  # Show BOTH the strict (ITT) and compliant samples in each panel.
  base <- an |>
    dplyr::filter(.data$block == "raw_aligned_means_cells",
                  .data$sample %in% c("strict_n1272", "compliant_n1073"),
                  .data$outcome %in% c("aligned_belief_change",
                                       "aligned_new_minus_old_weighted")) |>
    dplyr::mutate(
      direction = .dir_factor(.data$direction),
      Sample = factor(dplyr::recode(.data$sample,
                                    strict_n1272 = "Strict (ITT)",
                                    compliant_n1073 = "Compliant"),
                      levels = c("Strict (ITT)", "Compliant")),
      model = factor(.data$model, levels = .model_levels_s4)
    )

  panel <- function(oc, title) {
    d <- dplyr::filter(base, .data$outcome == oc)
    ggplot2::ggplot(d, ggplot2::aes(x = .data$estimate, y = .data$model,
                                    color = .data$direction,
                                    shape = .data$Sample, linetype = .data$Sample,
                                    group = interaction(.data$direction, .data$Sample))) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
      ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high),
                              height = .18, position = ggplot2::position_dodge(width = .6)) +
      ggplot2::geom_point(size = 2.2, fill = "white",
                          position = ggplot2::position_dodge(width = .6)) +
      ggplot2::scale_color_manual(values = bb_colors) +
      ggplot2::scale_shape_manual(values = c("Strict (ITT)" = 16, "Compliant" = 21)) +
      ggplot2::scale_linetype_manual(values = c("Strict (ITT)" = "solid", "Compliant" = "22")) +
      ggplot2::scale_y_discrete(limits = rev) +
      ggplot2::labs(title = title, x = NULL, y = NULL, color = NULL,
                    shape = "Sample", linetype = "Sample") +
      theme_si()
  }

  pa <- panel("aligned_belief_change",
              "A. Private belief change") +
    ggplot2::labs(x = "Aligned belief change (points; 95% CI)")
  pb <- panel("aligned_new_minus_old_weighted",
              "B. Weighted public sharing") +
    ggplot2::labs(x = "Aligned weighted post-sharing change (95% CI)")

  patchwork::wrap_plots(pa, pb, ncol = 1, guides = "collect") +
    patchwork::plot_annotation(
      title = "Study 4: private belief change and public sharing align in direction"
    ) &
    ggplot2::theme(legend.position = "bottom")
}

# =============================================================================
# 10. fig_post_composition — stance-category share of posts, pre vs post,
#     by direction. block: post_stance_composition (outcome=post_stance_category;
#     pre_*/post_* terms; directions debunk_all_models / bunk_complying_models /
#     bunk_gpt52).
# =============================================================================
fig_post_composition <- function(numbers = NULL) {
  an <- .an(numbers)

  stance_lab <- c(
    endorses      = "Endorses",
    leans_for     = "Leans for",
    neutral_mixed = "Neutral / mixed",
    no_stance     = "No stance",
    leans_against = "Leans against",
    rejects       = "Rejects"
  )
  stance_levels <- unname(stance_lab)

  dat <- an |>
    dplyr::filter(.data$block == "post_stance_composition",
                  .data$outcome == "post_stance_category",
                  # drop the separate GPT-5.2 facet (its defection is covered in the prose)
                  .data$direction %in% c("debunk_all_models", "bunk_complying_models")) |>
    dplyr::mutate(
      phase = ifelse(grepl("^pre_", .data$term), "Pre", "Post"),
      stance_key = sub("^(pre|post)_", "", .data$term),
      stance = factor(stance_lab[.data$stance_key], levels = stance_levels),
      phase = factor(.data$phase, levels = c("Pre", "Post")),
      cell = dplyr::recode(.data$direction,
        debunk_all_models     = "Debunking\n(all models)",
        bunk_complying_models = "Bunking\n(complying models)")
    ) |>
    dplyr::filter(!is.na(.data$stance))

  ggplot2::ggplot(dat, ggplot2::aes(x = .data$phase, y = .data$estimate, fill = .data$stance)) +
    ggplot2::geom_col(position = "fill", width = .7) +
    ggplot2::facet_wrap(~ cell) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_fill_brewer(palette = "RdBu", direction = -1) +
    ggplot2::labs(
      title = "Public-post stance composition before vs after the conversation",
      x = NULL, y = "Share of posts", fill = "Post stance"
    ) +
    theme_si()
}

# =============================================================================
# 10b. fig_post_form_composition — Study-4 communicative-mode composition of the
#      public social-media post, pre vs post conversation, by direction.
#      block: post_form_composition (sample strict_n1272,
#      outcome=post_response_type; pre_*/post_* terms; directions
#      pooled / bunk / debunk). estimate is ALREADY a percent (0-100); n = count.
# =============================================================================
fig_post_form_composition <- function(numbers = NULL) {
  an <- .an(numbers)

  type_lab <- c(
    assertion                 = "Assertion",
    question_raising          = "Question-raising",
    mixed_assertion_question  = "Mixed assertion / question",
    uncertainty_statement     = "Uncertainty statement",
    declines_to_post          = "Declines to post",
    meta_task                 = "Meta / off-task",
    unclassifiable            = "Unclassifiable"
  )
  type_levels <- unname(type_lab)

  dat <- an |>
    dplyr::filter(.data$block == "post_form_composition",
                  .data$sample == "strict_n1272",
                  .data$outcome == "post_response_type",
                  .data$direction %in% c("bunk", "debunk")) |>
    dplyr::mutate(
      timepoint = ifelse(grepl("^pre_", .data$term), "Pre", "Post"),
      type_key  = sub("^(pre|post)_", "", .data$term),
      type      = factor(type_lab[.data$type_key], levels = type_levels),
      timepoint = factor(.data$timepoint, levels = c("Pre", "Post")),
      direction = .dir_factor(.data$direction)
    ) |>
    dplyr::filter(!is.na(.data$type))

  ggplot2::ggplot(dat, ggplot2::aes(x = .data$timepoint, y = .data$estimate,
                                    fill = .data$type)) +
    ggplot2::geom_col(width = .7) +
    ggplot2::facet_wrap(~ direction) +
    ggplot2::scale_y_continuous(labels = function(v) paste0(v, "%")) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::labs(
      title = "Communicative mode of the public post before vs after the conversation",
      x = NULL, y = "Share of posts", fill = "Response type"
    ) +
    theme_si()
}

# =============================================================================
# 11. fig_secondary — bunk-minus-debunk on AI-perception items across studies.
#     blocks: perception_mediators (S1-3, term=bunk_minus_debunk) +
#             secondary_contrasts (S4, strict_n1272; debunk-minus-bunk style
#             contrast on the same perception family). S4 contrasts are sign-
#             flipped to a common bunk-minus-debunk orientation for comparability.
# =============================================================================
fig_secondary <- function(numbers = NULL) {
  an <- .an(numbers)

  # Studies 1-3 now carry BOTH samples (de-pool cleanup): map the full analytic
  # sample -> "Strict (ITT)" and the compliant subset -> "Compliant" so they
  # align with the Study-4 strict/compliant cells on the shared shape/linetype
  # scale.
  s13 <- an |>
    dplyr::filter(.data$block == "perception_mediators",
                  .data$term == "bunk_minus_debunk",
                  .data$sample %in% c("full_sample", "compliant")) |>
    dplyr::transmute(
      study = .data$model,
      Sample = dplyr::recode(.data$sample,
                             full_sample = "Strict (ITT)", compliant = "Compliant"),
      item = .data$outcome,
      estimate = .data$estimate, conf_low = .data$conf_low, conf_high = .data$conf_high
    )

  # S4 perception items, PER MODEL (no pooling) from perception_s4_by_model:
  # bunk-minus-debunk contrast on the same family, both samples.
  s4 <- an |>
    dplyr::filter(.data$block == "perception_s4_by_model",
                  .data$sample %in% c("strict_n1272", "compliant_n1073")) |>
    dplyr::transmute(
      study = .data$model,
      Sample = dplyr::recode(.data$sample,
                             strict_n1272 = "Strict (ITT)", compliant_n1073 = "Compliant"),
      item = dplyr::recode(.data$term, Impartiality = "Impartiality (unbiased)"),
      estimate = .data$estimate, conf_low = .data$conf_low, conf_high = .data$conf_high
    )

  dat <- dplyr::bind_rows(s13, s4) |>
    dplyr::mutate(
      study = factor(.data$study,
                     levels = c("Jailbroken", "Standard", "Truth-Constrained",
                                "Claude", "Gemini", "GPT-5.2", "Grok")),
      Sample = factor(.data$Sample, levels = c("Strict (ITT)", "Compliant")),
      item = factor(.data$item,
                    levels = c("Argument strength", "Provided new information",
                               "Collaborativeness", "Impartiality (unbiased)"))
    )

  ggplot2::ggplot(dat, ggplot2::aes(x = .data$estimate, y = .data$study,
                                    color = .data$study,
                                    shape = .data$Sample, linetype = .data$Sample,
                                    group = .data$Sample)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high),
                            height = .2, position = ggplot2::position_dodge(width = .5)) +
    ggplot2::geom_point(size = 2.2, fill = "white",
                        position = ggplot2::position_dodge(width = .5)) +
    ggplot2::facet_wrap(~ item, scales = "free_x") +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::scale_shape_manual(values = c("Strict (ITT)" = 16, "Compliant" = 21)) +
    ggplot2::scale_linetype_manual(values = c("Strict (ITT)" = "solid", "Compliant" = "22")) +
    ggplot2::guides(color = "none") +
    ggplot2::labs(
      title = "Bunk-minus-debunk gap on AI-perception items across studies",
      x = "Bunking minus debunking (perception scale units; 95% CI)",
      y = NULL, color = NULL, shape = "Sample", linetype = "Sample"
    ) +
    theme_si(base_size = 8)
}

# =============================================================================
# 12. fig_debrief_trajectory — post-to-debrief belief shift by study/model.
#     blocks: debrief (S1-3, term=post_to_debrief_shift, Bunking) +
#             debrief_shift (S4 strict_n1272, term=post_to_debrief_change)
# =============================================================================
fig_debrief_trajectory <- function(numbers = NULL) {
  an <- .an(numbers)

  # Studies 1-3 now carry BOTH samples (de-pool cleanup): map the full analytic
  # sample -> "Strict (ITT)" and the compliant subset -> "Compliant". Debrief is
  # Bunking-arm-only in S1-3 by design, so only the Bunking rows exist in each
  # sample; that is expected.
  s13 <- an |>
    dplyr::filter(.data$block == "debrief", .data$term == "post_to_debrief_shift",
                  .data$sample %in% c("full_sample", "compliant")) |>
    dplyr::transmute(
      study = "Studies 1-3",
      Sample = dplyr::recode(.data$sample,
                             full_sample = "Strict (ITT)", compliant = "Compliant"),
      model = .data$model,
      direction = .data$direction,
      estimate = .data$estimate, conf_low = .data$conf_low, conf_high = .data$conf_high
    )

  # Study 4: show BOTH the strict (ITT) and compliant post-to-debrief cells.
  s4 <- an |>
    dplyr::filter(.data$block == "debrief_shift",
                  .data$sample %in% c("strict_n1272", "compliant_n1073"),
                  .data$term == "post_to_debrief_change",
                  .data$model != "Pooled") |>
    dplyr::transmute(
      study = "Study 4",
      Sample = dplyr::recode(.data$sample,
                             strict_n1272 = "Strict (ITT)", compliant_n1073 = "Compliant"),
      model = .data$model,
      direction = .data$direction,
      estimate = .data$estimate, conf_low = .data$conf_low, conf_high = .data$conf_high
    )

  dat <- dplyr::bind_rows(s13, s4) |>
    dplyr::mutate(
      direction = .dir_factor(.data$direction),
      Sample = factor(.data$Sample, levels = c("Strict (ITT)", "Compliant")),
      model = factor(.data$model,
                     levels = c("Jailbroken", "Standard", "Truth-Constrained", .model_levels_s4)),
      study = factor(.data$study, levels = c("Studies 1-3", "Study 4"))
    )

  ggplot2::ggplot(dat, ggplot2::aes(x = .data$estimate, y = .data$model,
                                    color = .data$direction,
                                    shape = .data$Sample, linetype = .data$Sample,
                                    group = interaction(.data$direction, .data$Sample))) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high),
                            height = .2, position = ggplot2::position_dodge(width = .6),
                            na.rm = TRUE) +
    ggplot2::geom_point(size = 2.2, fill = "white",
                        position = ggplot2::position_dodge(width = .6)) +
    ggplot2::facet_grid(study ~ ., scales = "free_y", space = "free_y") +
    ggplot2::scale_color_manual(values = bb_colors) +
    ggplot2::scale_shape_manual(values = c("Strict (ITT)" = 16, "Compliant" = 21)) +
    ggplot2::scale_linetype_manual(values = c("Strict (ITT)" = "solid", "Compliant" = "22")) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::labs(
      title = "Post-to-debrief belief shift by study and model",
      x = "Belief change from post-conversation to post-debrief (points; 95% CI)",
      y = NULL, color = NULL, shape = "Sample", linetype = "Sample"
    ) +
    theme_si()
}

# =============================================================================
# 15. fig_attrition — technical non-delivery vs substantive dropout by
#     study/model. block: attrition_substantive_vs_technical
#     (terms technical_nondelivery_n / substantive_dropout_n).
# =============================================================================
fig_attrition <- function(numbers = NULL) {
  an <- .an(numbers)

  kind_levels <- c("Substantive mid-conversation dropout",
                   "Did not begin the conversation",
                   "Sent a message, no model reply")

  # Per-model loss counts + the model-assigned pool, from the UNIFIED attrition
  # block: all four studies are now classified by the SAME partial-chat-log
  # (__js_chatPartialData1) rule, so Studies 1-3 carry real "did not begin" counts
  # rather than being folded entirely into substantive dropout. Plotted as a
  # PERCENTAGE of each model-assigned pool because pool sizes differ across studies.
  blk <- an |>
    dplyr::filter(.data$block == "attrition_substantive_vs_technical",
                  !is.na(.data$model),
                  .data$model %in% c("Jailbroken", "Standard", "Truth-Constrained", .model_levels_s4))
  pool <- blk |> dplyr::filter(.data$term == "pool") |>
    dplyr::select(model, .pool = estimate)
  dat <- blk |>
    dplyr::filter(.data$term %in% c("substantive_midchat_n", "no_message_n", "technical_no_reply_n")) |>
    dplyr::left_join(pool, by = "model") |>
    dplyr::transmute(
      study = ifelse(.data$model %in% .model_levels_s4,
                     "Study 4 (frontier models)", "Studies 1-3 (GPT-4o)"),
      model = .data$model,
      kind = dplyr::recode(.data$term,
        substantive_midchat_n = "Substantive mid-conversation dropout",
        no_message_n          = "Did not begin the conversation",
        technical_no_reply_n  = "Sent a message, no model reply"),
      pct = 100 * .data$estimate / .data$.pool) |>
    dplyr::mutate(
      kind = factor(.data$kind, levels = kind_levels),
      study = factor(.data$study, levels = c("Studies 1-3 (GPT-4o)", "Study 4 (frontier models)")),
      model = factor(.data$model,
                     levels = c("Jailbroken", "Standard", "Truth-Constrained", .model_levels_s4)))

  ggplot2::ggplot(dat, ggplot2::aes(x = .data$pct, y = .data$model, fill = .data$kind)) +
    ggplot2::geom_col(width = .7) +
    ggplot2::facet_grid(study ~ ., scales = "free_y", space = "free_y") +
    ggplot2::scale_fill_manual(values = c(
      "Substantive mid-conversation dropout" = "#FB8C00",
      "Did not begin the conversation"        = "#8E24AA",
      "Sent a message, no model reply"        = "#B0BEC5")) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::labs(
      title = "Outcome-incomplete loss by model condition and type",
      subtitle = "As a percentage of each model-assigned pool (pool sizes differ across studies)",
      x = "Participants lost (% of the model-assigned pool)", y = NULL, fill = NULL
    ) +
    theme_si() + ggplot2::theme(legend.position = "bottom")
}

# =============================================================================
# 16. fig_extensive_margin — net percentage-point change in "would publicly
#     post" by direction/outcome. block: extensive_margin (term=net_pp_change).
# =============================================================================
fig_extensive_margin <- function(numbers = NULL) {
  an <- .an(numbers)

  # De-pool: the extensive_margin block carries an equal-model-weighted pooled
  # row (model == NA, across pro/anti/aligned posting) ALONGSIDE the four
  # per-model rows (all on direction-aligned posting). The pooled aligned row
  # collides with the per-model aligned rows, so exclude the pooled row and draw
  # only the four Study-4 models (no reported pooled Study-4 estimate here).
  dat <- an |>
    dplyr::filter(.data$block == "extensive_margin", .data$term == "net_pp_change",
                  !is.na(.data$model), .data$model %in% .model_levels_s4) |>
    dplyr::mutate(
      model = factor(.data$model, levels = .model_levels_s4),
      direction = .dir_factor(.data$direction)
    )

  ggplot2::ggplot(dat, ggplot2::aes(x = .data$estimate, y = .data$model, fill = .data$direction)) +
    ggplot2::geom_vline(xintercept = 0, color = "grey60") +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = .7), width = .6) +
    ggplot2::scale_fill_manual(values = bb_colors) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::labs(
      title = "Extensive margin: net change in willingness to post publicly",
      subtitle = "Direction-aligned public posting, by Study-4 model",
      x = "Net percentage-point change in “would publicly post”",
      y = NULL, fill = NULL
    ) +
    theme_si()
}

# =============================================================================
# N. fig_moderators — forest of standardized PER-CONDITION moderator slopes
#    block: moderators  (section "Moderators")
#    x = std slope (95% CI); y = moderator; colour = direction; facet by
#    study/stratum.  A 1-SD higher pre-treatment moderator is associated with
#    <slope> points more direction-aligned belief change within that condition.
# =============================================================================
fig_moderators <- function(numbers = NULL) {
  an <- .an(numbers)

  mod_lab <- c(
    baseline_belief    = "Baseline belief",
    ai_trust           = "AI trust",
    gcbs               = "Conspiracy mentality (GCBS)",
    ideology           = "Partisanship (Dem→Rep)",
    ideology_demrep    = "Partisanship (Dem→Rep)",
    ideology_socialcon = "Social ideology (Cons→Lib)",
    education          = "Education",
    age                = "Age"
  )

  study_levels <- c("Jailbroken", "Standard", "Truth-Constrained",
                    "Claude", "Gemini", "GPT-5.2", "Grok")

  dat <- an |>
    dplyr::filter(.data$section == "Moderators", .data$block == "moderators") |>
    dplyr::mutate(
      moderator = factor(unname(mod_lab[.data$term]),
                         levels = rev(unique(unname(mod_lab)))),
      direction = .dir_factor(.data$direction),
      study = factor(.data$model, levels = study_levels)
    )

  ggplot2::ggplot(
    dat,
    ggplot2::aes(x = .data$estimate, y = .data$moderator, color = .data$direction)
  ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high),
      height = .25, position = ggplot2::position_dodge(width = .55)
    ) +
    ggplot2::geom_point(size = 2, position = ggplot2::position_dodge(width = .55)) +
    ggplot2::facet_wrap(~ .data$study, ncol = 4) +
    ggplot2::scale_color_manual(values = bb_colors) +
    ggplot2::labs(
      title = "Who moves? Pre-treatment moderators of AI persuasion, by condition",
      subtitle = "Standardized OLS slope (HC3); 1-SD higher moderator → points of direction-aligned belief change",
      x = "Standardized slope on direction-aligned belief change (95% CI)",
      y = NULL, color = NULL
    ) +
    theme_si(base_size = 8)
}

# =============================================================================
# SI-reconciliation (2026-06-24): figure builders added for the new SI
# tables/figures. Use only shared helpers (.an/theme_si/bb_colors/.dir_factor).
# =============================================================================
fig_funnel_s13 <- function(numbers = NULL) {
  an <- .an(numbers)
  .fn_lab <- c(
    raw_responses          = "Raw responses",
    real_non_preview       = "Non-preview responses",
    passed_respond_minus1  = "Passed chatbot screener",
    passed_attention_nicks = "Passed attention check",
    reached_equivocality   = "Reached screening stage",
    valid_conspiracy       = "Valid conspiracy description",
    equivocal_belief       = "Equivocal (uncertain) belief",
    within_2575_window     = "Baseline belief in 25-75 window",
    analytic_sample        = "Analytic sample")
  .study_lab <- c(Jailbroken = "Study 1 (Jailbroken)",
                  Standard = "Study 2 (Standard)",
                  `Truth-Constrained` = "Study 3 (Truth-Constrained)")

  d <- an |>
    dplyr::filter(.data$block == "screening_funnel_s13") |>
    dplyr::group_by(.data$model) |>
    dplyr::arrange(.data$statistic, .by_group = TRUE) |>
    dplyr::mutate(
      excluded = dplyr::lag(.data$estimate) - .data$estimate,
      y = -.data$statistic,
      Study = factor(unname(.study_lab[.data$model]), levels = unname(.study_lab)),
      lab = paste0(.fn_lab[.data$term], "\n", scales::comma(round(.data$estimate)))) |>
    dplyr::ungroup()

  # connectors between consecutive boxes, and the excluded-count annotation
  seg <- d |>
    dplyr::filter(!is.na(.data$excluded)) |>
    dplyr::mutate(y_from = .data$y + 1 - 0.34, y_to = .data$y + 0.34,
                  excl_lab = paste0("–", scales::comma(round(.data$excluded))))

  ggplot2::ggplot(d) +
    ggplot2::geom_segment(data = seg,
      ggplot2::aes(x = 0, xend = 0, y = .data$y_from, yend = .data$y_to),
      colour = "#90A4AE", linewidth = 0.4,
      arrow = grid::arrow(length = grid::unit(1.6, "mm"), type = "closed")) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = -1, xmax = 1, ymin = .data$y - 0.34, ymax = .data$y + 0.34),
      fill = "#ECEFF1", colour = "#455A64", linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(x = 0, y = .data$y, label = .data$lab),
      size = 2.5, colour = "#263238", lineheight = 0.9) +
    ggplot2::geom_text(data = seg,
      ggplot2::aes(x = 1.18, y = (.data$y_from + .data$y_to) / 2, label = .data$excl_lab),
      size = 2.3, colour = "#B0431F", hjust = 0, fontface = "italic") +
    ggplot2::facet_wrap(~ Study, nrow = 1) +
    ggplot2::scale_x_continuous(limits = c(-1.2, 2.1)) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_si(base_size = 8) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(), legend.position = "none",
      strip.text = ggplot2::element_text(face = "bold"))
}


# =============================================================================
# Fig S5. Belief trajectory (pre -> post -> debrief) + direction-aligned change CDF.
#   PURE all_numbers reader (blocks "belief_trajectory" + "belief_change_ecdf").
#   Panel A: mean belief level by timepoint, color = arm, linetype = sample, faceted by study.
#   Panel B: step CDF of direction-aligned change, color = arm, linetype = sample, faceted by study.
# =============================================================================
fig_belief_trajectory <- function(numbers = NULL) {
  an <- .an(numbers)

  study_levels <- c("Jailbroken", "Standard", "Truth-Constrained", "Claude", "Gemini", "GPT-5.2", "Grok")
  sample_labels <- c(full_sample = "Full / strict", strict_n1272 = "Full / strict",
                     compliant = "Compliant", compliant_n1073 = "Compliant")

  # ---- Panel A: trajectories -------------------------------------------------
  traj <- an |>
    dplyr::filter(.data$block == "belief_trajectory") |>
    dplyr::transmute(
      study  = factor(.data$model, levels = study_levels),
      arm    = factor(.data$direction, levels = c("Bunking", "Debunking")),
      Sample = factor(unname(sample_labels[.data$sample]),
                      levels = c("Full / strict", "Compliant")),
      timepoint = factor(.data$outcome, levels = c("pre", "post", "debrief"),
                         labels = c("Pre", "Post", "Debrief")),
      estimate = .data$estimate, conf_low = .data$conf_low, conf_high = .data$conf_high
    )

  pA <- ggplot2::ggplot(
      traj,
      ggplot2::aes(x = .data$timepoint, y = .data$estimate,
                   color = .data$arm, group = interaction(.data$arm, .data$Sample),
                   linetype = .data$Sample)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$conf_low, ymax = .data$conf_high, fill = .data$arm),
      alpha = 0.12, color = NA,
      show.legend = FALSE) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 1.5, show.legend = FALSE) +
    ggplot2::facet_wrap(~ study, nrow = 1) +
    ggplot2::scale_color_manual(values = bb_colors,
                                name = "Arm") +
    ggplot2::scale_fill_manual(values = bb_colors, guide = "none") +
    ggplot2::scale_linetype_manual(values = c(`Full / strict` = "solid",
                                              Compliant = "22"), name = "Sample") +
    ggplot2::labs(title = "A. Belief level across timepoints",
                  x = NULL, y = "Mean belief (reverse-coded, higher = more belief)") +
    theme_si() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

  # ---- Panel B: aligned-change CDFs -----------------------------------------
  ecdf_df <- an |>
    dplyr::filter(.data$block == "belief_change_ecdf") |>
    dplyr::transmute(
      study  = factor(.data$model, levels = study_levels),
      arm    = factor(.data$direction, levels = c("Bunking", "Debunking")),
      Sample = factor(unname(sample_labels[.data$sample]),
                      levels = c("Full / strict", "Compliant")),
      # Survival curve S(x) = P(aligned change > x) rather than the CDF, so a
      # HIGHER curve unambiguously means "more participants moved further toward
      # the assigned position" = more effective. (Reading the CDF the other way is
      # what made the bunk-vs-debunk comparison hard to parse.)
      x = .data$statistic, surv = 1 - .data$estimate
    ) |>
    dplyr::arrange(.data$study, .data$arm, .data$Sample, .data$x)

  # Call out the one model whose ordering reverses: GPT-5.2 backfires under bunking,
  # so its debunking survival curve sits clearly above its bunking curve.
  .ann <- data.frame(
    study = factor("GPT-5.2", levels = study_levels),
    x = -58, surv = 0.32,
    label = "GPT-5.2 reverses:\nbunking backfires,\ndebunking dominates")

  pB <- ggplot2::ggplot(
      ecdf_df,
      ggplot2::aes(x = .data$x, y = .data$surv, color = .data$arm,
                   group = interaction(.data$arm, .data$Sample),
                   linetype = .data$Sample)) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.3, color = "grey70") +
    ggplot2::geom_step(linewidth = 0.6) +
    ggplot2::geom_text(data = .ann,
                       ggplot2::aes(x = .data$x, y = .data$surv, label = .data$label),
                       inherit.aes = FALSE, size = 1.9, color = "grey25",
                       hjust = 0, vjust = 1, lineheight = 0.9) +
    ggplot2::facet_wrap(~ study, nrow = 1) +
    ggplot2::scale_color_manual(values = bb_colors,
                                name = "Arm") +
    ggplot2::scale_linetype_manual(values = c(`Full / strict` = "solid",
                                              Compliant = "22"), name = "Sample") +
    ggplot2::labs(title = "B. Share whose direction-aligned change exceeds x (higher = more effective)",
                  x = "Direction-aligned belief change (toward the assigned position)",
                  y = "Share exceeding x") +
    theme_si()

  patchwork::wrap_plots(pA, pB, ncol = 1) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom")
}



# =============================================================================
# fig_topic_veracity — Fig S10: aligned-claim veracity by topic cluster x arm.
#   block "topic_veracity", term "topic_mean_veracity"; both samples faceted.
#   Pure all_numbers reader.
# =============================================================================
fig_topic_veracity <- function(numbers = NULL) {
  an <- .an(numbers)

  dat <- an |>
    dplyr::filter(.data$block == "topic_veracity",
                  .data$term == "topic_mean_veracity") |>
    dplyr::transmute(
      Sample = dplyr::recode(.data$sample,
                             all_conversations = "All conversations",
                             compliant = "Compliant"),
      topic = .data$model,
      direction = .dir_factor(.data$direction),
      estimate = .data$estimate,
      conf_low = .data$conf_low, conf_high = .data$conf_high
    )

  # order topics by the bunk-minus-debunk veracity gap in the all-conversations
  # sample (most-divergent topics at the top) for a stable, interpretable y axis.
  ord <- dat |>
    dplyr::filter(.data$Sample == "All conversations") |>
    dplyr::group_by(.data$topic) |>
    dplyr::summarise(
      gap = .data$estimate[.data$direction == "Debunking"][1] -
            .data$estimate[.data$direction == "Bunking"][1],
      .groups = "drop") |>
    dplyr::arrange(.data$gap)
  dat <- dat |>
    dplyr::mutate(topic = factor(.data$topic, levels = ord$topic),
                  Sample = factor(.data$Sample,
                                  levels = c("All conversations", "Compliant")))

  ggplot2::ggplot(
    dat,
    ggplot2::aes(x = .data$estimate, y = .data$topic, color = .data$direction)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high),
      orientation = "y", width = 0,
      position = ggplot2::position_dodge(width = 0.55)) +
    ggplot2::geom_point(
      size = 2, position = ggplot2::position_dodge(width = 0.55)) +
    ggplot2::scale_color_manual(values = bb_colors, name = NULL) +
    ggplot2::facet_wrap(~ Sample, nrow = 1) +
    ggplot2::coord_cartesian(xlim = c(0, 100)) +
    ggplot2::labs(
      title = "Aligned-claim veracity by conspiracy topic and persuasion arm",
      x = "Mean direction-aligned claim veracity (0-100, 95% CI)", y = NULL) +
    theme_si(base_size = 8)
}



# =============================================================================
# fig_conversation_length — Fig S11: distribution of per-conversation USER
#   message counts, faceted by study/model, coloured by arm. Pure reader of
#   block "conversation_length" (outcome=count_share). Dotted line marks the
#   10-user-message cap; 93-99% of conversations end before reaching it.
# =============================================================================
fig_conversation_length <- function(numbers = NULL) {
  an <- .an(numbers)

  dat <- an |>
    dplyr::filter(.data$block == "conversation_length",
                  .data$outcome == "count_share",
                  .data$sample %in% c("full_sample", "strict_n1272")) |>
    dplyr::filter(!is.na(.data$model), !is.na(.data$direction)) |>
    dplyr::mutate(
      k = .data$statistic,
      share = .data$estimate,
      direction = .dir_factor(.data$direction),
      model = factor(.data$model,
                     levels = c("Jailbroken", "Standard", "Truth-Constrained",
                                "Claude", "Gemini", "GPT-5.2", "Grok"))
    )

  caps <- dat |>
    dplyr::group_by(.data$model) |>
    dplyr::summarise(kcap = max(.data$k), .groups = "drop")

  ggplot2::ggplot(dat, ggplot2::aes(x = .data$k, y = .data$share,
                                    fill = .data$direction, color = .data$direction)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8),
                      width = 0.75, alpha = 0.85) +
    ggplot2::geom_vline(data = caps, ggplot2::aes(xintercept = .data$kcap - 0.5),
                        linetype = "dotted", color = "grey45", linewidth = 0.3) +
    ggplot2::facet_wrap(~ model, ncol = 3, scales = "free_y") +
    ggplot2::scale_fill_manual(values = bb_colors) +
    ggplot2::scale_color_manual(values = bb_colors) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(2)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(
      title = "Conversation length: distribution of user messages per conversation",
      subtitle = "Share of conversations by user-message count; dotted line marks the 10-message cap (93-99% of conversations end before it)",
      x = "User messages per conversation", y = "Share of conversations",
      fill = NULL, color = NULL
    ) +
    theme_si(base_size = 8)
}



# =============================================================================
# fig_simple_slopes — Fig S12: baseline-belief SIMPLE SLOPES.
#   Lin-model predicted direction-aligned belief change as a function of the
#   participant's pre-treatment belief, by arm (Bunking/Debunking), faceted by
#   study (rows) x sample (cols). Dashed reference line at pre = 50.
#   block: simple_slopes; pred rows term = "pred_<pre>", statistic = pre value.
# =============================================================================
fig_simple_slopes <- function(numbers = NULL) {
  an <- .an(numbers)
  .samp_lab <- c(strict = "Strict (ITT)", compliant = "Compliant")
  .study_levels <- c("Jailbroken", "Standard", "Truth-Constrained",
                     "Claude", "Gemini", "GPT-5.2", "Grok")

  pred <- an |>
    dplyr::filter(.data$block == "simple_slopes", grepl("^pred_", .data$term),
                  .data$direction %in% c("Bunking", "Debunking")) |>
    dplyr::transmute(
      Study = factor(.data$model, levels = .study_levels),
      Sample = factor(unname(.samp_lab[.data$sample]), levels = unname(.samp_lab)),
      direction = factor(.data$direction, levels = c("Bunking", "Debunking")),
      pre = .data$statistic, est = .data$estimate,
      lo = .data$conf_low, hi = .data$conf_high)

  ggplot2::ggplot(pred, ggplot2::aes(x = .data$pre, color = .data$direction, fill = .data$direction)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey75", linewidth = .3) +
    ggplot2::geom_vline(xintercept = 50, linetype = "dashed", color = "grey55", linewidth = .3) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo, ymax = .data$hi),
                         alpha = .15, color = NA) +
    ggplot2::geom_line(ggplot2::aes(y = .data$est), linewidth = .9) +
    ggplot2::facet_grid(Study ~ Sample) +
    ggplot2::scale_color_manual(values = bb_colors) +
    ggplot2::scale_fill_manual(values = bb_colors) +
    ggplot2::scale_x_continuous(breaks = c(25, 50, 75)) +
    ggplot2::labs(
      title = "Persuasion depends on baseline belief",
      subtitle = "Lin-model predicted direction-aligned belief change vs. pre-treatment belief (95% HC3 CI ribbons; dashed line = midpoint)",
      x = "Pre-treatment belief (reverse-coded, higher = more belief)",
      y = "Predicted aligned belief change (points)", color = NULL, fill = NULL) +
    theme_si(base_size = 8)
}



# =============================================================================
# fig_belief_sharing_dissociation — Study 4 private-belief -> public-sharing.
# Conversation-level (reads core_objects$s4$s4_compliant, like the main figure
# script); both axes signed toward the AI's assigned side. Panel A: per-person
# translation (scatter + per-condition OLS fit) — the slope is similar in the two
# arms. Panel B: mean weighted-sharing change by belief-change stratum (backfire /
# no change / moved toward AI) and condition — the between-arm gap persists even
# where private belief did not move toward the AI. Compliant subset.
# =============================================================================
fig_belief_sharing_dissociation <- function(core = NULL) {
  if (is.null(core)) core <- get("core_objects", envir = globalenv())
  d <- core$s4$s4_compliant
  d <- data.frame(
    Condition = .dir_factor(d$direction),
    dB = d$aligned_belief_change,
    dS = d$aligned_new_minus_old_weighted
  )
  d <- d[is.finite(d$dB) & is.finite(d$dS), ]

  pa <- ggplot2::ggplot(d, ggplot2::aes(x = .data$dB, y = .data$dS,
                                        color = .data$Condition, fill = .data$Condition)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::geom_point(alpha = .18, size = .8, stroke = 0) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, linewidth = 1, alpha = .18) +
    ggplot2::scale_color_manual(values = bb_colors) +
    ggplot2::scale_fill_manual(values = bb_colors, guide = "none") +
    ggplot2::labs(title = "A. Within-person translation (similar slopes)",
                  x = "Private belief change toward the AI's side (points)",
                  y = "Change in weighted sharing\n(toward the AI's side)",
                  color = NULL, fill = NULL) +
    theme_si()

  strata <- d
  strata$stratum <- factor(
    ifelse(strata$dB < 0, "Backfire\n(belief moved away)",
    ifelse(strata$dB == 0, "No change", "Moved toward AI")),
    levels = c("Backfire\n(belief moved away)", "No change", "Moved toward AI"))
  sm <- strata |>
    dplyr::group_by(.data$stratum, .data$Condition) |>
    dplyr::summarise(m = mean(.data$dS), se = stats::sd(.data$dS) / sqrt(dplyr::n()),
                     n = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(lo = .data$m - 1.96 * .data$se, hi = .data$m + 1.96 * .data$se)

  pb <- ggplot2::ggplot(sm, ggplot2::aes(x = .data$stratum, y = .data$m, color = .data$Condition)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lo, ymax = .data$hi), width = .12,
                           position = ggplot2::position_dodge(width = .5)) +
    ggplot2::geom_point(size = 2.6, position = ggplot2::position_dodge(width = .5)) +
    ggplot2::scale_color_manual(values = bb_colors) +
    ggplot2::labs(title = "B. Sharing shift even when belief did not move",
                  x = NULL, y = "Change in weighted sharing\n(toward the AI's side)",
                  color = NULL) +
    theme_si()

  patchwork::wrap_plots(pa, pb, ncol = 2, guides = "collect") +
    patchwork::plot_annotation(
      title = "Study 4 (compliant): private belief converts into public sharing asymmetrically") &
    ggplot2::theme(legend.position = "bottom")
}

# =============================================================================
# Pooled (S1-4) CAUSAL-FOREST figures. Read the live `all_numbers` (cf_* blocks
# from ext_causal_forest.R); the CATE-distribution figure additionally needs the
# per-row tau_hat, so it calls the memoized fit_causal_forest(). Labels/strata
# come from cf_mod_labels / cf_strata (defined in code/R/ext_causal_forest.R).
# =============================================================================

# Best linear projection of the swing: participant traits + study/model context.
fig_cf_blp <- function(numbers = NULL) {
  an <- .an(numbers); lab <- cf_mod_labels
  person <- an |>
    dplyr::filter(.data$block == "cf_blp_person") |>
    dplyr::transmute(panel = "Participant traits",
                     term = unname(lab[.data$term]),
                     estimate = .data$estimate, conf_low = .data$conf_low,
                     conf_high = .data$conf_high, p_value = .data$p_value)
  ctx <- an |>
    dplyr::filter(.data$block == "cf_blp_context") |>
    dplyr::transmute(panel = "Study / model", term = .data$term,
                     estimate = .data$estimate, conf_low = .data$conf_low,
                     conf_high = .data$conf_high, p_value = .data$p_value)
  d <- dplyr::bind_rows(person, ctx)
  d$sig <- ifelse(!is.na(d$p_value) & d$p_value < .05, "p < .05", "n.s.")
  d$panel <- factor(d$panel, levels = c("Participant traits", "Study / model"))
  d$term <- factor(d$term, levels = rev(unique(d$term[order(d$panel, d$estimate)])))
  ggplot2::ggplot(d, ggplot2::aes(.data$estimate, .data$term, color = .data$sig)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high), height = .25) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_grid(rows = dplyr::vars(.data$panel), scales = "free_y", space = "free_y") +
    ggplot2::scale_color_manual(values = c("p < .05" = "#D81B60", "n.s." = "grey55")) +
    ggplot2::labs(
      title = "Moderators of the bunk-debunk swing (causal-forest BLP)",
      subtitle = "Best linear projection of the CATE; continuous traits per within-study SD, 95% CI",
      x = "Slope on the bunk-debunk swing (belief points)", y = NULL, color = NULL) +
    theme_si(base_size = 8)
}

# Variable importance (split frequency), person traits vs design context.
fig_cf_importance <- function(numbers = NULL) {
  an <- .an(numbers); lab <- cf_mod_labels
  d <- an |>
    dplyr::filter(.data$block == "cf_importance") |>
    dplyr::mutate(variable = ifelse(.data$term %in% names(lab), unname(lab[.data$term]), .data$term),
                  kind = .data$note) |>
    dplyr::arrange(.data$estimate)
  d$variable <- factor(d$variable, levels = d$variable)
  ggplot2::ggplot(d, ggplot2::aes(.data$estimate, .data$variable, fill = .data$kind)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = c(person = "#1E88E5", context = "#F4A300", topic = "#43A047")) +
    ggplot2::labs(title = "Causal-forest variable importance",
                  subtitle = "Split-frequency importance for the bunk-debunk swing",
                  x = "Importance", y = NULL, fill = NULL) +
    theme_si(base_size = 8)
}

# AIPW swing by study/model, against the pooled ATE.
fig_cf_swing_by_model <- function(numbers = NULL) {
  an <- .an(numbers)
  ate <- an |> dplyr::filter(.data$block == "cf_ate", .data$term == "ATE") |> dplyr::pull(.data$estimate)
  d <- an |>
    dplyr::filter(.data$block == "cf_swing_by_stratum") |>
    dplyr::mutate(study_model = factor(.data$model, levels = rev(cf_strata)),
                  family = ifelse(.data$model %in% cf_strata[1:3],
                                  "GPT-4o variants (S1-3)", "Frontier models (S4)"))
  ggplot2::ggplot(d, ggplot2::aes(.data$estimate, .data$study_model, color = .data$family)) +
    ggplot2::geom_vline(xintercept = ate, linetype = "dashed", color = "grey50") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high), height = .2) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_color_manual(values = c("GPT-4o variants (S1-3)" = "#1E88E5",
                                           "Frontier models (S4)" = "#D81B60")) +
    ggplot2::labs(title = "Bunk-debunk swing by study and model",
                  subtitle = paste0("AIPW within-stratum swing; dashed line = pooled ATE (",
                                    formatC(ate, format = "f", digits = 1), ")"),
                  x = "Swing (belief points, 95% CI)", y = NULL, color = NULL) +
    theme_si(base_size = 8)
}

# Distribution of the estimated swing (CATE) by study/model (needs per-row tau).
fig_cf_cate_distribution <- function(core = NULL) {
  f <- fit_causal_forest(core)
  d <- f$dat; d$study_model <- factor(d$study_model, levels = cf_strata)
  ggplot2::ggplot(d, ggplot2::aes(.data$tau_hat)) +
    ggplot2::geom_histogram(bins = 40, fill = "#5E35B1", color = "white") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::facet_wrap(~ .data$study_model, scales = "free_y", nrow = 2) +
    ggplot2::labs(title = "Distribution of the estimated swing (CATE) by study and model",
                  x = "Estimated bunk-debunk swing (belief points)", y = "Participants") +
    theme_si(base_size = 8)
}

# Direction favored (per person): predicted aligned effect of debunking vs bunking.
# Each point is a participant; the 45-degree line is parity. Points BELOW the line
# (debunk_aligned > bunk_aligned) are debunk-favored; ABOVE are bunk-favored.
fig_cf_direction <- function(core = NULL) {
  f <- fit_causal_forest(core)
  d <- f$dat; d$study_model <- factor(d$study_model, levels = cf_strata)
  rng <- range(c(d$bunk_aligned, d$debunk_aligned), na.rm = TRUE)
  ggplot2::ggplot(d, ggplot2::aes(.data$bunk_aligned, .data$debunk_aligned, color = .data$dir_favored)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_point(alpha = 0.35, size = 0.7) +
    ggplot2::facet_wrap(~ .data$study_model, nrow = 2) +
    ggplot2::coord_equal(xlim = rng, ylim = rng) +
    ggplot2::scale_color_manual(values = bb_colors) +
    ggplot2::labs(
      title = "Which lever moves each person more: bunking vs debunking",
      subtitle = "Forest-predicted aligned effect of each arm; below the dashed line = debunking is the stronger lever",
      x = "Predicted aligned effect of BUNKING (belief points)",
      y = "Predicted aligned effect of DEBUNKING", color = "Favored") +
    theme_si(base_size = 8)
}

# AIPW bunk-debunk swing by conspiracy topic, against the pooled ATE.
fig_cf_swing_by_topic <- function(numbers = NULL) {
  an <- .an(numbers)
  ate <- an |> dplyr::filter(.data$block == "cf_ate", .data$term == "ATE") |> dplyr::pull(.data$estimate)
  d <- an |>
    dplyr::filter(.data$block == "cf_swing_by_topic", .data$term == "swing") |>
    dplyr::mutate(topic = stats::reorder(.data$model, .data$estimate))
  ggplot2::ggplot(d, ggplot2::aes(.data$estimate, .data$topic)) +
    ggplot2::geom_vline(xintercept = ate, linetype = "dashed", color = "grey50") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high), height = .2) +
    ggplot2::geom_point(size = 2.4, color = "#5E35B1") +
    ggplot2::labs(title = "Bunk-debunk swing by conspiracy topic",
                  subtitle = paste0("AIPW within-topic swing; dashed line = pooled ATE (",
                                    formatC(ate, format = "f", digits = 1), ")"),
                  x = "Swing (belief points, 95% CI)", y = NULL) +
    theme_si(base_size = 8)
}

# =============================================================================
# fig_paltering — belief change vs conversation-average aligned-claim veracity,
# for COMPLIANT BUNKING conversations pooled across Studies 1-4. Reads the precomputed
# "paltering" block (compute_paltering / ext_paltering.R): one point per conversation
# (statistic = veracity, estimate = belief change). Points are conversations; white
# points are veracity-octile means (95% CI); the smooth and bin means staying above
# zero at high veracity illustrate "paltering" (belief rises even when the
# conversation's claims are accurate).
# =============================================================================
fig_paltering <- function(numbers = NULL) {
  an <- .an(numbers)
  pal <- an |>
    dplyr::filter(.data$block == "paltering", .data$term == "paltering_point") |>
    dplyr::transmute(aligned_veracity = .data$statistic, delta = .data$estimate) |>
    dplyr::filter(is.finite(.data$aligned_veracity), is.finite(.data$delta))

  bins <- pal |>
    dplyr::mutate(q = dplyr::ntile(.data$aligned_veracity, 8)) |>
    dplyr::group_by(.data$q) |>
    dplyr::summarise(x = mean(.data$aligned_veracity), y = mean(.data$delta),
                     se = stats::sd(.data$delta) / sqrt(dplyr::n()), .groups = "drop") |>
    dplyr::mutate(lo = .data$y - 1.96 * .data$se, hi = .data$y + 1.96 * .data$se)

  ggplot2::ggplot(pal, ggplot2::aes(.data$aligned_veracity, .data$delta)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey55") +
    ggplot2::geom_point(alpha = 0.12, size = 0.7, color = "grey55", stroke = 0) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, span = 1,
                         color = "grey20", fill = "grey70", alpha = 0.25, linewidth = 0.8) +
    ggplot2::geom_linerange(data = bins, ggplot2::aes(x = .data$x, ymin = .data$lo, ymax = .data$hi),
                            inherit.aes = FALSE, color = "grey15") +
    ggplot2::geom_point(data = bins, ggplot2::aes(x = .data$x, y = .data$y),
                        inherit.aes = FALSE, color = "grey15", fill = "white", shape = 21, size = 2) +
    ggplot2::coord_cartesian(xlim = c(0, 100)) +
    ggplot2::labs(
      title = "Bunking moves belief even when the conversation is locally accurate",
      subtitle = "Compliant bunking conversations (Studies 1-4 pooled); points = conversations, white = veracity-octile means (95% CI)",
      x = "Conversation-average veracity of aligned claims (0-100)",
      y = "Belief change after bunking (points)") +
    theme_si()
}

# Person-trait BLP fit SEPARATELY within Studies 1-3 vs within Study 4. GCBS is
# observed only in Studies 1-3, so it appears for that group only -- clarifying its
# effect free of the pooled forest's missing-data/study confound.
fig_cf_subgroup_blp <- function(numbers = NULL) {
  an <- .an(numbers); lab <- cf_mod_labels
  d <- an |>
    dplyr::filter(.data$block == "cf_sub_blp_person") |>
    dplyr::mutate(Group = dplyr::recode(.data$sample,
                    compliant_s1s3 = "Studies 1-3 (GPT-4o)", compliant_s4 = "Study 4 (frontier)"),
                  term = unname(lab[.data$term]))
  ord <- d |> dplyr::filter(.data$sample == "compliant_s1s3") |>
    dplyr::arrange(.data$estimate) |> dplyr::pull(.data$term)
  d$term <- factor(d$term, levels = unique(c(ord, d$term)))
  ggplot2::ggplot(d, ggplot2::aes(.data$estimate, .data$term, color = .data$Group)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high),
                            height = .3, position = ggplot2::position_dodge(width = .6)) +
    ggplot2::geom_point(size = 2, position = ggplot2::position_dodge(width = .6)) +
    ggplot2::scale_color_manual(values = c("Studies 1-3 (GPT-4o)" = "#1E88E5",
                                           "Study 4 (frontier)" = "#D81B60")) +
    ggplot2::labs(title = "Person-trait moderators, fit separately by study group",
                  subtitle = "Causal-forest BLP within each group; GCBS is observed only in Studies 1-3",
                  x = "Slope on the bunk-debunk swing (belief points)", y = NULL, color = NULL) +
    theme_si(base_size = 8)
}
