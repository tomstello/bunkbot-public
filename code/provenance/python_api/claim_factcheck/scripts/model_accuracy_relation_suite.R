#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(sandwich)
})

# Resolve the repo root (dir containing both data/ and code/), works under Rscript and source().
.repo_root <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  p <- if (length(f)) dirname(normalizePath(f[1])) else normalizePath(getwd())
  repeat {
    if (dir.exists(file.path(p, "data")) && dir.exists(file.path(p, "code"))) return(p)
    pp <- dirname(p); if (pp == p) stop("repo root not found"); p <- pp
  }
}
REPO_ROOT <- .repo_root()
# Transient intermediates for this pipeline (not shipped). Keep old basenames here.
WORK_DIR <- file.path(REPO_ROOT, "output", "provenance_work", "claim_factcheck")
dir.create(WORK_DIR, recursive = TRUE, showWarnings = FALSE)

args <- commandArgs(trailingOnly = TRUE)
# TRANSIENT INTERMEDIATE (not shipped): conversation-complete accuracy dataset w/ focal item.
input_path <- if (length(args) >= 1) args[[1]] else
  file.path(WORK_DIR, "april04_complete_conversation_accuracy_dataset_with_focal_item_final.csv")
# TRANSIENT INTERMEDIATE (not shipped): accuracy-relation suite output prefix.
output_prefix <- if (length(args) >= 2) args[[2]] else
  file.path(WORK_DIR, "april04_accuracy_relation_suite")

df <- read_csv(input_path, show_col_types = FALSE, progress = FALSE) %>%
  filter(ready_for_accuracy_analysis, model_pooled != "GPT-5.2") %>%
  filter(!is.na(aligned_belief_change), !is.na(mean_veracity), !is.na(focal_claim_veracity)) %>%
  mutate(
    direction = factor(direction, levels = c("bunk", "debunk")),
    model_pooled = factor(model_pooled, levels = c("Gemini", "Claude", "Grok")),
    truth_bin = case_when(
      focal_claim_veracity < 33 ~ "False",
      focal_claim_veracity < 67 ~ "Mostly False",
      focal_claim_veracity <= 100 ~ "True",
      TRUE ~ NA_character_
    ),
    truth_bin = factor(truth_bin, levels = c("False", "Mostly False", "True")),
    focal_claim_checkability_10 = focal_claim_checkability / 10
  ) %>%
  filter(!is.na(truth_bin))

if (nrow(df) == 0) {
  stop("No complete non-GPT conversations available.")
}

specs <- list(
  bare = aligned_belief_change ~ mean_veracity_10 * direction * truth_bin + model_pooled,
  plus_checkability = aligned_belief_change ~ mean_veracity_10 * direction * truth_bin + model_pooled + focal_claim_checkability_10,
  plus_verbosity = aligned_belief_change ~ mean_veracity_10 * direction * truth_bin + model_pooled + log1p_eligible_queue_n,
  full = aligned_belief_change ~ mean_veracity_10 * direction * truth_bin + model_pooled + focal_claim_checkability_10 + log1p_eligible_queue_n
)

fit_spec <- function(formula, data) {
  fit <- lm(formula, data = data)
  vc <- sandwich::vcovHC(fit, type = "HC3")
  list(fit = fit, vcov = vc)
}

tidy_hc3 <- function(name, fit_obj) {
  fit <- fit_obj$fit
  vc <- fit_obj$vcov
  est <- coef(fit)
  se <- sqrt(diag(vc))
  z <- est / se
  p <- 2 * pnorm(abs(z), lower.tail = FALSE)
  tibble(
    model_spec = name,
    term = names(est),
    estimate = unname(est),
    std.error = unname(se),
    statistic = unname(z),
    p.value = unname(p),
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )
}

slope_vector <- function(fit, direction_value, truth_value, mean_from = 0, mean_to = 1) {
  base <- df %>%
    summarise(
      focal_claim_checkability_10 = mean(focal_claim_checkability_10, na.rm = TRUE),
      log1p_eligible_queue_n = mean(log1p_eligible_queue_n, na.rm = TRUE)
    )
  nd0 <- data.frame(
    mean_veracity_10 = mean_from,
    direction = factor(direction_value, levels = levels(df$direction)),
    truth_bin = factor(truth_value, levels = levels(df$truth_bin)),
    model_pooled = factor("Gemini", levels = levels(df$model_pooled)),
    focal_claim_checkability_10 = base$focal_claim_checkability_10,
    log1p_eligible_queue_n = base$log1p_eligible_queue_n
  )
  nd1 <- nd0
  nd1$mean_veracity_10 <- mean_to
  mm0 <- model.matrix(delete.response(terms(fit)), nd0)
  mm1 <- model.matrix(delete.response(terms(fit)), nd1)
  drop(mm1 - mm0)
}

estimate_linear_combo <- function(beta, vcov_mat, L) {
  est <- sum(L * beta)
  se <- sqrt(drop(t(L) %*% vcov_mat %*% L))
  tibble(
    estimate = est,
    std.error = se,
    statistic = est / se,
    p.value = 2 * pnorm(abs(est / se), lower.tail = FALSE),
    conf.low = est - 1.96 * se,
    conf.high = est + 1.96 * se
  )
}

fits <- lapply(specs, fit_spec, data = df)
coef_table <- bind_rows(lapply(names(fits), function(nm) tidy_hc3(nm, fits[[nm]])))
write_csv(coef_table, paste0(output_prefix, "_coefs.csv"))

slope_rows <- list()
contrast_rows <- list()
for (nm in names(fits)) {
  fit <- fits[[nm]]$fit
  vc <- fits[[nm]]$vcov
  beta <- coef(fit)
  for (tb in levels(df$truth_bin)) {
    for (dir in levels(df$direction)) {
      L <- slope_vector(fit, dir, tb)
      est <- estimate_linear_combo(beta, vc, L) %>%
        mutate(model_spec = nm, truth_bin = tb, direction = dir)
      slope_rows[[length(slope_rows) + 1]] <- est
    }
    L_bunk <- slope_vector(fit, "bunk", tb)
    L_debunk <- slope_vector(fit, "debunk", tb)
    est_diff <- estimate_linear_combo(beta, vc, L_debunk - L_bunk) %>%
      mutate(model_spec = nm, truth_bin = tb, contrast = "debunk_minus_bunk")
    contrast_rows[[length(contrast_rows) + 1]] <- est_diff
  }
}

slopes_df <- bind_rows(slope_rows) %>%
  mutate(
    direction = recode(direction, bunk = "Bunk", debunk = "Debunk"),
    model_spec = factor(model_spec, levels = c("bare", "plus_checkability", "plus_verbosity", "full"))
  )
write_csv(slopes_df, paste0(output_prefix, "_slopes.csv"))

contrast_df <- bind_rows(contrast_rows) %>%
  mutate(model_spec = factor(model_spec, levels = c("bare", "plus_checkability", "plus_verbosity", "full")))
write_csv(contrast_df, paste0(output_prefix, "_slope_contrasts.csv"))

cell_counts <- df %>%
  count(truth_bin, direction, name = "n")
write_csv(cell_counts, paste0(output_prefix, "_cell_counts.csv"))

pretty_spec <- c(
  bare = "Bare",
  plus_checkability = "+ Checkability",
  plus_verbosity = "+ Verbosity",
  full = "Full"
)

plot_df <- slopes_df %>%
  mutate(
    model_spec_label = factor(recode(as.character(model_spec), !!!pretty_spec), levels = pretty_spec),
    truth_bin = factor(truth_bin, levels = c("False", "Mostly False", "True"))
  )

theme_suite <- function() {
  theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#E6E0D5", linewidth = 0.55),
      panel.border = element_rect(color = "#CFC6B8", fill = NA, linewidth = 0.8),
      strip.text = element_text(face = "bold", size = 13),
      strip.background = element_rect(fill = "#F4EBDC", color = NA),
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 12.5, color = "#4D4338", margin = margin(b = 10)),
      axis.title = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.margin = margin(10, 14, 10, 10)
    )
}

slope_plot <- ggplot(
  plot_df,
  aes(x = estimate, y = model_spec_label, color = direction, shape = direction)
) +
  geom_vline(xintercept = 0, color = "#AAA08F", linewidth = 0.55) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = 0.95, alpha = 0.9) +
  geom_point(size = 3.4, stroke = 0.9, fill = "white") +
  facet_wrap(~ truth_bin, nrow = 1) +
  scale_color_manual(values = c("Bunk" = "#A33A2B", "Debunk" = "#1F6AA5")) +
  scale_shape_manual(values = c("Bunk" = 21, "Debunk" = 24)) +
  scale_x_continuous(labels = number_format(accuracy = 0.1)) +
  labs(
    x = "Belief-change points per 10-point increase in conversation claim veracity",
    y = NULL,
    color = NULL,
    shape = NULL,
    title = "How claim accuracy relates to belief updating depends on topic type",
    subtitle = "Conversation-level slopes from unweighted OLS with model fixed effects; each panel is a focal-topic truth bin."
  ) +
  theme_suite()

ggsave(paste0(output_prefix, "_slopes_figure.png"), slope_plot, width = 13.2, height = 6.4, dpi = 320)
ggsave(paste0(output_prefix, "_slopes_figure.pdf"), slope_plot, width = 13.2, height = 6.4, device = cairo_pdf)

contrast_plot <- contrast_df %>%
  mutate(
    model_spec_label = factor(recode(as.character(model_spec), !!!pretty_spec), levels = pretty_spec),
    truth_bin = factor(truth_bin, levels = c("False", "Mostly False", "True"))
  ) %>%
  ggplot(aes(x = estimate, y = model_spec_label)) +
  geom_vline(xintercept = 0, color = "#AAA08F", linewidth = 0.55) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = 0.95, color = "#3A4A5E") +
  geom_point(size = 3.4, shape = 21, stroke = 0.9, fill = "#3A4A5E", color = "#1E1A16") +
  facet_wrap(~ truth_bin, nrow = 1) +
  scale_x_continuous(labels = number_format(accuracy = 0.1)) +
  labs(
    x = "Difference in accuracy slope: debunk minus bunk",
    y = NULL,
    title = "Does claim accuracy matter more for debunking than bunking?",
    subtitle = "Positive values mean the veracity slope is steeper in debunk than bunk."
  ) +
  theme_suite()

ggsave(paste0(output_prefix, "_slope_contrasts_figure.png"), contrast_plot, width = 13.2, height = 6.2, dpi = 320)
ggsave(paste0(output_prefix, "_slope_contrasts_figure.pdf"), contrast_plot, width = 13.2, height = 6.2, device = cairo_pdf)

report_lines <- c(
  paste0("Analysis input: ", input_path),
  paste0("Conversation-complete sample excluding GPT-5.2: ", nrow(df)),
  "",
  "Cell counts by truth bin and direction:",
  capture.output(print(with(df, table(truth_bin, direction)))),
  "",
  "Specifications:",
  "- bare: accuracy x direction x truth_bin + model FE",
  "- plus_checkability: bare + focal_claim_checkability_10",
  "- plus_verbosity: bare + log1p_eligible_queue_n",
  "- full: bare + focal_claim_checkability_10 + log1p_eligible_queue_n",
  "",
  "Outputs:",
  paste0("- coefficients: ", paste0(output_prefix, "_coefs.csv")),
  paste0("- slopes: ", paste0(output_prefix, "_slopes.csv")),
  paste0("- slope contrasts: ", paste0(output_prefix, "_slope_contrasts.csv"))
)
writeLines(report_lines, paste0(output_prefix, "_report.txt"))
