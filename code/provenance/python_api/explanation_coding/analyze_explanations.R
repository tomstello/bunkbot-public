# analyze_explanations.R
# Merge the consensus persuasion-explanation codes onto the screened Studies 1-4
# analysis frame, compute theme prevalence by persuasion outcome / study / direction
# (incl. the Standard-vs-Truth-Constrained compliant-bunking contrast), and render
# the modernized version of the legacy ExplanationAnalysis.R figure.
#
# Inputs  (data/api_cached/explanation_coding/):
#   analysis_frame.csv             (from export_analysis_frame.R)
#   explanation_consolidated.csv   (from consolidate_explanation.py)
# Outputs (output/provenance_work/explanation_coding/):
#   explanation_theme_numbers.csv
#   figures/explanation_themes_by_persuasion.pdf/.png
#   figures/explanation_themes_standard_vs_truth.pdf/.png
#
# Run: Rscript analyze_explanations.R

suppressWarnings(suppressMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr)
  library(forcats); library(ggplot2); library(purrr)
}))

.repo_root <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  p <- if (length(f)) dirname(normalizePath(f[1])) else normalizePath(getwd())
  repeat {
    if (dir.exists(file.path(p, "data")) && dir.exists(file.path(p, "code"))) return(p)
    pp <- dirname(p); if (pp == p) stop("repo root not found"); p <- pp
  }
}
root     <- .repo_root()                                     # repo root (has data/ + code/)
data_dir <- file.path(root, "data", "api_cached", "explanation_coding")   # shipped inputs
# Derived analysis CSVs + figures are not shipped under data/; route to the working dir.
res_dir  <- file.path(root, "output", "provenance_work", "explanation_coding")
fig_dir  <- file.path(res_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# reuse the supplement design system (theme_si + bb_colors) if available.
# NOT SHIPPED: ../supplement_v2/R/figures_core.R is an external author-side tree; this
# source() is wrapped in try() and a built-in theme_si fallback is defined below.
suppressWarnings(try(source(file.path(root, "..", "supplement_v2", "R", "figures_core.R")), silent = TRUE))
if (!exists("theme_si")) theme_si <- function(base_size = 9) theme_minimal(base_size = base_size) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"), legend.position = "bottom")

# ---- taxonomy labels (mirror taxonomy.py order) ----
theme_meta <- tribble(
  ~key,        ~label,                                  ~valence,
  "evid_pos",  "Facts, logic & counterarguments",       "pos",
  "expt_pos",  "Expertise & trustworthiness",           "pos",
  "emph_pos",  "Empathy & politeness",                  "pos",
  "detl_pos",  "Detail, depth, novelty & tailoring",    "pos",
  "conx_pos",  "Conspiracy-specific mechanisms",        "pos",
  "priv_pos",  "Privacy / non-judgmental space",        "pos",
  "lack_neg",  "Merely reaffirmed prior belief",        "neg",
  "deep_neg",  "Repetitive / not novel / shallow",      "neg",
  "angr_neg",  "Made participant upset",                "neg",
  "evid_neg",  "Insufficient evidence / sourcing",      "neg",
  "bias_neg",  "Perceived bias",                        "neg",
  "mech_neg",  "Impersonal / verbose / robotic",        "neg",
  "trst_neg",  "Distrust of AI as a source",            "neg",
  "offt_neg",  "Off-topic / superficial",               "neg",
  "emot_neg",  "Emotional / experiential disconnect",   "neg"
)
theme_keys <- theme_meta$key

# ---- load ----
frame  <- read_csv(file.path(data_dir, "analysis_frame.csv"), show_col_types = FALSE)
consol <- read_csv(file.path(data_dir, "explanation_consolidated.csv"), show_col_types = FALSE)

# participant-level theme indicator: mentioned in EITHER field, among substantive responses
part_theme <- consol %>%
  filter(consensus_response_quality == "substantive") %>%
  group_by(ResponseId) %>%
  summarise(across(all_of(theme_keys), ~ as.integer(max(.x, na.rm = TRUE))),
            n_substantive_fields = n(), .groups = "drop")

dat <- frame %>%
  inner_join(part_theme, by = "ResponseId") %>%
  mutate(
    study_factor = factor(study_factor,
                          levels = c("Jailbroken", "Standard", "Truth-Constrained", "Sharing")),
    persuasion = factor(persuaded, levels = c(0, 1),
                        labels = c("Belief not moved toward AI", "Belief moved toward AI")),
    direction_f = factor(str_to_title(direction), levels = c("Bunk", "Debunk"))
  )
cat("merged participants with a codeable explanation:", nrow(dat), "\n")

# ---- prevalence helper (proportions + Wald CIs) ----
prev_by <- function(d, group_vars, block) {
  if (nrow(d) == 0) return(tibble())
  d %>%
    pivot_longer(all_of(theme_keys), names_to = "key", values_to = "present") %>%
    group_by(across(all_of(group_vars)), key) %>%
    summarise(n = n(), k = sum(present, na.rm = TRUE), .groups = "drop") %>%
    mutate(prevalence = k / n,
           se = sqrt(prevalence * (1 - prevalence) / n),
           ci_low = pmax(0, prevalence - 1.96 * se),
           ci_high = pmin(1, prevalence + 1.96 * se),
           block = block) %>%
    left_join(theme_meta, by = "key")
}

num_overall  <- prev_by(dat, c("persuasion"), "by_persuasion")
num_study    <- prev_by(dat, c("study_factor"), "by_study")
num_direction <- prev_by(dat, c("direction_f"), "by_direction")

# Standard vs Truth-Constrained, compliant bunking (the paper's core contrast)
bunk_comp <- dat %>% filter(direction == "bunk", compliant,
                            study_factor %in% c("Standard", "Truth-Constrained")) %>% droplevels()
num_s2s3 <- prev_by(bunk_comp, c("study_factor"), "standard_vs_truth_bunk_compliant")

# difference (Truth-Constrained - Standard) per theme, two-proportion Wald
diff_s2s3 <- num_s2s3 %>%
  select(study_factor, key, label, valence, n, k, prevalence) %>%
  pivot_wider(names_from = study_factor, values_from = c(n, k, prevalence)) %>%
  { if (all(c("prevalence_Standard", "prevalence_Truth-Constrained") %in% names(.))) . else tibble() }
if (nrow(diff_s2s3) > 0) {
  diff_s2s3 <- diff_s2s3 %>%
    mutate(
      p2 = `prevalence_Standard`, p3 = `prevalence_Truth-Constrained`,
      n2 = `n_Standard`, n3 = `n_Truth-Constrained`,
      diff = p3 - p2,
      se = sqrt(p2 * (1 - p2) / n2 + p3 * (1 - p3) / n3),
      z = diff / se, p_value = 2 * pnorm(abs(z), lower.tail = FALSE),
      ci_low = diff - 1.96 * se, ci_high = diff + 1.96 * se,
      block = "standard_vs_truth_bunk_compliant_DIFF"
    )
}

# primary-theme (dominant reason) distribution, by field
primary_dist <- consol %>%
  filter(consensus_response_quality == "substantive") %>%
  count(field, consensus_primary_theme, name = "n") %>%
  group_by(field) %>% mutate(prop = n / sum(n)) %>% ungroup() %>%
  mutate(block = "primary_theme_by_field")

# ---- write tidy numbers ----
numbers <- bind_rows(
  num_overall %>% rename(group = persuasion) %>% mutate(group = as.character(group)),
  num_study %>% rename(group = study_factor) %>% mutate(group = as.character(group)),
  num_direction %>% rename(group = direction_f) %>% mutate(group = as.character(group)),
  num_s2s3 %>% rename(group = study_factor) %>% mutate(group = as.character(group))
) %>%
  select(block, group, key, label, valence, n, k, prevalence, se, ci_low, ci_high)

if (nrow(diff_s2s3) > 0) {
  numbers <- bind_rows(numbers,
    diff_s2s3 %>% transmute(block, group = "Truth-Constrained - Standard", key, label, valence,
                            n = n3, k = NA_integer_, prevalence = diff, se, ci_low, ci_high))
}
write_csv(numbers, file.path(res_dir, "explanation_theme_numbers.csv"))
write_csv(primary_dist, file.path(res_dir, "explanation_primary_theme_by_field.csv"))
cat("wrote", file.path(res_dir, "explanation_theme_numbers.csv"), "(", nrow(numbers), "rows )\n")

# ---- Figure 1: theme prevalence by persuasion outcome (modernized legacy plot) ----
plot1 <- num_overall %>%
  mutate(valence_lab = if_else(valence == "pos", "Found persuasive (+)", "Found unpersuasive (-)"),
         label = fct_reorder(label, prevalence))
g1 <- ggplot(plot1, aes(x = prevalence * 100, y = label, fill = persuasion)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65, alpha = 0.9) +
  geom_errorbar(aes(xmin = ci_low * 100, xmax = ci_high * 100),
                position = position_dodge(width = 0.7), width = 0.25, linewidth = 0.4) +
  facet_grid(valence_lab ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = c("Belief not moved toward AI" = "#DB9D47",
                               "Belief moved toward AI" = "#33673B"), name = NULL) +
  labs(x = "% of participants mentioning the theme", y = NULL,
       title = "Why participants said the AI was or wasn't persuasive",
       subtitle = "Consensus of 5 frontier-model coders; themes mentioned in either open-ended response") +
  theme_si(base_size = 10)
ggsave(file.path(fig_dir, "explanation_themes_by_persuasion.pdf"), g1, width = 9, height = 8)
ggsave(file.path(fig_dir, "explanation_themes_by_persuasion.png"), g1, width = 9, height = 8, dpi = 150)

# ---- Figure 2: Standard vs Truth-Constrained, compliant bunking ----
if (nrow(num_s2s3) > 0) {
  plot2 <- num_s2s3 %>%
    mutate(valence_lab = if_else(valence == "pos", "Persuasive (+)", "Unpersuasive (-)"),
           label = fct_reorder(label, prevalence))
  g2 <- ggplot(plot2, aes(x = prevalence * 100, y = label, fill = study_factor)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65, alpha = 0.9) +
    geom_errorbar(aes(xmin = ci_low * 100, xmax = ci_high * 100),
                  position = position_dodge(width = 0.7), width = 0.25, linewidth = 0.4) +
    facet_grid(valence_lab ~ ., scales = "free_y", space = "free_y") +
    scale_fill_manual(values = c("Standard" = "#1E88E5", "Truth-Constrained" = "#D81B60"), name = NULL) +
    labs(x = "% of participants mentioning the theme", y = NULL,
         title = "Standard vs Truth-Constrained bunking: stated reasons",
         subtitle = "Compliant bunking conversations only") +
    theme_si(base_size = 10)
  ggsave(file.path(fig_dir, "explanation_themes_standard_vs_truth.pdf"), g2, width = 9, height = 8)
  ggsave(file.path(fig_dir, "explanation_themes_standard_vs_truth.png"), g2, width = 9, height = 8, dpi = 150)
}

cat("figures ->", fig_dir, "\n")
cat("done.\n")
