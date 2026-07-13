# analyze_explanations_by_arm.R
# Cross-arm breakdown of the consensus explanation codes: theme prevalence by
# study x persuader x direction, separately for COMPLIANT-only (the apples-to-apples
# sample for the mechanism claim) and ALL cases. S1-3 persuader = prompt condition
# (Jailbroken/Standard/Truth-Constrained, GPT-4o); S4 persuader = model
# (Claude/Gemini/GPT-5.2/Grok). Renders prevalence heatmaps + a tidy numbers table.
#
# Inputs : data/api_cached/explanation_coding/{analysis_frame.csv, explanation_consolidated.csv}
# Outputs: output/provenance_work/explanation_coding/explanation_theme_numbers_by_arm.csv
#          output/provenance_work/explanation_coding/figures/explanation_themes_heatmap_compliant.pdf/.png
#          output/provenance_work/explanation_coding/figures/explanation_themes_heatmap_allcases.pdf/.png
#          output/provenance_work/explanation_coding/figures/explanation_arm_compliance_rates.png
# Run    : Rscript analyze_explanations_by_arm.R

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

# NOT SHIPPED: ../supplement_v2/R/figures_core.R is an external author-side tree; the
# source() is wrapped in try() and a theme_si fallback is defined below.
suppressWarnings(try(source(file.path(root, "..", "supplement_v2", "R", "figures_core.R")), silent = TRUE))
if (!exists("theme_si")) theme_si <- function(base_size = 9) theme_minimal(base_size = base_size) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"), legend.position = "bottom")

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
persuader_levels <- c("Jailbroken", "Standard", "Truth-Constrained", "Claude", "Gemini", "GPT-5.2", "Grok")

# ---- load + participant-level theme indicator (mention in either field, substantive) ----
frame  <- read_csv(file.path(data_dir, "analysis_frame.csv"), show_col_types = FALSE)
consol <- read_csv(file.path(data_dir, "explanation_consolidated.csv"), show_col_types = FALSE)

part_theme <- consol %>%
  filter(consensus_response_quality == "substantive") %>%
  group_by(ResponseId) %>%
  summarise(across(all_of(theme_keys), ~ as.integer(max(.x, na.rm = TRUE))), .groups = "drop")

dat <- frame %>%
  inner_join(part_theme, by = "ResponseId") %>%
  mutate(
    setting = if_else(study == "Study 4", "Study 4 (frontier models)", "Studies 1-3 (GPT-4o prompts)"),
    persuader = factor(persuader, levels = persuader_levels),
    direction_f = factor(str_to_title(direction), levels = c("Bunk", "Debunk"))
  )
cat("participants with codeable explanation:", nrow(dat),
    "| compliant:", sum(dat$compliant, na.rm = TRUE), "\n")

# ---- prevalence helper ----
prev_by_arm <- function(d, sample_label) {
  if (nrow(d) == 0) return(tibble())
  d %>%
    pivot_longer(all_of(theme_keys), names_to = "key", values_to = "present") %>%
    group_by(setting, persuader, direction_f, key) %>%
    summarise(n = n(), k = sum(present, na.rm = TRUE), .groups = "drop") %>%
    mutate(prevalence = k / n,
           se = sqrt(prevalence * (1 - prevalence) / n),
           ci_low = pmax(0, prevalence - 1.96 * se),
           ci_high = pmin(1, prevalence + 1.96 * se),
           sample = sample_label) %>%
    left_join(theme_meta, by = "key")
}

num_compliant <- prev_by_arm(filter(dat, compliant %in% TRUE), "compliant")
num_all       <- prev_by_arm(dat, "all_cases")
numbers <- bind_rows(num_compliant, num_all) %>%
  select(sample, setting, persuader, direction = direction_f, key, label, valence,
         n, k, prevalence, se, ci_low, ci_high)
write_csv(numbers, file.path(res_dir, "explanation_theme_numbers_by_arm.csv"))
cat("wrote", file.path(res_dir, "explanation_theme_numbers_by_arm.csv"), "(", nrow(numbers), "rows )\n")

# ---- heatmap renderer ----
make_heatmap <- function(num, sample_label, title) {
  if (nrow(num) == 0) return(invisible())
  # order themes: + block on top, - block below, each by mean prevalence
  ord <- num %>% group_by(label, valence) %>% summarise(mp = mean(prevalence), .groups = "drop") %>%
    arrange(valence, desc(mp))                      # pos first, then neg; high prev first
  lev <- rev(c(ord$label[ord$valence == "pos"], ord$label[ord$valence == "neg"]))
  small_n_arms <- num %>% distinct(setting, persuader, direction_f, n) %>% filter(n < 40)
  cap <- if (nrow(small_n_arms) > 0)
    paste0("Cells outlined in grey have n < 40 (interpret with caution): ",
           paste(sprintf("%s/%s (n=%d)", small_n_arms$persuader, small_n_arms$direction_f,
                         small_n_arms$n), collapse = "; ")) else ""
  pd <- num %>% mutate(label = factor(label, levels = lev),
                       small_n = n < 40,
                       lab = sprintf("%.0f", prevalence * 100))
  g <- ggplot(pd, aes(x = persuader, y = label, fill = prevalence * 100)) +
    geom_tile(aes(color = small_n), linewidth = 0.5, width = 0.95, height = 0.95) +
    geom_text(aes(label = lab), size = 2.6,
              color = ifelse(pd$prevalence > 0.33, "white", "grey15")) +
    facet_grid(direction_f ~ setting, scales = "free_x", space = "free_x") +
    scale_fill_gradient(low = "#f7fbff", high = "#08519c", name = "% mentioning") +
    scale_color_manual(values = c(`TRUE` = "grey55", `FALSE` = NA), guide = "none") +
    labs(x = "Persuader (S1-3: prompt condition · S4: model)", y = NULL,
         title = title,
         subtitle = "Consensus of 5 frontier-model coders; % of participants mentioning each theme (either OE response)",
         caption = cap) +
    theme_si(base_size = 9) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1),
          panel.grid = element_blank(), legend.position = "right",
          plot.caption = element_text(size = 6.5, hjust = 0))
  ggsave(file.path(fig_dir, sprintf("explanation_themes_heatmap_%s.pdf", sample_label)), g, width = 11, height = 8)
  ggsave(file.path(fig_dir, sprintf("explanation_themes_heatmap_%s.png", sample_label)), g, width = 11, height = 8, dpi = 150)
}

make_heatmap(num_compliant, "compliant", "Reasons by study, condition & S4 model — COMPLIANT conversations only")
make_heatmap(num_all,       "allcases",  "Reasons by study, condition & S4 model — all cases")

# ---- compliance rates per arm (context for the filtering) ----
comp_rate <- dat %>%
  group_by(setting, persuader, direction_f) %>%
  summarise(n = n(), compliant = sum(compliant %in% TRUE), rate = compliant / n, .groups = "drop")
write_csv(comp_rate, file.path(res_dir, "explanation_arm_compliance_rates.csv"))
gc <- ggplot(comp_rate, aes(x = persuader, y = rate * 100, fill = direction_f)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_text(aes(label = sprintf("%.0f%%\n(n=%d)", rate * 100, n)),
            position = position_dodge(width = 0.7), vjust = -0.2, size = 2.4) +
  facet_grid(. ~ setting, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = c("Bunk" = "#D81B60", "Debunk" = "#1E88E5"), name = NULL) +
  ylim(0, 105) +
  labs(x = NULL, y = "% compliant", title = "Compliance rate by arm (among participants with a codeable explanation)") +
  theme_si(base_size = 9) + theme(axis.text.x = element_text(angle = 35, hjust = 1))
ggsave(file.path(fig_dir, "explanation_arm_compliance_rates.png"), gc, width = 10, height = 5, dpi = 150)

cat("figures ->", fig_dir, "\ndone.\n")
