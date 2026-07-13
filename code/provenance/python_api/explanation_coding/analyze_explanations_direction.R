# analyze_explanations_direction.R
# Bunking vs debunking contrast in stated reasons (COMPLIANT cases).
#   (1) Pooled per-theme contrast: prevalence (bunk, debunk), raw diff (Wald CI),
#       and persuader-adjusted diff from an HC3 LPM (present ~ direction + persuader).
#   (2) Heterogeneity: does the bunk-vs-debunk gap vary by study/prompt/model?
#       Joint HC3 Wald test of the direction x study_group interaction, per theme.
#   (3) Figures: a dumbbell of actual bunk vs debunk prevalence (levels, not a
#       difference axis) and a by-study breakdown of the direction gap.
#
# Outputs (output/provenance_work/explanation_coding/):
#          explanation_direction_contrasts.csv
#          explanation_direction_heterogeneity.csv
#          explanation_direction_by_study.csv
#          figures/explanation_direction_dumbbell.pdf/.png
#          figures/explanation_direction_by_study.pdf/.png
# Run    : Rscript analyze_explanations_direction.R

suppressWarnings(suppressMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr)
  library(forcats); library(ggplot2); library(purrr); library(sandwich)
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
fig_dir  <- file.path(res_dir, "figures"); dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# NOT SHIPPED: ../supplement_v2/R/figures_core.R is an external author-side tree; the
# source() is wrapped in try() and a theme_si fallback is defined below.
suppressWarnings(try(source(file.path(root, "..", "supplement_v2", "R", "figures_core.R")), silent = TRUE))
if (!exists("theme_si")) theme_si <- function(base_size = 9) theme_minimal(base_size = base_size) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"), legend.position = "bottom")
dir_cols <- c("Bunking" = "#D81B60", "Debunking" = "#1E88E5")

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
valence_lab <- c(pos = "Found persuasive (+)", neg = "Found unpersuasive (-)")

frame  <- read_csv(file.path(data_dir, "analysis_frame.csv"), show_col_types = FALSE)
consol <- read_csv(file.path(data_dir, "explanation_consolidated.csv"), show_col_types = FALSE)
part_theme <- consol %>% filter(consensus_response_quality == "substantive") %>%
  group_by(ResponseId) %>%
  summarise(across(all_of(theme_keys), ~ as.integer(max(.x, na.rm = TRUE))), .groups = "drop")

dat <- frame %>% inner_join(part_theme, by = "ResponseId") %>%
  filter(compliant %in% TRUE, direction %in% c("bunk", "debunk")) %>%
  mutate(direction = factor(direction, levels = c("bunk", "debunk")),
         persuader = factor(persuader),
         study_group = factor(if_else(study == "Study 4", "Study 4", study_factor),
                              levels = c("Jailbroken", "Standard", "Truth-Constrained", "Study 4")))
nb <- sum(dat$direction == "bunk"); nd <- sum(dat$direction == "debunk")
cat("compliant w/ codeable explanation:", nrow(dat), "| bunk:", nb, "debunk:", nd, "\n")

# ---- (1) pooled per-theme contrast ----
joint_wald <- function(fit, terms, vc) {
  ok <- terms %in% names(coef(fit)) & !is.na(coef(fit)[terms])
  terms <- terms[ok]
  if (!length(terms)) return(list(W = NA, df = 0, p = NA))
  b <- coef(fit)[terms]; V <- vc[terms, terms, drop = FALSE]
  W <- tryCatch(as.numeric(t(b) %*% solve(V) %*% b), error = function(e) NA)
  list(W = W, df = length(terms), p = if (is.na(W)) NA else pchisq(W, length(terms), lower.tail = FALSE))
}

contrast_one <- function(theme) {
  d <- dat %>% mutate(y = .data[[theme]])
  pb <- mean(d$y[d$direction == "bunk"]); pd <- mean(d$y[d$direction == "debunk"])
  se_raw <- sqrt(pb * (1 - pb) / nb + pd * (1 - pd) / nd); raw <- pd - pb
  fit <- lm(y ~ direction + persuader, data = d); vc <- sandwich::vcovHC(fit, type = "HC3")
  b <- coef(fit)["directiondebunk"]; s <- sqrt(vc["directiondebunk", "directiondebunk"])
  # interaction model for heterogeneity
  fiti <- lm(y ~ direction * study_group, data = d); vci <- sandwich::vcovHC(fiti, type = "HC3")
  it <- grep("directiondebunk:study_group", names(coef(fiti)), value = TRUE)
  hw <- joint_wald(fiti, it, vci)
  tibble(key = theme, prev_bunk = pb, prev_debunk = pd,
         raw_diff = raw, raw_ci_low = raw - 1.96 * se_raw, raw_ci_high = raw + 1.96 * se_raw,
         raw_p = 2 * pnorm(abs(raw / se_raw), lower.tail = FALSE),
         adj_diff = unname(b), adj_ci_low = unname(b - 1.96 * s), adj_ci_high = unname(b + 1.96 * s),
         adj_p = unname(2 * pnorm(abs(b / s), lower.tail = FALSE)),
         het_W = hw$W, het_df = hw$df, het_p = hw$p)
}
res <- map_dfr(theme_keys, contrast_one) %>% left_join(theme_meta, by = "key")
write_csv(res %>% select(key, label, valence, prev_bunk, prev_debunk, raw_diff, raw_ci_low, raw_ci_high,
                         raw_p, adj_diff, adj_ci_low, adj_ci_high, adj_p),
          file.path(res_dir, "explanation_direction_contrasts.csv"))
write_csv(res %>% select(key, label, valence, het_W, het_df, het_p) %>% arrange(het_p),
          file.path(res_dir, "explanation_direction_heterogeneity.csv"))

# ---- (2) by-study direction prevalences + diff ----
by_study <- dat %>%
  pivot_longer(all_of(theme_keys), names_to = "key", values_to = "present") %>%
  group_by(study_group, key, direction) %>%
  summarise(n = n(), p = mean(present), .groups = "drop") %>%
  pivot_wider(names_from = direction, values_from = c(n, p)) %>%
  mutate(diff = p_debunk - p_bunk,
         se = sqrt(p_bunk * (1 - p_bunk) / n_bunk + p_debunk * (1 - p_debunk) / n_debunk),
         ci_low = diff - 1.96 * se, ci_high = diff + 1.96 * se) %>%
  left_join(theme_meta, by = "key")
write_csv(by_study, file.path(res_dir, "explanation_direction_by_study.csv"))

# ---- console summary ----
cat("\n=== Pooled bunk vs debunk (persuader-adjusted HC3) + heterogeneity by study ===\n")
for (i in order(-abs(res$adj_diff))) with(res[i, ],
  cat(sprintf("  %-36s bunk %4.1f%% debunk %4.1f%%  adj %+5.1fpp p=%.2g | het p=%.2g %s\n",
              label, prev_bunk*100, prev_debunk*100, adj_diff*100, adj_p, het_p,
              ifelse(!is.na(het_p) & het_p < .05, "<-- varies by study", ""))))

# ---- (3a) dumbbell of actual prevalences (levels, not a difference axis) ----
dumb <- res %>%
  transmute(label, valence, Bunking = prev_bunk, Debunking = prev_debunk) %>%
  pivot_longer(c(Bunking, Debunking), names_to = "direction", values_to = "p")
ord <- res %>% arrange(valence, prev_debunk) %>% pull(label)
dumb <- dumb %>% mutate(label = factor(label, levels = ord),
                        valence_lab = factor(valence_lab[valence], levels = valence_lab))
seg <- res %>% mutate(label = factor(label, levels = ord),
                      valence_lab = factor(valence_lab[valence], levels = valence_lab))
g1 <- ggplot(dumb, aes(x = p * 100, y = label)) +
  geom_segment(data = seg, aes(x = prev_bunk * 100, xend = prev_debunk * 100, y = label, yend = label),
               color = "grey70", linewidth = 1) +
  geom_point(aes(color = direction), size = 3) +
  facet_grid(valence_lab ~ ., scales = "free_y", space = "free_y") +
  scale_color_manual(values = dir_cols, name = NULL) +
  labs(x = "% of participants mentioning the theme", y = NULL,
       title = "Why participants found AI (un)persuasive: bunking vs debunking",
       subtitle = sprintf("Compliant conversations; consensus of 5 coders (bunk n=%d, debunk n=%d)", nb, nd)) +
  theme_si(base_size = 10)
ggsave(file.path(fig_dir, "explanation_direction_dumbbell.pdf"), g1, width = 9, height = 7.5)
ggsave(file.path(fig_dir, "explanation_direction_dumbbell.png"), g1, width = 9, height = 7.5, dpi = 150)

# ---- (3b) by-study breakdown of the direction gap ----
ord2 <- res %>% arrange(valence, adj_diff) %>% pull(label)
bs <- by_study %>% mutate(label = factor(label, levels = ord2),
                          valence_lab = factor(valence_lab[valence], levels = valence_lab))
g2 <- ggplot(bs, aes(x = diff * 100, y = label, color = study_group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(position = position_dodge(width = 0.6), size = 1.9) +
  geom_errorbar(aes(xmin = ci_low * 100, xmax = ci_high * 100),
                position = position_dodge(width = 0.6), width = 0, linewidth = 0.4) +
  facet_grid(valence_lab ~ ., scales = "free_y", space = "free_y") +
  scale_color_brewer(palette = "Dark2", name = NULL) +
  labs(x = "Debunking − Bunking (percentage points)", y = NULL,
       title = "Does the bunking vs debunking gap vary by study / prompt / model?",
       subtitle = "Debunk minus bunk in % mentioning, within each study group (compliant)") +
  theme_si(base_size = 10)
ggsave(file.path(fig_dir, "explanation_direction_by_study.pdf"), g2, width = 9.5, height = 8)
ggsave(file.path(fig_dir, "explanation_direction_by_study.png"), g2, width = 9.5, height = 8, dpi = 150)

cat("\nfigures ->", fig_dir, "\ndone.\n")
