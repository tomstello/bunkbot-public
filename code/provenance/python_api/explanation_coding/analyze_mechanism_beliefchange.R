# analyze_mechanism_beliefchange.R
# Is STATING A CONSPIRACY-SPECIFIC MECHANISM (conx_pos) associated with belief change?
# Outcome = aligned_belief_change (direction-aligned: + = belief moved toward what the
# AI argued). COMPLIANT cases. Baseline-adjusted (belief_pre) + persuader FE, HC3.
# NOTE: explanations are reported AFTER the conversation, so this is an ASSOCIATION,
# not a manipulated cause (mechanism-mention may be a marker of engagement/persuasion).
#
#   (1) Context: adjusted association of EACH theme with aligned_belief_change (where
#       does mechanism rank as a correlate of movement?).
#   (2) Mechanism deep-dive: conx_pos x direction (effect within bunk and debunk),
#       and a persuasive-box-specific version (mechanism credited AS persuasive).
#   (3) Heterogeneity: conx_pos x study_group joint HC3 Wald (varies by study/model?).
#   (4) Figure: mean aligned change for mechanism-stated vs not, by direction x study.
#
# Outputs: output/provenance_work/explanation_coding/explanation_mechanism_beliefchange.csv (+ _by_theme.csv)
#          output/provenance_work/explanation_coding/figures/explanation_mechanism_beliefchange.pdf/.png
# Run    : Rscript analyze_mechanism_beliefchange.R

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

frame  <- read_csv(file.path(data_dir, "analysis_frame.csv"), show_col_types = FALSE)
consol <- read_csv(file.path(data_dir, "explanation_consolidated.csv"), show_col_types = FALSE)

# participant-level theme = mentioned in EITHER substantive response
part_any <- consol %>% filter(consensus_response_quality == "substantive") %>%
  group_by(ResponseId) %>%
  summarise(across(all_of(theme_keys), ~ as.integer(max(.x, na.rm = TRUE))), .groups = "drop")
# mechanism credited specifically in the PERSUASIVE box (sharper "found a mechanism persuasive")
conx_pers <- consol %>% filter(field == "persuasive", consensus_response_quality == "substantive") %>%
  group_by(ResponseId) %>% summarise(conx_pers = as.integer(any(conx_pos == 1)), .groups = "drop")

dat <- frame %>% inner_join(part_any, by = "ResponseId") %>%
  left_join(conx_pers, by = "ResponseId") %>%
  mutate(conx_pers = coalesce(conx_pers, 0L)) %>%
  filter(compliant %in% TRUE, direction %in% c("bunk", "debunk"),
         !is.na(aligned_belief_change), !is.na(belief_pre)) %>%
  mutate(direction = factor(direction, levels = c("bunk", "debunk")),
         persuader = factor(persuader),
         study_group = factor(if_else(study == "Study 4", "Study 4", study_factor),
                              levels = c("Jailbroken", "Standard", "Truth-Constrained", "Study 4")))
cat("compliant analytic n:", nrow(dat),
    "| bunk:", sum(dat$direction == "bunk"), "debunk:", sum(dat$direction == "debunk"), "\n")
cat("mean aligned_belief_change:", round(mean(dat$aligned_belief_change), 2), "\n\n")

# linear combination est/se from an HC3 vcov
lc <- function(fit, vc, weights) {
  b <- coef(fit); L <- setNames(rep(0, length(b)), names(b))
  L[names(weights)] <- weights
  est <- sum(L * b, na.rm = TRUE)
  se <- sqrt(as.numeric(t(L) %*% vc %*% L))
  z <- est / se
  c(est = est, se = se, lo = est - 1.96 * se, hi = est + 1.96 * se,
    p = 2 * pnorm(abs(z), lower.tail = FALSE))
}
joint_wald <- function(fit, terms, vc) {
  terms <- terms[terms %in% names(coef(fit)) & !is.na(coef(fit)[terms])]
  if (!length(terms)) return(c(W = NA, df = 0, p = NA))
  b <- coef(fit)[terms]; V <- vc[terms, terms, drop = FALSE]
  W <- tryCatch(as.numeric(t(b) %*% solve(V) %*% b), error = function(e) NA)
  c(W = W, df = length(terms), p = if (is.na(W)) NA else pchisq(W, length(terms), lower.tail = FALSE))
}

# ---- (1) context: adjusted association of each theme with aligned change ----
by_theme <- map_dfr(theme_keys, function(t) {
  d <- dat %>% mutate(x = .data[[t]])
  if (sd(d$x) == 0) return(tibble(key = t, est = NA, se = NA, lo = NA, hi = NA, p = NA, prevalence = mean(d$x)))
  fit <- lm(aligned_belief_change ~ x + belief_pre + direction + persuader, data = d)
  vc <- sandwich::vcovHC(fit, type = "HC3"); r <- lc(fit, vc, c(x = 1))
  tibble(key = t, est = r["est"], se = r["se"], lo = r["lo"], hi = r["hi"], p = r["p"], prevalence = mean(d$x))
}) %>% left_join(theme_meta, by = "key") %>% arrange(desc(est))
write_csv(by_theme %>% select(key, label, valence, prevalence, est, se, lo, hi, p),
          file.path(res_dir, "explanation_mechanism_beliefchange_by_theme.csv"))
cat("=== Adjusted association of each theme with aligned belief change (pp on 0-100 scale) ===\n")
for (i in seq_len(nrow(by_theme))) with(by_theme[i, ],
  cat(sprintf("  %-36s %+5.1f [%+5.1f,%+5.1f] p=%.2g  (prev %.0f%%)\n", label, est, lo, hi, p, prevalence*100)))

# ---- (2) mechanism deep-dive: conx_pos x direction ----
fitm <- lm(aligned_belief_change ~ conx_pos * direction + belief_pre + persuader, data = dat)
vcm <- sandwich::vcovHC(fitm, type = "HC3")
eff_bunk <- lc(fitm, vcm, c(conx_pos = 1))
eff_deb  <- lc(fitm, vcm, c(conx_pos = 1, `conx_pos:directiondebunk` = 1))
inter    <- lc(fitm, vcm, c(`conx_pos:directiondebunk` = 1))
# persuasive-box-specific
fitp <- lm(aligned_belief_change ~ conx_pers * direction + belief_pre + persuader, data = dat)
vcp <- sandwich::vcovHC(fitp, type = "HC3")
effp_bunk <- lc(fitp, vcp, c(conx_pers = 1))
effp_deb  <- lc(fitp, vcp, c(conx_pers = 1, `conx_pers:directiondebunk` = 1))
# heterogeneity by study group
fith <- lm(aligned_belief_change ~ conx_pos * study_group + belief_pre + direction, data = dat)
vch <- sandwich::vcovHC(fith, type = "HC3")
het <- joint_wald(fith, grep("conx_pos:study_group", names(coef(fith)), value = TRUE), vch)

cat("\n=== Mechanism (conx_pos, mentioned anywhere) vs aligned belief change ===\n")
cat(sprintf("  within BUNK   : %+5.2f [%+.2f,%+.2f] p=%.3g\n", eff_bunk["est"], eff_bunk["lo"], eff_bunk["hi"], eff_bunk["p"]))
cat(sprintf("  within DEBUNK : %+5.2f [%+.2f,%+.2f] p=%.3g\n", eff_deb["est"], eff_deb["lo"], eff_deb["hi"], eff_deb["p"]))
cat(sprintf("  bunk vs debunk interaction: %+5.2f p=%.3g\n", inter["est"], inter["p"]))
cat(sprintf("  het across study/model (conx x study_group): joint Wald p=%.3g\n", het["p"]))
cat("\n=== Mechanism credited AS PERSUASIVE (persuasive box only) ===\n")
cat(sprintf("  within BUNK   : %+5.2f [%+.2f,%+.2f] p=%.3g\n", effp_bunk["est"], effp_bunk["lo"], effp_bunk["hi"], effp_bunk["p"]))
cat(sprintf("  within DEBUNK : %+5.2f [%+.2f,%+.2f] p=%.3g\n", effp_deb["est"], effp_deb["lo"], effp_deb["hi"], effp_deb["p"]))

summary_rows <- tibble(
  measure = c("conx_any", "conx_any", "conx_any_interaction", "conx_persuasive", "conx_persuasive"),
  contrast = c("within_bunk", "within_debunk", "debunk_minus_bunk", "within_bunk", "within_debunk"),
  est = c(eff_bunk["est"], eff_deb["est"], inter["est"], effp_bunk["est"], effp_deb["est"]),
  ci_low = c(eff_bunk["lo"], eff_deb["lo"], inter["lo"], effp_bunk["lo"], effp_deb["lo"]),
  ci_high = c(eff_bunk["hi"], eff_deb["hi"], inter["hi"], effp_bunk["hi"], effp_deb["hi"]),
  p = c(eff_bunk["p"], eff_deb["p"], inter["p"], effp_bunk["p"], effp_deb["p"]),
  het_study_p = het["p"])
write_csv(summary_rows, file.path(res_dir, "explanation_mechanism_beliefchange.csv"))

# ---- (4) figure: mean aligned change by mechanism x direction x study group ----
cell <- dat %>%
  mutate(mech = factor(conx_pos, levels = c(0, 1), labels = c("No mechanism", "Stated mechanism")),
         direction_f = factor(str_to_title(as.character(direction)), levels = c("Bunk", "Debunk"))) %>%
  group_by(study_group, direction_f, mech) %>%
  summarise(n = n(), m = mean(aligned_belief_change),
            se = sd(aligned_belief_change) / sqrt(n), .groups = "drop") %>%
  mutate(lo = m - 1.96 * se, hi = m + 1.96 * se)
g <- ggplot(cell, aes(x = m, y = fct_rev(study_group), color = mech)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.5,
                position = position_dodge(width = 0.6)) +
  geom_point(size = 2.6, position = position_dodge(width = 0.6)) +
  geom_text(aes(label = paste0("n=", n)), position = position_dodge(width = 0.6),
            hjust = -0.25, size = 2.3, show.legend = FALSE) +
  facet_grid(. ~ direction_f) +
  scale_color_manual(values = c("No mechanism" = "grey60", "Stated mechanism" = "#08519c"), name = NULL) +
  labs(x = "Mean aligned belief change (toward the AI's position, 0-100 scale)", y = NULL,
       title = "Stating a conspiracy-specific mechanism vs belief change",
       subtitle = "Compliant cases; bars = 95% CI of the cell mean (unadjusted; see CSV for HC3-adjusted effects)") +
  theme_si(base_size = 9.5) + theme(legend.position = "bottom")
ggsave(file.path(fig_dir, "explanation_mechanism_beliefchange.pdf"), g, width = 10, height = 5.5)
ggsave(file.path(fig_dir, "explanation_mechanism_beliefchange.png"), g, width = 10, height = 5.5, dpi = 150)
cat("\nfigure ->", file.path(fig_dir, "explanation_mechanism_beliefchange.png"), "\ndone.\n")
