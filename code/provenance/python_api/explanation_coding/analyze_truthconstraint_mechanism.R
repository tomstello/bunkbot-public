# analyze_truthconstraint_mechanism.R
# Why does the truth constraint cut BUNKING efficacy but not DEBUNKING (compliant)?
#   (1) Selective efficacy penalty: aligned-belief-change gap Truth-Constrained - Standard,
#       within compliant bunking and within compliant debunking (HC3 CI).
#   (2) Interpretation shift: per-reason prevalence difference (S3 - S2), within bunking and
#       within debunking (HC3 CI) -- does the constraint change how each is experienced?
#   (3) Oaxaca-style reason decomposition: contribution_r = (prev_S3 - prev_S2) x weight_r,
#       weight_r = ridge reason->belief-change coef (from output/provenance_work/
#       explanation_coding/explanation_reason_change_bootstrap.csv).
#       Sum approximates the share of the efficacy gap "accounted for" by interpretation shifts.
#       DESCRIPTIVE: reasons are post-hoc, partly rationalization, so treat as a perceptual
#       signature of the gap, not clean mediation.
#
# Outputs: output/provenance_work/explanation_coding/truthconstraint_{efficacy_gap,reason_diffs,decomposition}.csv
#          output/provenance_work/explanation_coding/figures/truthconstraint_mechanism.pdf/.png
# Run    : Rscript analyze_truthconstraint_mechanism.R

suppressWarnings(suppressMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr)
  library(forcats); library(ggplot2); library(purrr); library(patchwork); library(sandwich)
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
# (also READS explanation_reason_change_bootstrap.csv produced here by figure_interpretation_and_change.R)
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
theme_keys <- theme_meta$key; labmap <- setNames(theme_meta$label, theme_meta$key)

frame  <- read_csv(file.path(data_dir, "analysis_frame.csv"), show_col_types = FALSE)
consol <- read_csv(file.path(data_dir, "explanation_consolidated.csv"), show_col_types = FALSE)
weights <- read_csv(file.path(res_dir, "explanation_reason_change_bootstrap.csv"), show_col_types = FALSE) %>%
  select(key, w_bunk = est_Bunking, w_debunk = est_Debunking)
part_any <- consol %>% filter(consensus_response_quality == "substantive") %>%
  group_by(ResponseId) %>%
  summarise(across(all_of(theme_keys), ~ as.integer(max(.x, na.rm = TRUE))), .groups = "drop")

# Standard (S2) vs Truth-Constrained (S3), compliant, both GPT-4o prompts
dat <- frame %>% inner_join(part_any, by = "ResponseId") %>%
  filter(compliant %in% TRUE, study_factor %in% c("Standard", "Truth-Constrained"),
         direction %in% c("bunk", "debunk"), !is.na(aligned_belief_change), !is.na(belief_pre)) %>%
  mutate(tc = as.integer(study_factor == "Truth-Constrained"),   # 1 = Truth-Constrained
         direction_f = factor(str_to_title(direction), levels = c("Bunk", "Debunk")))

hc <- function(fit, term) {
  v <- sandwich::vcovHC(fit, "HC3"); b <- coef(fit)[term]; s <- sqrt(v[term, term]); z <- b / s
  c(est = unname(b), lo = unname(b - 1.96 * s), hi = unname(b + 1.96 * s),
    p = unname(2 * pnorm(abs(z), lower.tail = FALSE)))
}

# (1) efficacy gap (S3 - S2) per direction
eff <- map_dfr(c("bunk", "debunk"), function(dir) {
  d <- filter(dat, direction == dir)
  r <- hc(lm(aligned_belief_change ~ tc + belief_pre, data = d), "tc")
  tibble(direction = str_to_title(dir), est = r["est"], lo = r["lo"], hi = r["hi"], p = r["p"],
         n_S2 = sum(d$tc == 0), n_S3 = sum(d$tc == 1))
})
write_csv(eff, file.path(res_dir, "truthconstraint_efficacy_gap.csv"))
cat("=== Efficacy gap: Truth-Constrained - Standard aligned belief change (compliant) ===\n")
for (i in seq_len(nrow(eff))) with(eff[i, ],
  cat(sprintf("  %-7s %+5.2f [%+.2f,%+.2f] p=%.3g  (n S2=%d, S3=%d)\n", direction, est, lo, hi, p, n_S2, n_S3)))

# (2) reason prevalence difference (S3 - S2) per direction
rd <- map_dfr(c("bunk", "debunk"), function(dir) {
  d <- filter(dat, direction == dir)
  map_dfr(theme_keys, function(k) {
    dd <- d %>% mutate(yy = .data[[k]])
    r <- hc(lm(yy ~ tc + belief_pre, data = dd), "tc")
    tibble(direction = str_to_title(dir), key = k, est = r["est"] * 100,
           lo = r["lo"] * 100, hi = r["hi"] * 100, p = r["p"],
           prev_S2 = mean(dd$yy[dd$tc == 0]), prev_S3 = mean(dd$yy[dd$tc == 1]))
  })
}) %>% left_join(theme_meta, by = "key") %>% mutate(label = labmap[key])
write_csv(rd, file.path(res_dir, "truthconstraint_reason_diffs.csv"))

# (3) Oaxaca-style decomposition of each direction's efficacy gap
decomp <- rd %>% left_join(weights, by = "key") %>%
  mutate(weight = if_else(direction == "Bunk", w_bunk, w_debunk),
         dprev = prev_S3 - prev_S2,                 # proportion difference
         contribution = dprev * weight) %>%          # points of aligned change
  select(direction, key, label, valence, prev_S2, prev_S3, dprev, weight, contribution)
write_csv(decomp, file.path(res_dir, "truthconstraint_decomposition.csv"))
cat("\n=== Reason decomposition of the efficacy gap (points of aligned change) ===\n")
for (dir in c("Bunk", "Debunk")) {
  dd <- decomp %>% filter(direction == dir) %>% arrange(contribution)
  expl <- sum(dd$contribution); tot <- eff$est[eff$direction == dir]
  cat(sprintf("\n%s: total gap %+.2f; reason-explained %+.2f (%.0f%% of gap)\n", dir, tot, expl,
              100 * expl / tot))
  for (i in seq_len(nrow(dd))) with(dd[i, ],
    cat(sprintf("   %-36s dprev %+5.1fpp x w %+5.2f = %+5.2f\n", label, dprev*100, weight, contribution)))
}

# ---- figure ----
lev <- decomp %>% filter(direction == "Bunk") %>% arrange(weight) %>% pull(label)
relab <- function(x) factor(recode(x, "Bunk" = "Bunking", "Debunk" = "Debunking"),
                            levels = c("Bunking", "Debunking"))
rd  <- rd  %>% mutate(label = factor(label, levels = lev), direction = relab(direction))
eff <- eff %>% mutate(direction = relab(direction))

p1 <- ggplot(eff, aes(x = est, y = direction, color = direction)) +
  geom_vline(xintercept = 0, color = "grey55") +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.12, linewidth = 0.6) +
  geom_point(size = 3) +
  scale_color_manual(values = dir_cols, guide = "none") +
  labs(x = "Truth-Constrained - Standard aligned belief change (points)", y = NULL,
       title = "A  Truth constraint cuts bunking efficacy, not debunking",
       subtitle = "Compliant Standard vs Truth-Constrained (GPT-4o); HC3 95% CI") +
  theme_si(base_size = 9.5)

p2 <- ggplot(rd, aes(x = est, y = label, color = direction)) +
  geom_vline(xintercept = 0, color = "grey55") +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.45,
                position = position_dodge(width = 0.6)) +
  geom_point(size = 2.2, position = position_dodge(width = 0.6)) +
  scale_color_manual(values = dir_cols, name = NULL) +
  labs(x = "Truth-Constrained - Standard difference in % mentioning (adjusted, HC3)", y = NULL,
       title = "B  ...yet stated reasons do NOT explain the gap: S3 bunking is seen as MORE factual/expert",
       subtitle = "Reason shift (S3-S2), ordered by belief-change weight; reasons explain ~0% of the gap") +
  theme_si(base_size = 9.5)

combined <- p1 / p2 + plot_layout(heights = c(0.28, 1))
ggsave(file.path(fig_dir, "truthconstraint_mechanism.pdf"), combined, width = 9.5, height = 9)
ggsave(file.path(fig_dir, "truthconstraint_mechanism.png"), combined, width = 9.5, height = 9, dpi = 150)
cat("\nfigure ->", file.path(fig_dir, "truthconstraint_mechanism.png"), "\ndone.\n")
