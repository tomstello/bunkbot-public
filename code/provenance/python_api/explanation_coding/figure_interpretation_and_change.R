# figure_interpretation_and_change.R
# Two-panel figure (shared reason rows), with uncertainty + significance throughout.
#   Panel A: which reasons distinguish DEBUNKING from BUNKING -- adjusted prevalence
#            difference (debunk - bunk) per reason, persuader-FE LPM, HC3 95% CI.
#   Panel B: how each reason relates to aligned belief change, BUNK vs DEBUNK, from a
#            ridge-penalized model (glmnet, themes shrunk jointly; baseline + per-MODEL
#            persuader free). 95% CIs and the bunk-vs-debunk GAP test come from a
#            stratified nonparametric bootstrap (B reps, fixed lambda).
# S4 models are kept SEPARATE (persuader has 7 levels incl. Claude/Gemini/GPT-5.2/Grok).
#
# Output: output/provenance_work/explanation_coding/figures/explanation_interpretation_and_change.pdf/.png
#         output/provenance_work/explanation_coding/explanation_reason_change_bootstrap.csv
# Run   : Rscript figure_interpretation_and_change.R   [B_boot]

suppressWarnings(suppressMessages({
  library(dplyr); library(readr); library(tidyr); library(stringr)
  library(forcats); library(ggplot2); library(purrr); library(patchwork)
  library(glmnet); library(sandwich)
}))
args <- commandArgs(trailingOnly = TRUE)
B_BOOT <- if (length(args) >= 1) as.integer(args[1]) else 1000

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
persuader_levels <- c("Jailbroken", "Standard", "Truth-Constrained", "Claude", "Gemini", "GPT-5.2", "Grok")

frame  <- read_csv(file.path(data_dir, "analysis_frame.csv"), show_col_types = FALSE)
consol <- read_csv(file.path(data_dir, "explanation_consolidated.csv"), show_col_types = FALSE)
part_any <- consol %>% filter(consensus_response_quality == "substantive") %>%
  group_by(ResponseId) %>%
  summarise(across(all_of(theme_keys), ~ as.integer(max(.x, na.rm = TRUE))), .groups = "drop")
dat <- frame %>% inner_join(part_any, by = "ResponseId") %>%
  filter(compliant %in% TRUE, direction %in% c("bunk", "debunk"),
         !is.na(aligned_belief_change), !is.na(belief_pre)) %>%
  mutate(persuader = factor(persuader, levels = persuader_levels),
         direction = factor(direction, levels = c("bunk", "debunk")))
cat("compliant analytic n:", nrow(dat), "| bootstrap reps:", B_BOOT, "\n")

# ===== Panel B: ridge + bootstrap =====
X <- model.matrix(~ ., data = dat[, c(theme_keys, "belief_pre", "persuader")])[, -1, drop = FALSE]
y <- dat$aligned_belief_change
pf <- as.integer(colnames(X) %in% theme_keys)
set.seed(1)
lam <- cv.glmnet(X, y, alpha = 0, penalty.factor = pf, standardize = TRUE)$lambda.1se
is_bunk <- dat$direction == "bunk"

fit_coefs <- function(rows) {
  f <- glmnet(X[rows, , drop = FALSE], y[rows], alpha = 0, lambda = lam,
              penalty.factor = pf, standardize = TRUE)
  co <- as.numeric(coef(f)); names(co) <- rownames(coef(f))
  co[theme_keys]
}
pt_bunk <- fit_coefs(which(is_bunk)); pt_deb <- fit_coefs(which(!is_bunk))

bunk_idx <- which(is_bunk); deb_idx <- which(!is_bunk)
boot_b <- matrix(NA_real_, B_BOOT, length(theme_keys), dimnames = list(NULL, theme_keys))
boot_d <- boot_b
set.seed(42)
for (b in seq_len(B_BOOT)) {
  boot_b[b, ] <- fit_coefs(sample(bunk_idx, length(bunk_idx), replace = TRUE))
  boot_d[b, ] <- fit_coefs(sample(deb_idx, length(deb_idx), replace = TRUE))
}
boot_gap <- boot_d - boot_b
ci <- function(m) apply(m, 2, quantile, probs = c(.025, .975), na.rm = TRUE)
cib <- ci(boot_b); cid <- ci(boot_d); cig <- ci(boot_gap)
gap_p <- sapply(theme_keys, function(k) {
  v <- boot_gap[, k]; 2 * min(mean(v > 0), mean(v < 0))
})

bdat <- bind_rows(
  tibble(key = theme_keys, direction = "Bunking", est = pt_bunk, lo = cib[1, ], hi = cib[2, ]),
  tibble(key = theme_keys, direction = "Debunking", est = pt_deb, lo = cid[1, ], hi = cid[2, ])
) %>% left_join(theme_meta, by = "key") %>% mutate(label = labmap[key])
gapdat <- tibble(key = theme_keys, gap_est = pt_deb - pt_bunk,
                 gap_lo = cig[1, ], gap_hi = cig[2, ], gap_p = gap_p,
                 pooled = (pt_bunk + pt_deb) / 2) %>% left_join(theme_meta, by = "key") %>%
  mutate(label = labmap[key])
write_csv(gapdat %>% left_join(bdat %>% select(key, direction, est, lo, hi) %>%
            pivot_wider(names_from = direction, values_from = c(est, lo, hi)), by = "key"),
          file.path(res_dir, "explanation_reason_change_bootstrap.csv"))

lev <- gapdat %>% arrange(pooled) %>% pull(label)     # shared order: low (bottom) -> high (top)
bdat <- bdat %>% mutate(label = factor(label, levels = lev),
                        direction = factor(direction, levels = c("Bunking", "Debunking")))
gap_sig <- gapdat %>% filter(gap_p < .05) %>% mutate(label = factor(label, levels = lev))
cat("\nreasons with significant bunk-vs-debunk gap (p<.05):",
    paste(gap_sig$label, collapse = "; "), "\n")

# ===== Panel A: adjusted debunk - bunk prevalence difference (HC3) =====
prevdiff <- map_dfr(theme_keys, function(k) {
  d <- dat %>% mutate(yy = .data[[k]])
  f <- lm(yy ~ direction + persuader, data = d); v <- sandwich::vcovHC(f, "HC3")
  b <- coef(f)["directiondebunk"]; s <- sqrt(v["directiondebunk", "directiondebunk"]); z <- b / s
  tibble(key = k, est = unname(b) * 100, lo = unname(b - 1.96 * s) * 100,
         hi = unname(b + 1.96 * s) * 100, p = 2 * pnorm(abs(z), lower.tail = FALSE))
}) %>% left_join(theme_meta, by = "key") %>%
  mutate(label = factor(labmap[key], levels = lev),
         side = if_else(est >= 0, "More in debunking", "More in bunking"),
         sig = p < .05)

panelA <- ggplot(prevdiff, aes(x = est, y = label, color = side, alpha = sig)) +
  geom_vline(xintercept = 0, color = "grey55") +
  geom_segment(aes(x = 0, xend = est, yend = label), linewidth = 0.9) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.5) +
  geom_point(size = 2.6) +
  scale_color_manual(values = c("More in debunking" = "#1E88E5", "More in bunking" = "#D81B60"), name = NULL) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.3), guide = "none") +
  labs(x = "Debunking - Bunking difference in % mentioning (adjusted, HC3)", y = NULL,
       title = "A  Bunking vs debunking are interpreted differently",
       subtitle = "Persuader-adjusted prevalence gap (faded = n.s.); + = emphasized more when the AI debunks") +
  theme_si(base_size = 9.5)

# ===== Panel B: reason -> belief change, by direction, bootstrap CIs + gap sig =====
xmax <- max(bdat$hi, na.rm = TRUE)
panelB <- ggplot(bdat, aes(x = est, y = label, color = direction)) +
  geom_vline(xintercept = 0, color = "grey55") +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.5,
                 position = position_dodge(width = 0.6)) +
  geom_point(size = 2.4, position = position_dodge(width = 0.6)) +
  geom_text(data = gap_sig, aes(x = xmax * 1.12, y = label, label = "gap*"),
            inherit.aes = FALSE, size = 2.6, color = "grey25", hjust = 0) +
  scale_color_manual(values = dir_cols, name = NULL) +
  coord_cartesian(xlim = c(min(bdat$lo, na.rm = TRUE), xmax * 1.25), clip = "off") +
  labs(x = "Ridge-penalized association with aligned belief change (points, 0-100)", y = NULL,
       title = "B  ...and succeed for different reasons",
       subtitle = "Ridge (glmnet); 95% bootstrap CIs. 'gap*' = bunk-vs-debunk difference significant (p<.05)") +
  theme_si(base_size = 9.5)

combined <- panelA / panelB + plot_layout(heights = c(1, 1)) +
  plot_annotation(theme = theme(plot.margin = margin(6, 30, 6, 6)))
ggsave(file.path(fig_dir, "explanation_interpretation_and_change.pdf"), combined, width = 10, height = 11)
ggsave(file.path(fig_dir, "explanation_interpretation_and_change.png"), combined, width = 10, height = 11, dpi = 150)
cat("wrote", file.path(fig_dir, "explanation_interpretation_and_change.png"), "\n")
print(gapdat %>% transmute(label, bunk = round(pt_bunk, 2), debunk = round(pt_deb, 2),
                           gap = round(gap_est, 2), gap_p = round(gap_p, 3)) %>% arrange(gap))
