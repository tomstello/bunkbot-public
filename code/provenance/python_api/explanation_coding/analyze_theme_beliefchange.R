# analyze_theme_beliefchange.R
# For EACH of the 15 stated-reason categories: is mentioning it associated with more
# or less belief change? Outcome = aligned_belief_change (+ = moved toward the AI's
# argued position). COMPLIANT cases, baseline-adjusted (belief_pre) + persuader FE, HC3.
# Reports, per theme: pooled association, association within bunk and within debunk
# (+ interaction), and a joint test of whether the association varies by study/model.
#
# CAVEAT: explanations are reported AFTER the conversation, so these are associations
# (partly genuine reasons, partly post-hoc rationalization), not manipulated causes.
#
# Outputs: output/provenance_work/explanation_coding/explanation_theme_beliefchange.csv
#          output/provenance_work/explanation_coding/figures/explanation_theme_beliefchange_pooled.pdf/.png
#          output/provenance_work/explanation_coding/figures/explanation_theme_beliefchange_by_direction.pdf/.png
# Run    : Rscript analyze_theme_beliefchange.R

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

frame  <- read_csv(file.path(data_dir, "analysis_frame.csv"), show_col_types = FALSE)
consol <- read_csv(file.path(data_dir, "explanation_consolidated.csv"), show_col_types = FALSE)
part_any <- consol %>% filter(consensus_response_quality == "substantive") %>%
  group_by(ResponseId) %>%
  summarise(across(all_of(theme_keys), ~ as.integer(max(.x, na.rm = TRUE))), .groups = "drop")

dat <- frame %>% inner_join(part_any, by = "ResponseId") %>%
  filter(compliant %in% TRUE, direction %in% c("bunk", "debunk"),
         !is.na(aligned_belief_change), !is.na(belief_pre)) %>%
  mutate(direction = factor(direction, levels = c("bunk", "debunk")),
         persuader = factor(persuader),
         study_group = factor(if_else(study == "Study 4", "Study 4", study_factor),
                              levels = c("Jailbroken", "Standard", "Truth-Constrained", "Study 4")))
cat("compliant analytic n:", nrow(dat), "| mean aligned change:", round(mean(dat$aligned_belief_change), 2), "\n\n")

# coefficient on theme indicator x (with given controls), HC3; robust to absence
getx <- function(d, controls) {
  if (sd(d$x) == 0) return(c(est = NA, se = NA, lo = NA, hi = NA, p = NA))
  f <- lm(as.formula(paste("aligned_belief_change ~ x +", controls)), data = d)
  v <- sandwich::vcovHC(f, type = "HC3")
  if (is.na(coef(f)["x"]) || !("x" %in% rownames(v))) return(c(est = NA, se = NA, lo = NA, hi = NA, p = NA))
  b <- unname(coef(f)["x"]); s <- unname(sqrt(v["x", "x"])); z <- b / s
  c(est = b, se = s, lo = b - 1.96 * s, hi = b + 1.96 * s, p = 2 * pnorm(abs(z), lower.tail = FALSE))
}
joint_wald <- function(fit, terms, vc) {
  terms <- terms[terms %in% rownames(vc) & !is.na(coef(fit)[terms])]
  if (length(terms) < 1) return(NA)
  b <- coef(fit)[terms]; V <- vc[terms, terms, drop = FALSE]
  W <- tryCatch(as.numeric(t(b) %*% solve(V) %*% b), error = function(e) NA)
  if (is.na(W)) NA else pchisq(W, length(terms), lower.tail = FALSE)
}

per_theme <- function(t) {
  d <- dat %>% mutate(x = .data[[t]])
  if (sd(d$x) == 0) return(NULL)                                   # skip ~0-prevalence (priv_pos)
  p0 <- getx(d, "belief_pre + direction + persuader")              # pooled
  eb <- getx(filter(d, direction == "bunk"),   "belief_pre + persuader")
  ed <- getx(filter(d, direction == "debunk"), "belief_pre + persuader")
  # interaction = debunk - bunk (independent subsets)
  if (!is.na(eb["est"]) && !is.na(ed["est"])) {
    idiff <- ed["est"] - eb["est"]; ise <- sqrt(eb["se"]^2 + ed["se"]^2)
    ip <- 2 * pnorm(abs(idiff / ise), lower.tail = FALSE)
  } else { idiff <- NA; ip <- NA }
  # heterogeneity across study group
  f2 <- lm(aligned_belief_change ~ x * study_group + belief_pre + direction, data = d)
  v2 <- sandwich::vcovHC(f2, type = "HC3")
  het <- joint_wald(f2, grep("x:study_group", names(coef(f2)), value = TRUE), v2)
  tibble(key = t, prevalence = mean(d$x),
         pooled_est = p0["est"], pooled_lo = p0["lo"], pooled_hi = p0["hi"], pooled_p = p0["p"],
         bunk_est = eb["est"], bunk_lo = eb["lo"], bunk_hi = eb["hi"], bunk_p = eb["p"],
         debunk_est = ed["est"], debunk_lo = ed["lo"], debunk_hi = ed["hi"], debunk_p = ed["p"],
         interaction_est = unname(idiff), interaction_p = unname(ip), het_study_p = het)
}

res <- map_dfr(theme_keys, per_theme) %>% left_join(theme_meta, by = "key") %>%
  filter(prevalence >= 0.01) %>% arrange(desc(pooled_est))   # drop ~0-prevalence (priv_pos)
write_csv(res, file.path(res_dir, "explanation_theme_beliefchange.csv"))

cat("=== Adjusted association of EACH stated reason with aligned belief change (pp, 0-100) ===\n")
cat(sprintf("%-36s %8s %8s %8s %7s %7s\n", "reason", "pooled", "bunk", "debunk", "inter_p", "het_p"))
for (i in seq_len(nrow(res))) with(res[i, ],
  cat(sprintf("%-36s %+7.1f%s %+7.1f%s %+7.1f%s  %.2g  %.2g\n",
              label,
              pooled_est, ifelse(pooled_p < .05, "*", " "),
              bunk_est,   ifelse(bunk_p   < .05, "*", " "),
              debunk_est, ifelse(debunk_p < .05, "*", " "),
              interaction_p, het_study_p)))

# ---- Figure A: pooled association forest ----
pa <- res %>% mutate(label = fct_reorder(label, pooled_est),
                     sign = if_else(pooled_est >= 0, "Associated with MORE change", "Associated with LESS change"))
gA <- ggplot(pa, aes(x = pooled_est, y = label, color = sign)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbar(aes(xmin = pooled_lo, xmax = pooled_hi), width = 0, linewidth = 0.5) +
  geom_point(size = 2.6) +
  scale_color_manual(values = c("Associated with MORE change" = "#1B7837",
                                "Associated with LESS change" = "#B2182B"), name = NULL) +
  labs(x = "Adjusted association with aligned belief change (points on 0-100 scale)", y = NULL,
       title = "Which stated reasons go with more vs less belief change?",
       subtitle = "Compliant cases; baseline + persuader adjusted (HC3). Association, not cause (reasons are post-hoc).") +
  theme_si(base_size = 10)
ggsave(file.path(fig_dir, "explanation_theme_beliefchange_pooled.pdf"), gA, width = 9, height = 6.5)
ggsave(file.path(fig_dir, "explanation_theme_beliefchange_pooled.png"), gA, width = 9, height = 6.5, dpi = 150)

# ---- Figure B: by direction ----
pb <- res %>%
  select(label, pooled_est, bunk_est, bunk_lo, bunk_hi, debunk_est, debunk_lo, debunk_hi) %>%
  pivot_longer(-c(label, pooled_est),
               names_to = c("direction", ".value"), names_sep = "_") %>%
  mutate(direction = recode(direction, bunk = "Bunking", debunk = "Debunking"),
         label = fct_reorder(label, pooled_est))
gB <- ggplot(pb, aes(x = est, y = label, color = direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 0.45,
                position = position_dodge(width = 0.6)) +
  geom_point(size = 2.3, position = position_dodge(width = 0.6)) +
  scale_color_manual(values = dir_cols, name = NULL) +
  labs(x = "Adjusted association with aligned belief change (points)", y = NULL,
       title = "Reason vs belief change, by condition (bunking vs debunking)",
       subtitle = "Compliant cases; baseline + persuader adjusted (HC3)") +
  theme_si(base_size = 10)
ggsave(file.path(fig_dir, "explanation_theme_beliefchange_by_direction.pdf"), gB, width = 9, height = 6.5)
ggsave(file.path(fig_dir, "explanation_theme_beliefchange_by_direction.png"), gB, width = 9, height = 6.5, dpi = 150)

cat("\nfigures ->", fig_dir, "\ndone.\n")
