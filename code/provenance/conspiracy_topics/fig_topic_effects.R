# fig_topic_effects.R — house-style forest of belief change by conspiracy topic,
# bunking vs debunking, pooled across all four studies (model-level slicing is
# unestimable: median S4 topic x model x condition cell n = 3, topic x model
# interaction p = .59). Topics with >= 40 participants. Ordered by the
# bunking - debunking gap, so the "truthful ammunition" gradient reads top to
# bottom: Area 51 (bunking wins) -> ... -> Moon Landing (near-unbunkable).
#
# Output: figures/revamp/fig_topic_effects.{png,pdf}

suppressPackageStartupMessages({ library(tidyverse) })

THIS_DIR <- (function(){
  a <- commandArgs(trailingOnly = FALSE); f <- grep("^--file=", a, value = TRUE)
  if (length(f)) return(dirname(normalizePath(sub("^--file=","",f[1])))); getwd()
})()
# Repo-root resolver: walk up to the dir containing both data/ and code/.
.repo_root <- function(start) {
  p <- normalizePath(start)
  repeat {
    if (dir.exists(file.path(p,"data")) && dir.exists(file.path(p,"code"))) return(p)
    pp <- dirname(p); if (pp == p) stop("repo root not found"); p <- pp
  }
}
REPO_ROOT <- .repo_root(THIS_DIR)
# NOT SHIPPED: the figure house-style helper (theme_bunkbot.R) and the figures/revamp
# output tree are part of the author's old figure_revamp tree, which is not in
# bunkbot-public. Keep the reference; supply theme_bunkbot.R to render this figure.
# TODO(provenance): repoint to the shipped figure theme/out-dir once exposed.
source(file.path(REPO_ROOT, "code/figure_revamp/theme_bunkbot.R"))  # NOT SHIPPED — see TODO
out_dir <- file.path(REPO_ROOT, "figures", "revamp")                # NOT SHIPPED out-dir — see TODO

# pooled_with_clusters.csv is a THIS_DIR-relative working file (from pooled_conspiracy_topics.R).
d <- readr::read_csv(file.path(THIS_DIR, "pooled_with_clusters.csv"), show_col_types = FALSE) %>%
  filter(topic != "Mixed / Unclassified", is.finite(belief_change)) %>%
  mutate(condition = factor(condition, levels = c("Bunking", "Debunking")))

MIN_TOPIC_N <- 40
eff <- d %>% group_by(topic, condition) %>%
  summarise(n = n(), m = mean(belief_change), se = sd(belief_change)/sqrt(n()),
            lo = m - qt(.975, n-1)*se, hi = m + qt(.975, n-1)*se, .groups = "drop")
keep <- d %>% count(topic) %>% filter(n >= MIN_TOPIC_N) %>% pull(topic)
eff <- eff %>% filter(topic %in% keep)

# order by bunking - debunking gap (descending: bunk-favoured at top)
gap <- eff %>% select(topic, condition, m) %>%
  pivot_wider(names_from = condition, values_from = m) %>%
  mutate(gap = Bunking - Debunking) %>% arrange(gap)   # ascending -> most bunk-favoured at top of plot
topic_order <- gap$topic
n_lab <- eff %>% select(topic, condition, n) %>%
  pivot_wider(names_from = condition, values_from = n) %>%
  transmute(topic, nlab = sprintf("%d / %d", Bunking, Debunking))

pd <- eff %>%
  mutate(row = as.integer(factor(topic, levels = topic_order)),
         y = row + ifelse(condition == "Bunking", 0.17, -0.17))
n_lab <- n_lab %>% mutate(row = as.integer(factor(topic, levels = topic_order)))

cat("=== topic order (bunk - debunk gap) ===\n")
print(as.data.frame(gap %>% transmute(topic, bunk = round(Bunking,1),
        debunk = round(Debunking,1), gap = round(gap,1))), row.names = FALSE)

x_lim <- c(-9, 47); star_x <- 44

p <- ggplot(pd, aes(x = m, y = y, color = condition)) +
  geom_zero_vline() +
  geom_linerange(aes(xmin = lo, xmax = hi), linewidth = bb_whisker_lw) +
  geom_point(size = bb_pt_size) +
  annotate("text", x = star_x, y = length(topic_order) + 0.75, label = "n  bunk / debunk",
           size = 1.95, color = bb_anno, family = bb_family, hjust = 0.5) +
  geom_text(data = n_lab, inherit.aes = FALSE, aes(x = star_x, y = row, label = nlab),
            size = 1.95, color = "grey55", family = bb_family, hjust = 0.5) +
  scale_color_manual(values = colors_bunkdebunk, breaks = c("Bunking", "Debunking"),
                     labels = c(lab_bunk_s13, lab_debunk_s13),
                     guide = guide_legend(override.aes = list(linewidth = bb_whisker_lw, size = bb_pt_size))) +
  scale_y_continuous(breaks = seq_along(topic_order), labels = topic_order,
                     limits = c(0.5, length(topic_order) + 1.1), expand = c(0, 0)) +
  scale_x_continuous(limits = x_lim, breaks = seq(0, 40, 10), labels = fmt_minus) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Belief change by conspiracy topic",
    subtitle = "Mean change toward the AI’s argued position, pooled across all four studies, 95% CIs. Topics with ≥ 40 participants,\nordered by the bunking-minus-debunking gap; positive = belief moved toward what the AI argued.",
    x = "Belief change toward the AI’s position (points on the 0–100 scale)",
    y = NULL
  ) +
  theme_bunkbot(grid = "x") +
  theme(legend.position = "bottom", legend.margin = margin(t = 4),
        axis.text.y = element_text(hjust = 1, size = 7),
        plot.margin = margin(6, 20, 4, 6))

save_bb(file.path(out_dir, "fig_topic_effects"), p, width = 7.0, height = 5.0)
cat("Done.\n")
