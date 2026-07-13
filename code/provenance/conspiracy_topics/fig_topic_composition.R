# fig_topic_composition.R — house-style heatmap of conspiracy-topic composition
# by study/model (the 7 variants: S1-3 GPT-4o regimes + S4 frontier models).
# Fill = share of that variant's sample on each topic (column-normalised).
# Mixed/Unclassified excluded from the tiles (noted) so the named-topic range
# isn't swamped by the ~47% idiosyncratic remainder.
#
# Output: figures/revamp/fig_topic_composition.{png,pdf}

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
dat <- readr::read_csv(file.path(THIS_DIR, "pooled_with_clusters.csv"), show_col_types = FALSE)

variant_levels <- c("Jailbroken", "Standard", "Truth-Constrained",
                    "Claude", "Gemini", "Grok", "GPT-5.2")
variant_lab <- c("Jailbroken"="Jailbroken","Standard"="Standard",
                 "Truth-Constrained"="Truth-\nconstrained","Claude"="Claude",
                 "Gemini"="Gemini","Grok"="Grok","GPT-5.2"="GPT-5.2")

# within-variant share (%) per topic, named topics only
vtot <- dat %>% count(variant, name = "n_variant")
comp <- dat %>%
  filter(topic != "Mixed / Unclassified") %>%
  count(variant, topic, name = "n") %>%
  right_join(tidyr::expand_grid(variant = variant_levels,
                                topic = unique(dat$topic[dat$topic != "Mixed / Unclassified"])),
             by = c("variant","topic")) %>%
  mutate(n = replace_na(n, 0)) %>%
  left_join(vtot, by = "variant") %>%
  mutate(share = 100 * n / n_variant)

topic_order <- dat %>% filter(topic != "Mixed / Unclassified") %>%
  count(topic) %>% arrange(n) %>% pull(topic)        # ascending -> largest at top
mixed_pct <- dat %>% group_by(variant) %>%
  summarise(p = 100*mean(topic == "Mixed / Unclassified"), .groups="drop")

comp <- comp %>%
  mutate(variant = factor(variant, levels = variant_levels),
         vlab = factor(variant_lab[as.character(variant)], levels = unname(variant_lab[variant_levels])),
         topic = factor(topic, levels = topic_order),
         lab = ifelse(share >= 0.5, sprintf("%.0f", share), ""),
         txt_col = ifelse(share >= 9, "white", "grey25"))

p <- ggplot(comp, aes(x = vlab, y = topic, fill = share)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = lab, color = txt_col), size = 2.0, family = bb_family) +
  # separator + group headers (GPT-4o vs frontier), like the cross-study forest
  annotate("segment", x = 3.5, xend = 3.5, y = 0.4, yend = length(topic_order) + 0.9,
           color = "grey55", linewidth = 0.5) +
  annotate("text", x = 2, y = length(topic_order) + 1.15, label = "GPT-4o · Studies 1–3",
           size = 2.4, fontface = "bold", color = "grey30", family = bb_family) +
  annotate("text", x = 5.5, y = length(topic_order) + 1.15, label = "Frontier models · Study 4",
           size = 2.4, fontface = "bold", color = "grey30", family = bb_family) +
  scale_color_identity() +
  scale_fill_gradient(low = "#FFFFFF", high = bb_blue, limits = c(0, NA),
                      name = "Share of variant's\nparticipants (%)") +
  scale_x_discrete(position = "bottom") +
  coord_cartesian(clip = "off", ylim = c(0.5, length(topic_order) + 0.5)) +
  labs(
    title = "Conspiracy-topic composition by study and model",
    subtitle = "Each column = one study/model; cell = share of that variant's participants whose focal conspiracy fell in the topic.\nAn additional ~47% (range 43–54%) were idiosyncratic and unclustered (not shown).",
    x = NULL, y = NULL
  ) +
  theme_bunkbot(grid = "none") +
  theme(axis.text.x = element_text(size = 6.5, lineheight = 0.9),
        axis.text.y = element_text(size = 7),
        legend.position = "right",
        legend.key.width = unit(7, "pt"),
        legend.key.height = unit(16, "pt"),
        plot.margin = margin(14, 8, 4, 6),
        panel.grid = element_blank())

save_bb(file.path(out_dir, "fig_topic_composition"), p, width = 7.0, height = 5.2)

cat("Mixed/Unclassified share by variant:\n"); print(as.data.frame(mixed_pct), digits = 3)
cat("Done.\n")
