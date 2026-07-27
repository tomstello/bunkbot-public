# =============================================================================
# make_figures.R
# -----------------------------------------------------------------------------
# Generates the three main-text data figures (Figs 2-4) in their polished,
# print-ready form. Writes PNG + PDF into figures/manuscript/.
# All data, models, estimands, and sample definitions come from the same
# analysis engine (code/R/build_all_numbers.R) used by the manuscript and SI;
# this script controls only the visual styling/layout.
#
# Run from repo root (this is the `make figures` target):
#   Rscript figures/manuscript/make_figures.R
# =============================================================================

suppressWarnings(suppressMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forcats)
  library(patchwork)
  library(scales)
  library(readr)
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(sandwich)
  library(purrr)
}))

find_repo_root <- function(start) {
  d <- normalizePath(start, mustWork = FALSE)
  for (i in 1:8) {
    if (file.exists(file.path(d, "code", "R", "build_all_numbers.R"))) return(d)
    d <- dirname(d)
  }
  stop("Could not find the repo root.")
}

.args <- commandArgs(FALSE)
.file_arg <- sub("^--file=", "", grep("^--file=", .args, value = TRUE))
.script_dir <- if (length(.file_arg)) dirname(normalizePath(.file_arg[1])) else getwd()
REPO_ROOT <- tryCatch(find_repo_root(getwd()),
                      error = function(e) find_repo_root(.script_dir))

source(file.path(REPO_ROOT, "code", "R", "figures_v2.R"))
source(file.path(REPO_ROOT, "code", "bunkbot_helpers.R"))

source(file.path(REPO_ROOT, "code", "R", "build_all_numbers.R"))
.b <- load_or_build_all_numbers(REPO_ROOT)  # cached if readable, else live rebuild

an <- .b$all_numbers
all_numbers <- .b$all_numbers
core_objects <- .b$core
paths <- pkg_paths(REPO_ROOT)

outdir <- file.path(REPO_ROOT, "figures", "manuscript")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# ---- visual system ----------------------------------------------------------
colors_main <- c(Bunking = "#C7375A", Debunking = "#0868AC")
fills_main <- c(Bunking = "#EFB4C1", Debunking = "#A9CEE8")
ci_main <- c(Bunking = "#D9718B", Debunking = "#4D96C7")
low_veracity <- "#8F2B2D"
high_veracity <- "#E7E5E1"
ink <- "#222222"
soft_grid <- "#DDDDDD"
zero_line <- "#777777"
font_family <- "Helvetica Neue"
MODELS <- c("Claude", "Gemini", "Grok", "GPT-5.2")

as_cond <- function(x) {
  factor(ifelse(x %in% c("bunk", "Bunking"), "Bunking", "Debunking"),
         levels = c("Bunking", "Debunking"))
}

minus <- scales::label_number(style_negative = "minus", accuracy = 1)
minus_dz <- scales::label_number(style_negative = "minus", accuracy = 0.1)
signed_num <- function(x, accuracy = 0.1) {
  out <- scales::number(x, accuracy = accuracy, style_negative = "minus")
  ifelse(is.na(x), "", ifelse(x > 0, paste0("+", out), out))
}
plain_num <- function(x, accuracy = 0.01) {
  ifelse(is.na(x), "", scales::number(x, accuracy = accuracy))
}
sig_label <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < .001, "***",
                ifelse(p < .01, "**", ifelse(p < .05, "*", "ns"))))
}

theme_clean <- function(base = 9.8, grid = "both") {
  th <- theme_minimal(base_size = base, base_family = font_family) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.line = element_line(colour = "#A9A9A9", linewidth = 0.35),
      axis.ticks = element_line(colour = "#888888", linewidth = 0.35),
      axis.ticks.length = unit(2.4, "pt"),
      axis.title.x = element_text(face = "bold", colour = ink, margin = margin(t = 5)),
      axis.title.y = element_text(face = "bold", colour = ink, margin = margin(r = 5)),
      axis.text = element_text(colour = "#3A3A3A"),
      axis.text.y = element_text(face = "bold", colour = ink),
      strip.text = element_text(face = "bold", colour = ink, size = base),
      strip.background = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", colour = ink),
      legend.text = element_text(colour = ink),
      legend.key.width = unit(12, "pt"),
      legend.key.height = unit(8, "pt"),
      legend.spacing.x = unit(6, "pt"),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      plot.tag = element_text(face = "bold", colour = ink, size = base + 4),
      plot.tag.position = c(0, 1),
      plot.margin = margin(4, 6, 4, 4)
    )
  th
}

tag_theme <- theme(
  plot.tag = element_text(face = "bold", colour = ink, family = font_family, size = 15),
  plot.margin = margin(4, 4, 4, 4)
)

save_fig <- function(p, name, w, h) {
  png_path <- file.path(outdir, paste0(name, ".png"))
  pdf_path <- file.path(outdir, paste0(name, ".pdf"))
  ggsave(png_path, p, width = w, height = h, dpi = 600, bg = "white")
  tryCatch(
    ggsave(pdf_path, p, width = w, height = h, device = cairo_pdf, bg = "white"),
    error = function(e) ggsave(pdf_path, p, width = w, height = h, bg = "white")
  )
  message("  wrote ", basename(png_path), " and ", basename(pdf_path))
}

lin_cell3 <- function(mod, dat, cond, vc) {
  nd <- dat
  nd$condition_factor <- factor(cond, levels = levels(dat$condition_factor))
  linear_combo(mod, colMeans(model_matrix_for_fit(mod, nd)), vc = vc)
}

lin_con3 <- function(mod, dat, a, b, vc) {
  na <- dat
  nb <- dat
  na$condition_factor <- factor(a, levels = levels(dat$condition_factor))
  nb$condition_factor <- factor(b, levels = levels(dat$condition_factor))
  linear_combo(mod, colMeans(model_matrix_for_fit(mod, na)) -
                 colMeans(model_matrix_for_fit(mod, nb)), vc = vc)
}

fit_dir_p <- function(d, col) {
  d <- d[is.finite(d[[col]]) & !is.na(d$direction), ]
  cf <- summary(stats::lm(stats::reformulate("direction", col), data = d))$coefficients
  r <- grep("direction", rownames(cf))
  if (!length(r)) return(NA_real_)
  cf[r[1], "Pr(>|t|)"]
}

message("\n================ Polished manuscript figures ================\n")
BASE <- 9.8

# =============================================================================
# Figure 2: Study 1 composite
# =============================================================================
message("Figure 2 ...")
s1 <- core_objects$s13 %>% filter(study_factor == "Jailbroken")

d_ts <- s1 %>%
  select(response_id, condition_factor,
         belief_rating_pre_rc, belief_rating_post_rc, belief_rating_debrf_rc) %>%
  pivot_longer(starts_with("belief_rating_"), names_to = "time", values_to = "belief") %>%
  # dplyr::recode qualified explicitly: build_all_numbers() attaches car (numbers_s4.R),
  # whose car::recode masks dplyr's when the engine is rebuilt from scratch (no cache).
  mutate(time = dplyr::recode(time,
                       belief_rating_pre_rc = "Before",
                       belief_rating_post_rc = "After conversation",
                       belief_rating_debrf_rc = "After debriefing"),
         time = factor(time, c("Before", "After conversation", "After debriefing")))
m_ts <- lmer(belief ~ time * condition_factor + (1 | response_id), data = d_ts)
emm_df <- as_tibble(emmeans(m_ts, ~ time * condition_factor))
base_b <- emm_df %>% filter(time == "Before") %>% select(condition_factor, baseline = emmean)
emm_norm <- emm_df %>%
  left_join(base_b, by = "condition_factor") %>%
  mutate(norm_mean = emmean - baseline,
         norm_low = lower.CL - baseline,
         norm_up = upper.CL - baseline) %>%
  filter(is.finite(norm_mean), is.finite(norm_low))
p2a_lab <- emm_norm %>%
  filter(time != "Before") %>%
  mutate(label = signed_num(norm_mean),
         label_y = ifelse(norm_mean >= 0, norm_up, norm_low),
         label_vjust = ifelse(norm_mean >= 0, -0.45, 1.35))

p2a <- ggplot(emm_norm, aes(time, norm_mean, group = condition_factor,
                            colour = condition_factor, fill = condition_factor)) +
  geom_hline(yintercept = 0, linetype = "22", colour = zero_line, linewidth = 0.6) +
  geom_line(linewidth = 1.15, position = position_dodge(width = 0.18), alpha = 0.78) +
  geom_errorbar(aes(ymin = norm_low, ymax = norm_up), width = 0,
                linewidth = 0.62, position = position_dodge(width = 0.18)) +
  geom_point(size = 2.7, shape = 21, stroke = 0.66,
             position = position_dodge(width = 0.18)) +
  geom_text(data = p2a_lab,
            aes(y = label_y, label = label, vjust = label_vjust),
            position = position_dodge(width = 0.18),
            size = 2.45, family = font_family, fontface = "bold",
            show.legend = FALSE) +
  scale_colour_manual(values = colors_main, guide = "none") +
  scale_fill_manual(values = fills_main, guide = "none") +
  scale_y_continuous(breaks = seq(-15, 15, 5), limits = c(-16, 16), labels = minus) +
  scale_x_discrete(labels = c("Before", "After\nconversation", "After\ndebriefing"),
                   expand = expansion(add = 0.32)) +
  labs(x = NULL, y = "Belief change\nfrom baseline") +
  theme_clean(BASE, "both")

chg <- s1 %>%
  transmute(Condition = condition_factor, change) %>%
  filter(is.finite(change))
th <- 0:60
surv <- bind_rows(lapply(c("Bunking", "Debunking"), function(cc) {
  v <- chg$change[chg$Condition == cc]
  tibble(Condition = cc, t = th, frac = vapply(th, function(x) mean(v >= x), numeric(1)))
})) %>%
  mutate(Condition = factor(Condition, names(colors_main)))
band <- surv %>%
  filter(t < 60) %>%
  pivot_wider(names_from = Condition, values_from = frac) %>%
  mutate(ymin = pmin(Bunking, Debunking),
         ymax = pmax(Bunking, Debunking),
         leader = ifelse(Bunking >= Debunking, "Bunking", "Debunking"))

p2b <- ggplot(surv, aes(t, frac, colour = Condition)) +
  geom_rect(data = band, aes(xmin = t, xmax = t + 1, ymin = ymin, ymax = ymax, fill = leader),
            inherit.aes = FALSE, alpha = 0.18, colour = NA) +
  geom_step(linewidth = 0.72, direction = "hv") +
  scale_colour_manual(values = colors_main, guide = "none") +
  scale_fill_manual(values = colors_main, guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 0.86), breaks = seq(0, 0.8, 0.2),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_x_continuous(breaks = seq(0, 60, 20),
                     expand = expansion(mult = c(0.01, 0.02))) +
  labs(x = "Minimum belief change (points)", y = "Share of participants") +
  theme_clean(BASE, "both")

ni_map <- setNames(1:10, c(1, 4, 5, 6, 7, 8, 9, 10, 11, 12))
pc <- s1 %>% transmute(
  Condition = condition_factor,
  `Argument strength` = as.numeric(arg_strength),
  `New information` = ni_map[as.character(new_info)],
  `Collaborative tone` = as.numeric(collaborative) - 71,
  `Unbiased / impartial` = as.numeric(unbiased) - 71
)
items <- c("Argument strength", "New information", "Collaborative tone", "Unbiased / impartial")
perc <- bind_rows(lapply(items, function(it) {
  v <- pc[[it]]
  ok <- is.finite(v) & !is.na(pc$Condition)
  rg <- range(v[ok])
  sc <- (v - rg[1]) / (rg[2] - rg[1])
  tibble(item = it, Condition = pc$Condition[ok], scaled = sc[ok]) %>%
    group_by(item, Condition) %>%
    summarise(n = n(), m = mean(scaled), se = stats::sd(scaled) / sqrt(n()), .groups = "drop") %>%
    mutate(lo = pmax(0, m - stats::qt(.975, n - 1) * se),
           hi = pmin(1, m + stats::qt(.975, n - 1) * se))
})) %>%
  mutate(item = factor(item, rev(items)),
         Condition = factor(Condition, names(colors_main)))
perc_i <- perc %>%
  mutate(yi = as.numeric(item) + ifelse(Condition == "Bunking", 0.16, -0.16),
         label = plain_num(m),
         label_x = pmin(0.985, hi + 0.035))

p2c <- ggplot(perc_i, aes(colour = Condition)) +
  geom_point(aes(m, yi), size = 1.55, shape = 21, fill = NA, stroke = 0.62) +
  geom_segment(aes(x = lo, xend = hi, y = yi, yend = yi),
               linewidth = 0.98, show.legend = FALSE) +
  geom_segment(aes(x = lo, xend = lo, y = yi - 0.05, yend = yi + 0.05),
               linewidth = 0.68, show.legend = FALSE) +
  geom_segment(aes(x = hi, xend = hi, y = yi - 0.05, yend = yi + 0.05),
               linewidth = 0.68, show.legend = FALSE) +
  scale_colour_manual(values = colors_main, name = "AI instruction") +
  scale_y_continuous(breaks = seq_along(levels(perc$item)), labels = levels(perc$item),
                     expand = expansion(add = 0.5)) +
  scale_x_continuous(breaks = seq(0, 1, 0.25),
                     labels = label_number(accuracy = 0.01)) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(x = "Mean rating (rescaled 0-1)", y = NULL) +
  guides(colour = guide_legend(override.aes = list(size = 3.2, linewidth = 1.1))) +
  theme_clean(BASE, "both")

rawc <- pc %>%
  mutate(.id = row_number()) %>%
  pivot_longer(all_of(items), names_to = "item", values_to = "raw") %>%
  filter(is.finite(raw), !is.na(Condition)) %>%
  group_by(item) %>%
  mutate(raw_span = max(raw, na.rm = TRUE) - min(raw, na.rm = TRUE),
         scaled = (raw - min(raw, na.rm = TRUE)) / raw_span) %>%
  ungroup() %>%
  mutate(item = factor(item, levels = levels(perc$item)),
         Condition = factor(Condition, names(colors_main)),
         yi = as.numeric(item) + ifelse(Condition == "Bunking", 0.16, -0.16))
raw_bins <- rawc %>%
  count(item, Condition, yi, scaled, name = "n_level") %>%
  group_by(item, Condition) %>%
  mutate(prop = n_level / sum(n_level)) %>%
  ungroup()

p2c_likert <- ggplot() +
  geom_point(data = raw_bins,
             aes(scaled, yi, colour = Condition, size = prop),
             alpha = 0.48, stroke = 0, show.legend = FALSE) +
  geom_segment(data = perc_i,
               aes(x = lo, xend = hi, y = yi, yend = yi),
               colour = ink, linewidth = 0.62, show.legend = FALSE) +
  geom_segment(data = perc_i,
               aes(x = lo, xend = lo, y = yi - 0.045, yend = yi + 0.045),
               colour = ink, linewidth = 0.56, show.legend = FALSE) +
  geom_segment(data = perc_i,
               aes(x = hi, xend = hi, y = yi - 0.045, yend = yi + 0.045),
               colour = ink, linewidth = 0.56, show.legend = FALSE) +
  scale_colour_manual(values = colors_main, name = "AI instruction") +
  scale_size_area(max_size = 3.1, guide = "none") +
  scale_y_continuous(breaks = seq_along(levels(perc$item)), labels = levels(perc$item),
                     expand = expansion(mult = 0)) +
  scale_x_continuous(breaks = seq(0, 1, 0.25),
                     labels = label_number(accuracy = 0.01)) +
  coord_cartesian(xlim = c(0, 1),
                  ylim = c(0.76, length(levels(perc$item)) + 0.24)) +
  labs(x = "Rating (rescaled 0-1)", y = NULL) +
  guides(colour = guide_legend(override.aes = list(size = 3.2, linewidth = 1.1))) +
  theme_clean(BASE, "both")

gpre <- paste0("x", c(2, 4, 6, 8, 10, 12, 14), "_gcbs_pre")
gpost <- paste0("x", c(2, 4, 6, 8, 10, 12, 14), "_gcbs_post")
s1d <- s1 %>%
  mutate(gcbs_pre = rowMeans(across(all_of(gpre)), na.rm = TRUE),
         gcbs_post = rowMeans(across(all_of(gpost)), na.rm = TRUE))
dz_one <- function(pre, post, lab) {
  tibble(pre = as.numeric(pre), post = as.numeric(post), Condition = s1d$condition_factor) %>%
    filter(is.finite(pre), is.finite(post)) %>%
    mutate(diff = post - pre) %>%
    group_by(Condition) %>%
    summarise(n = n(), dz = mean(diff) / stats::sd(diff), .groups = "drop") %>%
    mutate(measure = lab,
           lo = dz - stats::qt(.975, n - 1) / sqrt(n),
           hi = dz + stats::qt(.975, n - 1) / sqrt(n))
}
dz <- bind_rows(dz_one(s1d$genai_trust, s1d$trust2, "Trust in AI"),
                dz_one(s1d$gcbs_pre, s1d$gcbs_post, "General conspiracy beliefs")) %>%
  mutate(measure = factor(measure, c("Trust in AI", "General conspiracy beliefs")),
         Condition = factor(Condition, names(colors_main)))
dz_span <- diff(range(c(dz$lo, dz$hi), na.rm = TRUE))
if (!is.finite(dz_span) || dz_span == 0) dz_span <- 1
dz_i <- dz %>%
  mutate(label = signed_num(dz),
         y = ifelse(Condition == "Bunking", 2, 1),
         label_y = y + 0.22)
dz_xlim <- range(c(dz_i$lo, dz_i$hi), na.rm = TRUE) +
  c(-dz_span * 0.06, dz_span * 0.06)

p2d <- ggplot(dz_i, aes(dz, y, colour = Condition, fill = Condition)) +
  geom_vline(xintercept = 0, colour = zero_line, linewidth = 0.55) +
  geom_linerange(aes(xmin = lo, xmax = hi), orientation = "y", linewidth = 0.62) +
  geom_point(size = 2.7, shape = 21, stroke = 0.64) +
  geom_text(aes(x = dz, y = label_y, label = label),
            size = 2.25, family = font_family, fontface = "bold",
            show.legend = FALSE) +
  facet_wrap(~ measure, ncol = 1, strip.position = "top") +
  scale_colour_manual(values = colors_main, guide = "none") +
  scale_fill_manual(values = fills_main, guide = "none") +
  scale_x_continuous(limits = dz_xlim, labels = minus_dz,
                     breaks = function(lims) pretty(lims, n = 4),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(breaks = c(1, 2), labels = c("Debunking", "Bunking"),
                     expand = expansion(add = 0.42)) +
  labs(x = "Standardized change (dz)", y = NULL) +
  theme_clean(BASE, "both") +
  theme(strip.placement = "outside",
        panel.spacing.y = unit(0.5, "lines"))

p2c_bar <- ggplot(perc_i, aes(fill = Condition)) +
  geom_rect(aes(xmin = 0, xmax = m, ymin = yi - 0.13, ymax = yi + 0.13),
            alpha = 0.78, colour = NA, show.legend = TRUE) +
  geom_segment(data = filter(perc_i, Condition == "Bunking"),
               aes(x = lo, xend = hi, y = yi, yend = yi),
               colour = ci_main["Bunking"], linewidth = 0.8, show.legend = FALSE) +
  geom_segment(data = filter(perc_i, Condition == "Debunking"),
               aes(x = lo, xend = hi, y = yi, yend = yi),
               colour = ci_main["Debunking"], linewidth = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = colors_main, name = "AI instruction") +
  scale_y_continuous(breaks = seq_along(levels(perc$item)), labels = levels(perc$item),
                     expand = expansion(mult = 0)) +
  scale_x_continuous(breaks = seq(0, 1, 0.25),
                     labels = label_number(accuracy = 0.01)) +
  coord_cartesian(xlim = c(0, 1),
                  ylim = c(0.62, length(levels(perc$item)) + 0.38)) +
  labs(x = "Mean rating (rescaled 0-1)", y = NULL) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.78))) +
  theme_clean(BASE, "both")

dz_bar <- dz %>%
  mutate(y = ifelse(Condition == "Bunking", 2, 1),
         xmin = pmin(0, dz),
         xmax = pmax(0, dz))
p2d_bar <- ggplot(dz_bar, aes(fill = Condition)) +
  geom_vline(xintercept = 0, colour = zero_line, linewidth = 0.55) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = y - 0.18, ymax = y + 0.18),
            alpha = 0.78, colour = NA, show.legend = FALSE) +
  geom_segment(aes(x = lo, xend = hi, y = y, yend = y),
               colour = ink, linewidth = 0.58, show.legend = FALSE) +
  geom_segment(aes(x = lo, xend = lo, y = y - 0.055, yend = y + 0.055),
               colour = ink, linewidth = 0.52, show.legend = FALSE) +
  geom_segment(aes(x = hi, xend = hi, y = y - 0.055, yend = y + 0.055),
               colour = ink, linewidth = 0.52, show.legend = FALSE) +
  facet_wrap(~ measure, ncol = 1, strip.position = "top") +
  scale_fill_manual(values = colors_main, guide = "none") +
  scale_y_continuous(breaks = c(1, 2), labels = c("Debunking", "Bunking"),
                     expand = expansion(mult = 0)) +
  scale_x_continuous(labels = minus_dz, expand = expansion(mult = 0.07)) +
  coord_cartesian(ylim = c(0.52, 2.48)) +
  labs(x = "Standardized change (dz)", y = NULL) +
  theme_clean(BASE, "both") +
  theme(strip.placement = "outside",
        panel.spacing.y = unit(0.5, "lines"))

# Study 1 / Figure 2 — the canonical manuscript version is the "barcd" encoding:
# the original-style bar chart with flat CIs in panel c plus a coefficient plot
# in panel d. (The plain and Likert panel-c encodings were alternate explorations
# and are not part of the manuscript bundle.)
fig2 <- free(p2a) + free(p2b) + free(p2c_bar, type = "space", side = "t") + p2d +
  plot_layout(ncol = 2, widths = c(1.08, 0.92), heights = c(1.18, 1),
              guides = "collect") +
  plot_annotation(tag_levels = "a", theme = tag_theme) &
  theme(legend.position = "bottom")
save_fig(fig2, "fig_study1_merged", 7.2, 6.6)

# =============================================================================
# Figure 3: effects + claim accuracy
# =============================================================================
message("Figure 3 ...")
s13 <- core_objects$s13
labels3 <- read_claim_labels(paths)
conv3 <- conv_aligned_veracity(labels3)
map_var <- c(Study1 = "Jailbroken", Study2 = "Standard", Study3 = "Truth-Constrained")
VARS <- c("Jailbroken", "Standard", "Truth-Constrained", "Truth-Constrained\n(Compliant)")

variants <- list(
  "Jailbroken" = s13 %>% filter(study_factor == "Jailbroken"),
  "Standard" = s13 %>% filter(study_factor == "Standard"),
  "Truth-Constrained" = s13 %>% filter(study_factor == "Truth-Constrained"),
  "Truth-Constrained\n(Compliant)" = s13 %>% filter(study_factor == "Truth-Constrained", compliant)
)

ate <- purrr::imap_dfr(variants, function(dat, v) {
  mod <- lm(change ~ condition_factor + belief_rating_pre_rc, data = dat)
  vc <- sandwich::vcovHC(mod, "HC3")
  bind_rows(
    lin_cell3(mod, dat, "Bunking", vc) %>% mutate(Condition = "Bunking"),
    lin_cell3(mod, dat, "Debunking", vc) %>% mutate(Condition = "Debunking")
  ) %>%
    transmute(Variant = v, Condition, predicted = estimate, conf.low, conf.high)
}) %>%
  mutate(Variant = factor(Variant, levels = VARS),
         Condition = factor(Condition, names(colors_main)))
ate_lab <- ate %>%
  mutate(label = signed_num(predicted),
         label_y = conf.low - 0.48)

pv <- purrr::imap_dfr(variants, function(dat, v) {
  mod <- lm(change ~ condition_factor + belief_rating_pre_rc, data = dat)
  vc <- sandwich::vcovHC(mod, "HC3")
  tibble(Variant = v, p = lin_con3(mod, dat, "Bunking", "Debunking", vc)$p.value)
}) %>%
  mutate(Variant = factor(Variant, levels = VARS),
         p_label = sig_label(p))
sig3 <- ate %>%
  group_by(Variant) %>%
  summarise(y = max(conf.high, na.rm = TRUE) + 0.7, .groups = "drop") %>%
  left_join(pv, by = "Variant") %>%
  mutate(x = as.numeric(Variant))

p3a <- ggplot(ate, aes(Variant, predicted, colour = Condition, fill = Condition)) +
  geom_hline(yintercept = 0, linetype = "22", colour = zero_line, linewidth = 0.55) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                 position = position_dodge(width = 0.56), linewidth = 0.72,
                 show.legend = FALSE) +
  geom_point(position = position_dodge(width = 0.56),
             size = 2.75, shape = 21, stroke = 0.65) +
  geom_text(data = ate_lab,
            aes(y = label_y, label = label),
            position = position_dodge(width = 0.56),
            size = 2.35, family = font_family, fontface = "bold",
            show.legend = FALSE) +
  geom_segment(data = sig3,
               aes(x = x - 0.23, xend = x + 0.23, y = y, yend = y),
               inherit.aes = FALSE, linewidth = 0.5, colour = ink) +
  geom_text(data = sig3,
            aes(x = x, y = y + 0.42, label = p_label,
                fontface = ifelse(p_label == "ns", "italic", "bold")),
            inherit.aes = FALSE, size = 3.5, family = font_family, colour = ink) +
  scale_colour_manual(values = colors_main, name = "AI instruction") +
  scale_fill_manual(values = fills_main, guide = "none") +
  scale_y_continuous(labels = minus, breaks = seq(0, 16, 4),
                     limits = c(0, 17), expand = expansion(mult = c(0, 0.02))) +
  scale_x_discrete(labels = c("Jailbroken", "Standard", "Truth-\nconstrained",
                              "Truth-\nconstrained\n(compliant)")) +
  labs(x = NULL, y = "Belief change toward\nAI side (points)") +
  guides(colour = guide_legend(order = 1,
                               override.aes = list(size = 3.1, linewidth = 1.1))) +
  theme_clean(BASE, "both") +
  theme(axis.text.x = element_text(size = BASE - 0.9, face = "bold", colour = ink,
                                   lineheight = 0.92))

vconv3 <- conv3 %>%
  filter(study %in% names(map_var), n_aligned >= 1, !is.na(aligned_veracity)) %>%
  transmute(Variant = factor(unname(map_var[study]), levels = unname(map_var)),
            Condition = factor(ifelse(direction == "bunk", "Bunking", "Debunking"),
                               levels = names(colors_main)),
            avg_veracity = aligned_veracity)
facet_labs3 <- c("Jailbroken" = "Jailbroken",
                 "Standard" = "Standard",
                 "Truth-Constrained" = "Truth-\nconstrained")

p3b <- ggplot(vconv3, aes(Condition, avg_veracity, fill = Condition)) +
  geom_violin(width = 0.84, alpha = 0.7, colour = NA, trim = FALSE) +
  geom_boxplot(width = 0.20, outlier.shape = NA, alpha = 0.86,
               colour = "#333333", linewidth = 0.35) +
  facet_wrap(~ Variant, nrow = 1, labeller = labeller(Variant = facet_labs3)) +
  scale_y_continuous(breaks = seq(0, 100, 25),
                     expand = expansion(mult = c(0.02, 0.04))) +
  scale_fill_manual(values = fills_main, guide = "none") +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = NULL, y = "Aligned-claim\nveracity") +
  theme_clean(BASE - 0.4, "both") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

algn3 <- labels3 %>%
  filter(study_source %in% names(map_var)) %>%
  mutate(is_aligned = aligned_flag(stance_to_focal, directness_to_focal, direction)) %>%
  filter(is_aligned, !is.na(veracity_score))
conv_summary3 <- algn3 %>%
  group_by(Variant = factor(unname(map_var[study_source]), levels = unname(map_var)),
           Condition = ifelse(direction == "bunk", "Bunking", "Debunking"),
           conversation_id) %>%
  summarise(n_claims = n(),
            n_low = sum(veracity_score < 40),
            n_high = n_claims - n_low,
            .groups = "drop") %>%
  group_by(Variant, Condition) %>%
  summarise(avg_high_per_conv = mean(n_high),
            avg_low_per_conv = mean(n_low),
            pct_low = 100 * mean(n_low) / mean(n_claims),
            .groups = "drop") %>%
  pivot_longer(c(avg_high_per_conv, avg_low_per_conv),
               names_to = "vc", values_to = "avg_count") %>%
  mutate(vc = factor(vc, levels = c("avg_high_per_conv", "avg_low_per_conv"),
                     labels = c("Higher veracity", "Low veracity")),
         Condition = factor(Condition, names(colors_main)))

p3c <- ggplot(conv_summary3, aes(Condition, avg_count, fill = vc)) +
  geom_col(width = 0.72, colour = "white", linewidth = 0.35) +
  facet_wrap(~ Variant, nrow = 1, labeller = labeller(Variant = facet_labs3)) +
  scale_fill_manual(values = c("Higher veracity" = high_veracity,
                               "Low veracity" = low_veracity),
                    name = "Claim type") +
  scale_y_continuous(breaks = seq(0, 15, 5),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = "Aligned claims\nper conversation") +
  guides(fill = guide_legend(order = 2, nrow = 1, byrow = TRUE,
                             override.aes = list(colour = NA),
                             theme = theme(
                               legend.title = element_text(
                                 face = "bold", colour = ink,
                                 margin = margin(t = 2, r = 4)
                               )
                             ))) +
  theme_clean(BASE - 0.4, "both") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        panel.grid = element_blank())

right3 <- p3b / p3c + plot_layout(heights = c(1, 0.95))
fig3 <- (p3a | right3) +
  plot_layout(widths = c(1.38, 1.02), guides = "collect") +
  plot_annotation(tag_levels = "a", theme = tag_theme) &
  theme(legend.position = "bottom")
save_fig(fig3, "figure3_ATE_and_veracity_aligned", 7.2, 4.75)

# =============================================================================
# Figure 4: Study 4 belief and posting forests
# =============================================================================
message("Figure 4 ...")
s4 <- core_objects$s4$s4
ylev <- rev(MODELS)

star_by_model <- function(d, col, levs) {
  d %>%
    group_by(model_pooled) %>%
    group_modify(~ tibble(p = fit_dir_p(.x, col))) %>%
    ungroup() %>%
    transmute(Model = factor(model_pooled, levs), p = p)
}

make_forest_clean <- function(dat, xlab, show_y = TRUE, legend = FALSE, stars = NULL,
                              label_off = 0.25) {
  hi_max <- max(dat$hi, na.rm = TRUE)
  lo_min <- min(c(dat$lo, 0), na.rm = TRUE)
  span <- hi_max - lo_min
  pad <- span * 0.025
  dat <- dat %>%
    mutate(label = signed_num(est),
           y = as.numeric(Model) + ifelse(Condition == "Bunking", 0.16, -0.16),
           label_y = y + ifelse(Condition == "Bunking", label_off, -label_off)) %>%
    # a bunking label (above row k) and the next row's debunking label (below
    # row k+1) share the same vertical band; when their x values nearly
    # coincide, anchor the left one leftward and the right one rightward so
    # the texts split apart instead of overprinting
    mutate(.band = as.numeric(Model) + ifelse(Condition == "Bunking", 0.5, -0.5)) %>%
    group_by(.band) %>%
    mutate(.clash = dplyr::n() == 2 & abs(max(est) - min(est)) < span * 0.075,
           .left  = rank(est, ties.method = "first") == 1,
           label_hjust = ifelse(.clash, ifelse(.left, 1, 0), 0.5),
           label_x = est + ifelse(.clash, ifelse(.left, -1, 1) * span * 0.015, 0)) %>%
    ungroup()
  xlo <- min(c(dat$lo, 0), na.rm = TRUE) - span * 0.06
  xhi <- max(dat$hi, na.rm = TRUE) + span * 0.08
  sigdat <- NULL
  if (!is.null(stars)) {
    sigdat <- dat %>%
      group_by(Model) %>%
      summarise(x_sig = max(hi, na.rm = TRUE) + pad, .groups = "drop") %>%
      left_join(stars, by = "Model") %>%
      mutate(y = as.numeric(Model),
             x_text = x_sig + span * 0.026,
             label = sig_label(p))
    xhi <- max(sigdat$x_text, na.rm = TRUE) + span * 0.05
  }
  g <- ggplot(dat, aes(est, y, colour = Condition, fill = Condition)) +
    geom_vline(xintercept = 0, linetype = "22", colour = zero_line, linewidth = 0.58) +
    geom_linerange(aes(xmin = lo, xmax = hi),
                   orientation = "y", linewidth = 0.72, show.legend = FALSE) +
    geom_point(size = 2.75, shape = 21, stroke = 0.65) +
    geom_text(aes(x = label_x, y = label_y, label = label, hjust = label_hjust),
              size = 2.15, family = font_family, fontface = "bold",
              show.legend = FALSE) +
    scale_colour_manual(values = colors_main, name = "AI instruction",
                        guide = if (legend) "legend" else "none") +
    scale_fill_manual(values = fills_main, guide = "none") +
    scale_x_continuous(limits = c(xlo, xhi),
                       breaks = function(lims) pretty(lims, n = 4),
                       labels = minus,
                       expand = expansion(mult = 0)) +
    scale_y_continuous(breaks = seq_along(levels(dat$Model)),
                       labels = levels(dat$Model),
                       expand = expansion(add = label_off + 0.20)) +
    labs(x = xlab, y = NULL) +
    guides(colour = guide_legend(override.aes = list(size = 3.1, linewidth = 1.1))) +
    theme_clean(BASE, "none")
  if (!is.null(sigdat)) {
    g <- g +
      geom_segment(data = sigdat,
                   aes(x = x_sig, xend = x_sig, y = y - 0.17, yend = y + 0.17),
                   inherit.aes = FALSE, linewidth = 0.45, colour = "#666666")
    if (nrow(filter(sigdat, label != "ns"))) {
      g <- g +
        geom_text(data = filter(sigdat, label != "ns"),
                  aes(x = x_text, y = y, label = label),
                  inherit.aes = FALSE, size = 3.4, family = font_family,
                  fontface = "bold",
                  colour = ink, hjust = 0)
    }
    if (nrow(filter(sigdat, label == "ns"))) {
      g <- g +
        geom_text(data = filter(sigdat, label == "ns"),
                  aes(x = x_text, y = y, label = label),
                  inherit.aes = FALSE, size = 3.1, family = font_family,
                  fontface = "italic",
                  colour = "#666666", hjust = 0)
    }
  }
  if (!show_y) {
    g <- g + theme(axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.line.y = element_blank())
  }
  g
}

f4a <- an %>%
  filter(block == "raw_aligned_means_cells", sample == "strict_n1272",
         outcome == "aligned_belief_change", term == "raw_mean") %>%
  transmute(Model = factor(model, ylev), Condition = as_cond(direction),
            est = estimate, lo = conf_low, hi = conf_high)
p4a <- make_forest_clean(f4a, "Belief change toward AI side (points)",
                         show_y = TRUE, legend = TRUE,
                         stars = star_by_model(s4, "aligned_belief_change", ylev),
                         label_off = 0.28)

sp <- s4 %>%
  filter(is.finite(share_pre_4), is.finite(share_post_4),
         is.finite(pre_direction_score), is.finite(post_direction_score)) %>%
  mutate(
    cat_pre = ifelse(share_pre_4 > 50 & pre_direction_score > 50, "pro",
                     ifelse(share_pre_4 > 50 & pre_direction_score < 50, "anti", "none")),
    cat_post = ifelse(share_post_4 > 50 & post_direction_score > 50, "pro",
                      ifelse(share_post_4 > 50 & post_direction_score < 50, "anti", "none")),
    aligned_target = ifelse(direction == "bunk", "pro", "anti"),
    opp_target = ifelse(direction == "bunk", "anti", "pro"),
    aligned_chg = (cat_post == aligned_target) - (cat_pre == aligned_target),
    mis_chg = (cat_pre == opp_target) - (cat_post == opp_target)
  )

em_cell <- function(col) {
  sp %>%
    group_by(model_pooled, direction) %>%
    summarise(n = n(), m = mean(.data[[col]]),
              v = mean(.data[[col]]^2) - mean(.data[[col]])^2,
              .groups = "drop") %>%
    mutate(Model = factor(model_pooled, ylev),
           Condition = as_cond(direction),
           est = 100 * m,
           se = 100 * sqrt(v / n),
           lo = est - 1.96 * se,
           hi = est + 1.96 * se)
}

pb3 <- make_forest_clean(em_cell("aligned_chg"), "Increase in AI-side posting (pp)",
                         show_y = TRUE,
                         stars = star_by_model(sp, "aligned_chg", ylev),
                         label_off = 0.32)
pc3 <- make_forest_clean(em_cell("mis_chg"), "Decrease in opposing-side posting (pp)",
                         show_y = TRUE,
                         stars = star_by_model(sp, "mis_chg", ylev),
                         label_off = 0.32)

design3 <- "#AAAAAAAAAA#\nBBBBBBCCCCCC"
fig4 <- p4a + pb3 + pc3 +
  plot_layout(design = design3, heights = c(1.12, 1), guides = "collect") +
  plot_annotation(tag_levels = "a", theme = tag_theme) &
  theme(legend.position = "bottom")
save_fig(fig4, "figure4_belief_and_posting", 7.2, 5.15)

cat("\nPolished manuscript figures written to: ", outdir, "\n")
cat("============================================================\n")
