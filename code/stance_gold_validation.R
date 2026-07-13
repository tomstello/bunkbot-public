# =============================================================================
# stance_gold_validation.R
# -----------------------------------------------------------------------------
# Human gold-standard validation of the 5-model post-stance ensemble (Study 4).
#
# Compares a human coder's hand-codes for a stratified gold sample of social-media
# posts against the LLM ensemble's consensus (median-of-5) stance score and
# categorical labels. Reproduces the "Human gold validation" block that the
# stance-reliability pipeline leaves pending.
#
#   human codes : data/validation/gold_coding/stance_gold_coderA.csv
#   AI ensemble : data/api_cached/sharing_and_stance/study4_stance_classifications.csv
#   joined on   : item_id  (= ResponseId::timepoint)
#
# Writes a markdown report + a tidy metrics CSV to data/validation/.
# Base R only (no extra packages). Run from the repo root:
#   Rscript code/stance_gold_validation.R
# =============================================================================

find_repo_root <- function(start) {
  d <- normalizePath(start, mustWork = FALSE)
  for (i in 1:8) {
    if (dir.exists(file.path(d, "data", "validation", "gold_coding"))) return(d)
    d <- dirname(d)
  }
  stop("Could not find the repo root (looked for data/validation/gold_coding).")
}
.args     <- commandArgs(FALSE)
.file_arg <- sub("^--file=", "", grep("^--file=", .args, value = TRUE))
.sdir     <- if (length(.file_arg)) dirname(normalizePath(.file_arg[1])) else getwd()
ROOT      <- tryCatch(find_repo_root(getwd()), error = function(e) find_repo_root(.sdir))

GOLD <- file.path(ROOT, "data", "validation", "gold_coding", "stance_gold_coderA.csv")
AI   <- file.path(ROOT, "data", "api_cached", "sharing_and_stance", "study4_stance_classifications.csv")

gold <- read.csv(GOLD, stringsAsFactors = FALSE, na.strings = c("", "NA"))
ai   <- read.csv(AI,   stringsAsFactors = FALSE, na.strings = c("", "NA"))

# ---- join human gold codes to the AI consensus on item_id --------------------
g <- gold[!is.na(gold$item_id), ]
keep_ai <- c("item_id", "consensus_applicable", "consensus_score", "consensus_category",
             "consensus_response_type", "consensus_focal_relevance", "score_sd", "n_scored")
m <- merge(g, ai[, keep_ai], by = "item_id", all.x = TRUE, suffixes = c("_h", "_ai"))

n_human   <- nrow(g)
n_matched <- sum(!is.na(m$consensus_applicable))
cat(sprintf("human-coded rows: %d | matched to AI ensemble: %d\n", n_human, n_matched))
if (n_matched < n_human)
  cat(sprintf("  (%d human rows had no AI match)\n", n_human - n_matched))

# =============================================================================
# helpers (base R)
# =============================================================================
icc21 <- function(x, y) {
  # two-way random, single measures, agreement: ICC(2,1) for 2 raters
  M <- cbind(x, y); M <- M[stats::complete.cases(M), , drop = FALSE]
  n <- nrow(M); k <- 2
  rm_ <- rowMeans(M); cm <- colMeans(M); gm <- mean(M)
  MSR <- k * sum((rm_ - gm)^2) / (n - 1)
  MSC <- n * sum((cm - gm)^2) / (k - 1)
  SSE <- sum((sweep(sweep(M, 1, rm_), 2, cm - gm))^2)
  MSE <- SSE / ((n - 1) * (k - 1))
  (MSR - MSE) / (MSR + (k - 1) * MSE + (k / n) * (MSC - MSE))
}
cohen_kappa <- function(a, b, weighted = FALSE, levels_order = NULL) {
  ok <- !is.na(a) & !is.na(b); a <- a[ok]; b <- b[ok]
  lv <- if (is.null(levels_order)) sort(unique(c(a, b))) else levels_order
  a <- factor(a, lv); b <- factor(b, lv); R <- length(lv)
  O <- table(a, b); O <- O / sum(O)
  E <- outer(rowSums(O), colSums(O))
  if (!weighted) {
    po <- sum(diag(O)); pe <- sum(diag(E))
  } else {
    idx <- seq_len(R); W <- 1 - (outer(idx, idx, "-")^2) / (R - 1)^2
    po <- sum(W * O); pe <- sum(W * E)
  }
  (po - pe) / (1 - pe)
}
pct_agree <- function(a, b) { ok <- !is.na(a) & !is.na(b); mean(a[ok] == b[ok]) }
dirn <- function(score) ifelse(is.na(score), NA, ifelse(score > 50, "pro",
                        ifelse(score < 50, "anti", "neutral50")))

# =============================================================================
# 1. STANCE SCORE (0-100) — human vs ensemble consensus, applicable posts only
# =============================================================================
sc <- m[!is.na(m$stance_score) & !is.na(m$consensus_score), ]
h  <- as.numeric(sc$stance_score); c_ <- as.numeric(sc$consensus_score)
r_p   <- cor(h, c_)
r_s   <- cor(rank(h), rank(c_))
mae   <- mean(abs(h - c_))
bias  <- mean(c_ - h)                          # AI minus human
within10 <- mean(abs(h - c_) <= 10)
icc_v <- icc21(h, c_)

# =============================================================================
# 2. DIRECTIONAL (pro vs anti) — the discretization used in the main text
#    among posts both human and AI score (applicable), excluding exact-50 ties
# =============================================================================
hd <- dirn(h); cd <- dirn(c_)
nonneutral <- hd != "neutral50" & cd != "neutral50"
dir_agree  <- mean(hd[nonneutral] == cd[nonneutral])
dir_kappa  <- cohen_kappa(hd[nonneutral], cd[nonneutral])
n_dir      <- sum(nonneutral)
# 3-way incl. neutral@50
dir3_agree <- mean(hd == cd)
dir3_kappa <- cohen_kappa(hd, cd)

# =============================================================================
# 3. CATEGORICAL FIELDS — human vs consensus
# =============================================================================
stance_lv <- c("argues_against", "leans_against", "neutral_uncommitted",
               "mixed_both_sides", "leans_for", "argues_for")
cat_rows <- m[!is.na(m$stance_category) & !is.na(m$consensus_category), ]
stance_cat_agree  <- pct_agree(cat_rows$stance_category, cat_rows$consensus_category)
stance_cat_kappa  <- cohen_kappa(cat_rows$stance_category, cat_rows$consensus_category)
# quadratic-weighted kappa over the ordered 6-level stance scale (applicable only)
ord_rows <- cat_rows[cat_rows$stance_category %in% stance_lv &
                     cat_rows$consensus_category %in% stance_lv, ]
stance_cat_wkappa <- cohen_kappa(ord_rows$stance_category, ord_rows$consensus_category,
                                 weighted = TRUE, levels_order = stance_lv)

rt_rows <- m[!is.na(m$response_type) & !is.na(m$consensus_response_type), ]
rt_agree <- pct_agree(rt_rows$response_type, rt_rows$consensus_response_type)
rt_kappa <- cohen_kappa(rt_rows$response_type, rt_rows$consensus_response_type)

fr_rows <- m[!is.na(m$focal_relevance) & !is.na(m$consensus_focal_relevance), ]
fr_agree <- pct_agree(fr_rows$focal_relevance, fr_rows$consensus_focal_relevance)
fr_kappa <- cohen_kappa(fr_rows$focal_relevance, fr_rows$consensus_focal_relevance)

# applicability agreement: did human and AI agree a post was scoreable at all?
m$h_appl  <- !is.na(m$stance_score)
m$ai_appl <- m$consensus_applicable == "True"
appl_agree <- mean(m$h_appl == m$ai_appl, na.rm = TRUE)
appl_kappa <- cohen_kappa(m$h_appl, m$ai_appl)

# =============================================================================
# report
# =============================================================================
f3 <- function(x) formatC(x, format = "f", digits = 3)
f1 <- function(x) formatC(x, format = "f", digits = 1)
pc0 <- function(x) paste0(formatC(100 * x, format = "f", digits = 0), "%")

lines <- c(
  "# Stance ensemble — human gold-standard validation",
  "",
  sprintf("One human coder (TC) hand-coded a stratified gold sample of %d Study-4 posts (pre + post),", n_human),
  "blind to condition and to the ensemble's labels, following the same rubric given to the five",
  "LLM raters. Codes are compared to the ensemble **consensus** (median of 5 models for the score;",
  "majority for categorical fields). Of the hand-coded posts,",
  sprintf("%d matched an ensemble record; %d were rated applicable (non-blank stance) by both.", n_matched, nrow(sc)),
  "",
  "## Stance score (0-100)",
  "",
  sprintf("- Human vs consensus Pearson r: **%s** (n = %d)", f3(r_p), nrow(sc)),
  sprintf("- Spearman rho: **%s**", f3(r_s)),
  sprintf("- ICC(2,1) absolute agreement: **%s**", f3(icc_v)),
  sprintf("- Mean absolute error: **%s** points; mean signed (AI - human): %s; within 10 pts: %s",
          f1(mae), f1(bias), pc0(within10)),
  "",
  "## Directional stance (pro- vs anti-conspiracy; the main-text discretization)",
  "",
  sprintf("- Agreement on pro/anti (posts both score off the 50 midpoint, n = %d): **%s**; Cohen's kappa **%s**",
          n_dir, pc0(dir_agree), f3(dir_kappa)),
  sprintf("- 3-way pro/neutral(=50)/anti agreement (n = %d): %s; kappa %s",
          sum(!is.na(hd) & !is.na(cd)), pc0(dir3_agree), f3(dir3_kappa)),
  "",
  "## Categorical fields (human vs consensus)",
  "",
  sprintf("- stance_category (6 levels, n = %d): agreement %s; kappa %s; quadratic-weighted kappa **%s**",
          nrow(cat_rows), pc0(stance_cat_agree), f3(stance_cat_kappa), f3(stance_cat_wkappa)),
  sprintf("- response_type (n = %d): agreement %s; kappa %s", nrow(rt_rows), pc0(rt_agree), f3(rt_kappa)),
  sprintf("- focal_relevance (n = %d): agreement %s; kappa %s", nrow(fr_rows), pc0(fr_agree), f3(fr_kappa)),
  sprintf("- scoreable-vs-not_applicable (n = %d): agreement %s; kappa %s",
          nrow(m), pc0(appl_agree), f3(appl_kappa)),
  ""
)
report <- file.path(ROOT, "data", "validation", "stance_gold_validation_report.md")
writeLines(lines, report)
cat("\n", paste(lines, collapse = "\n"), "\n", sep = "")

metrics <- data.frame(
  metric = c("n_human_coded", "n_matched", "n_applicable_both",
             "score_pearson_r", "score_spearman_rho", "score_icc21", "score_mae",
             "score_bias_ai_minus_human", "score_within10",
             "dir_proanti_agree", "dir_proanti_kappa", "n_dir_proanti",
             "dir3way_agree", "dir3way_kappa",
             "stance_category_agree", "stance_category_kappa", "stance_category_wkappa",
             "response_type_agree", "response_type_kappa",
             "focal_relevance_agree", "focal_relevance_kappa",
             "applicable_agree", "applicable_kappa"),
  value = c(n_human, n_matched, nrow(sc),
            r_p, r_s, icc_v, mae, bias, within10,
            dir_agree, dir_kappa, n_dir, dir3_agree, dir3_kappa,
            stance_cat_agree, stance_cat_kappa, stance_cat_wkappa,
            rt_agree, rt_kappa, fr_agree, fr_kappa, appl_agree, appl_kappa)
)
write.csv(metrics, file.path(ROOT, "data", "validation", "stance_gold_validation_metrics.csv"),
          row.names = FALSE)
cat("\nWrote:", report, "\n")
