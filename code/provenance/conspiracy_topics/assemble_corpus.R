# Pooled conspiracy-text corpus across Studies 1-4 (embedding input).
# embed_text = conRestatement (fallback conSummary). Joined to study / variant
# (S1-3 regime or S4 model) / condition / aligned belief change.
suppressPackageStartupMessages({library(tidyverse); library(janitor)})

# ---- repo-root resolver (walks up to the dir containing both data/ and code/) --
.repo_root <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  p <- if (length(f)) dirname(normalizePath(f[1])) else normalizePath(getwd())
  repeat {
    if (dir.exists(file.path(p,"data")) && dir.exists(file.path(p,"code"))) return(p)
    pp <- dirname(p); if (pp == p) stop("repo root not found"); p <- pp
  }
}
REPO_ROOT <- .repo_root()
THIS_DIR  <- file.path(REPO_ROOT, "code/provenance/conspiracy_topics")

# Shipped S1-3 processed inputs (clean study files) live under data/processed_s1s3/.
PROC_S1S3 <- file.path(REPO_ROOT, "data/processed_s1s3")
# Shipped S4 social-sharing merged table (conRestatement/conSummary source).
S4_MERGED <- file.path(REPO_ROOT,
  "data/api_cached/sharing_and_stance/study4_sharing_analysis_merged.csv.gz")

# NOT SHIPPED: the S4 analytic-strict frame is built at RUNTIME by
# build_s4_data() in code/bunkbot_helpers.R; there is no static file in data/.
# To run this provenance script standalone, materialise it once (e.g. from the
# analysis pipeline) into the working path below, then re-run.
NOT_SHIPPED_S4_STRICT <- file.path(REPO_ROOT,
  "output/provenance_work/conspiracy_topics/s4_analytic_strict.csv")

mk_text <- function(restate, summ) dplyr::case_when(
  !is.na(restate) & nchar(trimws(restate)) > 5 ~ trimws(restate),
  !is.na(summ)    & nchar(trimws(summ))    > 5 ~ trimws(summ),
  TRUE ~ NA_character_)

# ---- S1-3: clean files joined to the analysis frame (data_s13 `d`) ----------
# NOT SHIPPED: data_s13.R (the old figure_revamp tree) builds the S1-3 analysis
# frame `d`. The repo equivalent is the S1-3 frame produced by the analysis
# pipeline (code/bunkbot_helpers.R); there is no static file. To run standalone,
# source whatever script yields `d` here, or materialise `d` upstream.
# TODO(provenance): repoint to the shipped S1-3 frame builder once exposed; the
# figure_revamp/data_s13.R script is not part of bunkbot-public.
source(file.path(REPO_ROOT, "code/figure_revamp/data_s13.R"))  # d  (NOT SHIPPED — see TODO above)
read_txt <- function(f, study){
  readr::read_csv(file.path(PROC_S1S3, f), show_col_types=FALSE) %>% clean_names() %>%
    transmute(response_id, study_lab = study,
              embed_text = mk_text(con_restatement, con_summary))
}
s13_txt <- bind_rows(
  read_txt("study1_jailbroken_clean.csv.gz","Study 1"),
  read_txt("study2_standard_clean.csv.gz","Study 2"),
  read_txt("study3_truth_constrained_clean.csv.gz","Study 3")) %>% distinct(response_id,.keep_all=TRUE)

s13 <- d %>%
  transmute(response_id, study = paste0("S", as.integer(study_factor)),
            variant = as.character(study_factor),
            condition = as.character(condition_factor),
            belief_change = change) %>%
  left_join(s13_txt %>% select(response_id, embed_text), by="response_id")

# ---- S4: strict sample joined to social-sharing text ------------------------
# s4_analytic_strict is NOT shipped (built at runtime by build_s4_data()); read
# from the working path materialised upstream (see NOT_SHIPPED_S4_STRICT above).
s4_meta <- readr::read_csv(NOT_SHIPPED_S4_STRICT, show_col_types=FALSE) %>%
  transmute(response_id = ResponseId, study = "S4", variant = model_pooled,
            condition = ifelse(direction=="bunk","Bunking","Debunking"),
            belief_change = aligned_belief_change)
s4_txt <- readr::read_csv(S4_MERGED, show_col_types=FALSE) %>%
  transmute(response_id = ResponseId, embed_text = mk_text(conRestatement, conSummary)) %>%
  distinct(response_id,.keep_all=TRUE)
s4 <- s4_meta %>% left_join(s4_txt, by="response_id")

corpus <- bind_rows(s13, s4)
cat("=== corpus rows by study (before dropping missing text) ===\n")
print(corpus %>% count(study, has_text = !is.na(embed_text)) %>% tidyr::pivot_wider(names_from=has_text, values_from=n, values_fill=0) %>% as.data.frame())
corpus <- corpus %>% filter(!is.na(embed_text))
cat("\n=== final corpus N =", nrow(corpus), "===\n")
print(corpus %>% count(study, variant) %>% as.data.frame())
cat("\n=== example texts ===\n")
print(corpus %>% group_by(study) %>% slice_head(n=1) %>% transmute(study,variant,condition, text=substr(embed_text,1,140)) %>% as.data.frame())
cat("\n=== text length distribution (chars) ===\n"); print(summary(nchar(corpus$embed_text)))
# Keep THIS_DIR-relative: pooled_conspiracy_topics.R reads it from the same dir.
readr::write_csv(corpus, file.path(THIS_DIR, "pooled_conspiracy_corpus.csv"))
cat("\nWrote pooled_conspiracy_corpus.csv\n")
