# Pooled conspiracy-topic taxonomy across Studies 1-4 — adapted from an earlier
# internal pipeline,
# upgraded to embed with Google's gemini-embedding-2 (task_type CLUSTERING) and
# to pool all four Bunkbot studies at once.
#
# Pipeline: embed conspiracy restatements -> PCA(30) -> HDBSCAN grid (minPts
# 10:25, target ~8-14 retained clusters) -> exemplar-based labels -> breakdown
# by study / condition / model.
#
# Requires OPENROUTER_API_KEY (the Gemini embedding/label models are called via
# OpenRouter). Embeddings are cached to the shipped
# data/api_cached/topic_modeling/topic_embeddings_gemini.rds so re-runs cost nothing.
#
# Run modes:
#   Rscript pooled_conspiracy_topics.R test   # embed 3 texts, print dims, stop
#   Rscript pooled_conspiracy_topics.R        # full run
#
# Shipped outputs:
#   data/api_cached/topic_modeling/topic_assignments.csv (minimal join key
#     consumed by the analysis pipeline via paths$topic_assignments).
# Working outputs (this dir): pooled_with_clusters.csv, cluster_exemplars.csv,
#   hdbscan_grid_summary.csv, topic_by_study.csv, topic_by_condition.csv,
#   topic_by_model_s4.csv, topic_effects.csv

suppressPackageStartupMessages({
  library(tidyverse); library(httr); library(jsonlite); library(dbscan)
})

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
args <- commandArgs(trailingOnly = TRUE)
TEST_MODE <- length(args) && args[[1]] == "test"

# Working corpus stays THIS_DIR-relative (written by assemble_corpus.R here).
CORPUS_CSV   <- file.path(THIS_DIR, "pooled_conspiracy_corpus.csv")
# Shipped embedding cache: data/api_cached/topic_modeling/topic_embeddings_gemini.rds.
EMBED_CACHE  <- file.path(REPO_ROOT, "data/api_cached/topic_modeling/topic_embeddings_gemini.rds")
# Shipped minimal join key consumed by the analysis pipeline (paths$topic_assignments).
TOPIC_ASSIGN_CSV <- file.path(REPO_ROOT, "data/api_cached/topic_modeling/topic_assignments.csv")
PCA_N        <- 30
# Author preference: many clusters, each big enough for subsequent analyses.
# minPts in dbscan::hdbscan is the minimum cluster size, so it doubles as the
# analyzability floor; we sweep it and prefer the solution with the MOST
# retained clusters (subject to a noise ceiling).
MIN_CLUSTER_N <- suppressWarnings(as.integer(Sys.getenv("BB_MIN_CLUSTER_N", "20")))
HDBSCAN_MINPTS_GRID <- 8:30
NOISE_CEILING <- 0.55
EMBED_MODEL <- Sys.getenv("BB_EMBED_MODEL", "google/gemini-embedding-2-preview")
LABEL_MODEL <- Sys.getenv("BB_LABEL_MODEL", "google/gemini-3.1-pro-preview")  # for cluster labels
ORKEY <- Sys.getenv("OPENROUTER_API_KEY")
if (!nzchar(ORKEY)) {                     # fallback: a local key file (keeps secret out of code)
  kf <- c(file.path(THIS_DIR, ".openrouter_key"), path.expand("~/.openrouter_key"))
  kf <- kf[file.exists(kf)]
  if (length(kf)) ORKEY <- trimws(readLines(kf[1], warn = FALSE)[1])
}
OR_BASE <- "https://openrouter.ai/api/v1"
or_headers <- function() add_headers(
  Authorization = paste("Bearer", ORKEY), `Content-Type` = "application/json",
  `HTTP-Referer` = "https://github.com/bunkbot", `X-Title` = "Bunkbot Conspiracy Topics")

# Short standardization map (keyed by cluster id) tidying a handful of the LLM's
# cluster labels for the journal figures; every other retained cluster keeps its
# LLM label verbatim. (HDBSCAN/PCA are deterministic, so cluster ids are stable
# given the cached embeddings.)
LABEL_STANDARDIZE <- c(
  "9"  = "Election Fraud",
  "13" = "JFK Assassination",
  "14" = "Celebrity Faked Deaths",
  "16" = "9/11 Inside Job",
  "19" = "Chemtrails",
  "20" = "Vaccine Harms")

# ===================== EMBEDDING (gemini-embedding-2 via OpenRouter) =========
embed_batch <- function(texts) {
  # OpenAI-compatible /embeddings; returns matrix [length(texts) x dim]
  body <- toJSON(list(model = EMBED_MODEL, input = as.list(texts)), auto_unbox = TRUE)
  for (attempt in 1:5) {
    resp <- tryCatch(POST(paste0(OR_BASE, "/embeddings"), or_headers(),
                          body = body, encode = "raw", timeout(120)),
                     error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) == 200) {
      pr <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
      vals <- lapply(pr$data, function(e) as.numeric(e$embedding))
      m <- do.call(rbind, vals)
      if (nrow(m) == length(texts)) return(m)
      stop("Embedding count mismatch: got ", nrow(m), " for ", length(texts), " inputs")
    }
    msg <- if (is.null(resp)) "no response" else paste0("HTTP ", status_code(resp), ": ",
              substr(content(resp, as = "text", encoding = "UTF-8"), 1, 300))
    if (attempt == 5) stop("Embedding failed after retries — ", msg)
    Sys.sleep(2 * attempt)
  }
}

embed_corpus <- function(corpus) {
  cache <- if (file.exists(EMBED_CACHE)) readRDS(EMBED_CACHE) else
    list(ids = character(0), mat = NULL)
  need <- setdiff(corpus$response_id, cache$ids)
  if (length(need)) {
    sub <- corpus %>% filter(response_id %in% need)
    cat(sprintf("Embedding %d new texts (model=%s)...\n", nrow(sub), EMBED_MODEL))
    bs <- 50; chunks <- split(seq_len(nrow(sub)), ceiling(seq_len(nrow(sub)) / bs))
    new_mat <- NULL; new_ids <- character(0)
    for (ci in seq_along(chunks)) {
      idx <- chunks[[ci]]
      m <- embed_batch(sub$embed_text[idx])
      stopifnot(nrow(m) == length(idx))
      new_mat <- rbind(new_mat, m); new_ids <- c(new_ids, sub$response_id[idx])
      cat(sprintf("  chunk %d/%d (%d rows)\n", ci, length(chunks), length(idx)))
      # persist incrementally
      cache$ids <- c(cache$ids, sub$response_id[idx]); cache$mat <- rbind(cache$mat, m)
      saveRDS(cache, EMBED_CACHE)
    }
  }
  # return matrix aligned to corpus order
  cache$mat[match(corpus$response_id, cache$ids), , drop = FALSE]
}

# ============================= CLUSTER SELECTION =============================
cluster_balance_cv <- function(sizes){ sizes <- sizes[sizes >= MIN_CLUSTER_N]
  if (length(sizes) < 2) return(NA_real_); sd(sizes)/mean(sizes) }
study_segregation <- function(assign, meta){
  df <- tibble(cluster = assign, study = meta$study) %>% filter(cluster > 0) %>%
    add_count(cluster, name="cn") %>% filter(cn >= MIN_CLUSTER_N)
  if (!nrow(df)) return(NA_real_)
  glob <- prop.table(table(df$study))
  cs <- df %>% group_by(cluster) %>%
    summarise(n=n(), tv = sum(abs(prop.table(table(factor(study, levels=names(glob)))) - glob))/2, .groups="drop")
  weighted.mean(cs$tv, cs$n)
}
solution_metrics <- function(assign, meta){
  tab <- table(assign); sizes <- as.numeric(tab[names(tab) != "0"])
  retained <- sizes[sizes >= MIN_CLUSTER_N]
  tibble(n_clusters = max(assign), retained_clusters = length(retained),
         noise_fraction = mean(assign == 0),
         largest_retained = if (length(retained)) max(retained) else 0,
         retained_cv = cluster_balance_cv(sizes),
         study_segregation = study_segregation(assign, meta))
}
# Prefer MANY retained clusters (each >= MIN_CLUSTER_N, the analyzability floor),
# subject to a noise ceiling; break ties by lower noise / study-segregation.
rank_grid <- function(g) g %>%
  mutate(noise_ok = noise_fraction <= NOISE_CEILING) %>%
  arrange(desc(noise_ok), desc(retained_clusters), noise_fraction, study_segregation, retained_cv) %>%
  mutate(rank = row_number())

central_texts <- function(df, pcs, cid, n = 30){
  idx <- which(df$cluster_id == cid); if (!length(idx)) return(character(0))
  cm <- pcs[idx, , drop = FALSE]; ctr <- colMeans(cm)
  d <- sqrt(rowSums((cm - matrix(ctr, nrow(cm), ncol(cm), byrow = TRUE))^2))
  unique(df$embed_text[idx[order(d)]])[seq_len(min(n, length(idx)))]
}

# ===================== LLM LABELING (via OpenRouter chat) ====================
label_cluster <- function(texts){
  if (!nzchar(ORKEY)) return(NA_character_)
  prompt <- paste0(
    "These are participant-endorsed conspiracy beliefs grouped by semantic similarity. ",
    "Give a short, plain-language topic label (2-5 words) for a journal figure. ",
    "Avoid generic labels like 'miscellaneous'; reply with only the label.\n\n",
    paste0(seq_along(texts), ". ", texts, collapse = "\n"))
  body <- toJSON(list(model = LABEL_MODEL,
                      messages = list(
                        list(role = "system", content = "You are a concise labeling assistant for journal figures."),
                        list(role = "user", content = prompt)),
                      temperature = 0.2, max_tokens = 512), auto_unbox = TRUE)
  resp <- tryCatch(POST(paste0(OR_BASE, "/chat/completions"), or_headers(),
                        body = body, encode = "raw", timeout(60)), error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) return(NA_character_)
  pr <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  out <- tryCatch(pr$choices[[1]]$message$content, error = function(e) NA_character_)
  if (is.null(out) || !nzchar(trimws(out))) NA_character_
  else trimws(gsub('^["\']|["\']$', "", trimws(out)))
}

# ================================= RUN ======================================
if (!nzchar(ORKEY)) {
  stop("OPENROUTER_API_KEY not set and no .openrouter_key file found.\n")
}
corpus <- readr::read_csv(CORPUS_CSV, show_col_types = FALSE)

if (TEST_MODE) {
  cat("TEST: embedding 3 texts...\n")
  m <- embed_batch(corpus$embed_text[1:3])
  cat(sprintf("OK — returned %d x %d matrix; |row1| = %.3f\n", nrow(m), ncol(m), sqrt(sum(m[1,]^2))))
  quit(save = "no")
}

emb <- embed_corpus(corpus)
stopifnot(nrow(emb) == nrow(corpus), !anyNA(emb[,1]))
cat(sprintf("Embedding matrix: %d x %d\n", nrow(emb), ncol(emb)))

cat("PCA...\n")
pca <- prcomp(emb, center = TRUE, scale. = FALSE)
pcs <- pca$x[, 1:PCA_N]
cat(sprintf("  %d PCs explain %.1f%% variance\n", PCA_N,
            100 * sum(pca$sdev[1:PCA_N]^2)/sum(pca$sdev^2)))

cat("HDBSCAN grid...\n")
grid <- map_dfr(HDBSCAN_MINPTS_GRID, function(mp){
  cl <- hdbscan(pcs, minPts = mp)$cluster
  bind_cols(tibble(minPts = mp), solution_metrics(cl, corpus)) %>%
    mutate(assign = list(cl))
})
ranked <- rank_grid(grid %>% select(-assign)) %>%
  left_join(grid %>% select(minPts, assign), by = "minPts")
readr::write_csv(ranked %>% select(-assign), file.path(THIS_DIR, "hdbscan_grid_summary.csv"))
best <- ranked %>% slice(1)
cat(sprintf("Selected: minPts=%d, retained clusters=%d, noise=%.3f\n",
            best$minPts, best$retained_clusters, best$noise_fraction))

corpus <- corpus %>% mutate(cluster_raw = best$assign[[1]]) %>%
  add_count(cluster_raw, name = "cn") %>%
  mutate(cluster_id = if_else(cluster_raw > 0 & cn >= MIN_CLUSTER_N, cluster_raw, 0L))

cids <- sort(unique(corpus$cluster_id[corpus$cluster_id > 0]))
cat(sprintf("Labeling %d clusters...\n", length(cids)))
exrows <- list(); labs <- tibble(cluster_id = integer(), gemini_label = character(), label = character())
for (cid in cids) {
  tx  <- central_texts(corpus, pcs, cid, 30)
  gem <- label_cluster(tx); if (is.na(gem) || !nzchar(gem)) gem <- paste0("Cluster ", cid)  # LLM label
  lab <- LABEL_STANDARDIZE[[as.character(cid)]]; if (is.null(lab)) lab <- gem                # standardize a handful
  labs <- bind_rows(labs, tibble(cluster_id = cid, gemini_label = gem, label = lab))
  exrows[[as.character(cid)]] <- tibble(cluster_id = cid, rank = seq_along(tx), exemplar = tx)
  cat(sprintf("  cluster %d (n=%d): %s%s\n", cid, sum(corpus$cluster_id == cid), lab,
              if (!identical(lab, gem)) sprintf("  [llm: %s]", gem) else ""))
}
readr::write_csv(bind_rows(exrows), file.path(THIS_DIR, "cluster_exemplars.csv"))
# cached labeler output consumed by the API-free engine (bunkbot_helpers.R)
readr::write_csv(labs %>% dplyr::select(cluster_id, gemini_label),
                 file.path(THIS_DIR, "cluster_labels_gemini.csv"))
readr::write_csv(labs %>% dplyr::select(cluster_id, gemini_label),
                 file.path(dirname(TOPIC_ASSIGN_CSV), "cluster_labels_gemini.csv"))

corpus <- corpus %>% left_join(labs, by = "cluster_id") %>%
  mutate(topic = if_else(cluster_id == 0L, "Mixed / Unclassified", label)) %>%
  select(response_id, study, variant, condition, belief_change, cluster_id, topic, embed_text)
readr::write_csv(corpus, file.path(THIS_DIR, "pooled_with_clusters.csv"))

# Shipped minimal join key for the analysis pipeline (paths$topic_assignments).
dir.create(dirname(TOPIC_ASSIGN_CSV), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(corpus %>% select(response_id, study, cluster_id, topic), TOPIC_ASSIGN_CSV)

# ============================= BREAKDOWNS ===================================
topic_lvls <- corpus %>% filter(topic != "Mixed / Unclassified") %>%
  count(topic, sort = TRUE) %>% pull(topic)
topic_lvls <- c(topic_lvls, "Mixed / Unclassified")
corpus <- corpus %>% mutate(topic = factor(topic, levels = topic_lvls))

by_study <- corpus %>% count(topic, study) %>% pivot_wider(names_from = study, values_from = n, values_fill = 0)
by_cond  <- corpus %>% count(topic, condition) %>% pivot_wider(names_from = condition, values_from = n, values_fill = 0)
by_model <- corpus %>% filter(study == "S4") %>% count(topic, variant) %>%
  pivot_wider(names_from = variant, values_from = n, values_fill = 0)
topic_eff <- corpus %>% group_by(topic, condition) %>%
  summarise(n = n(), mean_change = mean(belief_change, na.rm = TRUE), .groups = "drop")

readr::write_csv(by_study, file.path(THIS_DIR, "topic_by_study.csv"))
readr::write_csv(by_cond,  file.path(THIS_DIR, "topic_by_condition.csv"))
readr::write_csv(by_model, file.path(THIS_DIR, "topic_by_model_s4.csv"))
readr::write_csv(topic_eff, file.path(THIS_DIR, "topic_effects.csv"))

cat("\n=== TOPIC x STUDY ===\n"); print(as.data.frame(by_study))
cat("\n=== TOPIC x CONDITION ===\n"); print(as.data.frame(by_cond))
cat("\n=== TOPIC x MODEL (S4) ===\n"); print(as.data.frame(by_model))
cat("\nDone. Wrote pooled_with_clusters.csv + breakdown CSVs (this dir) and\n",
    "shipped data/api_cached/topic_modeling/topic_assignments.csv.\n", sep = "")
