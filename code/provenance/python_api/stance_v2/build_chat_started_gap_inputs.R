#!/usr/bin/env Rscript

# Build the 81 pre-conversation post records that were absent from the original
# stance/orientation audit because the participants did not submit the Study 4
# outcome page after a confirmed initial chatbot response. The resulting JSONL
# is transient provenance input; the consolidated gap CSVs are shipped.

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[1]) else normalizePath(".")
root <- normalizePath(file.path(dirname(script_path), "..", "..", "..", ".."))
source(file.path(root, "code", "bunkbot_helpers.R"))

paths <- pkg_paths(root)
d <- build_s4_data(paths)
gap <- build_s4_chat_started_pool(d$s4_raw) |>
  filter(!.data$completed) |>
  arrange(.data$ResponseId)

stopifnot(nrow(gap) == 81L, all(gap$initial_ai == 1L))

task_wording <- paste(
  "Write a social media post -- Imagine you are making a public post on a",
  "social media platform about the topic below. Write the post as you",
  "naturally would online. It's okay to be uncertain or mixed."
)
records <- lapply(seq_len(nrow(gap)), function(i) {
  row <- gap[i, , drop = FALSE]
  list(
    item_id = paste0(row$ResponseId, "::pre"),
    ResponseId = row$ResponseId,
    timepoint = "pre",
    task_wording = task_wording,
    focal_claim_restatement = trimws(row$conRestatement),
    focal_claim_summary = trimws(row$conSummary),
    participant_topic_description = trimws(row$mid_topic),
    participant_reasons = trimws(row$mid_evidence),
    post_text = row$social_post,
    v1_score = NULL,
    v1_confidence = NULL
  )
})

out_dir <- file.path(root, "output", "provenance_work", "stance_v2")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out <- file.path(out_dir, "chat_started_gap_inputs.jsonl")
writeLines(vapply(records, jsonlite::toJSON, character(1), auto_unbox = TRUE,
                  null = "null", na = "null"), out, useBytes = TRUE)
message("wrote ", length(records), " records -> ", out)
