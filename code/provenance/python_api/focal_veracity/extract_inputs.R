# extract_inputs.R -------------------------------------------------------------
# Focal-statement veracity pipeline, stage 0 (API-free): build the scoring input
# from the ENGINE's analytic frames so the scored set is exactly the analytic
# samples (S1 1,092 / S2 814 / S3 818 / S4 1,272), not a production superset.
#
# What gets scored (design decision, 2026-07-04): the AI RESTATEMENT
# (`conRestatement`) -- the clean declarative statement of the participant's
# focal conspiracy that anchored every belief rating. The participant's own
# free-text description is typically hedged/equivocal ("I wonder if...") and is
# passed along as CONTEXT only. The restatement's `category`
# (affirms/denies/unclear) travels with each row so denial-phrased restatements
# can be classified and handled downstream instead of silently scored as if
# they asserted a conspiracy (the flaw in the earlier, superseded runs).
#
# Output: output/provenance_work/focal_veracity/focal_veracity_inputs.csv
# Run:    Rscript code/provenance/python_api/focal_veracity/extract_inputs.R

repo_root <- (function(start) {
  p <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(p, "data")) && dir.exists(file.path(p, "code"))) return(p)
    parent <- dirname(p)
    if (identical(parent, p)) stop("repo root (dir containing data/ and code/) not found")
    p <- parent
  }
})(if (interactive()) getwd() else dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))

message("repo root: ", repo_root)
source(file.path(repo_root, "code", "bunkbot_helpers.R"))
source(file.path(repo_root, "code", "R", "formatting.R"))
source(file.path(repo_root, "code", "R", "ape_rescore.R"))
source(file.path(repo_root, "code", "R", "pipeline_core.R"))
suppressPackageStartupMessages(library(dplyr))

core <- build_core_objects(repo_root)

first_nonempty <- function(...) {
  args <- list(...)
  out <- rep(NA_character_, length(args[[1]]))
  for (a in args) {
    a <- as.character(a)
    fill <- is.na(out) | !nzchar(trimws(out))
    ok <- !is.na(a) & nzchar(trimws(a)) & toupper(trimws(a)) != "NA"
    out[fill & ok] <- a[fill & ok]
  }
  out
}

need <- function(d, cols, where) {
  miss <- setdiff(cols, names(d))
  if (length(miss)) stop(sprintf("missing columns in %s: %s", where, paste(miss, collapse = ", ")))
}

s13 <- core$s13
need(s13, c("response_id", "study_factor", "direction", "category",
            "con_restatement", "con_summary", "mid_topic", "belief_rating_pre_rc"), "core$s13")
inp13 <- s13 |>
  transmute(
    study = dplyr::recode(as.character(study_factor),
                          "Jailbroken" = "study1", "Standard" = "study2",
                          "Truth-Constrained" = "study3"),
    regime = as.character(study_factor),
    response_id = as.character(response_id),
    direction = as.character(direction),
    category = as.character(category),
    statement = as.character(con_restatement),
    con_summary = as.character(con_summary),
    participant_description = first_nonempty(mid_topic),
    belief_pre_rc = suppressWarnings(as.numeric(belief_rating_pre_rc))
  )

s4 <- core$s4$s4_with_compliance
need(s4, c("ResponseId", "model_pooled", "direction", "restatement_orientation",
           "conRestatement", "conSummary", "mid_topic"), "core$s4$s4_with_compliance")
inp4 <- s4 |>
  transmute(
    study = "study4",
    regime = paste0("S4_", as.character(model_pooled)),
    response_id = as.character(ResponseId),
    direction = as.character(direction),
    # S4 has no affirms/denies category column; the restatement-orientation audit
    # (pro_conspiracy / pro_official consensus) is the equivalent passthrough
    category = as.character(restatement_orientation),
    statement = as.character(conRestatement),
    con_summary = as.character(conSummary),
    participant_description = first_nonempty(mid_topic, if ("bunk_topic" %in% names(s4)) s4$bunk_topic else NA,
                                             if ("debunk_topic" %in% names(s4)) s4$debunk_topic else NA),
    belief_pre_rc = suppressWarnings(as.numeric(if ("belief_rating_pre_4" %in% names(s4)) s4$belief_rating_pre_4 else NA))
  )

inp <- bind_rows(inp13, inp4) |>
  filter(!is.na(statement), nzchar(trimws(statement)), toupper(trimws(statement)) != "NA")

counts <- inp |> count(study)
message(paste(capture.output(print(counts)), collapse = "\n"))
stopifnot(sum(counts$n[counts$study %in% c("study1", "study2", "study3")]) >= 2700,
          counts$n[counts$study == "study4"] >= 1250)

work <- file.path(repo_root, "output", "provenance_work", "focal_veracity")
dir.create(work, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(work, "focal_veracity_inputs.csv")
readr::write_csv(inp, out_path)
message("wrote ", out_path, " (", nrow(inp), " rows)")
