# export_analysis_frame.R
# Build the per-participant analysis frame (Studies 1-4) the explanation coder
# needs: identifiers, study/condition/direction, aligned belief change, baseline
# belief, and compliance. Reuses the canonical builders in bunkbot_helpers.R so
# the screened samples match the rest of the paper (S1-3: 1092/814/818; S4: 1272).
#
# Output: data/api_cached/explanation_coding/analysis_frame.csv
# Run:    Rscript export_analysis_frame.R

suppressWarnings(suppressMessages({
  library(dplyr); library(readr); library(janitor); library(stringr)
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
root  <- .repo_root()                                        # repo root (has data/ + code/)
code  <- file.path(root, "code")                             # canonical helpers live here
source(file.path(code, "bunkbot_helpers.R"))
paths <- pkg_paths(root)
out   <- file.path(root, "data", "api_cached", "explanation_coding", "analysis_frame.csv")

# ---- Studies 1-3 (screened) ----
# S1-3 vary the persuader by PROMPT (jailbroken/standard/truth-constrained) on one
# base model (GPT-4o), so the "persuader" arm = study_factor; model_pooled is NA.
s13 <- build_s1s3(paths) %>%
  transmute(
    ResponseId = response_id,
    study = study,                                   # "Study 1/2/3"
    study_factor = as.character(study_factor),       # Jailbroken/Standard/Truth-Constrained
    model_pooled = NA_character_,
    persuader = as.character(study_factor),
    condition = condition,
    direction = direction,
    aligned_belief_change = as.numeric(aligned_belief_change),
    belief_pre = as.numeric(belief_rating_pre_rc),
    compliant = as.logical(compliant)
  )

# ---- Study 4 (strict sample) ----
s4raw <- build_s4_data(paths)$s4_with_compliance
s4 <- s4raw %>%
  mutate(
    study = "Study 4",
    study_factor = "Sharing",
    condition = if ("condition" %in% names(.)) condition else paste0("treatment_mid_", as.character(direction))
  ) %>%
  transmute(
    ResponseId = ResponseId,
    study = study,
    study_factor = study_factor,
    model_pooled = as.character(model_pooled),       # Claude / Gemini / GPT-5.2 / Grok
    persuader = as.character(model_pooled),          # S4 varies the persuader by MODEL
    condition = as.character(condition),
    direction = as.character(direction),
    aligned_belief_change = as.numeric(aligned_belief_change),
    belief_pre = as.numeric(belief_rating_pre_4),
    compliant = as.logical(strict_compliant)
  )

frame <- bind_rows(s13, s4) %>%
  mutate(
    persuaded = as.integer(aligned_belief_change > 0),
    study_factor = factor(study_factor,
                          levels = c("Jailbroken", "Standard", "Truth-Constrained", "Sharing"))
  )

dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
write_csv(frame, out)

cat("wrote", nrow(frame), "rows ->", out, "\n")
print(table(frame$study_factor, useNA = "ifany"))
cat("compliant:", sum(frame$compliant, na.rm = TRUE), "/", nrow(frame), "\n")
cat("persuaded (aligned change > 0):", sum(frame$persuaded, na.rm = TRUE), "\n")
cat("NA aligned_belief_change:", sum(is.na(frame$aligned_belief_change)), "\n")
