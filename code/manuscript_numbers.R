# manuscript_numbers.R ========================================================
# Dump the manuscript-value manifest: every statistic reported in the Word
# manuscript, pulled live from ALL_NUMBERS and formatted exactly as the
# manuscript prints it.
#
#   Rscript code/manuscript_numbers.R        (from the repo root; ~seconds when
#                                             output/_all_numbers.rds is fresh)
#
# Driven by the spec table code/manuscript_wiring/manifest_spec.csv
# (key, doc_value, source_type, block, filters, stat_col, format, rmd_ref, notes):
# each `all_numbers` row selects ONE row of ALL_NUMBERS exactly like num() does
# (same uniqueness discipline), pulls one statistic column, and formats it.
# Output: output/manuscript_numbers.csv + .json, consumed by
# code/manuscript_wiring/check_manuscript.py (check / redline / clean modes).
#
# Discipline: this file computes NOTHING. Every value comes from the one live
# recompute (build_all_numbers() -> ALL_NUMBERS). Quantities the manuscript
# needs that are not yet in ALL_NUMBERS get an additive engine module
# (code/R/ext_manuscript_s13.R / ext_manuscript_s4meth.R), never a side-computation here.
# ==============================================================================

suppressWarnings(suppressMessages({ library(dplyr); library(jsonlite) }))

.root <- local({
  d <- normalizePath(".", mustWork = TRUE)
  for (i in 1:8) {
    if (file.exists(file.path(d, "code", "R", "build_all_numbers.R"))) break
    d <- dirname(d)
  }
  d
})

source(file.path(.root, "code", "R", "build_all_numbers.R"))
source(file.path(.root, "code", "R", "access.R"))
ALL_NUMBERS <- load_or_build_all_numbers(.root)$all_numbers

spec_path <- file.path(.root, "code", "manuscript_wiring", "manifest_spec.csv")
if (!file.exists(spec_path)) {
  stop("manifest_spec.csv not found. The manuscript-wiring manifest (and the Word ",
       "manuscript it checks) are authors-only working files not distributed with ",
       "the public repository; `make manuscript-check` / `manuscript-wire` require them.",
       call. = FALSE)
}
spec <- utils::read.csv(spec_path, stringsAsFactors = FALSE, colClasses = "character")

## ---- formatting (manuscript conventions: unicode minus, trimmed p) ----------
.minus <- function(s) sub("-", "−", s)
fmt_dp <- function(x, d, signed = FALSE) {
  s <- if (signed && x > 0) "+" else ""
  paste0(s, .minus(formatC(as.numeric(x), format = "f", digits = d)))
}
fmt_int_comma <- function(x) formatC(round(as.numeric(x)), format = "d", big.mark = ",")
fmt_p_doc <- function(p, sig = 2) {
  # manuscript convention: exact p to 2 significant figures (".23", ".045");
  # sig = 3 keeps the 3-digit style for spots the doc prints that way (".106")
  p <- as.numeric(p)
  if (is.na(p)) return(NA_character_)
  if (p < .001) return("< .001")
  s <- if (sig == 3) sub("0+$", "", formatC(p, format = "f", digits = 3))
       else format(signif(p, 2), scientific = FALSE, trim = TRUE)
  s <- sub("^0", "", s)
  if (grepl("^\\.\\d$", s)) s <- paste0(s, "0")   # .5 -> .50
  paste0("= ", s)
}
format_value <- function(x, fmt) {
  switch(fmt,
    int_comma  = fmt_int_comma(x),
    "0dp"      = fmt_dp(x, 0),
    "1dp"      = fmt_dp(x, 1),
    "2dp"      = fmt_dp(x, 2),
    "3dp"      = fmt_dp(x, 3),
    signed_0dp = fmt_dp(x, 0, signed = TRUE),
    signed_1dp = fmt_dp(x, 1, signed = TRUE),
    signed_2dp = fmt_dp(x, 2, signed = TRUE),
    abs_0dp    = fmt_dp(abs(as.numeric(x)), 0),   # prose reports the magnitude
    abs_1dp    = fmt_dp(abs(as.numeric(x)), 1),
    abs_2dp    = fmt_dp(abs(as.numeric(x)), 2),
    neg_1dp    = fmt_dp(-as.numeric(x), 1),       # aligned value, doc signs the decrease
    neg_2dp    = fmt_dp(-as.numeric(x), 2),
    p          = fmt_p_doc(x),
    p3         = fmt_p_doc(x, sig = 3),
    # family bound over several tests ("ps > .59"): x is the family's minimum p,
    # printed as the 2dp floor
    p_gt       = paste0("> ", sub("^0", "", formatC(floor(as.numeric(x) * 100) / 100,
                                                    format = "f", digits = 2))),
    # same floor without the comparator (doc supplies "≥" literally: "all p ≥ .15")
    p_floor2   = sub("^0", "", formatC(floor(as.numeric(x) * 100) / 100,
                                       format = "f", digits = 2)),
    pct0       = fmt_dp(x, 0),                # value already on the 0-100 scale
    pct1       = fmt_dp(x, 1),
    pct0_prop  = fmt_dp(100 * as.numeric(x), 0),   # value is a proportion
    pct1_prop  = fmt_dp(100 * as.numeric(x), 1),
    r2dp       = sub("^0", "", fmt_dp(x, 2)), # correlations: -0.42 -> -.42... doc style
    stop(sprintf("unknown format '%s'", fmt))
  )
}

## ---- selection ---------------------------------------------------------------
parse_filters <- function(s) {
  s <- trimws(s)
  if (!nzchar(s)) return(list())
  parts <- strsplit(s, ";", fixed = TRUE)[[1]]
  out <- list()
  for (p in parts) {
    kv <- strsplit(p, "=", fixed = TRUE)[[1]]
    k <- trimws(kv[1]); v <- trimws(paste(kv[-1], collapse = "="))
    out[[k]] <- if (identical(v, "NA")) NA else v
  }
  out
}

pull_value <- function(row_spec) {
  flt <- parse_filters(row_spec$filters)
  sel <- do.call(num, c(list(block = row_spec$block), flt, list(data = ALL_NUMBERS)))
  col <- row_spec$stat_col
  if (startsWith(col, "note:")) {
    field <- sub("^note:", "", col)
    note <- sel$note
    m <- regmatches(note, regexec(paste0("(?:^|[;\\s])", field, "\\s*=\\s*([^;]+)"),
                                  note, perl = TRUE))[[1]]
    if (length(m) < 2) stop(sprintf("note field '%s' absent in note: %s", field, note))
    val <- trimws(m[2])
    # p's are sometimes packed pre-formatted ("< .001"): map to a numeric that
    # format_value(p) prints identically
    if (grepl("<", val, fixed = TRUE))
      return(0.999 * as.numeric(gsub("[^0-9eE.]", "", val)))
    return(as.numeric(gsub("[^0-9eE+.-]", "", val)))
  }
  if (col %in% c("t_from_est_se", "t_from_est_se_abs")) {
    t <- as.numeric(sel$estimate) / as.numeric(sel$se)
    return(if (col == "t_from_est_se_abs") abs(t) else t)
  }
  if (!col %in% names(sel)) stop(sprintf("unknown stat_col '%s'", col))
  as.numeric(sel[[col]])
}

## ---- run ---------------------------------------------------------------------
res <- vector("list", nrow(spec)); errs <- character(0)
for (i in seq_len(nrow(spec))) {
  s <- spec[i, ]
  if (identical(s$format, "verbatim")) {
    # external constants (cited reliability stats, nearest-10 word counts): wired
    # for anchor integrity + provenance only; the spec's doc_value IS the value
    res[[i]] <- data.frame(key = s$key, value_raw = NA_real_,
                           value_formatted = s$doc_value,
                           block = s$block, filters = s$filters,
                           note = "verbatim constant (see spec notes)",
                           stringsAsFactors = FALSE)
    next
  }
  if (!identical(s$source_type, "all_numbers")) {
    errs <- c(errs, sprintf("[pending] %s: source_type=%s (needs ext_manuscript port)",
                            s$key, s$source_type))
    next
  }
  res[[i]] <- tryCatch({
    raw <- pull_value(s)
    data.frame(key = s$key, value_raw = raw,
               value_formatted = format_value(raw, s$format),
               block = s$block, filters = s$filters,
               note = paste0(s$stat_col, " / ", s$format),
               stringsAsFactors = FALSE)
  }, error = function(e) {
    errs <<- c(errs, sprintf("[fail] %s: %s", s$key, conditionMessage(e)))
    NULL
  })
}
manifest <- bind_rows(res)

out_csv  <- file.path(.root, "output", "manuscript_numbers.csv")
out_json <- file.path(.root, "output", "manuscript_numbers.json")
utils::write.csv(manifest, out_csv, row.names = FALSE)
jsonlite::write_json(manifest, out_json, dataframe = "rows", pretty = TRUE)

cat(sprintf("manifest: %d/%d keys -> %s\n", nrow(manifest), nrow(spec), out_csv))
if (length(errs)) {
  cat("issues:\n"); cat(paste0("  ", errs, collapse = "\n"), "\n")
  quit(status = 1)
}
