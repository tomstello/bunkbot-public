# si_crosswalk.R ----------------------------------------------------------------
# Build the SI numbering crosswalk from the rendered LaTeX. The SI PDF numbers
# tables/figures with flat S-counters (\renewcommand{\thetable}{S\arabic{table}}
# in supplement/Bunkbot_SI.Rmd), while the bookdown HTML/Word outputs use
# section.sequence numbers ("Table 4.3"). This script parses output/Bunkbot_SI.tex
# in document order and emits one row per float:
#   kind (table|figure), label (tab:.../fig:...), s_number (S17),
#   word_number (4.3), section_title, caption_stub.
#
# Used to (a) verify every manuscript "Table S# / Fig. S#" pointer, and
# (b) drive code/postprocess_word_si.py, which rewrites the rendered .docx
# captions/cross-references to the S-numbering.
#
# Run: Rscript code/R/si_crosswalk.R [path/to/Bunkbot_SI.tex]
#      (default: output/Bunkbot_SI.tex; also processes Bunkbot_SI_extended.tex
#       if present) -> output/si_label_crosswalk{,_extended}.csv

repo_root <- (function(start) {
  p <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(p, "data")) && dir.exists(file.path(p, "code"))) return(p)
    parent <- dirname(p)
    if (identical(parent, p)) stop("repo root not found")
    p <- parent
  }
})(if (interactive()) getwd() else dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))

build_crosswalk <- function(tex_path) {
  stopifnot(file.exists(tex_path))
  tex <- readLines(tex_path, warn = FALSE)

  sec_idx <- 0L
  tab_count_all <- 0L; fig_count_all <- 0L   # flat S counters
  tab_count_sec <- 0L; fig_count_sec <- 0L   # per-section (Word) counters
  sec_title <- NA_character_
  rows <- list()

  # helper: caption stub + label. Only trust a \label that sits IMMEDIATELY inside
  # the \caption{...} (i.e., \caption{\label{...}); unlabeled captions are real
  # (some figures carry no label) and get "(unlabeled)".
  grab <- function(i) {
    chunk <- paste(tex[i:min(i + 3, length(tex))], collapse = " ")
    cap <- sub(".*?\\\\caption\\{", "", chunk)
    if (grepl("^\\\\label\\{", cap)) {
      label <- sub("^\\\\label\\{([^}]+)\\}.*", "\\1", cap)
      stub <- sub("^\\\\label\\{[^}]+\\}", "", cap)
    } else {
      label <- "(unlabeled)"
      stub <- cap
    }
    stub <- gsub("\\\\[a-zA-Z]+\\*?(\\[[^]]*\\])?(\\{[^{}]*\\})?", "", stub)
    stub <- gsub("[{}]", "", stub)
    stub <- trimws(substr(trimws(stub), 1, 110))
    list(label = label, stub = stub)
  }

  in_table_env <- FALSE
  for (i in seq_along(tex)) {
    line <- tex[i]
    if (grepl("^\\\\section\\*?\\{", line)) {
      sec_idx <- sec_idx + 1L
      sec_title <- sub("^\\\\section\\*?\\{([^}]*)\\}.*", "\\1", line)
      tab_count_sec <- 0L; fig_count_sec <- 0L
    }
    if (grepl("\\\\begin\\{longtable\\}", line) || grepl("\\\\begin\\{table\\}", line)) in_table_env <- TRUE
    if (grepl("\\\\caption\\{", line)) {
      g <- grab(i)
      is_table <- in_table_env || grepl("^tab:", g$label)
      if (is_table) {
        tab_count_all <- tab_count_all + 1L; tab_count_sec <- tab_count_sec + 1L
        rows[[length(rows) + 1]] <- data.frame(
          kind = "table", label = g$label,
          s_number = paste0("S", tab_count_all),
          word_number = paste0(sec_idx, ".", tab_count_sec),
          section_title = sec_title, caption_stub = g$stub,
          stringsAsFactors = FALSE)
      } else {
        fig_count_all <- fig_count_all + 1L; fig_count_sec <- fig_count_sec + 1L
        rows[[length(rows) + 1]] <- data.frame(
          kind = "figure", label = g$label,
          s_number = paste0("S", fig_count_all),
          word_number = paste0(sec_idx, ".", fig_count_sec),
          section_title = sec_title, caption_stub = g$stub,
          stringsAsFactors = FALSE)
      }
    }
    if (grepl("\\\\end\\{longtable\\}", line) || grepl("\\\\end\\{table\\}", line)) in_table_env <- FALSE
  }
  do.call(rbind, rows)
}

paths <- commandArgs(trailingOnly = TRUE)
if (!length(paths)) paths <- file.path(repo_root, "output", "Bunkbot_SI.tex")
for (p in paths) {
  cw <- build_crosswalk(p)
  suffix <- if (grepl("extended", basename(p))) "_extended" else ""
  out <- file.path(repo_root, "output", paste0("si_label_crosswalk", suffix, ".csv"))
  utils::write.csv(cw, out, row.names = FALSE)
  message(sprintf("%s: %d tables, %d figures -> %s",
                  basename(p), sum(cw$kind == "table"), sum(cw$kind == "figure"), out))
}
ext_p <- file.path(repo_root, "output", "Bunkbot_SI_extended.tex")
if (length(paths) == 1 && file.exists(ext_p) && !identical(paths, ext_p)) {
  cw <- build_crosswalk(ext_p)
  out <- file.path(repo_root, "output", "si_label_crosswalk_extended.csv")
  utils::write.csv(cw, out, row.names = FALSE)
  message(sprintf("%s: %d tables, %d figures -> %s",
                  basename(ext_p), sum(cw$kind == "table"), sum(cw$kind == "figure"), out))
}
