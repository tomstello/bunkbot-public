# Formatting, package, and reporting helpers for the dynamic Bunkbot SI.

si_require <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Missing required package(s): ", paste(missing, collapse = ", "),
      ". Install them (e.g. via renv::restore()).",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Walk up from `start` to the repo root (the directory holding code/bunkbot_helpers.R).
si_repo_root <- function(start = getwd()) {
  cur <- normalizePath(start, mustWork = TRUE)
  for (i in seq_len(8)) {
    if (file.exists(file.path(cur, "code", "bunkbot_helpers.R"))) return(cur)
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }
  stop("Could not locate the repo root (code/bunkbot_helpers.R) from ", start, call. = FALSE)
}

fmt_num <- function(x, digits = 1) {
  ifelse(is.na(x), "", formatC(as.numeric(x), format = "f", digits = digits))
}

fmt_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < .001 ~ "< .001",
    TRUE ~ sub("^0", "", formatC(p, format = "f", digits = 3))
  )
}

fmt_ci <- function(est, lo, hi, digits = 1) {
  paste0(fmt_num(est, digits), " [", fmt_num(lo, digits), ", ", fmt_num(hi, digits), "]")
}

si_kable <- function(x, caption = NULL, digits = 2, booktabs = TRUE, escape = TRUE, ...) {
  # kableExtra::kbl ONLY for HTML output: with bookdown::html_document2 +
  # kable_styling(), plain knitr::kable() emits the caption's (\#tab:label) token so
  # bookdown numbers each table TWICE ("Table 1.1: Table 1.2: ..."); kbl() numbers it
  # once. For LaTeX/Word/markdown, kbl() would emit HTML (which breaks the .docx
  # target — "Functions that produce HTML output found"), so use knitr::kable there.
  ktab <- if (knitr::is_html_output()) kableExtra::kbl else knitr::kable
  ktab(x, caption = caption, digits = digits, booktabs = booktabs, escape = escape, ...)
}

tex_escape <- function(s) {
  if (is.null(s) || !nzchar(s)) return(s)
  s <- gsub("\\\\", "\\\\textbackslash{}", s)
  s <- gsub("([&%$#_{}])", "\\\\\\1", s)
  s <- gsub("~", "\\\\textasciitilde{}", s)
  s <- gsub("\\^", "\\\\textasciicircum{}", s)
  s
}

si_kable_styled <- function(x, caption = NULL, digits = 2, ...) {
  # LaTeX/Word/Markdown: emit a plain booktabs kable (longtable for page breaks),
  # avoiding kableExtra's LaTeX package dependencies (wrapfig/colortbl/…) so the PDF
  # compiles in any complete TeX install; pandoc converts the same for Word.
  if (knitr::is_latex_output()) {
    # knitr escapes cell content but NOT the caption, so technical tokens with
    # underscores/$ (e.g. n_chars, model_pooled) would break LaTeX — escape it here.
    return(si_kable(x, caption = tex_escape(caption), digits = digits, longtable = TRUE, ...))
  }
  if (!knitr::is_html_output()) {
    return(si_kable(x, caption = caption, digits = digits, ...))
  }
  tab <- si_kable(x, caption = caption, digits = digits, ...)
  kableExtra::kable_styling(tab, full_width = FALSE, bootstrap_options = c("striped", "condensed"))
}
