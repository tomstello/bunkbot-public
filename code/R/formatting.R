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
  # PDF: use fixed-width paragraph columns so long labels, confidence intervals,
  # variable names, and verbatim item wording wrap inside the printable area. Wide
  # tables move to landscape pages; longtable preserves page breaks and repeated
  # headers. This branch is LaTeX-only, so the existing HTML table remains unchanged.
  if (knitr::is_latex_output()) {
    ascii_for_pdf <- function(s) {
      raw <- as.character(s)
      out <- suppressWarnings(iconv(raw, from = "UTF-8", to = "ASCII//TRANSLIT", sub = NA))
      bad <- is.na(out)
      if (any(bad)) {
        out[bad] <- suppressWarnings(iconv(
          raw[bad], from = "latin1", to = "ASCII//TRANSLIT", sub = "?"
        ))
      }
      # R under a C locale may render otherwise valid Unicode as literal
      # <U+XXXX> tokens before iconv sees it; normalize those common symbols too.
      out <- gsub("<U+2212>", "-", out, fixed = TRUE)
      out <- gsub("<U+2014>", "---", out, fixed = TRUE)
      out <- gsub("<U+2013>", "--", out, fixed = TRUE)
      out <- gsub("<U+00D7>", "x", out, fixed = TRUE)
      out <- gsub("<U+2265>", ">=", out, fixed = TRUE)
      out <- gsub("<U+2264>", "<=", out, fixed = TRUE)
      out
    }
    latex_cell <- function(s) {
      s <- ascii_for_pdf(as.character(s))
      s <- get("escape_latex", asNamespace("kableExtra"))(s)
      # Technical identifiers are common in these tables. Allow a line break
      # after each escaped underscore and path separator without changing text.
      s <- gsub("\\_", "\\_\\allowbreak{}", s, fixed = TRUE)
      gsub("/", "/\\allowbreak{}", s, fixed = TRUE)
    }
    x_print <- x
    x_print[] <- lapply(x_print, function(z) {
      if (!is.character(z) && !is.factor(z)) return(z)
      latex_cell(z)
    })
    names(x_print) <- latex_cell(names(x_print))
    n_cols <- ncol(x)
    use_landscape <- n_cols >= 7L

    # Estimate useful column proportions from the headers and the 75th percentile
    # of cell lengths. Square-root compression stops one prose-heavy column from
    # starving the identifier columns while still giving it most of the room.
    char_weight <- vapply(seq_len(n_cols), function(i) {
      vals <- nchar(as.character(x[[i]]), type = "width", allowNA = TRUE)
      vals <- vals[is.finite(vals)]
      body <- if (length(vals)) unname(stats::quantile(vals, 0.75, names = FALSE)) else 1
      max(nchar(names(x)[i], type = "width"), body, 1)
    }, numeric(1))
    numeric_col <- vapply(x, function(z) is.numeric(z) || is.integer(z), logical(1))
    weight <- sqrt(pmin(char_weight, 64))
    weight[numeric_col] <- pmax(1.5, 0.72 * weight[numeric_col])

    # With 3pt tabular padding, these totals fit inside the portrait/landscape
    # text blocks created by the document's 0.85in margins.
    usable_inches <- if (use_landscape) 8.35 else 6.15
    widths <- usable_inches * weight / sum(weight)
    min_width <- if (use_landscape) 0.55 else 0.48
    widths <- pmax(widths, min_width)
    widths <- usable_inches * widths / sum(widths)

    tab <- kableExtra::kbl(
      x_print,
      format = "latex",
      caption = tex_escape(ascii_for_pdf(caption)),
      digits = digits,
      booktabs = TRUE,
      longtable = TRUE,
      row.names = FALSE,
      escape = FALSE,
      ...
    )
    for (i in seq_len(n_cols)) {
      tab <- kableExtra::column_spec(
        tab,
        i,
        width = sprintf("%.2fin", widths[i]),
        latex_valign = "p"
      )
    }
    tab <- kableExtra::kable_styling(
      tab,
      latex_options = "repeat_header",
      repeat_header_method = "replace",
      repeat_header_text = "\\textit{(continued)}",
      font_size = if (use_landscape) 7 else 8,
      position = "center"
    )
    if (use_landscape) tab <- kableExtra::landscape(tab)
    # kableExtra repeats the bookdown label inside the continued caption, which
    # creates duplicate PDF destinations. Keep the caption, but label only the
    # first occurrence of each longtable.
    remove_continued_label <- function(s) {
      prefix <- "\\caption[]{"
      marker <- paste0(prefix, "\\label{")
      hit <- regexpr(marker, s, fixed = TRUE)[1]
      if (hit < 0) return(s)
      label_start <- hit + nchar(prefix)
      tail <- substr(s, label_start, nchar(s))
      label_end <- regexpr("}", tail, fixed = TRUE)[1]
      if (label_end < 0) return(s)
      paste0(
        substr(s, 1, label_start - 1),
        substr(tail, label_end + 1, nchar(tail))
      )
    }
    tab[] <- vapply(as.character(tab), remove_continued_label, character(1))
    return(tab)
  }
  if (!knitr::is_html_output()) {
    return(si_kable(x, caption = caption, digits = digits, ...))
  }
  tab <- si_kable(x, caption = caption, digits = digits, ...)
  kableExtra::kable_styling(tab, full_width = FALSE, bootstrap_options = c("striped", "condensed"))
}
