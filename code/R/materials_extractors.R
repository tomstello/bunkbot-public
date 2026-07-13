# Materials-section extractors for Qualtrics instruments and prompt/code assets.
#
# These helpers intentionally use fixed manifests rather than recursive scans.
# They read the replication-package survey QSFs and selected python_api
# prompt/codebook/template files needed for supplement Materials tables.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

materials_survey_manifest <- function(root = ".") {
  out <- data.frame(
    study = c("Study 1", "Study 2", "Study 3", "Study 4"),
    survey_name = c(
      "FAR AI Deception Experiment - Study 1",
      "FAR AI Deception Experiment - Study 2",
      "FAR AI Deception Experiment - Study 3 Truth",
      "Bunkbot social sharing only - Study 4"
    ),
    relative_path = c(
      "data/survey_definitions/study1_jailbroken.qsf",
      "data/survey_definitions/study2_standard.qsf",
      "data/survey_definitions/study3_truth_constrained.qsf",
      "data/survey_definitions/study4_social_sharing.qsf"
    ),
    stringsAsFactors = FALSE
  )
  out$path <- file.path(root, out$relative_path)
  out
}

materials_api_manifest <- function(root = ".") {
  out <- data.frame(
    material_id = c(
      "claim_extraction_system_prompt",
      "claim_taxonomy_system_prompt_template",
      "claim_factcheck_eligibility_system_prompt",
      "claim_factcheck_system_prompt",
      "focal_claim_veracity_system_prompt",
      "claim_role_relative_to_focal_system_prompt",
      "social_post_stance_system_prompt",
      "restatement_direction_audit_system_prompt",
      "restatement_orientation_audit_system_prompt",
      "paraphrase_drift_audit_system_prompt",
      "gold_stance_coding_instructions",
      "claim_category_v2_codebook",
      "messages_template"
    ),
    material_type = c(
      rep("prompt", 10),
      "coding_instructions",
      "codebook",
      "template"
    ),
    relative_path = c(
      "code/provenance/python_api/claim_factcheck/scripts/extract_claims.py",
      "code/provenance/python_api/claim_factcheck/scripts/classify_claims_v2.py",
      "code/provenance/python_api/claim_factcheck/scripts/classify_factcheck_eligibility.py",
      "code/provenance/python_api/claim_factcheck/scripts/fact_check_claims.py",
      "code/provenance/python_api/focal_veracity/score_focal_statement_veracity.py",
      "code/provenance/python_api/claim_factcheck/scripts/classify_claim_role_relative_to_focal.py",
      "code/provenance/python_api/stance_v2/score_post_stance_v2.py",
      "code/provenance/python_api/stance_v2/restatement_direction_audit.py",
      "code/provenance/python_api/stance_v2/restatement_orientation_audit_v2.py",
      "code/provenance/python_api/stance_v2/paraphrase_drift_audit.py",
      "code/provenance/python_api/stance_v2/build_gold_sample.py",
      "code/provenance/python_api/claim_factcheck/codebooks/claim_category_v2_codebook.csv",
      "code/provenance/python_api/claim_factcheck/templates/messages_template.csv"
    ),
    extractor = c(
      "python_literal",
      "python_return_triple",
      rep("python_literal", 9),
      "csv_table",
      "file_text"
    ),
    object_name = c(
      "SYSTEM_PROMPT",
      "build_system_prompt",
      "SYSTEM_PROMPT",
      "SYSTEM_PROMPT",
      "SYSTEM_PROMPT",
      "SYSTEM_PROMPT",
      "SYSTEM_PROMPT",
      "SYSTEM_PROMPT",
      "SYSTEM_PROMPT",
      "SYSTEM",
      "INSTRUCTIONS",
      NA_character_,
      NA_character_
    ),
    stringsAsFactors = FALSE
  )
  out$path <- file.path(root, out$relative_path)
  # Drop manifest entries whose source file is not shipped. Degrade gracefully so
  # one missing provenance script does not nuke the entire materials list.
  .exists <- file.exists(out$path)
  if (any(!.exists)) {
    warning("materials_api_manifest: dropping ", sum(!.exists),
            " entr", if (sum(!.exists) == 1) "y" else "ies",
            " with missing source file(s): ",
            paste(basename(out$path[!.exists]), collapse = ", "), call. = FALSE)
    out <- out[.exists, , drop = FALSE]
  }
  out
}

materials_check_manifest_files <- function(manifest) {
  missing <- manifest$path[!file.exists(manifest$path)]
  if (length(missing) > 0) {
    stop(
      "Manifest path(s) not found: ",
      paste(missing, collapse = "; "),
      call. = FALSE
    )
  }
  invisible(manifest)
}

materials_strip_html <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("<script\\b[^>]*>.*?</script>", " ", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("<style\\b[^>]*>.*?</style>", " ", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("<br\\s*/?>", "\n", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("</p\\s*>", "\n", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("<[^>]+>", " ", x, perl = TRUE)
  x <- materials_decode_html_entities(x)
  x <- gsub("[\r\t]+", " ", x, perl = TRUE)
  x <- gsub(" *\n+ *", "\n", x, perl = TRUE)
  x <- gsub("[ ]{2,}", " ", x, perl = TRUE)
  trimws(x)
}

materials_decode_html_entities <- function(x) {
  named <- c(
    nbsp = " ",
    amp = "&",
    lt = "<",
    gt = ">",
    quot = "\"",
    apos = "'",
    ndash = "-",
    mdash = "--",
    lsquo = "'",
    rsquo = "'",
    ldquo = "\"",
    rdquo = "\""
  )
  for (entity in names(named)) {
    x <- gsub(
      paste0("&", entity, ";"),
      named[[entity]],
      x,
      fixed = TRUE
    )
  }

  decode_numeric <- function(pattern, base) {
    regmatches(x, gregexpr(pattern, x, perl = TRUE)) <<- lapply(
      regmatches(x, gregexpr(pattern, x, perl = TRUE)),
      function(matches) {
        if (length(matches) == 0) {
          return(matches)
        }
        vapply(matches, function(m) {
          number <- sub("^&#x?", "", sub(";$", "", m), ignore.case = TRUE)
          code <- suppressWarnings(strtoi(number, base = base))
          if (is.na(code)) m else intToUtf8(code)
        }, character(1))
      }
    )
  }
  decode_numeric("&#[0-9]+;", 10)
  decode_numeric("&#x[0-9A-Fa-f]+;", 16)
  x
}

materials_read_qsf <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package `jsonlite` is required to read Qualtrics .qsf files.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("QSF file not found: ", path, call. = FALSE)
  }
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

materials_qsf_blocks <- function(qsf, study = NA_character_, source_path = NA_character_) {
  block_element <- Filter(function(x) identical(x$Element, "BL"), qsf$SurveyElements)
  if (length(block_element) == 0) {
    return(materials_empty_df(c(
      "study", "source_path", "survey_id", "block_id", "block_position",
      "block_type", "block_description", "question_id", "question_position"
    )))
  }

  blocks <- block_element[[1]]$Payload
  rows <- list()
  row_i <- 1
  block_names <- names(blocks)

  for (block_i in seq_along(blocks)) {
    block <- blocks[[block_i]]
    elements <- block$BlockElements %||% list()
    question_position <- 0

    for (element in elements) {
      if (!identical(element$Type %||% "", "Question")) {
        next
      }
      question_position <- question_position + 1
      rows[[row_i]] <- data.frame(
        study = study,
        source_path = source_path,
        survey_id = qsf$SurveyEntry$SurveyID %||% NA_character_,
        block_id = block$ID %||% block_names[[block_i]] %||% NA_character_,
        block_position = block_i,
        block_type = block$Type %||% NA_character_,
        block_description = materials_strip_html(block$Description %||% ""),
        question_id = element$QuestionID %||% NA_character_,
        question_position = question_position,
        stringsAsFactors = FALSE
      )
      row_i <- row_i + 1
    }
  }

  if (length(rows) == 0) {
    materials_empty_df(c(
      "study", "source_path", "survey_id", "block_id", "block_position",
      "block_type", "block_description", "question_id", "question_position"
    ))
  } else {
    do.call(rbind, rows)
  }
}

materials_qsf_questions <- function(qsf, study = NA_character_, source_path = NA_character_) {
  survey_questions <- Filter(function(x) identical(x$Element, "SQ"), qsf$SurveyElements)
  blocks <- materials_qsf_blocks(qsf, study = study, source_path = source_path)
  block_lookup <- if (nrow(blocks) == 0) {
    NULL
  } else {
    blocks[!duplicated(blocks$question_id), , drop = FALSE]
  }

  rows <- lapply(seq_along(survey_questions), function(i) {
    question <- survey_questions[[i]]
    payload <- question$Payload %||% list()
    qid <- payload$QuestionID %||% question$PrimaryAttribute %||% NA_character_
    data.frame(
      study = study,
      source_path = source_path,
      survey_id = qsf$SurveyEntry$SurveyID %||% NA_character_,
      survey_name = qsf$SurveyEntry$SurveyName %||% NA_character_,
      question_order = i,
      question_id = qid,
      data_export_tag = payload$DataExportTag %||% NA_character_,
      question_description = materials_strip_html(payload$QuestionDescription %||% ""),
      question_type = payload$QuestionType %||% NA_character_,
      selector = payload$Selector %||% NA_character_,
      sub_selector = payload$SubSelector %||% NA_character_,
      question_text = materials_strip_html(payload$QuestionText %||% ""),
      question_text_raw = as.character(payload$QuestionText %||% ""),
      force_response = materials_nested_value(
        payload,
        c("Validation", "Settings", "ForceResponse")
      ) %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })

  out <- if (length(rows) == 0) {
    materials_empty_df(c(
      "study", "source_path", "survey_id", "survey_name", "question_order",
      "question_id", "data_export_tag", "question_description", "question_type",
      "selector", "sub_selector", "question_text", "question_text_raw",
      "force_response"
    ))
  } else {
    do.call(rbind, rows)
  }

  if (!is.null(block_lookup) && nrow(out) > 0) {
    keep <- c(
      "question_id", "block_id", "block_position", "block_type",
      "block_description", "question_position"
    )
    out <- merge(
      out,
      block_lookup[, keep, drop = FALSE],
      by = "question_id",
      all.x = TRUE,
      sort = FALSE
    )
    out <- out[order(out$question_order), , drop = FALSE]
    rownames(out) <- NULL
  }

  out
}

materials_qsf_choices <- function(qsf, study = NA_character_, source_path = NA_character_) {
  survey_questions <- Filter(function(x) identical(x$Element, "SQ"), qsf$SurveyElements)
  sections <- c("Choices", "Answers", "Labels")
  rows <- list()
  row_i <- 1

  for (question in survey_questions) {
    payload <- question$Payload %||% list()
    qid <- payload$QuestionID %||% question$PrimaryAttribute %||% NA_character_

    for (section in sections) {
      items <- payload[[section]]
      if (is.null(items) || length(items) == 0) {
        next
      }
      item_names <- names(items)
      if (is.null(item_names)) {
        item_names <- as.character(seq_along(items))
      }

      for (i in seq_along(items)) {
        item <- items[[i]]
        choice_text <- if (is.list(item)) {
          item$Display %||% item$Description %||% item$Text %||% item$Label %||% ""
        } else {
          item
        }
        rows[[row_i]] <- data.frame(
          study = study,
          source_path = source_path,
          survey_id = qsf$SurveyEntry$SurveyID %||% NA_character_,
          question_id = qid,
          data_export_tag = payload$DataExportTag %||% NA_character_,
          section = section,
          choice_id = item_names[[i]],
          choice_order = i,
          choice_text = materials_strip_html(choice_text),
          choice_text_raw = as.character(choice_text %||% ""),
          text_entry = as.character(materials_list_value(item, "TextEntry") %||% NA_character_),
          stringsAsFactors = FALSE
        )
        row_i <- row_i + 1
      }
    }
  }

  if (length(rows) == 0) {
    materials_empty_df(c(
      "study", "source_path", "survey_id", "question_id", "data_export_tag",
      "section", "choice_id", "choice_order", "choice_text", "choice_text_raw",
      "text_entry"
    ))
  } else {
    do.call(rbind, rows)
  }
}

materials_extract_qsf <- function(path, study = NA_character_) {
  qsf <- materials_read_qsf(path)
  list(
    questions = materials_qsf_questions(qsf, study = study, source_path = path),
    choices = materials_qsf_choices(qsf, study = study, source_path = path),
    blocks = materials_qsf_blocks(qsf, study = study, source_path = path)
  )
}

materials_extract_surveys <- function(root = ".", key_qids = NULL) {
  manifest <- materials_survey_manifest(root)
  materials_check_manifest_files(manifest)

  extracted <- lapply(seq_len(nrow(manifest)), function(i) {
    materials_extract_qsf(manifest$path[[i]], study = manifest$study[[i]])
  })

  questions <- do.call(rbind, lapply(extracted, `[[`, "questions"))
  choices <- do.call(rbind, lapply(extracted, `[[`, "choices"))
  blocks <- do.call(rbind, lapply(extracted, `[[`, "blocks"))

  if (!is.null(key_qids)) {
    questions$is_key_item <- questions$question_id %in% key_qids
    choices$is_key_item <- choices$question_id %in% key_qids
  }

  list(
    manifest = manifest,
    questions = questions,
    choices = choices,
    blocks = blocks
  )
}

materials_key_item_wording <- function(root = ".", qids = NULL, export_tags = NULL) {
  surveys <- materials_extract_surveys(root)
  questions <- surveys$questions
  if (!is.null(qids)) {
    questions <- questions[questions$question_id %in% qids, , drop = FALSE]
  }
  if (!is.null(export_tags)) {
    questions <- questions[questions$data_export_tag %in% export_tags, , drop = FALSE]
  }
  questions[, c(
    "study", "survey_name", "question_id", "data_export_tag",
    "block_description", "question_type", "selector", "question_text"
  ), drop = FALSE]
}

materials_read_api_strings <- function(root = ".") {
  manifest <- materials_api_manifest(root)
  materials_check_manifest_files(manifest)
  string_manifest <- manifest[manifest$extractor %in% c(
    "python_literal",
    "python_return_triple",
    "file_text"
  ), , drop = FALSE]

  rows <- lapply(seq_len(nrow(string_manifest)), function(i) {
    row <- string_manifest[i, , drop = FALSE]
    text <- switch(
      row$extractor,
      python_literal = materials_extract_python_literal(row$path, row$object_name),
      python_return_triple = materials_extract_python_return_triple(row$path, row$object_name),
      file_text = paste(readLines(row$path, warn = FALSE), collapse = "\n"),
      stop("Unknown string extractor: ", row$extractor, call. = FALSE)
    )
    data.frame(
      material_id = row$material_id,
      material_type = row$material_type,
      relative_path = row$relative_path,
      path = row$path,
      object_name = row$object_name,
      text = text,
      n_chars = nchar(text, type = "chars", allowNA = TRUE),
      n_lines = length(strsplit(text, "\n", fixed = TRUE)[[1]]),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

materials_read_claim_codebook <- function(root = ".") {
  manifest <- materials_api_manifest(root)
  row <- manifest[manifest$material_id == "claim_category_v2_codebook", , drop = FALSE]
  materials_check_manifest_files(row)
  out <- utils::read.csv(row$path, stringsAsFactors = FALSE, check.names = FALSE)
  out$source_path <- row$path
  out
}

materials_read_message_template <- function(root = ".") {
  manifest <- materials_api_manifest(root)
  row <- manifest[manifest$material_id == "messages_template", , drop = FALSE]
  materials_check_manifest_files(row)
  paste(readLines(row$path, warn = FALSE), collapse = "\n")
}

materials_read_api_tables <- function(root = ".") {
  list(
    manifest = materials_api_manifest(root),
    prompts = materials_read_api_strings(root),
    claim_codebook = materials_read_claim_codebook(root),
    messages_template = materials_read_message_template(root)
  )
}

materials_extract_python_literal <- function(path, object_name) {
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")
  assignment_start <- regexpr(
    paste0("(?m)^", object_name, "\\s*=\\s*"),
    source,
    perl = TRUE
  )
  if (assignment_start < 0) {
    stop("Could not find Python object `", object_name, "` in ", path, call. = FALSE)
  }

  rest <- substring(source, assignment_start + attr(assignment_start, "match.length"))
  rest_trimmed <- sub("^\\s+", "", rest)
  if (grepl('^"""', rest_trimmed)) {
    return(materials_extract_triple_body(rest_trimmed, '"""'))
  }
  if (grepl("^'''", rest_trimmed)) {
    return(materials_extract_triple_body(rest_trimmed, "'''"))
  }
  if (startsWith(rest_trimmed, "(")) {
    block <- materials_extract_balanced_block(rest_trimmed, "(", ")")
    return(materials_concat_python_adjacent_strings(block))
  }

  stop(
    "Unsupported Python literal shape for `",
    object_name,
    "` in ",
    path,
    call. = FALSE
  )
}

materials_extract_python_return_triple <- function(path, function_name) {
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")
  function_start <- regexpr(
    paste0("(?m)^def\\s+", function_name, "\\s*\\("),
    source,
    perl = TRUE
  )
  if (function_start < 0) {
    stop("Could not find Python function `", function_name, "` in ", path, call. = FALSE)
  }
  function_source <- substring(source, function_start)
  return_start <- regexpr("return\\s+f?([rubfRUBF]*)(\"\"\"|''')", function_source, perl = TRUE)
  if (return_start < 0) {
    stop(
      "Could not find a triple-quoted return in Python function `",
      function_name,
      "` in ",
      path,
      call. = FALSE
    )
  }
  return_source <- substring(function_source, return_start)
  quote_match <- regexpr("(\"\"\"|''')", return_source, perl = TRUE)
  quote <- regmatches(return_source, quote_match)
  materials_extract_triple_body(substring(return_source, quote_match), quote)
}

materials_extract_triple_body <- function(x, quote) {
  if (!startsWith(x, quote)) {
    stop("Internal parser error: triple-quoted string did not start with quote.", call. = FALSE)
  }
  body_start <- nchar(quote) + 1
  rest <- substring(x, body_start)
  end <- regexpr(quote, rest, fixed = TRUE)
  if (end < 0) {
    stop("Unterminated triple-quoted Python string.", call. = FALSE)
  }
  substring(rest, 1, end - 1)
}

materials_extract_balanced_block <- function(x, opener = "(", closer = ")") {
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  depth <- 0
  in_single <- FALSE
  in_double <- FALSE
  escaped <- FALSE

  for (i in seq_along(chars)) {
    ch <- chars[[i]]
    prev <- if (i > 1) chars[[i - 1]] else ""
    next2 <- if (i + 2 <= length(chars)) paste0(chars[i:(i + 2)], collapse = "") else ""

    if (escaped) {
      escaped <- FALSE
      next
    }
    if ((in_single || in_double) && ch == "\\") {
      escaped <- TRUE
      next
    }
    if (!in_double && next2 == "'''") {
      in_single <- !in_single
      next
    }
    if (!in_single && next2 == '"""') {
      in_double <- !in_double
      next
    }
    if (!in_double && ch == "'") {
      in_single <- !in_single
      next
    }
    if (!in_single && ch == '"') {
      in_double <- !in_double
      next
    }
    if (!in_single && !in_double && ch == opener) {
      depth <- depth + 1
    }
    if (!in_single && !in_double && ch == closer) {
      depth <- depth - 1
      if (depth == 0) {
        return(paste0(chars[seq_len(i)], collapse = ""))
      }
    }
  }
  stop("Unbalanced Python literal block.", call. = FALSE)
}

materials_concat_python_adjacent_strings <- function(block) {
  lines <- strsplit(block, "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)
  lines <- lines[!lines %in% c("(", ")")]
  lines <- lines[grepl('^([rubfRUBF]*)"', lines) | grepl("^([rubfRUBF]*)'", lines)]
  if (length(lines) == 0) {
    return("")
  }
  pieces <- vapply(lines, materials_parse_python_simple_string, character(1))
  paste0(pieces, collapse = "")
}

materials_parse_python_simple_string <- function(x) {
  x <- trimws(x)
  x <- sub("^[rubfRUBF]*", "", x)
  quote <- substring(x, 1, 1)
  if (!quote %in% c("'", "\"")) {
    return(x)
  }
  end <- regexpr(paste0("(?<!\\\\)", quote), substring(x, 2), perl = TRUE)
  if (end < 0) {
    return(x)
  }
  body <- substring(x, 2, end)
  body <- gsub("\\\\n", "\n", body, fixed = TRUE)
  body <- gsub("\\\\t", "\t", body, fixed = TRUE)
  body <- gsub(paste0("\\\\", quote), quote, body, fixed = TRUE)
  body <- gsub("\\\\\\\\", "\\\\", body, fixed = TRUE)
  body
}

materials_nested_value <- function(x, keys) {
  value <- x
  for (key in keys) {
    if (!is.list(value) || is.null(value[[key]])) {
      return(NULL)
    }
    value <- value[[key]]
  }
  value
}

materials_list_value <- function(x, key) {
  if (is.list(x)) x[[key]] else NULL
}

materials_empty_df <- function(cols) {
  out <- as.data.frame(setNames(replicate(length(cols), character(0), simplify = FALSE), cols))
  out
}

# Verbatim persuader / APE-evaluator prompts, read directly from the deployed
# prompt file so the appendix reproduces the exact deployed text (not a manual
# transcription). Source: prompts/base_v7_persuasion_degree.yaml.
materials_read_persuader_prompts <- function(root = ".") {
  si_require(c("yaml", "tibble"))
  f <- file.path(root, "prompts",
                 "base_v7_persuasion_degree.yaml")
  pick <- c(
    bunk            = "prompt_human_bunk_assistant",
    debunk          = "prompt_human_debunk_assistant",
    bunk_no_lying   = "prompt_human_bunk_assistant_no_lying",
    debunk_no_lying = "prompt_human_debunk_assistant_no_lying",
    evaluator       = "evaluate_prompt_2",
    strongreject    = "strongreject_rubric_prompt"
  )
  labels <- c(
    bunk            = "Bunking persuader prompt (Studies 1, 2, 4)",
    debunk          = "Debunking persuader prompt (Studies 1, 2, 4)",
    bunk_no_lying   = "Truth-constrained bunking persuader prompt (Study 3)",
    debunk_no_lying = "Truth-constrained debunking persuader prompt (Study 3)",
    evaluator       = "Attempt-to-persuade (APE) binary evaluator prompt",
    strongreject    = "StrongREJECT refusal rubric"
  )
  if (!file.exists(f)) {
    stop("Persuader-prompt YAML not found at: ", f,
         " (Appendix A cannot be reproduced without it).", call. = FALSE)
  }
  # Read with an EXPLICIT UTF-8 connection so the multibyte prompt text (curly
  # quotes, em-dashes, etc.) parses identically regardless of the render locale.
  # yaml::read_yaml() reads via readLines() under the session locale, which
  # silently drops keys when the locale is not UTF-8 (e.g. a bare `R CMD` render).
  con <- file(f, encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  txt <- paste(readLines(con, warn = FALSE), collapse = "\n")
  y <- yaml::yaml.load(txt)$prompts
  out <- tibble::tibble(
    key   = names(pick),
    label = unname(labels[names(pick)]),
    text  = vapply(pick, function(k) {
      v <- y[[k]]; if (is.null(v)) NA_character_ else trimws(v)
    }, character(1))
  )
  # Hard error if any verbatim prompt failed to load: a blank Appendix A is a
  # silent reproducibility failure, so fail the build loudly instead.
  missing <- out$key[is.na(out$text) | !nzchar(out$text)]
  if (length(missing)) {
    stop("Persuader-prompt YAML parsed but these prompts are missing/empty: ",
         paste(missing, collapse = ", "),
         " (likely a non-UTF-8 read of ", basename(f), ").", call. = FALSE)
  }
  out
}
