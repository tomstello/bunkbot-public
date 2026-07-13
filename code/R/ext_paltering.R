# ext_paltering.R ------------------------------------------------------------------------
# Per-conversation (aligned-claim veracity, direction-aligned belief change) points for
# COMPLIANT BUNKING conversations, pooled across Studies 1-4, for the paltering figure.
# Precomputed at build time (uses pkg_paths/read_claim_labels/conv_aligned_veracity from
# bunkbot_helpers.R) so the figure can read it from all_numbers at render time, rather than
# re-reading raw claim labels (the SI render does not source bunkbot_helpers.R).
#
# block "paltering"; one row per compliant-bunking conversation:
#   model = study label; direction = "bunk"; term = "paltering_point";
#   estimate = direction-aligned belief change; statistic = conversation-average aligned
#   claim veracity (0-100); n = number of aligned claims.
compute_paltering <- function(core_objects) {
  si_require(c("dplyr", "tibble"))
  paths <- pkg_paths(core_objects$pkg_root)
  conv <- conv_aligned_veracity(read_claim_labels(paths)) |>
    dplyr::select(conversation_id, direction, n_aligned, aligned_veracity)

  s13 <- core_objects$s13 |>
    dplyr::filter(condition_factor == "Bunking", compliant %in% c(TRUE, 1, "TRUE")) |>
    dplyr::transmute(conversation_id = response_id,
                     study = dplyr::recode(as.character(study_factor),
                       Jailbroken = "Study 1", Standard = "Study 2", `Truth-Constrained` = "Study 3"),
                     direction = "bunk", delta = change)
  s4 <- core_objects$s4$s4_compliant |>
    dplyr::filter(direction == "bunk") |>
    dplyr::transmute(conversation_id = ResponseId, study = "Study 4",
                     direction = "bunk", delta = aligned_belief_change)

  pal <- dplyr::bind_rows(s13, s4) |>
    dplyr::inner_join(conv, by = c("conversation_id", "direction")) |>
    dplyr::filter(.data$n_aligned >= 1, is.finite(.data$delta), is.finite(.data$aligned_veracity))

  if (!nrow(pal))
    return(std_row(tibble::tibble(model = character()), "Veracity", "paltering", "compliant"))

  pal |>
    dplyr::transmute(model = .data$study, direction = "bunk", term = "paltering_point",
                     n = .data$n_aligned, estimate = .data$delta, statistic = .data$aligned_veracity,
                     note = "Compliant bunking conversation; estimate = direction-aligned belief change, statistic = conversation-average aligned-claim veracity.") |>
    std_row("Veracity", "paltering", "compliant")
}
