#' @title Linkage Module: Report
#' @description Generate a markdown report summarising linkage results across
#'   all linked datasets. Reports match rates, unmatched analyses, and a
#'   cross-linkage summary table.
#' @name linkage-report
NULL

#' Generate linkage report
#'
#' Takes a named list of linked objects and generates a markdown report
#' summarising match statistics, unmatched analyses, and an overall comparison
#' table across all linkages.
#'
#' @param linked_objects A named list of linked S3 objects. Expected names
#'   include any combination of:
#'   \describe{
#'     \item{programs_enrolled}{An \code{alccdf_linked_programs_enrolled} object}
#'     \item{programs_clients}{An \code{alccdf_linked_programs_clients} object}
#'     \item{programs_staff}{An \code{alccdf_linked_programs_staff} object}
#'     \item{clients_programs}{An \code{alccdf_linked_clients_programs} object}
#'   }
#' @param output_dir Directory where the report file will be written.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible character string: the path to the generated report file.
#'
#' @details
#' The report includes:
#' \itemize{
#'   \item A header with generation timestamp and package version
#'   \item For each linked dataset:
#'     \itemize{
#'       \item Match key used
#'       \item Match count and match rate
#'       \item Unmatched record summary
#'     }
#'   \item A summary comparison table across all linkages
#' }
#'
#' @examples
#' \dontrun{
#' linked <- list(
#'   programs_enrolled = linkage_programs_enrolled(prog, enrolled),
#'   programs_clients  = linkage_programs_clients(prog, clients),
#'   programs_staff    = linkage_programs_staff(prog, staff),
#'   clients_programs  = linkage_clients_programs(clients, prog)
#' )
#' report_path <- linkage_report(linked, output_dir = "output/reports")
#' }
#'
#' @export
linkage_report <- function(linked_objects, output_dir, verbose = TRUE) {
  # --- Validate inputs ----
  if (!is.list(linked_objects) || length(linked_objects) == 0) {
    cli::cli_abort("Expected a non-empty named list of linked objects.")
  }

  if (is.null(names(linked_objects)) || any(names(linked_objects) == "")) {
    cli::cli_abort("All elements in {.arg linked_objects} must be named.")
  }

  if (verbose) {
    .msg_header("Generating Linkage Report", level = 2)
  }

  # --- Create output directory ----
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  report_path <- file.path(output_dir, "linkage_report.md")

  # --- Build report content ----
  lines <- character()

  # Header
  lines <- c(lines,
    "# ALccdfDB Linkage Report",
    "",
    paste0("**Generated:** ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("**Package version:** ",
           as.character(utils::packageVersion("ALccdfDB"))),
    paste0("**Linkages included:** ", length(linked_objects)),
    ""
  )

  # --- Per-linkage sections ----
  summary_rows <- list()

  for (name in names(linked_objects)) {
    obj <- linked_objects[[name]]

    if (verbose) .msg_step("Processing linkage: {name}")

    meta <- obj$meta
    diag <- obj$diagnostics

    # Extract common fields from meta
    join_key   <- meta$join_key %||% "unknown"
    join_type  <- meta$join_type %||% "unknown"
    backbone   <- meta$backbone %||% "unknown"
    n_rows     <- meta$n_rows %||% NA_integer_
    n_matched  <- meta$n_matched %||% NA_integer_
    match_rate <- meta$match_rate %||% NA_real_

    # Section header
    lines <- c(lines,
      paste0("## ", .linkage_display_name(name)),
      ""
    )

    # Linkage details table
    lines <- c(lines,
      "| Property | Value |",
      "|:---------|:------|",
      paste0("| **Join type** | ", join_type, " |"),
      paste0("| **Join key** | `", join_key, "` |"),
      paste0("| **Backbone** | ", backbone, " |"),
      paste0("| **Result rows** | ", format(n_rows, big.mark = ","), " |"),
      paste0("| **Matched** | ", format(n_matched, big.mark = ","), " |"),
      paste0("| **Match rate** | ", match_rate, "% |"),
      ""
    )

    # Unmatched analysis
    lines <- c(lines, "### Unmatched Analysis", "")

    # Extract unmatched counts from diagnostics
    if (!is.null(diag)) {
      unmatched_items <- .extract_unmatched_info(name, diag)
      if (length(unmatched_items) > 0) {
        for (item in unmatched_items) {
          lines <- c(lines, paste0("- ", item))
        }
        lines <- c(lines, "")
      } else {
        lines <- c(lines, "No unmatched records detected.", "")
      }
    } else {
      lines <- c(lines, "No diagnostics available.", "")
    }

    # Collect summary row
    summary_rows[[name]] <- tibble::tibble(
      linkage    = .linkage_display_name(name),
      join_key   = join_key,
      backbone   = backbone,
      n_rows     = n_rows,
      n_matched  = n_matched,
      match_rate = paste0(match_rate, "%")
    )
  }

  # --- Summary table across all linkages ----
  lines <- c(lines,
    "## Summary: All Linkages",
    ""
  )

  if (length(summary_rows) > 0) {
    summary_df <- dplyr::bind_rows(summary_rows)

    # Build markdown table
    lines <- c(lines,
      "| Linkage | Join Key | Backbone | Result Rows | Matched | Match Rate |",
      "|:--------|:---------|:---------|------------:|--------:|-----------:|"
    )

    for (i in seq_len(nrow(summary_df))) {
      row <- summary_df[i, ]
      lines <- c(lines, paste0(
        "| ", row$linkage,
        " | `", row$join_key, "`",
        " | ", row$backbone,
        " | ", format(row$n_rows, big.mark = ","),
        " | ", format(row$n_matched, big.mark = ","),
        " | ", row$match_rate,
        " |"
      ))
    }

    lines <- c(lines, "")
  }

  # Footer
  lines <- c(lines,
    "---",
    "",
    paste0("*Report generated by ALccdfDB v",
           as.character(utils::packageVersion("ALccdfDB")), "*"),
    ""
  )

  # --- Write report ----
  writeLines(lines, report_path)

  if (verbose) {
    .msg_success("Linkage report written to {.path {report_path}}")
    .msg_step("Report covers {length(linked_objects)} linkage(s)")
  }

  invisible(report_path)
}


# ---- Internal helpers --------------------------------------------------------

#' Generate a display name for a linkage
#'
#' @param name Internal linkage name (e.g., "programs_enrolled")
#' @return Character string display name
#' @keywords internal
.linkage_display_name <- function(name) {
  display_map <- list(
    programs_enrolled = "Programs x Enrolled Children",
    programs_clients  = "Programs x Subsidy Clients",
    programs_staff    = "Programs x Staff",
    clients_programs  = "Clients x Programs"
  )

  if (name %in% names(display_map)) {
    display_map[[name]]
  } else {
    # Convert snake_case to Title Case
    gsub("_", " ", stringr::str_to_title(name))
  }
}


#' Extract unmatched information from diagnostics
#'
#' @param name Linkage name
#' @param diag Diagnostics list
#' @return Character vector of unmatched information lines
#' @keywords internal
.extract_unmatched_info <- function(name, diag) {
  items <- character()

  # Programs-enrolled
  if (name == "programs_enrolled") {
    if (!is.null(diag$n_unmatched_programs)) {
      items <- c(items, paste0(
        "**Programs with no enrolled children:** ",
        format(diag$n_unmatched_programs, big.mark = ",")
      ))
    }
    if (!is.null(diag$unmatched_enrolled) && nrow(diag$unmatched_enrolled) > 0) {
      items <- c(items, paste0(
        "**Enrolled records with unmatched facility_id:** ",
        format(nrow(diag$unmatched_enrolled), big.mark = ",")
      ))
    }
  }

  # Programs-clients
  if (name == "programs_clients") {
    if (!is.null(diag$n_unmatched_programs)) {
      items <- c(items, paste0(
        "**Programs with no active clients:** ",
        format(diag$n_unmatched_programs, big.mark = ",")
      ))
    }
    if (!is.null(diag$unmatched_clients) && nrow(diag$unmatched_clients) > 0) {
      items <- c(items, paste0(
        "**Client records with unmatched provider_id:** ",
        format(nrow(diag$unmatched_clients), big.mark = ",")
      ))
    }
  }

  # Programs-staff
  if (name == "programs_staff") {
    if (!is.null(diag$n_unmatched_programs)) {
      items <- c(items, paste0(
        "**Programs with no staff records:** ",
        format(diag$n_unmatched_programs, big.mark = ",")
      ))
    }
    if (!is.null(diag$unmatched_staff) && nrow(diag$unmatched_staff) > 0) {
      items <- c(items, paste0(
        "**Staff records with unmatched facility_name:** ",
        format(nrow(diag$unmatched_staff), big.mark = ",")
      ))
    }
  }

  # Clients-programs
  if (name == "clients_programs") {
    if (!is.null(diag$n_unmatched)) {
      items <- c(items, paste0(
        "**Client rows without a matching program:** ",
        format(diag$n_unmatched, big.mark = ",")
      ))
    }
    if (!is.null(diag$n_unmatched_client_ids)) {
      items <- c(items, paste0(
        "**Unique client provider_ids not in programs:** ",
        format(diag$n_unmatched_client_ids, big.mark = ",")
      ))
    }
    if (!is.null(diag$n_unmatched_program_ids)) {
      items <- c(items, paste0(
        "**Programs with no client records:** ",
        format(diag$n_unmatched_program_ids, big.mark = ",")
      ))
    }
  }

  # Generic fallback for unknown linkage types
  if (length(items) == 0) {
    # Try to extract any diagnostic with "unmatched" in the name
    unmatched_keys <- grep("unmatched", names(diag), value = TRUE)
    for (key in unmatched_keys) {
      val <- diag[[key]]
      if (is.numeric(val) || is.integer(val)) {
        label <- gsub("_", " ", gsub("^n_", "", key))
        items <- c(items, paste0(
          "**", stringr::str_to_title(label), ":** ",
          format(val, big.mark = ",")
        ))
      } else if (is.data.frame(val) && nrow(val) > 0) {
        label <- gsub("_", " ", key)
        items <- c(items, paste0(
          "**", stringr::str_to_title(label), ":** ",
          format(nrow(val), big.mark = ","), " records"
        ))
      }
    }
  }

  items
}
