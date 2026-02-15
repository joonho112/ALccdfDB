#' @title Program Module: Configuration
#' @description Configure a program data processing pipeline for DHR child care
#'   program data (Licensed Centers, Family/Group Homes, Faith-Based Exempt,
#'   License Excepted).
#' @name program-config
NULL

#' Configure a program data processing pipeline
#'
#' Sets up paths and parameters for processing program-level data from a DHR
#' snapshot. At least one program file path must be provided.
#'
#' @param snapshot_date Date of the administrative snapshot (Date or character
#'   parseable to Date via \code{as.Date()})
#' @param center_path Path to Licensed Day Care Center Excel file (NULL if not
#'   available)
#' @param home_path Path to Family and Group Home Excel file (NULL if not
#'   available)
#' @param exempt_path Path to Exempt Center (Faith-Based) Excel file (NULL if
#'   not available)
#' @param excepted_path Path to License Excepted Programs Excel file (NULL if
#'   not available)
#' @param output_dir Output directory for exports (auto-generated from
#'   snapshot_date if NULL)
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_program_config} S3 object containing paths,
#'   snapshot_date, and pipeline parameters.
#'
#' @examples
#' \dontrun{
#' cfg <- program_config(
#'   snapshot_date = "2025-06-11",
#'   center_path   = "data/Centers_06_11_25.xlsx",
#'   home_path     = "data/FamilyHomes_06_11_25.xlsx"
#' )
#' cfg
#' }
#'
#' @export
program_config <- function(snapshot_date,
                           center_path = NULL,
                           home_path = NULL,
                           exempt_path = NULL,
                           excepted_path = NULL,
                           output_dir = NULL,
                           verbose = TRUE) {
  # --- Parse snapshot_date ----
  if (is.character(snapshot_date)) {
    snapshot_date <- as.Date(snapshot_date)
  }
  if (!inherits(snapshot_date, "Date") || is.na(snapshot_date)) {
    cli::cli_abort("{.arg snapshot_date} must be a valid Date or date string.")
  }

  # --- Validate file paths ----
  paths <- list(
    center      = center_path,
    family_home = home_path,
    exempt      = exempt_path,
    excepted    = excepted_path
  )

  for (nm in names(paths)) {
    if (!is.null(paths[[nm]])) {
      if (!file.exists(paths[[nm]])) {
        cli::cli_abort("File not found for {.field {nm}}: {.path {paths[[nm]]}}")
      }
    }
  }

  # --- At least one path required ----
  provided <- !vapply(paths, is.null, logical(1))
  if (!any(provided)) {
    cli::cli_abort("At least one program file path must be provided.")
  }

  # --- Default output directory ----
  if (is.null(output_dir)) {
    output_dir <- file.path("output", format(snapshot_date, "%Y-%m-%d"))
  }

  # --- Build config object ----
  config <- structure(
    list(
      snapshot_date   = snapshot_date,
      paths           = paths,
      output_dir      = output_dir,
      verbose         = verbose,
      types_available = names(paths)[provided]
    ),
    class = "alccdf_program_config"
  )

  if (verbose) {
    .msg_header("Program Configuration", level = 2)
    .msg_step("Snapshot date: {snapshot_date}")
    .msg_step("Types available: {paste(config$types_available, collapse = ', ')}")
    for (nm in config$types_available) {
      .msg_step("  {nm}: {paths[[nm]]}")
    }
  }

  config
}


#' Print method for program configuration objects
#'
#' @param x An \code{alccdf_program_config} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_program_config <- function(x, ...) {
  cli::cli_h2("ALccdfDB Program Configuration")
  cli::cli_alert_info("Snapshot: {x$snapshot_date}")
  cli::cli_alert_info("Types: {paste(x$types_available, collapse = ', ')}")
  for (nm in x$types_available) {
    cli::cli_alert_info("  {nm}: {x$paths[[nm]]}")
  }

  cli::cli_alert_info("Output: {x$output_dir}")
  invisible(x)
}
