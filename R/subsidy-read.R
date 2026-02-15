#' @title Subsidy Module: Read
#' @description Read and ingest DHR subsidy Excel files with automatic format
#'   version detection and column mapping. Supports two data types: Enrolled
#'   Children (Active And Enrolled Children Report) and Active Clients With
#'   Addresses.
#' @name subsidy-read
NULL

#' Read a single subsidy Excel file
#'
#' Reads a DHR subsidy Excel file (Enrolled Children or Active Clients With
#' Addresses), automatically detects the format version (legacy vs. current for
#' enrolled data), applies the appropriate column mapping, and returns a
#' standardised raw object.
#'
#' @param path Path to the Excel file
#' @param type Character string: one of \code{"enrolled"} or \code{"clients"}
#' @param snapshot_date Date of the administrative snapshot
#' @param sheet Sheet name or index to read (NULL for the first sheet)
#' @param skip Number of header rows to skip. If NULL, auto-detected:
#'   \code{"enrolled"} tries skip = 0 first (current format), falls back to
#'   skip = 2 (legacy); \code{"clients"} always skip = 2.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_subsidy_raw} S3 object
#'
#' @details
#' Format detection for enrolled data:
#' \itemize{
#'   \item \strong{Current (Version B)}: 20 columns with skip = 0. Contains
#'     "Parent ID/SSN" and "Child ID/SSN" columns.
#'   \item \strong{Legacy (Version A)}: 18 columns with skip = 2. Does NOT
#'     contain "Parent ID/SSN" or "Child ID/SSN" columns.
#' }
#'
#' For clients data, the format is always 14 columns with skip = 2.
#'
#' @examples
#' \dontrun{
#' raw <- subsidy_read(
#'   path = "data/ActiveEnrolledChildren_06_11_25.xlsx",
#'   type = "enrolled",
#'   snapshot_date = as.Date("2025-06-11")
#' )
#'
#' raw_clients <- subsidy_read(
#'   path = "data/ActiveClientsWithAddresses_06_11_25.xlsx",
#'   type = "clients",
#'   snapshot_date = as.Date("2025-06-11")
#' )
#' }
#'
#' @export
subsidy_read <- function(path,
                         type,
                         snapshot_date,
                         sheet = NULL,
                         skip = NULL,
                         verbose = TRUE) {
  # --- Validate type ----
  valid_types <- c("enrolled", "clients")
  if (!type %in% valid_types) {
    cli::cli_abort(
      "{.arg type} must be one of {.val {valid_types}}, not {.val {type}}."
    )
  }

  # --- Validate path ----
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }

  # --- Parse snapshot_date ----
  if (is.character(snapshot_date)) {
    snapshot_date <- as.Date(snapshot_date)
  }

  # --- Auto-detect skip ----
  if (is.null(skip)) {
    if (type == "clients") {
      skip <- 2L
    } else {
      # For enrolled, try skip = 0 first; will determine in format detection
      skip <- 0L
    }
  }

  if (verbose) {
    .msg_header("Reading Subsidy Data: {type}", level = 3)
    .msg_step("File: {path}")
  }

  # --- Read Excel ----
  if (type == "enrolled" && is.null(skip)) {
    # Already handled above; skip = 0L by default for enrolled
  }

  read_args <- list(path = path, skip = skip, col_types = "text")
  if (!is.null(sheet)) read_args$sheet <- sheet
  df <- do.call(readxl::read_excel, read_args)
  df <- tibble::as_tibble(df)

  if (verbose) {
    .msg_step("Skip: {skip} rows")
    .msg_step("Read {nrow(df)} rows x {ncol(df)} columns")
  }

  # --- Remove completely empty rows ----
  all_na <- apply(df, 1, function(x) all(is.na(x) | x == ""))
  if (any(all_na)) {
    df <- df[!all_na, ]
    if (verbose) .msg_step("Removed {sum(all_na)} empty rows")
  }

  # --- Detect format version ----
  format_version <- .detect_subsidy_format(df, type)

  # If enrolled auto-detected as legacy but user didn't specify skip,

  # re-read with skip = 2
  if (type == "enrolled" && format_version == "legacy" && skip == 0L) {
    if (verbose) .msg_step("Re-reading with skip = 2 for legacy format")
    skip <- 2L
    read_args$skip <- skip
    df <- do.call(readxl::read_excel, read_args)
    df <- tibble::as_tibble(df)

    # Remove empty rows again
    all_na <- apply(df, 1, function(x) all(is.na(x) | x == ""))
    if (any(all_na)) df <- df[!all_na, ]

    if (verbose) .msg_step("Re-read {nrow(df)} rows x {ncol(df)} columns")
  }

  if (verbose) .msg_step("Detected format: {format_version}")

  # --- Select column map file ----
  map_file <- .get_subsidy_column_map_file(type, format_version)
  if (verbose) .msg_step("Column map: {basename(map_file)}")

  # --- Apply column mapping ----
  df <- .apply_column_map(df, map_file, verbose = verbose)

  # --- Build S3 object ----
  obj <- .make_alccdf_obj(
    data           = df,
    class_name     = "alccdf_subsidy_raw",
    module         = "subsidy",
    stage          = "raw",
    snapshot_date  = snapshot_date,
    extra_meta     = list(
      subsidy_type   = type,
      format_version = format_version,
      source_file    = normalizePath(path, mustWork = FALSE),
      skip           = skip,
      sheet          = sheet %||% 1L
    )
  )
  obj <- .log_step(obj, glue::glue(
    "Read {type} subsidy data from {basename(path)} ({format_version}): ",
    "{nrow(df)} rows x {ncol(df)} cols"
  ))

  if (verbose) {
    .msg_success(
      "Subsidy read complete: {nrow(df)} rows, format = {format_version}"
    )
  }

  obj
}


# ---- Internal helpers --------------------------------------------------------

#' Detect subsidy format version
#'
#' For enrolled data, checks whether "Parent ID/SSN" column is present
#' (current format) or not (legacy format). For clients data, always returns
#' "current".
#'
#' @param df A data frame freshly read from Excel
#' @param type Subsidy type ("enrolled" or "clients")
#' @return Character string: \code{"legacy"} or \code{"current"}
#' @keywords internal
.detect_subsidy_format <- function(df, type) {
  if (type == "clients") {
    return("current")
  }

  # For enrolled: check if "Parent ID/SSN" column exists (case-insensitive)
  col_names_lower <- tolower(names(df))
  has_parent_ssn <- any(grepl("parent\\s*id.*ssn|parent\\s*ssn", col_names_lower))

  if (has_parent_ssn) "current" else "legacy"
}


#' Get the path to the appropriate subsidy column map CSV
#'
#' @param type Subsidy type ("enrolled" or "clients")
#' @param format_version \code{"legacy"} or \code{"current"}
#' @return Full path to the CSV file within \code{inst/extdata/mappings/}
#' @keywords internal
.get_subsidy_column_map_file <- function(type, format_version) {
  base_name <- switch(type,
    enrolled = "subsidy_enrolled_column_map",
    clients  = "subsidy_clients_column_map"
  )

  if (format_version == "legacy" && type == "enrolled") {
    base_name <- paste0(base_name, "_legacy")
  }

  filename <- paste0(base_name, ".csv")

  # Try installed package location first, then dev location
  pkg_path <- system.file("extdata", "mappings", filename,
                           package = "ALccdfDB")
  if (pkg_path != "") return(pkg_path)

  # Fallback for development: look relative to package root
  dev_path <- file.path("inst", "extdata", "mappings", filename)
  if (file.exists(dev_path)) return(dev_path)

  cli::cli_abort("Column map file not found: {.file {filename}}")
}


#' Print method for raw subsidy objects
#'
#' @param x An \code{alccdf_subsidy_raw} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_subsidy_raw <- function(x, ...) {
  cli::cli_h2("ALccdfDB Subsidy Data (Raw)")
  cli::cli_alert_info("Type: {x$meta$subsidy_type}")
  cli::cli_alert_info("Format: {x$meta$format_version}")
  cli::cli_alert_info("Snapshot: {x$meta$snapshot_date}")
  cli::cli_alert_info("Dimensions: {x$meta$n_rows} rows x {x$meta$n_cols} cols")
  cli::cli_alert_info("Source: {basename(x$meta$source_file)}")
  if (length(x$meta$processing_log) > 0) {
    cli::cli_h3("Processing Log")
    for (entry in x$meta$processing_log) {
      cli::cli_text(entry)
    }
  }
  invisible(x)
}
