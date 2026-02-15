#' @title Melissa Module: Import
#' @description Import post-processed Melissa geocoding result files (programs
#'   and subsidy households) with automatic column mapping and coordinate
#'   validation.
#' @name melissa-import
NULL

#' Import Melissa geocoding results for programs
#'
#' Reads a post-processed Melissa geocoding Excel file for child care programs,
#' applies the \code{melissa_column_map.csv} column mapping, converts coordinate
#' fields to numeric, and validates that coordinates fall within Alabama bounds.
#'
#' @param path Path to the Melissa program geocoding Excel file
#'   (e.g., "program_geocoded_by_melissa.xlsx")
#' @param snapshot_date Date of the geocoding run (Date or character parseable
#'   via \code{as.Date()})
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_melissa_programs} S3 object
#'
#' @examples
#' \dontrun{
#' mel <- melissa_import_programs(
#'   path = "data/program_geocoded_by_melissa.xlsx",
#'   snapshot_date = "2025-09-24"
#' )
#' mel
#' }
#'
#' @export
melissa_import_programs <- function(path, snapshot_date, verbose = TRUE) {
  # --- Validate path ----
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }

  # --- Parse snapshot_date ----
  if (is.character(snapshot_date)) {
    snapshot_date <- as.Date(snapshot_date)
  }

  if (verbose) {
    .msg_header("Importing Melissa Program Geocoding", level = 3)
    .msg_step("File: {path}")
  }

  # --- Read Excel ----
  df <- readxl::read_excel(path, col_types = "text")
  df <- tibble::as_tibble(df)

  if (verbose) {
    .msg_step("Read {nrow(df)} rows x {ncol(df)} columns")
  }

  # --- Remove completely empty rows ----
  all_na <- apply(df, 1, function(x) all(is.na(x) | x == ""))
  if (any(all_na)) {
    df <- df[!all_na, ]
    if (verbose) .msg_step("Removed {sum(all_na)} empty rows")
  }

  # --- Apply column mapping ----
  map_file <- .get_melissa_column_map_file()
  if (verbose) .msg_step("Column map: {basename(map_file)}")
  df <- .apply_column_map(df, map_file, verbose = verbose)

  # --- Convert coordinates to numeric ----
  df <- .melissa_convert_coordinates(df, verbose = verbose)

  # --- Filter to program source if applicable ----
  if ("source" %in% names(df)) {
    program_rows <- tolower(df$source) == "program"
    if (any(program_rows) && !all(program_rows)) {
      n_before <- nrow(df)
      df <- df[program_rows, ]
      if (verbose) {
        .msg_step("Filtered to source='program': {nrow(df)} of {n_before} rows")
      }
    }
  }

  # --- Build S3 object ----
  obj <- .make_alccdf_obj(
    data          = df,
    class_name    = "alccdf_melissa_programs",
    module        = "melissa",
    stage         = "imported",
    snapshot_date = snapshot_date,
    extra_meta    = list(
      data_type   = "programs",
      source_file = normalizePath(path, mustWork = FALSE)
    )
  )
  obj <- .log_step(obj, glue::glue(
    "Imported Melissa program geocoding from {basename(path)}: ",
    "{nrow(df)} rows x {ncol(df)} cols"
  ))

  if (verbose) {
    .msg_success(
      "Melissa program import complete: {nrow(df)} rows"
    )
  }

  obj
}


#' Import Melissa geocoding results for subsidy households
#'
#' Reads a post-processed Melissa geocoding Excel file for subsidy household
#' addresses, applies the \code{melissa_column_map.csv} column mapping,
#' converts coordinate fields to numeric, and validates Alabama bounds.
#'
#' @param path Path to the Melissa household geocoding Excel file
#'   (e.g., "subsidy_households_geocoded_by_melissa.xlsx")
#' @param snapshot_date Date of the geocoding run (Date or character parseable
#'   via \code{as.Date()})
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_melissa_households} S3 object
#'
#' @examples
#' \dontrun{
#' mel_hh <- melissa_import_households(
#'   path = "data/subsidy_households_geocoded_by_melissa.xlsx",
#'   snapshot_date = "2025-09-24"
#' )
#' mel_hh
#' }
#'
#' @export
melissa_import_households <- function(path, snapshot_date, verbose = TRUE) {
  # --- Validate path ----
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }

  # --- Parse snapshot_date ----
  if (is.character(snapshot_date)) {
    snapshot_date <- as.Date(snapshot_date)
  }

  if (verbose) {
    .msg_header("Importing Melissa Household Geocoding", level = 3)
    .msg_step("File: {path}")
  }

  # --- Read Excel ----
  df <- readxl::read_excel(path, col_types = "text")
  df <- tibble::as_tibble(df)

  if (verbose) {
    .msg_step("Read {nrow(df)} rows x {ncol(df)} columns")
  }

  # --- Remove completely empty rows ----
  all_na <- apply(df, 1, function(x) all(is.na(x) | x == ""))
  if (any(all_na)) {
    df <- df[!all_na, ]
    if (verbose) .msg_step("Removed {sum(all_na)} empty rows")
  }

  # --- Apply column mapping ----
  map_file <- .get_melissa_column_map_file()
  if (verbose) .msg_step("Column map: {basename(map_file)}")
  df <- .apply_column_map(df, map_file, verbose = verbose)

  # --- Convert coordinates to numeric ----
  df <- .melissa_convert_coordinates(df, verbose = verbose)

  # --- Filter to household source if applicable ----
  if ("source" %in% names(df)) {
    household_rows <- tolower(df$source) == "household"
    if (any(household_rows) && !all(household_rows)) {
      n_before <- nrow(df)
      df <- df[household_rows, ]
      if (verbose) {
        .msg_step("Filtered to source='household': {nrow(df)} of {n_before} rows")
      }
    }
  }

  # --- Build S3 object ----
  obj <- .make_alccdf_obj(
    data          = df,
    class_name    = "alccdf_melissa_households",
    module        = "melissa",
    stage         = "imported",
    snapshot_date = snapshot_date,
    extra_meta    = list(
      data_type   = "households",
      source_file = normalizePath(path, mustWork = FALSE)
    )
  )
  obj <- .log_step(obj, glue::glue(
    "Imported Melissa household geocoding from {basename(path)}: ",
    "{nrow(df)} rows x {ncol(df)} cols"
  ))

  if (verbose) {
    .msg_success(
      "Melissa household import complete: {nrow(df)} rows"
    )
  }

  obj
}


# ---- Internal helpers --------------------------------------------------------

#' Get the path to the Melissa column map CSV
#'
#' @return Full path to melissa_column_map.csv
#' @keywords internal
.get_melissa_column_map_file <- function() {
  filename <- "melissa_column_map.csv"

  # Try installed package location first
  pkg_path <- system.file("extdata", "mappings", filename,
                           package = "ALccdfDB")
  if (pkg_path != "") return(pkg_path)

  # Fallback for development
  dev_path <- file.path("inst", "extdata", "mappings", filename)
  if (file.exists(dev_path)) return(dev_path)

  cli::cli_abort("Column map file not found: {.file {filename}}")
}


#' Convert Melissa coordinate columns to numeric
#'
#' @param df A data frame with latitude/longitude as character columns
#' @param verbose Logical; print progress messages?
#' @return Data frame with numeric latitude and longitude columns
#' @keywords internal
.melissa_convert_coordinates <- function(df, verbose = TRUE) {
  for (col in c("latitude", "longitude")) {
    if (col %in% names(df)) {
      original <- df[[col]]
      df[[col]] <- suppressWarnings(as.numeric(original))
      n_coerced <- sum(is.na(df[[col]]) & !is.na(original))
      if (n_coerced > 0 && verbose) {
        .msg_warn("{n_coerced} values in {.field {col}} could not be converted to numeric")
      }
    }
  }
  df
}


#' Print method for Melissa program objects
#'
#' @param x An \code{alccdf_melissa_programs} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_melissa_programs <- function(x, ...) {
  cli::cli_h2("ALccdfDB Melissa Geocoding (Programs)")
  cli::cli_alert_info("Snapshot: {x$meta$snapshot_date}")
  cli::cli_alert_info("Dimensions: {x$meta$n_rows} rows x {x$meta$n_cols} cols")
  cli::cli_alert_info("Source: {basename(x$meta$source_file)}")
  if ("latitude" %in% names(x$data)) {
    n_valid <- sum(!is.na(x$data$latitude))
    cli::cli_alert_info("Valid coordinates: {n_valid}/{x$meta$n_rows} ({round(n_valid/x$meta$n_rows*100, 1)}%)")
  }
  if (length(x$meta$processing_log) > 0) {
    cli::cli_h3("Processing Log")
    for (entry in x$meta$processing_log) {
      cli::cli_text(entry)
    }
  }
  invisible(x)
}


#' Print method for Melissa household objects
#'
#' @param x An \code{alccdf_melissa_households} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_melissa_households <- function(x, ...) {
  cli::cli_h2("ALccdfDB Melissa Geocoding (Households)")
  cli::cli_alert_info("Snapshot: {x$meta$snapshot_date}")
  cli::cli_alert_info("Dimensions: {x$meta$n_rows} rows x {x$meta$n_cols} cols")
  cli::cli_alert_info("Source: {basename(x$meta$source_file)}")
  if ("latitude" %in% names(x$data)) {
    n_valid <- sum(!is.na(x$data$latitude))
    cli::cli_alert_info("Valid coordinates: {n_valid}/{x$meta$n_rows} ({round(n_valid/x$meta$n_rows*100, 1)}%)")
  }
  if (length(x$meta$processing_log) > 0) {
    cli::cli_h3("Processing Log")
    for (entry in x$meta$processing_log) {
      cli::cli_text(entry)
    }
  }
  invisible(x)
}
