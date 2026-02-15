#' @title Program Module: Read
#' @description Read and ingest DHR program Excel files with automatic format
#'   version detection and column mapping.
#' @name program-read
NULL

#' Read a single program Excel file
#'
#' Reads a DHR program Excel file (Centers, Family/Group Homes, Exempt, or
#' Excepted), automatically detects the format version (legacy vs. current),
#' applies the appropriate column mapping, and returns a standardised raw
#' object.
#'
#' @param path Path to the Excel file
#' @param type Character string: one of \code{"center"}, \code{"family_home"},
#'   \code{"exempt"}, or \code{"excepted"}
#' @param snapshot_date Date of the administrative snapshot
#' @param sheet Sheet name or index to read (NULL for the first sheet)
#' @param skip Number of header rows to skip. If NULL, auto-detected from type:
#'   \code{"exempt"} = 0, all others = 2.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_program_raw} S3 object
#'
#' @examples
#' \dontrun{
#' raw <- program_read(
#'   path = "data/Centers_06_11_25.xlsx",
#'   type = "center",
#'   snapshot_date = as.Date("2025-06-11")
#' )
#' }
#'
#' @export
program_read <- function(path,
                         type,
                         snapshot_date,
                         sheet = NULL,
                         skip = NULL,
                         verbose = TRUE) {
  # --- Validate type ----
  valid_types <- c("center", "family_home", "exempt", "excepted")
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
    skip <- if (type == "exempt") 0L else 2L
  }

  if (verbose) {
    .msg_header("Reading Program Data: {type}", level = 3)
    .msg_step("File: {path}")
    .msg_step("Skip: {skip} rows")
  }

  # --- Read Excel ----
  read_args <- list(path = path, skip = skip, col_types = "text")
  if (!is.null(sheet)) read_args$sheet <- sheet
  df <- do.call(readxl::read_excel, read_args)
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

  # --- Detect format version ----
  format_version <- .detect_program_format(df, type)
  if (verbose) .msg_step("Detected format: {format_version}")

  # --- Select column map file ----
  map_file <- .get_column_map_file(type, format_version)
  if (verbose) .msg_step("Column map: {basename(map_file)}")

  # --- Apply column mapping ----
  df <- .apply_column_map(df, map_file, verbose = verbose)

  # --- Build S3 object ----
  obj <- .make_alccdf_obj(
    data           = df,
    class_name     = "alccdf_program_raw",
    module         = "program",
    stage          = "raw",
    snapshot_date  = snapshot_date,
    extra_meta     = list(
      program_type   = type,
      format_version = format_version,
      source_file    = normalizePath(path, mustWork = FALSE),
      skip           = skip,
      sheet          = sheet %||% 1L
    )
  )
  obj <- .log_step(obj, glue::glue(
    "Read {type} data from {basename(path)} ({format_version}): ",
    "{nrow(df)} rows x {ncol(df)} cols"
  ))

  if (verbose) {
    .msg_success(
      "Program read complete: {nrow(df)} rows, format = {format_version}"
    )
  }

  obj
}


#' Read all program files from a configuration
#'
#' Convenience wrapper that reads all program files specified in a
#' \code{alccdf_program_config} object and returns them as a named list.
#'
#' @param config An \code{alccdf_program_config} object created by
#'   \code{\link{program_config}}
#' @return A named list of \code{alccdf_program_raw} objects, one per available
#'   type.
#'
#' @examples
#' \dontrun{
#' cfg <- program_config(
#'   snapshot_date = "2025-06-11",
#'   center_path   = "data/Centers.xlsx",
#'   home_path     = "data/FamilyHomes.xlsx"
#' )
#' raw_list <- program_read_all(cfg)
#' names(raw_list)
#' }
#'
#' @export
program_read_all <- function(config) {
  .assert_class(config, "alccdf_program_config", "program_read_all")

  raw_list <- list()
  for (type in config$types_available) {
    raw_list[[type]] <- program_read(
      path          = config$paths[[type]],
      type          = type,
      snapshot_date = config$snapshot_date,
      verbose       = config$verbose
    )
  }

  raw_list
}


# ---- Internal helpers --------------------------------------------------------

#' Detect program format version
#'
#' Checks whether the data frame contains a "Facility Tier" column (current
#' format) or not (legacy format). The detection is case-insensitive.
#'
#' @param df A data frame freshly read from Excel
#' @param type Program type (used for context in messages)
#' @return Character string: \code{"legacy"} or \code{"current"}
#' @keywords internal
.detect_program_format <- function(df, type) {
  col_names_lower <- tolower(names(df))
  has_tier <- any(grepl("facility\\s*tier", col_names_lower))

  if (type %in% c("exempt", "excepted")) {
    # Exempt and excepted always have Facility Tier in current data
    return("current")
  }

  if (has_tier) "current" else "legacy"
}


#' Get the path to the appropriate column map CSV
#'
#' @param type Program type
#' @param format_version \code{"legacy"} or \code{"current"}
#' @return Full path to the CSV file within \code{inst/extdata/mappings/}
#' @keywords internal
.get_column_map_file <- function(type, format_version) {
  base_name <- switch(type,
    center      = "center_column_map",
    family_home = "family_home_column_map",
    exempt      = "exempt_column_map",
    excepted    = "excepted_column_map"
  )

  if (format_version == "legacy" && type %in% c("center", "family_home")) {
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


#' Apply column mapping to a data frame
#'
#' Reads a column mapping CSV and renames data frame columns from raw Excel
#' names to clean standardised names. Matching is case-insensitive and tolerant
#' of minor whitespace/punctuation differences.
#'
#' @param df A data frame with raw column names
#' @param map_file Path to the column mapping CSV (must have \code{raw_name} and
#'   \code{clean_name} columns)
#' @param verbose Logical; print warnings about unmapped columns?
#' @return A tibble with renamed columns
#' @keywords internal
.apply_column_map <- function(df, map_file, verbose = TRUE) {
  # Read mapping
  mapping <- utils::read.csv(map_file, stringsAsFactors = FALSE)
  if (!all(c("raw_name", "clean_name") %in% names(mapping))) {
    cli::cli_abort(
      "Column map {.path {map_file}} must contain {.field raw_name} and ",
      "{.field clean_name} columns."
    )
  }

  # Normalise function: lowercase, strip non-alphanumeric (keep spaces), squish
  normalise <- function(x) {
    x <- tolower(x)
    x <- gsub("[^a-z0-9 ]", "", x)
    x <- trimws(gsub("\\s+", " ", x))
    x
  }

  # Build lookup: normalised raw_name -> clean_name
  map_lookup <- stats::setNames(mapping$clean_name, normalise(mapping$raw_name))

  # Normalise df column names

  df_names_norm <- normalise(names(df))

  # Rename columns

  new_names <- character(ncol(df))
  matched   <- logical(ncol(df))

  for (i in seq_along(df_names_norm)) {
    if (df_names_norm[i] %in% names(map_lookup)) {
      new_names[i] <- map_lookup[df_names_norm[i]]
      matched[i]   <- TRUE
    } else {
      # Keep original name as a fallback (snake_case-ified)
      new_names[i] <- gsub("[^a-z0-9]+", "_", tolower(names(df)[i]))
      new_names[i] <- gsub("^_|_$", "", new_names[i])
      matched[i]   <- FALSE
    }
  }

  if (verbose && any(!matched)) {
    unmapped <- names(df)[!matched]
    .msg_warn(
      "Unmapped columns ({length(unmapped)}): {paste(unmapped, collapse = ', ')}"
    )
  }

  names(df) <- new_names
  tibble::as_tibble(df)
}


#' Print method for raw program objects
#'
#' @param x An \code{alccdf_program_raw} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_program_raw <- function(x, ...) {
  cli::cli_h2("ALccdfDB Program Data (Raw)")
  cli::cli_alert_info("Type: {x$meta$program_type}")
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
