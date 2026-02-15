#' @title Staff Module: Read
#' @description Read and ingest Alabama Pathways EC Professional Level staff
#'   CSV files with automatic column mapping.
#' @name staff-read
NULL

#' Read a staff/professional-level CSV file
#'
#' Reads an Alabama Pathways EC Professional Level Users CSV file, applies the
#' \code{staff_column_map.csv} column mapping, and returns a standardised raw
#' object.
#'
#' @param path Path to the CSV file (e.g.,
#'   "AlabamaPathways_ECProfessionalLevelUsers_2025-06-04.csv")
#' @param snapshot_date Date of the administrative snapshot
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_staff_raw} S3 object
#'
#' @examples
#' \dontrun{
#' raw <- staff_read(
#'   path = "data/AlabamaPathways_ECProfessionalLevelUsers_2025-06-04.csv",
#'   snapshot_date = as.Date("2025-06-04")
#' )
#' }
#'
#' @export
staff_read <- function(path, snapshot_date, verbose = TRUE) {
  # --- Validate path ----
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }

  # --- Parse snapshot_date ----
  if (is.character(snapshot_date)) {
    snapshot_date <- as.Date(snapshot_date)
  }

  if (verbose) {
    .msg_header("Reading Staff Data", level = 3)
    .msg_step("File: {path}")
  }

  # --- Read CSV ----
  # Use readr if available, otherwise fall back to utils::read.csv
  if (requireNamespace("readr", quietly = TRUE)) {
    df <- readr::read_csv(path, col_types = readr::cols(.default = "c"),
                          show_col_types = FALSE)
  } else {
    df <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character")
  }
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

  # --- Get column map file ----
  map_file <- .get_staff_column_map_file()
  if (verbose) .msg_step("Column map: {basename(map_file)}")

  # --- Apply column mapping ----
  df <- .apply_column_map(df, map_file, verbose = verbose)

  # --- Build S3 object ----
  obj <- .make_alccdf_obj(
    data          = df,
    class_name    = "alccdf_staff_raw",
    module        = "staff",
    stage         = "raw",
    snapshot_date = snapshot_date,
    extra_meta    = list(
      source_file = normalizePath(path, mustWork = FALSE)
    )
  )
  obj <- .log_step(obj, glue::glue(
    "Read staff data from {basename(path)}: ",
    "{nrow(df)} rows x {ncol(df)} cols"
  ))

  if (verbose) {
    .msg_success("Staff read complete: {nrow(df)} rows")
  }

  obj
}


# ---- Internal helpers --------------------------------------------------------

#' Get the path to the staff column map CSV
#'
#' @return Full path to staff_column_map.csv
#' @keywords internal
.get_staff_column_map_file <- function() {
  filename <- "staff_column_map.csv"

  # Try installed package location first
  pkg_path <- system.file("extdata", "mappings", filename,
                           package = "ALccdfDB")
  if (pkg_path != "") return(pkg_path)

  # Fallback for development
  dev_path <- file.path("inst", "extdata", "mappings", filename)
  if (file.exists(dev_path)) return(dev_path)

  cli::cli_abort("Column map file not found: {.file {filename}}")
}


#' Print method for raw staff objects
#'
#' @param x An \code{alccdf_staff_raw} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_staff_raw <- function(x, ...) {
  cli::cli_h2("ALccdfDB Staff Data (Raw)")
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
