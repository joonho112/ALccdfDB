#' @title Subsidy Module: Clean
#' @description Clean and standardise raw subsidy data. Handles PII separation,
#'   random ID generation, date parsing, address standardisation, copayment
#'   conversion, and county name standardisation.
#' @name subsidy-clean
NULL

#' Clean raw subsidy data
#'
#' Applies a comprehensive cleaning pipeline to an \code{alccdf_subsidy_raw}
#' object. The most critical step is PII separation: parent and child names
#' and SSNs are extracted into a separate PII table, replaced by random IDs
#' in the main data.
#'
#' @param raw_obj An \code{alccdf_subsidy_raw} object from
#'   \code{\link{subsidy_read}}
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_subsidy_clean} S3 object with:
#'   \describe{
#'     \item{data}{Main data tibble with PII fields removed and random IDs added}
#'     \item{pii}{PII lookup table linking random IDs to names/SSNs}
#'     \item{meta}{Metadata including cleaning steps}
#'   }
#'
#' @details
#' The cleaning pipeline applies the following operations in order:
#' \enumerate{
#'   \item \strong{PII Separation}: Generate random IDs for parent and child.
#'     Move \code{parent_name}, \code{child_name}, \code{parent_ssn}, and
#'     \code{child_ssn} into a separate PII table. Main data retains only the
#'     random IDs.
#'   \item \strong{Date Parsing}: Parse \code{parent_dob}, \code{child_dob},
#'     \code{eligibility_begin_date}, \code{eligibility_end_date},
#'     \code{placement_start_date}, \code{placement_end_date},
#'     \code{placement_date} from character to Date.
#'   \item \strong{Address Standardisation}: Standardise \code{family_address}
#'     and \code{provider_address} using \code{.standardize_address()}.
#'   \item \strong{Copayment Conversion}: Convert \code{copay_weekly} to numeric
#'     (clients data only).
#'   \item \strong{Age Conversion}: Convert \code{child_age} to numeric.
#'   \item \strong{County Standardisation}: Title-case county names.
#'   \item \strong{Snapshot Date}: Add \code{snapshot_date} column.
#' }
#'
#' @examples
#' \dontrun{
#' raw   <- subsidy_read("EnrolledChildren.xlsx", "enrolled",
#'                        as.Date("2025-06-11"))
#' clean <- subsidy_clean(raw)
#' clean$pii  # PII lookup table
#' }
#'
#' @export
subsidy_clean <- function(raw_obj, verbose = TRUE) {
  .assert_class(raw_obj, "alccdf_subsidy_raw", "subsidy_clean")

  df            <- raw_obj$data
  subsidy_type  <- raw_obj$meta$subsidy_type
  snapshot_date <- raw_obj$meta$snapshot_date

  if (verbose) {
    .msg_header("Cleaning Subsidy Data: {subsidy_type}", level = 3)
  }

  # Track initial row count
  n_start <- nrow(df)

  # --- 1. PII Separation ----
  pii_result <- .separate_subsidy_pii(df, verbose = verbose)
  df  <- pii_result$data
  pii <- pii_result$pii

  if (verbose) {
    .msg_step("PII separated: {nrow(pii)} records in PII table")
    .msg_step("Random IDs generated: parent_id and child_id")
  }


  # --- 2. Parse date fields ----
  date_cols <- c("parent_dob", "child_dob",
                 "eligibility_begin_date", "eligibility_end_date",
                 "placement_start_date", "placement_end_date",
                 "placement_date")
  n_dates_parsed <- 0L
  for (col in date_cols) {
    if (col %in% names(df)) {
      df[[col]] <- .parse_subsidy_date(df[[col]])
      n_valid <- sum(!is.na(df[[col]]))
      n_dates_parsed <- n_dates_parsed + 1L
    }
  }
  if (verbose) .msg_step("Parsed {n_dates_parsed} date fields")

  # --- 3. Standardise addresses ----
  addr_cols <- c("family_address", "provider_address")
  for (col in addr_cols) {
    if (col %in% names(df)) {
      df[[col]] <- .standardize_address(df[[col]])
      if (verbose) .msg_step("Standardised {col}")
    }
  }

  # --- 4. Convert copay_weekly to numeric ----
  if ("copay_weekly" %in% names(df)) {
    # Remove dollar signs, commas, and whitespace before conversion
    df$copay_weekly <- gsub("[\\$,\\s]", "", df$copay_weekly)
    df$copay_weekly <- suppressWarnings(as.numeric(df$copay_weekly))
    if (verbose) {
      n_valid <- sum(!is.na(df$copay_weekly))
      .msg_step("Converted copay_weekly to numeric: {n_valid}/{nrow(df)} valid")
    }
  }

  # --- 5. Convert child_age to numeric ----
  if ("child_age" %in% names(df)) {
    df$child_age <- suppressWarnings(as.numeric(df$child_age))
    if (verbose) {
      n_valid <- sum(!is.na(df$child_age))
      .msg_step("Converted child_age to numeric: {n_valid}/{nrow(df)} valid")
    }
  }

  # --- 6. Standardise county names ----
  if ("county" %in% names(df)) {
    df$county <- stringr::str_to_title(stringr::str_squish(df$county))
    # Fix common casing issues
    df$county <- gsub("\\bDe\\b", "De", df$county)  # DeKalb
    if (verbose) {
      n_counties <- length(unique(na.omit(df$county)))
      .msg_step("Standardised county names: {n_counties} unique")
    }
  }

  # --- 7. Add snapshot_date column ----
  df$snapshot_date <- snapshot_date

  # --- Build S3 object ----
  obj <- .make_alccdf_obj(
    data          = df,
    class_name    = "alccdf_subsidy_clean",
    module        = "subsidy",
    stage         = "clean",
    snapshot_date = snapshot_date,
    extra_meta    = list(
      subsidy_type   = subsidy_type,
      format_version = raw_obj$meta$format_version,
      source_file    = raw_obj$meta$source_file,
      n_rows_raw     = n_start,
      has_pii        = TRUE,
      n_pii_records  = nrow(pii),
      cleaning_steps = c(
        "pii_separation",
        "date_parsing",
        "address_standardisation",
        "copay_conversion",
        "age_conversion",
        "county_standardisation",
        "snapshot_date_added"
      )
    )
  )

  # Attach PII table to the object
  obj$pii <- pii

  obj <- .log_step(obj, glue::glue(
    "Cleaned {subsidy_type} subsidy data: {nrow(df)} rows, ",
    "{ncol(df)} cols (from {n_start} raw rows), ",
    "{nrow(pii)} PII records separated"
  ))

  if (verbose) {
    .msg_success("Cleaning complete: {nrow(df)} rows x {ncol(df)} cols")
  }

  obj
}


# ---- Internal helpers --------------------------------------------------------

#' Separate PII fields from subsidy data
#'
#' Generates random IDs for parents and children, extracts PII fields into a
#' separate lookup table, and removes PII from the main data.
#'
#' @param df Data frame with raw subsidy columns
#' @param verbose Logical; print progress messages?
#' @return A list with \code{data} (main data with PII removed) and \code{pii}
#'   (PII lookup table)
#' @keywords internal
.separate_subsidy_pii <- function(df, verbose = TRUE) {
  n <- nrow(df)

  # Generate random IDs
  random_parent_ids <- .generate_random_ids(prefix = "P", n = n, width = 7)
  random_child_ids  <- .generate_random_ids(prefix = "C", n = n, width = 7)

  # Build PII table
  pii <- tibble::tibble(
    random_parent_id = random_parent_ids,
    random_child_id  = random_child_ids
  )

  # Add PII fields that exist
  if ("parent_name" %in% names(df)) {
    pii$parent_name <- df$parent_name
  }
  if ("parent_ssn" %in% names(df)) {
    pii$parent_ssn <- df$parent_ssn
  }
  if ("child_name" %in% names(df)) {
    pii$child_name <- df$child_name
  }
  if ("child_ssn" %in% names(df)) {
    pii$child_ssn <- df$child_ssn
  }

  # Add random IDs to main data
  df$random_parent_id <- random_parent_ids
  df$random_child_id  <- random_child_ids

  # Remove PII fields from main data
  pii_cols <- c("parent_name", "child_name", "parent_ssn", "child_ssn")
  cols_to_remove <- intersect(pii_cols, names(df))
  if (length(cols_to_remove) > 0) {
    df <- df[, !names(df) %in% cols_to_remove, drop = FALSE]
    if (verbose) {
      .msg_step("Removed PII columns from main data: {paste(cols_to_remove, collapse = ', ')}")
    }
  }

  list(data = tibble::as_tibble(df), pii = tibble::as_tibble(pii))
}


#' Parse subsidy date fields
#'
#' Handles character dates that may come from Excel as serial numbers, POSIXct
#' text, or standard date strings. Identical logic to program date parsing.
#'
#' @param x Character vector of date values
#' @return Date vector
#' @keywords internal
.parse_subsidy_date <- function(x) {
  if (all(is.na(x))) return(as.Date(rep(NA, length(x))))

  # If already Date, return as-is
  if (inherits(x, "Date")) return(x)

  # If POSIXct, convert directly
  if (inherits(x, "POSIXct")) return(as.Date(x))

  # Try Excel serial number (numeric strings like "45814")
  numeric_vals <- suppressWarnings(as.numeric(x))
  is_serial <- !is.na(numeric_vals) & numeric_vals > 30000 & numeric_vals < 60000

  result <- as.Date(rep(NA, length(x)))

  # Parse serial numbers (Excel origin = 1899-12-30)
  if (any(is_serial, na.rm = TRUE)) {
    result[is_serial] <- as.Date(numeric_vals[is_serial], origin = "1899-12-30")
  }

  # Parse remaining as date strings
  remaining <- !is_serial & !is.na(x)
  if (any(remaining)) {
    parsed <- suppressWarnings(
      lubridate::parse_date_time(
        x[remaining],
        orders = c("Ymd HMS", "Ymd", "mdY", "mdy", "dmy"),
        quiet = TRUE
      )
    )
    result[remaining] <- as.Date(parsed)
  }

  result
}


#' Print method for clean subsidy objects
#'
#' @param x An \code{alccdf_subsidy_clean} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_subsidy_clean <- function(x, ...) {
  cli::cli_h2("ALccdfDB Subsidy Data (Clean)")
  cli::cli_alert_info("Type: {x$meta$subsidy_type}")
  cli::cli_alert_info("Snapshot: {x$meta$snapshot_date}")
  cli::cli_alert_info("Dimensions: {x$meta$n_rows} rows x {x$meta$n_cols} cols")
  cli::cli_alert_info("Raw rows: {x$meta$n_rows_raw}")
  cli::cli_alert_info("PII records: {x$meta$n_pii_records}")

  # Show column names (main data)
  cols <- names(x$data)
  cli::cli_alert_info("Main columns: {paste(cols, collapse = ', ')}")

  # Show PII columns
  if (!is.null(x$pii)) {
    pii_cols <- names(x$pii)
    cli::cli_alert_info("PII columns: {paste(pii_cols, collapse = ', ')}")
  }

  if (length(x$meta$processing_log) > 0) {
    cli::cli_h3("Processing Log")
    for (entry in x$meta$processing_log) {
      cli::cli_text(entry)
    }
  }
  invisible(x)
}
