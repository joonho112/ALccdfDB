#' @title Staff Module: Clean
#' @description Clean and standardise raw staff data. Handles PII separation,
#'   county standardisation, career lattice level cleaning, and operating status
#'   conversion.
#' @name staff-clean
NULL

#' Clean raw staff data
#'
#' Applies a comprehensive cleaning pipeline to an \code{alccdf_staff_raw}
#' object. PII fields (staff_name, user_email) are separated into a distinct
#' \code{$pii} table linked by a randomly generated \code{random_staff_id}.
#' All cleaning steps are logged for reproducibility.
#'
#' @param raw_obj An \code{alccdf_staff_raw} object from \code{\link{staff_read}}
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_staff_clean} S3 object with components:
#'   \describe{
#'     \item{data}{Main data frame with PII removed and random_staff_id added}
#'     \item{pii}{PII table with random_staff_id, staff_name, user_email}
#'     \item{meta}{Metadata including processing log}
#'   }
#'
#' @details
#' The cleaning pipeline applies the following operations in order:
#' \enumerate{
#'   \item Generate random staff IDs for PII linkage
#'   \item Separate PII fields (staff_name, user_email) into \code{$pii} table
#'   \item Standardise county names (user_county, facility_county) to title case
#'   \item Clean career_lattice_level (standardise levels)
#'   \item Convert currently_operating to logical
#'   \item Add snapshot_date column
#' }
#'
#' @examples
#' \dontrun{
#' raw   <- staff_read("AlabamaPathways_ECProfessionalLevelUsers.csv", "2025-06-04")
#' clean <- staff_clean(raw)
#' clean$data     # main data without PII
#' clean$pii      # PII table
#' }
#'
#' @export
staff_clean <- function(raw_obj, verbose = TRUE) {
  .assert_class(raw_obj, "alccdf_staff_raw", "staff_clean")

  df            <- raw_obj$data
  snapshot_date <- raw_obj$meta$snapshot_date

  if (verbose) {
    .msg_header("Cleaning Staff Data", level = 3)
  }

  n_start <- nrow(df)

  # --- 1. Generate random staff IDs ----
  set.seed(NULL)  # ensure randomness
  df$random_staff_id <- .generate_random_ids(prefix = "STF", n = nrow(df),
                                              width = 6)
  if (verbose) .msg_step("Generated {nrow(df)} random staff IDs")

  # --- 2. Separate PII ----
  pii_cols <- c("staff_name", "user_email")
  available_pii <- intersect(pii_cols, names(df))

  pii_table <- df[, c("random_staff_id", available_pii), drop = FALSE]
  pii_table <- tibble::as_tibble(pii_table)

  # Remove PII from main data
  df <- df[, setdiff(names(df), pii_cols), drop = FALSE]
  df <- tibble::as_tibble(df)

  if (verbose) {
    .msg_step("Separated PII ({length(available_pii)} fields) into $pii table")
  }

  # --- 3. Standardise county names ----
  county_cols <- c("user_county", "facility_county")
  for (col in county_cols) {
    if (col %in% names(df)) {
      df[[col]] <- stringr::str_to_title(stringr::str_squish(df[[col]]))
      df[[col]] <- gsub("\\bDe\\b", "De", df[[col]])  # DeKalb
    }
  }
  if (verbose) {
    n_user_counties <- if ("user_county" %in% names(df)) {
      length(unique(na.omit(df$user_county)))
    } else 0
    n_fac_counties <- if ("facility_county" %in% names(df)) {
      length(unique(na.omit(df$facility_county)))
    } else 0
    .msg_step("Standardised county names: {n_user_counties} user, {n_fac_counties} facility")
  }

  # --- 4. Clean career_lattice_level ----
  if ("career_lattice_level" %in% names(df)) {
    df$career_lattice_level <- .clean_career_lattice_level(df$career_lattice_level)
    if (verbose) {
      n_levels <- length(unique(na.omit(df$career_lattice_level)))
      .msg_step("Cleaned career_lattice_level: {n_levels} unique levels")
    }
  }

  # --- 5. Convert currently_operating to logical ----
  if ("currently_operating" %in% names(df)) {
    df$currently_operating <- .parse_logical_field(df$currently_operating)
    if (verbose) {
      n_true <- sum(df$currently_operating, na.rm = TRUE)
      n_false <- sum(!df$currently_operating, na.rm = TRUE)
      .msg_step("Converted currently_operating: {n_true} TRUE, {n_false} FALSE")
    }
  }

  # --- 6. Add snapshot_date column ----
  df$snapshot_date <- snapshot_date

  # --- Build S3 object ----
  meta <- list(
    module = "staff",
    stage = "clean",
    snapshot_date = snapshot_date,
    n_rows = nrow(df),
    n_cols = ncol(df),
    created_at = Sys.time(),
    package_version = as.character(utils::packageVersion("ALccdfDB")),
    processing_log = character(),
    source_file    = raw_obj$meta$source_file,
    n_rows_raw     = n_start,
    n_pii_fields   = length(available_pii),
    cleaning_steps = c(
      "random_id_generation",
      "pii_separation",
      "county_standardisation",
      "career_lattice_cleaning",
      "operating_status_conversion"
    )
  )

  obj <- list(
    data = tibble::as_tibble(df),
    pii  = pii_table,
    meta = meta,
    diagnostics = list()
  )
  class(obj) <- "alccdf_staff_clean"

  obj$meta$processing_log <- c(
    obj$meta$processing_log,
    paste(
      format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"),
      glue::glue("Cleaned staff data: {nrow(df)} rows, ",
                  "{ncol(df)} cols (from {n_start} raw rows), ",
                  "{nrow(pii_table)} PII records separated")
    )
  )

  if (verbose) {
    .msg_success(
      "Staff cleaning complete: {nrow(df)} rows x {ncol(df)} cols + {nrow(pii_table)} PII records"
    )
  }

  obj
}


# ---- Internal helpers --------------------------------------------------------

#' Clean career lattice level values
#'
#' Standardises career lattice level text to a consistent format.
#' Handles common variations in spacing, casing, and abbreviations.
#'
#' @param x Character vector of career lattice level values
#' @return Character vector of standardised levels
#' @keywords internal
.clean_career_lattice_level <- function(x) {
  x <- stringr::str_squish(x)
  x <- stringr::str_to_title(x)

  # Map common variants to standardised forms
  x <- dplyr::case_when(
    is.na(x) | x == "" ~ NA_character_,
    grepl("^Level\\s*I$", x, ignore.case = TRUE) ~ "Level I",
    grepl("^Level\\s*II$", x, ignore.case = TRUE) ~ "Level II",
    grepl("^Level\\s*III$", x, ignore.case = TRUE) ~ "Level III",
    grepl("^Level\\s*IV$", x, ignore.case = TRUE) ~ "Level IV",
    grepl("^Level\\s*V$", x, ignore.case = TRUE) ~ "Level V",
    grepl("^Level\\s*VI$", x, ignore.case = TRUE) ~ "Level VI",
    grepl("^Level\\s*VII$", x, ignore.case = TRUE) ~ "Level VII",
    grepl("^Level\\s*VIII$", x, ignore.case = TRUE) ~ "Level VIII",
    grepl("^Level\\s*IX$", x, ignore.case = TRUE) ~ "Level IX",
    grepl("^Level\\s*X$", x, ignore.case = TRUE) ~ "Level X",
    grepl("^Beginning", x, ignore.case = TRUE) ~ "Beginning",
    grepl("^Initial", x, ignore.case = TRUE) ~ "Initial",
    grepl("^Associate", x, ignore.case = TRUE) ~ "Associate",
    grepl("^Professional", x, ignore.case = TRUE) ~ "Professional",
    grepl("^Master", x, ignore.case = TRUE) ~ "Master",
    grepl("^Advanced", x, ignore.case = TRUE) ~ "Advanced",
    TRUE ~ x
  )

  x
}


#' Parse a logical/boolean field from character
#'
#' Converts various text representations of TRUE/FALSE to logical values.
#' Handles "Yes"/"No", "Y"/"N", "TRUE"/"FALSE", "1"/"0", "T"/"F".
#'
#' @param x Character vector
#' @return Logical vector
#' @keywords internal
.parse_logical_field <- function(x) {
  x <- stringr::str_squish(tolower(x))
  dplyr::case_when(
    is.na(x) | x == "" ~ NA,
    x %in% c("yes", "y", "true", "t", "1") ~ TRUE,
    x %in% c("no", "n", "false", "f", "0") ~ FALSE,
    TRUE ~ NA
  )
}


#' Print method for clean staff objects
#'
#' @param x An \code{alccdf_staff_clean} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_staff_clean <- function(x, ...) {
  cli::cli_h2("ALccdfDB Staff Data (Clean)")
  cli::cli_alert_info("Snapshot: {x$meta$snapshot_date}")
  cli::cli_alert_info("Main data: {x$meta$n_rows} rows x {x$meta$n_cols} cols")
  cli::cli_alert_info("PII table: {nrow(x$pii)} rows x {ncol(x$pii)} cols")
  cli::cli_alert_info("Raw rows: {x$meta$n_rows_raw}")
  cli::cli_alert_info("PII fields separated: {x$meta$n_pii_fields}")

  # Show career lattice distribution if available
  if ("career_lattice_level" %in% names(x$data)) {
    level_dist <- table(x$data$career_lattice_level, useNA = "ifany")
    level_str <- paste(names(level_dist), level_dist, sep = ": ", collapse = ", ")
    cli::cli_alert_info("Career levels: {level_str}")
  }

  cols <- names(x$data)
  cli::cli_alert_info("Main columns: {paste(cols, collapse = ', ')}")
  cli::cli_alert_info("PII columns: {paste(names(x$pii), collapse = ', ')}")

  if (length(x$meta$processing_log) > 0) {
    cli::cli_h3("Processing Log")
    for (entry in x$meta$processing_log) {
      cli::cli_text(entry)
    }
  }
  invisible(x)
}
