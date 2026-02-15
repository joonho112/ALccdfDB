#' @title Program Module: Validate
#' @description Validate cleaned program data against 10 quality checks
#'   covering required columns, duplicates, capacity, county names, dates,
#'   operating hours, age ranges, addresses, facility tiers, and snapshot dates.
#' @name program-validate
NULL

#' Validate cleaned program data
#'
#' Runs 10 data-quality checks against an \code{alccdf_program_clean} object.
#' In strict mode, any ERROR-level check causes an immediate abort.
#'
#' @param clean_obj An \code{alccdf_program_clean} object from
#'   \code{\link{program_clean}}
#' @param strict Logical; if TRUE, ERROR-level failures abort with an error
#'   message. Default FALSE (report only).
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_program_validation} S3 object (inherits from
#'   \code{alccdf_validation}).
#'
#' @details
#' The 10 checks performed are:
#' \enumerate{
#'   \item \strong{required_columns} (ERROR): Core columns must be present.
#'   \item \strong{duplicate_facility_id} (ERROR): Facility IDs must be unique.
#'   \item \strong{positive_capacity} (WARN): Capacities must be positive or NA.
#'   \item \strong{valid_county} (WARN): County must match one of Alabama's 67.
#'   \item \strong{expiration_date_range} (WARN): Dates not more than 2 years past.
#'   \item \strong{operating_hours_range} (WARN): Hours between 0 and 24.
#'   \item \strong{age_range_logic} (ERROR): age_min must be < age_max.
#'   \item \strong{address_completeness} (WARN): Address should not be missing.
#'   \item \strong{facility_tier_valid} (WARN): Star 1--5 or NA only.
#'   \item \strong{snapshot_date_present} (ERROR): snapshot_date must exist.
#' }
#'
#' @examples
#' \dontrun{
#' validation <- program_validate(clean_obj)
#' validation
#' validation$checks
#' }
#'
#' @export
program_validate <- function(clean_obj, strict = FALSE, verbose = TRUE) {
  .assert_class(clean_obj, "alccdf_program_clean", "program_validate")

  df            <- clean_obj$data
  snapshot_date <- clean_obj$meta$snapshot_date

  if (verbose) {
    .msg_header("Validating Program Data", level = 3)
    .msg_step("Rows: {nrow(df)}, Strict: {strict}")
  }

  checks <- tibble::tibble(
    check_id    = character(),
    description = character(),
    status      = character(),
    n_issues    = integer(),
    detail      = character()
  )
  issues_list <- list()

  # --- Check 1: Required columns (ERROR) ----
  required_cols <- c("facility_id", "facility_name", "facility_type",
                     "county", "snapshot_date")
  missing_cols  <- setdiff(required_cols, names(df))
  if (length(missing_cols) == 0) {
    checks <- dplyr::bind_rows(checks, .make_check(
      "required_columns", "All required columns present", "PASS"
    ))
  } else {
    checks <- dplyr::bind_rows(checks, .make_check(
      "required_columns",
      "Missing required columns",
      "ERROR",
      n_issues = length(missing_cols),
      detail   = paste(missing_cols, collapse = ", ")
    ))
  }

  # --- Check 2: Duplicate facility_id (ERROR) ----
  if ("facility_id" %in% names(df)) {
    fid <- df$facility_id[!is.na(df$facility_id)]
    dup_ids <- fid[duplicated(fid)]
    n_dups  <- length(unique(dup_ids))
    if (n_dups == 0) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "duplicate_facility_id", "No duplicate facility IDs", "PASS"
      ))
    } else {
      checks <- dplyr::bind_rows(checks, .make_check(
        "duplicate_facility_id",
        "Duplicate facility IDs found",
        "ERROR",
        n_issues = n_dups,
        detail   = paste(utils::head(unique(dup_ids), 10), collapse = ", ")
      ))
      issues_list[["duplicate_facility_id"]] <- tibble::tibble(
        facility_id = unique(dup_ids)
      )
    }
  }

  # --- Check 3: Positive capacity (WARN) ----
  cap_issues <- 0L
  for (cap_col in c("day_capacity", "night_capacity")) {
    if (cap_col %in% names(df)) {
      bad <- !is.na(df[[cap_col]]) & df[[cap_col]] <= 0
      cap_issues <- cap_issues + sum(bad, na.rm = TRUE)
    }
  }
  if (cap_issues == 0) {
    checks <- dplyr::bind_rows(checks, .make_check(
      "positive_capacity", "All capacities positive or NA", "PASS"
    ))
  } else {
    checks <- dplyr::bind_rows(checks, .make_check(
      "positive_capacity",
      "Non-positive capacity values found",
      "WARN",
      n_issues = cap_issues
    ))
  }

  # --- Check 4: Valid county (WARN) ----
  if ("county" %in% names(df)) {
    county_vals <- unique(na.omit(as.character(df$county)))
    n_missing_county <- sum(is.na(df$county))
    n_unique_counties <- length(county_vals)
    if (n_unique_counties > 0 && n_unique_counties <= 67) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "valid_county",
        paste0("County: ", n_unique_counties, " unique values (factor)"),
        "PASS"
      ))
    } else if (n_unique_counties > 67) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "valid_county",
        paste0("More than 67 county values (", n_unique_counties, "); Alabama has 67 counties"),
        "WARN",
        n_issues = n_unique_counties - 67
      ))
    }
    if (n_missing_county > 0) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "county_missing",
        paste0(n_missing_county, " records with missing county"),
        "WARN",
        n_issues = n_missing_county
      ))
    }
  }

  # --- Check 5: Expiration date range (WARN) ----
  if ("expiration_date" %in% names(df)) {
    valid_dates <- df$expiration_date[!is.na(df$expiration_date)]
    cutoff_past <- Sys.Date() - 730  # approximately 2 years
    too_old     <- valid_dates < cutoff_past
    n_old       <- sum(too_old, na.rm = TRUE)
    if (n_old == 0) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "expiration_date_range", "Expiration dates within expected range", "PASS"
      ))
    } else {
      checks <- dplyr::bind_rows(checks, .make_check(
        "expiration_date_range",
        "Expiration dates more than 2 years past",
        "WARN",
        n_issues = n_old,
        detail   = glue::glue("Oldest: {min(valid_dates[too_old])}")
      ))
    }
  }

  # --- Check 6: Operating hours range (WARN) ----
  hours_issues <- 0L
  for (h_col in c("day_oper_hours", "night_oper_hours")) {
    if (h_col %in% names(df)) {
      vals <- df[[h_col]][!is.na(df[[h_col]])]
      bad  <- vals < 0 | vals > 24
      hours_issues <- hours_issues + sum(bad, na.rm = TRUE)
    }
  }
  if (hours_issues == 0) {
    checks <- dplyr::bind_rows(checks, .make_check(
      "operating_hours_range", "Operating hours within 0-24 range", "PASS"
    ))
  } else {
    checks <- dplyr::bind_rows(checks, .make_check(
      "operating_hours_range",
      "Operating hours outside 0-24 range",
      "WARN",
      n_issues = hours_issues
    ))
  }

  # --- Check 7: Age range logic (ERROR) ----
  if (all(c("age_min", "age_max") %in% names(df))) {
    both_present <- !is.na(df$age_min) & !is.na(df$age_max)
    bad_age      <- both_present & df$age_min >= df$age_max
    n_bad_age    <- sum(bad_age, na.rm = TRUE)
    if (n_bad_age == 0) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "age_range_logic", "Age ranges valid (min < max)", "PASS"
      ))
    } else {
      checks <- dplyr::bind_rows(checks, .make_check(
        "age_range_logic",
        "Age range violations (min >= max)",
        "ERROR",
        n_issues = n_bad_age
      ))
      if ("facility_id" %in% names(df)) {
        issues_list[["age_range_logic"]] <- df[bad_age, c(
          "facility_id", "age_min", "age_max"
        )]
      }
    }
  } else {
    checks <- dplyr::bind_rows(checks, .make_check(
      "age_range_logic",
      "Age range columns not present (skipped)",
      "INFO"
    ))
  }

  # --- Check 8: Address completeness (WARN) ----
  if ("facility_address" %in% names(df)) {
    missing_addr <- is.na(df$facility_address) |
      df$facility_address == "" |
      nchar(df$facility_address) < 5
    n_missing <- sum(missing_addr, na.rm = TRUE)
    if (n_missing == 0) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "address_completeness", "All facility addresses present", "PASS"
      ))
    } else {
      checks <- dplyr::bind_rows(checks, .make_check(
        "address_completeness",
        "Missing or incomplete facility addresses",
        "WARN",
        n_issues = n_missing
      ))
    }
  }

  # --- Check 9: Facility tier valid (WARN) ----
  if ("facility_tier" %in% names(df)) {
    valid_tiers <- c("Star 1", "Star 2", "Star 3", "Star 4", "Star 5")
    non_na_tiers <- df$facility_tier[!is.na(df$facility_tier)]
    bad_tiers   <- non_na_tiers[!as.character(non_na_tiers) %in% valid_tiers]
    if (length(bad_tiers) == 0) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "facility_tier_valid", "Facility tiers valid (Star 1-5 or NA)", "PASS"
      ))
    } else {
      checks <- dplyr::bind_rows(checks, .make_check(
        "facility_tier_valid",
        "Invalid facility tier values",
        "WARN",
        n_issues = length(bad_tiers),
        detail   = paste(unique(as.character(bad_tiers)), collapse = ", ")
      ))
    }
  } else {
    checks <- dplyr::bind_rows(checks, .make_check(
      "facility_tier_valid",
      "Facility tier column not present (legacy format)",
      "INFO"
    ))
  }

  # --- Check 10: Snapshot date present (ERROR) ----
  if ("snapshot_date" %in% names(df) && all(!is.na(df$snapshot_date))) {
    checks <- dplyr::bind_rows(checks, .make_check(
      "snapshot_date_present", "Snapshot date present on all rows", "PASS"
    ))
  } else {
    n_missing_snap <- if ("snapshot_date" %in% names(df)) {
      sum(is.na(df$snapshot_date))
    } else {
      nrow(df)
    }
    checks <- dplyr::bind_rows(checks, .make_check(
      "snapshot_date_present",
      "Missing snapshot_date values",
      "ERROR",
      n_issues = n_missing_snap
    ))
  }

  # --- Combine issues ----
  all_issues <- if (length(issues_list) > 0) {
    dplyr::bind_rows(
      purrr::map2(issues_list, names(issues_list), function(tbl, nm) {
        tbl$check_id <- nm
        tbl
      })
    )
  } else {
    tibble::tibble()
  }

  # --- Build validation result ----
  result <- .build_validation_result(
    checks        = checks,
    issues        = all_issues,
    module        = "program",
    stage         = "validation",
    snapshot_date = snapshot_date,
    strict        = strict,
    class_name    = c("alccdf_program_validation", "alccdf_validation")
  )

  if (verbose) {
    n_pass <- sum(checks$status == "PASS")
    n_err  <- sum(checks$status == "ERROR")
    n_warn <- sum(checks$status == "WARN")
    n_info <- sum(checks$status == "INFO")
    .msg_step("Checks: {n_pass} PASS, {n_err} ERROR, {n_warn} WARN, {n_info} INFO")
    if (result$passed) {
      .msg_success("Validation passed")
    } else {
      .msg_warn("Validation completed with errors")
    }
  }

  result
}


#' Print method for program validation objects
#'
#' @param x An \code{alccdf_program_validation} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_program_validation <- function(x, ...) {
  # Delegate to the base validation print method
  NextMethod("print", x)
}
