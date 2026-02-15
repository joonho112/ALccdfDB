#' @title Subsidy Module: Validate
#' @description Validate cleaned subsidy data against 7 quality checks
#'   covering required columns, addresses, copayment ranges, age ranges,
#'   provider references, duplicate records, and PII separation.
#' @name subsidy-validate
NULL

#' Validate cleaned subsidy data
#'
#' Runs 7 data-quality checks against an \code{alccdf_subsidy_clean} object.
#' In strict mode, any ERROR-level check causes an immediate abort.
#'
#' @param clean_obj An \code{alccdf_subsidy_clean} object from
#'   \code{\link{subsidy_clean}}
#' @param strict Logical; if TRUE, ERROR-level failures abort with an error
#'   message. Default FALSE (report only).
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_subsidy_validation} S3 object (inherits from
#'   \code{alccdf_validation}).
#'
#' @details
#' The 7 checks performed are:
#' \enumerate{
#'   \item \strong{required_columns} (ERROR): Core columns must be present.
#'   \item \strong{valid_address} (WARN): At least one address field
#'     (\code{family_address} or \code{provider_address}) should be non-empty.
#'   \item \strong{copay_range} (WARN): Weekly copayment in 0--500 range (clients
#'     type only).
#'   \item \strong{age_range} (WARN): Child age between 0 and 18.
#'   \item \strong{provider_reference} (INFO): \code{provider_id} should be
#'     non-empty.
#'   \item \strong{duplicate_records} (WARN): Exact duplicates on case_id +
#'     child key columns.
#'   \item \strong{pii_separation} (ERROR): Verify that SSN columns are NOT
#'     present in the main data table.
#' }
#'
#' @examples
#' \dontrun{
#' validation <- subsidy_validate(clean_obj)
#' validation
#' validation$checks
#' }
#'
#' @export
subsidy_validate <- function(clean_obj, strict = FALSE, verbose = TRUE) {
  .assert_class(clean_obj, "alccdf_subsidy_clean", "subsidy_validate")

  df            <- clean_obj$data
  subsidy_type  <- clean_obj$meta$subsidy_type
  snapshot_date <- clean_obj$meta$snapshot_date

  if (verbose) {
    .msg_header("Validating Subsidy Data: {subsidy_type}", level = 3)
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
  required_cols <- c("case_id", "random_parent_id", "random_child_id",
                     "snapshot_date")
  # Add type-specific required columns
  if (subsidy_type == "enrolled") {
    required_cols <- c(required_cols, "county", "child_dob")
  } else if (subsidy_type == "clients") {
    required_cols <- c(required_cols, "county", "child_dob", "provider_id")
  }

  missing_cols <- setdiff(required_cols, names(df))
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

  # --- Check 2: Valid address (WARN) ----
  addr_cols <- intersect(c("family_address", "provider_address"), names(df))
  if (length(addr_cols) > 0) {
    # Check that at least one address column has non-empty values per row
    has_addr <- apply(df[, addr_cols, drop = FALSE], 1, function(row) {
      any(!is.na(row) & row != "" & nchar(row) >= 5)
    })
    n_missing_addr <- sum(!has_addr)
    if (n_missing_addr == 0) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "valid_address", "All records have at least one valid address", "PASS"
      ))
    } else {
      checks <- dplyr::bind_rows(checks, .make_check(
        "valid_address",
        "Records with no valid address",
        "WARN",
        n_issues = n_missing_addr
      ))
    }
  } else {
    checks <- dplyr::bind_rows(checks, .make_check(
      "valid_address",
      "No address columns present (skipped)",
      "INFO"
    ))
  }

  # --- Check 3: Copay range (WARN) - clients only ----
  if (subsidy_type == "clients" && "copay_weekly" %in% names(df)) {
    copay_vals <- df$copay_weekly[!is.na(df$copay_weekly)]
    out_of_range <- copay_vals < 0 | copay_vals > 500
    n_bad_copay <- sum(out_of_range, na.rm = TRUE)
    if (n_bad_copay == 0) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "copay_range", "All copayment values in reasonable range (0-500)", "PASS"
      ))
    } else {
      checks <- dplyr::bind_rows(checks, .make_check(
        "copay_range",
        "Copayment values outside 0-500 range",
        "WARN",
        n_issues = n_bad_copay,
        detail   = glue::glue("Range: {min(copay_vals)}-{max(copay_vals)}")
      ))
    }
  } else if (subsidy_type != "clients") {
    checks <- dplyr::bind_rows(checks, .make_check(
      "copay_range",
      "Copayment check not applicable for enrolled data",
      "INFO"
    ))
  }

  # --- Check 4: Age range (WARN) ----
  if ("child_age" %in% names(df)) {
    age_vals <- df$child_age[!is.na(df$child_age)]
    out_of_range <- age_vals < 0 | age_vals > 18
    n_bad_age <- sum(out_of_range, na.rm = TRUE)
    if (n_bad_age == 0) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "age_range", "All child ages in reasonable range (0-18)", "PASS"
      ))
    } else {
      checks <- dplyr::bind_rows(checks, .make_check(
        "age_range",
        "Child ages outside 0-18 range",
        "WARN",
        n_issues = n_bad_age,
        detail   = glue::glue("Range: {min(age_vals)}-{max(age_vals)}")
      ))
    }
  } else {
    checks <- dplyr::bind_rows(checks, .make_check(
      "age_range",
      "Child age column not present (skipped)",
      "INFO"
    ))
  }

  # --- Check 5: Provider reference (INFO) ----
  if ("provider_id" %in% names(df)) {
    missing_provider <- is.na(df$provider_id) | df$provider_id == ""
    n_missing_prov <- sum(missing_provider)
    if (n_missing_prov == 0) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "provider_reference", "All records have provider_id", "PASS"
      ))
    } else {
      checks <- dplyr::bind_rows(checks, .make_check(
        "provider_reference",
        "Records missing provider_id",
        "INFO",
        n_issues = n_missing_prov
      ))
    }
  } else {
    checks <- dplyr::bind_rows(checks, .make_check(
      "provider_reference",
      "provider_id column not present (skipped)",
      "INFO"
    ))
  }

  # --- Check 6: Duplicate records (WARN) ----
  # Determine key columns based on type
  if (subsidy_type == "enrolled") {
    dup_key_cols <- intersect(
      c("case_id", "random_child_id", "facility_id"),
      names(df)
    )
  } else {
    dup_key_cols <- intersect(
      c("case_id", "random_child_id", "provider_id"),
      names(df)
    )
  }

  if (length(dup_key_cols) >= 2) {
    key_df <- df[dup_key_cols]
    all_keys_na <- apply(key_df, 1, function(x) all(is.na(x)))
    non_na_rows <- !all_keys_na
    dup_mask <- logical(nrow(df))
    if (any(non_na_rows)) {
      dup_mask[non_na_rows] <- duplicated(df[non_na_rows, dup_key_cols, drop = FALSE])
    }
    n_dups <- sum(dup_mask)
    if (n_dups == 0) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "duplicate_records",
        glue::glue("No duplicate records on [{paste(dup_key_cols, collapse = ', ')}]"),
        "PASS"
      ))
    } else {
      checks <- dplyr::bind_rows(checks, .make_check(
        "duplicate_records",
        "Duplicate records found",
        "WARN",
        n_issues = n_dups,
        detail   = glue::glue("Key: {paste(dup_key_cols, collapse = ', ')}")
      ))
    }
  }

  # --- Check 7: PII separation (ERROR) ----
  pii_danger_cols <- c("parent_ssn", "child_ssn", "parent_name", "child_name")
  leaked_pii <- intersect(pii_danger_cols, names(df))
  if (length(leaked_pii) == 0) {
    checks <- dplyr::bind_rows(checks, .make_check(
      "pii_separation", "PII fields properly separated from main data", "PASS"
    ))
  } else {
    checks <- dplyr::bind_rows(checks, .make_check(
      "pii_separation",
      "PII fields found in main data table",
      "ERROR",
      n_issues = length(leaked_pii),
      detail   = paste(leaked_pii, collapse = ", ")
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
    module        = "subsidy",
    stage         = "validation",
    snapshot_date = snapshot_date,
    strict        = strict,
    class_name    = c("alccdf_subsidy_validation", "alccdf_validation")
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


#' Print method for subsidy validation objects
#'
#' @param x An \code{alccdf_subsidy_validation} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_subsidy_validation <- function(x, ...) {
  # Delegate to the base validation print method
  NextMethod("print", x)
}
