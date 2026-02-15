#' @title Staff Module: Validate
#' @description Validate cleaned staff data against 4 quality checks covering
#'   required columns, duplicate records, valid professional levels, and PII
#'   separation.
#' @name staff-validate
NULL

#' Validate cleaned staff data
#'
#' Runs 4 data-quality checks against an \code{alccdf_staff_clean} object.
#' In strict mode, any ERROR-level check causes an immediate abort.
#'
#' @param clean_obj An \code{alccdf_staff_clean} object from
#'   \code{\link{staff_clean}}
#' @param strict Logical; if TRUE, ERROR-level failures abort with an error
#'   message. Default FALSE (report only).
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_staff_validation} S3 object (inherits from
#'   \code{alccdf_validation}).
#'
#' @details
#' The 4 checks performed are:
#' \enumerate{
#'   \item \strong{required_columns} (ERROR): Core columns must be present in
#'     the main data.
#'   \item \strong{duplicate_records} (WARN): Duplicate combinations of
#'     staff name + facility name.
#'   \item \strong{valid_professional_levels} (WARN): Career lattice levels
#'     must match expected set.
#'   \item \strong{pii_separation} (ERROR): PII fields (staff_name, user_email)
#'     must not appear in the main data.
#' }
#'
#' @examples
#' \dontrun{
#' validation <- staff_validate(clean_obj)
#' validation
#' validation$checks
#' }
#'
#' @export
staff_validate <- function(clean_obj, strict = FALSE, verbose = TRUE) {
  .assert_class(clean_obj, "alccdf_staff_clean", "staff_validate")

  df            <- clean_obj$data
  pii           <- clean_obj$pii
  snapshot_date <- clean_obj$meta$snapshot_date

  if (verbose) {
    .msg_header("Validating Staff Data", level = 3)
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
  required_cols <- c("random_staff_id", "user_county", "facility_name",
                     "facility_county", "provider_type", "currently_operating",
                     "position", "career_lattice_level", "snapshot_date")
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

  # --- Check 2: Duplicate records (WARN) ----
  # Check for duplicates based on name + facility in PII-joined data
  if (!is.null(pii) && "staff_name" %in% names(pii) &&
      "random_staff_id" %in% names(df) && "facility_name" %in% names(df)) {
    joined <- dplyr::left_join(
      df[, c("random_staff_id", "facility_name"), drop = FALSE],
      pii[, c("random_staff_id", "staff_name"), drop = FALSE],
      by = "random_staff_id"
    )
    dup_key <- paste(joined$staff_name, joined$facility_name, sep = "|||")
    dup_key <- dup_key[!is.na(joined$staff_name) & !is.na(joined$facility_name)]
    n_dups  <- sum(duplicated(dup_key))
    if (n_dups == 0) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "duplicate_records", "No duplicate name + facility combinations", "PASS"
      ))
    } else {
      dup_names <- unique(dup_key[duplicated(dup_key)])
      checks <- dplyr::bind_rows(checks, .make_check(
        "duplicate_records",
        "Duplicate name + facility combinations found",
        "WARN",
        n_issues = n_dups,
        detail   = glue::glue("{length(dup_names)} unique duplicate combinations")
      ))
    }
  } else {
    checks <- dplyr::bind_rows(checks, .make_check(
      "duplicate_records",
      "Cannot check duplicates (missing name or facility columns)",
      "INFO"
    ))
  }

  # --- Check 3: Valid professional levels (WARN) ----
  if ("career_lattice_level" %in% names(df)) {
    expected_levels <- c(
      "Level I", "Level II", "Level III", "Level IV", "Level V",
      "Level VI", "Level VII", "Level VIII", "Level IX", "Level X",
      "Beginning", "Initial", "Associate", "Professional", "Master", "Advanced"
    )
    non_na_levels <- df$career_lattice_level[!is.na(df$career_lattice_level)]
    bad_levels    <- non_na_levels[!non_na_levels %in% expected_levels]
    if (length(bad_levels) == 0) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "valid_professional_levels",
        "All career lattice levels valid or NA",
        "PASS"
      ))
    } else {
      unique_bad <- unique(bad_levels)
      checks <- dplyr::bind_rows(checks, .make_check(
        "valid_professional_levels",
        "Unexpected career lattice levels found",
        "WARN",
        n_issues = length(bad_levels),
        detail   = paste(utils::head(unique_bad, 10), collapse = ", ")
      ))
      issues_list[["valid_professional_levels"]] <- tibble::tibble(
        career_lattice_level = unique_bad
      )
    }
  } else {
    checks <- dplyr::bind_rows(checks, .make_check(
      "valid_professional_levels",
      "Career lattice level column not present",
      "WARN",
      n_issues = 1L,
      detail   = "career_lattice_level column missing"
    ))
  }

  # --- Check 4: PII separation (ERROR) ----
  pii_fields_in_main <- intersect(c("staff_name", "user_email"), names(df))
  if (length(pii_fields_in_main) == 0) {
    checks <- dplyr::bind_rows(checks, .make_check(
      "pii_separation",
      "PII fields (staff_name, user_email) not in main data",
      "PASS"
    ))
  } else {
    checks <- dplyr::bind_rows(checks, .make_check(
      "pii_separation",
      "PII fields found in main data",
      "ERROR",
      n_issues = length(pii_fields_in_main),
      detail   = paste(pii_fields_in_main, collapse = ", ")
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
    module        = "staff",
    stage         = "validation",
    snapshot_date = snapshot_date,
    strict        = strict,
    class_name    = c("alccdf_staff_validation", "alccdf_validation")
  )

  if (verbose) {
    n_pass <- sum(checks$status == "PASS")
    n_err  <- sum(checks$status == "ERROR")
    n_warn <- sum(checks$status == "WARN")
    n_info <- sum(checks$status == "INFO")
    .msg_step("Checks: {n_pass} PASS, {n_err} ERROR, {n_warn} WARN, {n_info} INFO")
    if (result$passed) {
      .msg_success("Staff validation passed")
    } else {
      .msg_warn("Staff validation completed with errors")
    }
  }

  result
}


#' Print method for staff validation objects
#'
#' @param x An \code{alccdf_staff_validation} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_staff_validation <- function(x, ...) {
  # Delegate to the base validation print method
  NextMethod("print", x)
}
