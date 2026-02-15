#' @title Melissa Module: Validate
#' @description Validate Melissa geocoding results against three quality checks
#'   covering coordinate bounds, match quality, and ID match rate.
#' @name melissa-validate
NULL

#' Validate Melissa geocoding results
#'
#' Runs 3 data-quality checks against an \code{alccdf_melissa_programs} or
#' \code{alccdf_melissa_households} object. In strict mode, any ERROR-level
#' check causes an immediate abort.
#'
#' @param obj An \code{alccdf_melissa_programs} or
#'   \code{alccdf_melissa_households} object from
#'   \code{\link{melissa_import_programs}} or
#'   \code{\link{melissa_import_households}}
#' @param strict Logical; if TRUE, ERROR-level failures abort with an error
#'   message. Default FALSE (report only).
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_melissa_validation} S3 object (inherits from
#'   \code{alccdf_validation}).
#'
#' @details
#' The 3 checks performed are:
#' \enumerate{
#'   \item \strong{coordinate_bounds} (ERROR): Latitude must be within
#'     \[30.14, 35.01\] and longitude within \[-88.47, -84.89\] (Alabama bounds).
#'   \item \strong{match_quality} (WARN): Flags if >10% of rows have poor
#'     Melissa match quality based on RESULTCODE/STATUSCODE.
#'   \item \strong{id_match_rate} (WARN): Reports the percentage of random_id
#'     values that are non-empty.
#' }
#'
#' @examples
#' \dontrun{
#' validation <- melissa_validate(mel_programs)
#' validation
#' validation$checks
#' }
#'
#' @export
melissa_validate <- function(obj, strict = FALSE, verbose = TRUE) {
  # Accept either programs or households Melissa objects
  valid_classes <- c("alccdf_melissa_programs", "alccdf_melissa_households")
  if (!inherits(obj, valid_classes)) {
    cli::cli_abort(c(
      "Expected an {.cls alccdf_melissa_programs} or {.cls alccdf_melissa_households} object.",
      "i" = "Got {.cls {class(obj)[1]}} instead."
    ))
  }

  df            <- obj$data
  snapshot_date <- obj$meta$snapshot_date

  if (verbose) {
    .msg_header("Validating Melissa Geocoding Data", level = 3)
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

  # --- Check 1: Coordinate bounds (ERROR) ----
  # Alabama bounds: Lat [30.14, 35.01], Lon [-88.47, -84.89]
  al_lat_min <- 30.14
  al_lat_max <- 35.01
  al_lon_min <- -88.47
  al_lon_max <- -84.89

  if (all(c("latitude", "longitude") %in% names(df))) {
    lat_vals <- df$latitude[!is.na(df$latitude)]
    lon_vals <- df$longitude[!is.na(df$longitude)]

    out_of_bounds_lat <- lat_vals < al_lat_min | lat_vals > al_lat_max
    out_of_bounds_lon <- lon_vals < al_lon_min | lon_vals > al_lon_max
    n_oob <- sum(out_of_bounds_lat, na.rm = TRUE) +
             sum(out_of_bounds_lon, na.rm = TRUE)

    if (n_oob == 0) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "coordinate_bounds",
        "All coordinates within Alabama bounds",
        "PASS"
      ))
    } else {
      n_lat_oob <- sum(out_of_bounds_lat, na.rm = TRUE)
      n_lon_oob <- sum(out_of_bounds_lon, na.rm = TRUE)
      detail_msg <- glue::glue(
        "{n_lat_oob} lat outside [{al_lat_min}, {al_lat_max}], ",
        "{n_lon_oob} lon outside [{al_lon_min}, {al_lon_max}]"
      )
      checks <- dplyr::bind_rows(checks, .make_check(
        "coordinate_bounds",
        "Coordinates outside Alabama bounds",
        "ERROR",
        n_issues = n_oob,
        detail   = detail_msg
      ))

      # Collect issue rows
      lat_issue_rows <- which(!is.na(df$latitude) &
        (df$latitude < al_lat_min | df$latitude > al_lat_max))
      lon_issue_rows <- which(!is.na(df$longitude) &
        (df$longitude < al_lon_min | df$longitude > al_lon_max))
      issue_rows <- sort(unique(c(lat_issue_rows, lon_issue_rows)))

      if (length(issue_rows) > 0) {
        issue_cols <- intersect(c("random_id", "latitude", "longitude",
                                   "county", "facility_name"), names(df))
        issues_list[["coordinate_bounds"]] <- df[issue_rows, issue_cols,
                                                   drop = FALSE]
      }
    }
  } else {
    checks <- dplyr::bind_rows(checks, .make_check(
      "coordinate_bounds",
      "Coordinate columns not present (skipped)",
      "ERROR",
      n_issues = 1L,
      detail   = "latitude and/or longitude columns missing"
    ))
  }

  # --- Check 2: Match quality (WARN) ----
  # Check RESULTCODE and STATUSCODE for poor matches
  # Good Melissa results typically start with "GS" (GeoSomething)
  # Poor matches often have empty or error-prefixed result codes
  if ("result_code" %in% names(df) || "status_code" %in% names(df)) {
    n_total <- nrow(df)
    n_poor <- 0L

    if ("result_code" %in% names(df)) {
      # Melissa result codes: good codes typically contain "GS" prefix
      result_codes <- df$result_code
      # Poor match: empty, NA, or contains "GE" (geographic error) patterns
      poor_result <- is.na(result_codes) | result_codes == "" |
        grepl("^GE", result_codes, ignore.case = TRUE)
      n_poor <- sum(poor_result, na.rm = TRUE)
    } else if ("status_code" %in% names(df)) {
      status_codes <- df$status_code
      poor_status <- is.na(status_codes) | status_codes == "" |
        grepl("^E", status_codes, ignore.case = TRUE)
      n_poor <- sum(poor_status, na.rm = TRUE)
    }

    poor_pct <- if (n_total > 0) round(n_poor / n_total * 100, 1) else 0
    threshold <- 10

    if (poor_pct <= threshold) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "match_quality",
        glue::glue("Match quality acceptable ({poor_pct}% poor matches)"),
        "PASS"
      ))
    } else {
      checks <- dplyr::bind_rows(checks, .make_check(
        "match_quality",
        glue::glue("{poor_pct}% poor Melissa matches (threshold: {threshold}%)"),
        "WARN",
        n_issues = n_poor,
        detail   = glue::glue("{n_poor}/{n_total} rows with poor match quality")
      ))
    }
  } else {
    checks <- dplyr::bind_rows(checks, .make_check(
      "match_quality",
      "Match quality columns not present (skipped)",
      "INFO"
    ))
  }

  # --- Check 3: ID match rate (WARN) ----
  if ("random_id" %in% names(df)) {
    n_total <- nrow(df)
    n_with_id <- sum(!is.na(df$random_id) & df$random_id != "", na.rm = TRUE)
    id_pct <- if (n_total > 0) round(n_with_id / n_total * 100, 1) else 0
    n_missing_id <- n_total - n_with_id

    if (id_pct >= 95) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "id_match_rate",
        glue::glue("ID match rate: {id_pct}% ({n_with_id}/{n_total})"),
        "PASS"
      ))
    } else if (id_pct >= 80) {
      checks <- dplyr::bind_rows(checks, .make_check(
        "id_match_rate",
        glue::glue("ID match rate: {id_pct}% ({n_with_id}/{n_total})"),
        "WARN",
        n_issues = n_missing_id,
        detail   = glue::glue("{n_missing_id} rows without random_id")
      ))
    } else {
      checks <- dplyr::bind_rows(checks, .make_check(
        "id_match_rate",
        glue::glue("Low ID match rate: {id_pct}% ({n_with_id}/{n_total})"),
        "WARN",
        n_issues = n_missing_id,
        detail   = glue::glue("{n_missing_id} rows without random_id")
      ))
    }
  } else {
    checks <- dplyr::bind_rows(checks, .make_check(
      "id_match_rate",
      "random_id column not present",
      "WARN",
      n_issues = nrow(df),
      detail   = "Cannot assess ID match rate"
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
    module        = "melissa",
    stage         = "validation",
    snapshot_date = snapshot_date,
    strict        = strict,
    class_name    = c("alccdf_melissa_validation", "alccdf_validation")
  )

  if (verbose) {
    n_pass <- sum(checks$status == "PASS")
    n_err  <- sum(checks$status == "ERROR")
    n_warn <- sum(checks$status == "WARN")
    n_info <- sum(checks$status == "INFO")
    .msg_step("Checks: {n_pass} PASS, {n_err} ERROR, {n_warn} WARN, {n_info} INFO")
    if (result$passed) {
      .msg_success("Melissa validation passed")
    } else {
      .msg_warn("Melissa validation completed with errors")
    }
  }

  result
}


#' Print method for Melissa validation objects
#'
#' @param x An \code{alccdf_melissa_validation} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_melissa_validation <- function(x, ...) {
  # Delegate to the base validation print method
  NextMethod("print", x)
}
