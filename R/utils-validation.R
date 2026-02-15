#' @title Validation Utility Functions
#' @description Shared validation infrastructure used by all modules.
#' @name utils-validation
#' @keywords internal
NULL

#' Create a validation check result
#'
#' @param check_id Character string check identifier
#' @param description Human-readable description
#' @param status One of "PASS", "ERROR", "WARN", "INFO"
#' @param n_issues Number of issues found
#' @param detail Optional detail string
#' @return A one-row tibble
#' @keywords internal
.make_check <- function(check_id, description, status, n_issues = 0L,
                         detail = NA_character_) {
  stopifnot(status %in% c("PASS", "ERROR", "WARN", "INFO"))
  tibble::tibble(
    check_id = check_id,
    description = description,
    status = status,
    n_issues = as.integer(n_issues),
    detail = detail
  )
}

#' Build a validation result object
#'
#' @param checks A tibble of check results (from .make_check)
#' @param issues A tibble of specific problematic rows (optional)
#' @param module Module name
#' @param stage Processing stage
#' @param snapshot_date Snapshot date
#' @param strict Whether strict mode was used
#' @param class_name S3 class name for the validation object
#' @return An S3 validation object
#' @keywords internal
.build_validation_result <- function(checks, issues = NULL, module, stage = "validation",
                                      snapshot_date = NULL, strict = FALSE,
                                      class_name = NULL) {
  n_errors <- sum(checks$status == "ERROR")
  n_warnings <- sum(checks$status == "WARN")
  n_info <- sum(checks$status == "INFO")
  passed <- n_errors == 0 || !strict

  if (is.null(class_name)) {
    class_name <- paste0("alccdf_", module, "_validation")
  }

  obj <- structure(
    list(
      passed = passed,
      n_errors = n_errors,
      n_warnings = n_warnings,
      n_info = n_info,
      checks = checks,
      issues = issues %||% tibble::tibble(),
      meta = list(
        module = module,
        snapshot_date = snapshot_date,
        validated_at = Sys.time(),
        strict = strict
      )
    ),
    class = class_name
  )

  if (strict && n_errors > 0) {
    error_checks <- checks[checks$status == "ERROR", ]
    cli::cli_abort(c(
      "Validation failed in strict mode ({n_errors} error{?s}):",
      set_names(error_checks$description, rep("x", nrow(error_checks)))
    ))
  }

  obj
}

#' Print method for validation objects
#'
#' @param x Validation object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_validation <- function(x, ...) {
  status_icon <- if (x$passed) "\u2714" else "\u2718"
  status_word <- if (x$passed) "PASSED" else "FAILED"

  cli::cli_h2("ALccdfDB Validation Report: {x$meta$module}")
  cli::cli_alert_info("Status: {status_icon} {status_word}")
  if (!is.null(x$meta$snapshot_date)) {
    cli::cli_alert_info("Snapshot: {x$meta$snapshot_date}")
  }
  cli::cli_alert_info("Checks: {nrow(x$checks)} total")

  if (x$n_errors > 0) cli::cli_alert_danger("Errors: {x$n_errors}")
  if (x$n_warnings > 0) cli::cli_alert_warning("Warnings: {x$n_warnings}")
  if (x$n_info > 0) cli::cli_alert_info("Info: {x$n_info}")

  # Print individual checks
  for (i in seq_len(nrow(x$checks))) {
    row <- x$checks[i, ]
    icon <- switch(row$status,
      "PASS" = "\u2714",
      "ERROR" = "\u2718",
      "WARN" = "\u26A0",
      "INFO" = "\u2139"
    )
    msg <- glue::glue("{icon} [{row$status}] {row$description}")
    if (row$n_issues > 0) {
      msg <- glue::glue("{msg} ({row$n_issues} issue{if(row$n_issues > 1) 's' else ''})")
    }
    cli::cli_text(msg)
  }

  invisible(x)
}
