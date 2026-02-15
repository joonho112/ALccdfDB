#' @title Internal Helper Utilities
#' @description Common helper functions used across modules.
#' @name utils-helpers
#' @keywords internal
NULL

#' Create an ALccdfDB S3 object
#'
#' @param data A data frame or tibble
#' @param class_name Character string for the S3 class name
#' @param module Character string for the module name
#' @param stage Character string for the processing stage
#' @param snapshot_date Date of the snapshot
#' @param extra_meta Named list of additional metadata
#' @param diagnostics Named list of diagnostics
#' @return An S3 object of the specified class
#' @keywords internal
.make_alccdf_obj <- function(data, class_name, module, stage,
                              snapshot_date = NULL, extra_meta = list(),
                              diagnostics = list()) {
  meta <- list(
    module = module,
    stage = stage,
    snapshot_date = snapshot_date,
    n_rows = nrow(data),
    n_cols = ncol(data),
    created_at = Sys.time(),
    package_version = as.character(utils::packageVersion("ALccdfDB")),
    processing_log = character()
  )
  meta <- c(meta, extra_meta)

  obj <- list(
    data = tibble::as_tibble(data),
    meta = meta,
    diagnostics = diagnostics
  )
  class(obj) <- class_name
  obj
}

#' Append to the processing log of an ALccdfDB object
#'
#' @param obj An ALccdfDB S3 object
#' @param msg Character string describing the processing step
#' @return The modified object (invisibly)
#' @keywords internal
.log_step <- function(obj, msg) {
  timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
  obj$meta$processing_log <- c(
    obj$meta$processing_log,
    paste(timestamp, msg)
  )
  obj
}

#' Extract data from an ALccdfDB S3 object
#'
#' @param obj An ALccdfDB S3 object
#' @return A tibble
#' @export
alccdf_data <- function(obj) {
  if (!is.list(obj) || is.null(obj$data)) {
    cli::cli_abort("Expected an ALccdfDB S3 object with a {.field data} element.")
  }
  obj$data
}

#' Extract metadata from an ALccdfDB S3 object
#'
#' @param obj An ALccdfDB S3 object
#' @return A list of metadata
#' @export
alccdf_meta <- function(obj) {
  if (!is.list(obj) || is.null(obj$meta)) {
    cli::cli_abort("Expected an ALccdfDB S3 object with a {.field meta} element.")
  }
  obj$meta
}

#' Validate that an object has the expected class
#'
#' @param obj Object to check
#' @param expected_class Character string of expected class
#' @param fn_name Name of the calling function for error messages
#' @keywords internal
.assert_class <- function(obj, expected_class, fn_name = NULL) {
  if (!inherits(obj, expected_class)) {
    fn_msg <- if (!is.null(fn_name)) paste0(" in ", fn_name, "()") else ""
    cli::cli_abort(c(
      paste0("Expected an {.cls {expected_class}} object", fn_msg, "."),
      "i" = "Got {.cls {class(obj)[1]}} instead."
    ))
  }
  invisible(TRUE)
}

#' Check if a suggested package is available
#'
#' @param pkg Character string package name
#' @param reason Why the package is needed
#' @keywords internal
.check_suggested <- function(pkg, reason = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    reason_msg <- if (!is.null(reason)) {
      glue::glue(" ({reason})")
    } else {
      ""
    }
    cli::cli_abort(c(
      "Package {.pkg {pkg}} is required{reason_msg}.",
      "i" = "Install it with {.code install.packages(\"{pkg}\")}."
    ))
  }
  invisible(TRUE)
}

#' Generate a random ID string
#'
#' @param prefix Character prefix for the ID
#' @param n Number of IDs to generate
#' @param width Number of digits in the numeric part
#' @return Character vector of IDs
#' @keywords internal
.generate_random_ids <- function(prefix = "ID", n = 1, width = 5) {
  nums <- sample.int(10^width - 1, size = n, replace = FALSE)
  sprintf(paste0(prefix, "%0", width, "d"), nums)
}
