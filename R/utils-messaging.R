#' @title Internal Messaging Utilities
#' @description CLI-based messaging helpers for consistent user feedback.
#' @name utils-messaging
#' @keywords internal
NULL

#' Display a processing step message
#' @param msg Character string message (supports cli glue syntax)
#' @param .envir Environment for cli glue evaluation
#' @keywords internal
.msg_step <- function(msg, .envir = parent.frame()) {
  cli::cli_alert_info(msg, .envir = .envir)
}

#' Display a success message
#' @param msg Character string message (supports cli glue syntax)
#' @param .envir Environment for cli glue evaluation
#' @keywords internal
.msg_success <- function(msg, .envir = parent.frame()) {
  cli::cli_alert_success(msg, .envir = .envir)
}

#' Display a warning message
#' @param msg Character string message (supports cli glue syntax)
#' @param .envir Environment for cli glue evaluation
#' @keywords internal
.msg_warn <- function(msg, .envir = parent.frame()) {
  cli::cli_alert_warning(msg, .envir = .envir)
}

#' Display an error message
#' @param msg Character string message (supports cli glue syntax)
#' @param .envir Environment for cli glue evaluation
#' @keywords internal
.msg_error <- function(msg, .envir = parent.frame()) {
  cli::cli_alert_danger(msg, .envir = .envir)
}

#' Display a header message
#' @param msg Character string message (supports cli glue syntax)
#' @param level Header level (1, 2, or 3)
#' @param .envir Environment for cli glue evaluation
#' @keywords internal
.msg_header <- function(msg, level = 1, .envir = parent.frame()) {
  switch(as.character(level),
    "1" = cli::cli_h1(msg, .envir = .envir),
    "2" = cli::cli_h2(msg, .envir = .envir),
    "3" = cli::cli_h3(msg, .envir = .envir)
  )
}

#' Format a count for display
#' @param n Numeric count
#' @param label Label for the count
#' @param .envir Environment for cli glue evaluation
#' @keywords internal
.msg_count <- function(n, label, .envir = parent.frame()) {
  cli::cli_alert_info("{.val {n}} {label}", .envir = .envir)
}
