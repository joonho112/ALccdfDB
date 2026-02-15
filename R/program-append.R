#' @title Program Module: Append
#' @description Combine multiple cleaned program types (Centers, Family/Group
#'   Homes, Exempt, Excepted) into a single unified data frame with aligned
#'   columns and consistent facility type labels.
#' @name program-append
NULL

#' Append multiple program types into a unified data set
#'
#' Takes one or more \code{alccdf_program_clean} objects and stacks them into a
#' single data frame, aligning columns and filling type-specific fields with NA
#' where absent.
#'
#' @param ... One or more \code{alccdf_program_clean} objects, or a single
#'   named list of such objects (as returned by \code{\link{program_clean_all}}).
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_program_unified} S3 object containing the combined
#'   data.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Validates that all inputs are \code{alccdf_program_clean} objects.
#'   \item Validates that all inputs share the same \code{snapshot_date}.
#'   \item Aligns columns: the union of all columns is used; missing columns in
#'     any type are filled with NA.
#'   \item Checks for duplicate \code{facility_id} across types (warns only).
#'   \item Returns a unified object with provenance metadata.
#' }
#'
#' @examples
#' \dontrun{
#' unified <- program_append_types(clean_center, clean_home, clean_exempt)
#' unified <- program_append_types(clean_list)  # also works with a list
#' }
#'
#' @export
program_append_types <- function(..., verbose = TRUE) {
  # --- Collect inputs ----
  inputs <- list(...)

  # If a single unnamed list was passed, unwrap it
  if (length(inputs) == 1 && is.list(inputs[[1]]) &&
      !inherits(inputs[[1]], "alccdf_program_clean")) {
    inputs <- inputs[[1]]
  }

  if (length(inputs) == 0) {
    cli::cli_abort("At least one {.cls alccdf_program_clean} object required.")
  }

  # --- Validate all inputs ----
  for (i in seq_along(inputs)) {
    .assert_class(inputs[[i]], "alccdf_program_clean", "program_append_types")
  }

  if (verbose) {
    .msg_header("Appending Program Types", level = 3)
    types <- vapply(inputs, function(x) x$meta$program_type, character(1))
    .msg_step("Types to append: {paste(types, collapse = ', ')}")
  }

  # --- Check consistent snapshot dates ----
  snap_dates <- vapply(inputs, function(x) {
    as.character(x$meta$snapshot_date)
  }, character(1))
  unique_dates <- unique(snap_dates)
  if (length(unique_dates) > 1) {
    .msg_warn(
      "Multiple snapshot dates detected: {paste(unique_dates, collapse = ', ')}. ",
      "Proceeding but data may span multiple snapshots."
    )
  }
  snapshot_date <- as.Date(unique_dates[1])

  # --- Extract data frames ----
  dfs <- lapply(inputs, function(x) x$data)

  # --- Align columns (union) and bind ----
  all_cols  <- unique(unlist(lapply(dfs, names)))
  aligned   <- lapply(dfs, function(df) {
    missing <- setdiff(all_cols, names(df))
    for (col in missing) {
      df[[col]] <- NA
    }
    df[all_cols]
  })

  combined <- dplyr::bind_rows(aligned)

  if (verbose) {
    .msg_step("Combined: {nrow(combined)} rows x {ncol(combined)} cols")
  }

  # --- Check for cross-type duplicate facility_id ----
  if ("facility_id" %in% names(combined)) {
    fids <- combined$facility_id[!is.na(combined$facility_id)]
    dup_fids <- fids[duplicated(fids)]
    if (length(dup_fids) > 0) {
      n_unique_dups <- length(unique(dup_fids))
      .msg_warn(
        "{n_unique_dups} facility_id(s) appear in multiple program types"
      )
    }
  }

  # --- Ensure facility_type is a factor ----
  if ("facility_type" %in% names(combined)) {
    combined$facility_type <- factor(
      combined$facility_type,
      levels = alccdf_facility_type_levels()
    )
  }

  # --- Build types summary ----
  type_counts <- if ("facility_type" %in% names(combined)) {
    as.list(table(combined$facility_type, useNA = "ifany"))
  } else {
    list()
  }

  # --- Build S3 object ----
  obj <- .make_alccdf_obj(
    data          = combined,
    class_name    = "alccdf_program_unified",
    module        = "program",
    stage         = "unified",
    snapshot_date = snapshot_date,
    extra_meta    = list(
      types_included = vapply(inputs, function(x) x$meta$program_type,
                              character(1)),
      type_counts    = type_counts,
      n_types        = length(inputs)
    )
  )
  obj <- .log_step(obj, glue::glue(
    "Appended {length(inputs)} program types: {nrow(combined)} total rows"
  ))

  if (verbose) {
    .msg_success("Append complete: {nrow(combined)} rows from {length(inputs)} types")
  }

  obj
}


#' Print method for unified program objects
#'
#' @param x An \code{alccdf_program_unified} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_program_unified <- function(x, ...) {
  cli::cli_h2("ALccdfDB Program Data (Unified)")
  cli::cli_alert_info("Snapshot: {x$meta$snapshot_date}")
  cli::cli_alert_info("Dimensions: {x$meta$n_rows} rows x {x$meta$n_cols} cols")
  cli::cli_alert_info(
    "Types: {paste(x$meta$types_included, collapse = ', ')}"
  )

  if (length(x$meta$type_counts) > 0) {
    cli::cli_h3("Type Counts")
    for (nm in names(x$meta$type_counts)) {
      cli::cli_alert_info("  {nm}: {x$meta$type_counts[[nm]]}")
    }
  }

  if (length(x$meta$processing_log) > 0) {
    cli::cli_h3("Processing Log")
    for (entry in x$meta$processing_log) {
      cli::cli_text(entry)
    }
  }
  invisible(x)
}
