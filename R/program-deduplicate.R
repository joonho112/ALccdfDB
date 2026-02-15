#' @title Program Module: Deduplicate
#' @description Identify and remove exact duplicate records from unified program
#'   data based on configurable key columns.
#' @name program-deduplicate
NULL

#' Deduplicate unified program data
#'
#' Identifies and removes exact duplicate records from an
#' \code{alccdf_program_unified} object. By default, duplicates are identified
#' using \code{facility_id}, but additional key columns can be specified.
#'
#' @param unified_obj An \code{alccdf_program_unified} object from
#'   \code{\link{program_append_types}}
#' @param key_cols Character vector of column names used to identify duplicates.
#'   Default is \code{"facility_id"}.
#' @param keep Strategy for which duplicate to keep: \code{"first"} (default)
#'   or \code{"last"}.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_program_deduped} S3 object
#'
#' @details
#' When duplicates are found on the specified key columns, only the first (or
#' last) occurrence is retained. The removed duplicates are stored in
#' \code{obj$diagnostics$removed_duplicates} for inspection.
#'
#' @examples
#' \dontrun{
#' deduped <- program_deduplicate(unified)
#' deduped <- program_deduplicate(unified, key_cols = c("facility_id", "facility_type"))
#' }
#'
#' @export
program_deduplicate <- function(unified_obj,
                                key_cols = "facility_id",
                                keep = c("first", "last"),
                                verbose = TRUE) {
  .assert_class(unified_obj, "alccdf_program_unified", "program_deduplicate")
  keep <- match.arg(keep)

  df            <- unified_obj$data
  snapshot_date <- unified_obj$meta$snapshot_date

  if (verbose) {
    .msg_header("Deduplicating Program Data", level = 3)
    .msg_step("Key columns: {paste(key_cols, collapse = ', ')}")
    .msg_step("Keep strategy: {keep}")
    .msg_step("Input rows: {nrow(df)}")
  }

  # --- Validate key columns exist ----
  missing_keys <- setdiff(key_cols, names(df))
  if (length(missing_keys) > 0) {
    cli::cli_abort(
      "Key columns not found in data: {.field {paste(missing_keys, collapse = ', ')}}"
    )
  }

  # --- Identify duplicates ----
  # Remove rows where all key columns are NA (cannot assess duplicates)
  key_df       <- df[key_cols]
  all_keys_na  <- apply(key_df, 1, function(x) all(is.na(x)))

  # Among rows with non-NA keys, find duplicates
  non_na_rows  <- !all_keys_na
  dup_mask     <- logical(nrow(df))

  if (any(non_na_rows)) {
    if (keep == "first") {
      dup_mask[non_na_rows] <- duplicated(df[non_na_rows, key_cols, drop = FALSE])
    } else {
      dup_mask[non_na_rows] <- duplicated(
        df[non_na_rows, key_cols, drop = FALSE], fromLast = TRUE
      )
    }
  }

  n_dups <- sum(dup_mask)

  if (verbose) {
    .msg_step("Duplicates found: {n_dups}")
  }

  # --- Store removed duplicates ----
  removed <- df[dup_mask, ]
  df_out  <- df[!dup_mask, ]

  if (verbose && n_dups > 0) {
    # Summarise duplicates by facility_type if available
    if ("facility_type" %in% names(removed)) {
      dup_by_type <- table(removed$facility_type, useNA = "ifany")
      for (nm in names(dup_by_type)) {
        .msg_step("  Removed from {nm}: {dup_by_type[[nm]]}")
      }
    }
  }

  # --- Build S3 object ----
  obj <- .make_alccdf_obj(
    data          = df_out,
    class_name    = "alccdf_program_deduped",
    module        = "program",
    stage         = "deduped",
    snapshot_date = snapshot_date,
    extra_meta    = list(
      types_included     = unified_obj$meta$types_included,
      key_cols           = key_cols,
      keep               = keep,
      n_duplicates_found = n_dups,
      n_rows_before      = nrow(df),
      n_rows_after       = nrow(df_out)
    ),
    diagnostics   = list(
      removed_duplicates = removed
    )
  )
  obj <- .log_step(obj, glue::glue(
    "Deduplicated on [{paste(key_cols, collapse = ', ')}]: ",
    "removed {n_dups}, kept {nrow(df_out)} rows"
  ))

  if (verbose) {
    .msg_success(
      "Deduplication complete: {nrow(df_out)} rows ({n_dups} removed)"
    )
  }

  obj
}


#' Print method for deduplicated program objects
#'
#' @param x An \code{alccdf_program_deduped} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_program_deduped <- function(x, ...) {
  cli::cli_h2("ALccdfDB Program Data (Deduplicated)")
  cli::cli_alert_info("Snapshot: {x$meta$snapshot_date}")
  cli::cli_alert_info("Dimensions: {x$meta$n_rows} rows x {x$meta$n_cols} cols")
  cli::cli_alert_info("Key columns: {paste(x$meta$key_cols, collapse = ', ')}")
  cli::cli_alert_info("Duplicates removed: {x$meta$n_duplicates_found}")
  cli::cli_alert_info(
    "Rows: {x$meta$n_rows_before} -> {x$meta$n_rows_after}"
  )

  if (length(x$meta$processing_log) > 0) {
    cli::cli_h3("Processing Log")
    for (entry in x$meta$processing_log) {
      cli::cli_text(entry)
    }
  }
  invisible(x)
}
