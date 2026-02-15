#' @title Database Module: Write
#' @description Write ALccdfDB data snapshots to DuckDB with automatic
#'   schema detection, column registration, and snapshot tracking.
#' @name db-write
NULL

#' Write a data snapshot to DuckDB
#'
#' Extracts data from an ALccdfDB S3 object, adds a \code{snapshot_date} column
#' if not present, writes to a DuckDB table (append mode), and registers the
#' snapshot in the internal tracking table.
#'
#' @param conn A DBI connection object from \code{\link{db_init}}.
#' @param obj An ALccdfDB S3 object (any module) or a data frame.
#' @param table_name Character. Name of the target DuckDB table.
#' @param snapshot_date Date. Snapshot date. If NULL, extracted from the object's
#'   metadata or defaults to today.
#' @param is_current Logical. Mark this snapshot as the current (active) one?
#'   Default FALSE.
#' @param overwrite Logical. If TRUE and the table already exists with data for
#'   this snapshot_date, the old data is removed first. Default FALSE.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible table name.
#'
#' @examples
#' \dontrun{
#' conn <- db_init("output/alccdfdb.duckdb")
#' db_write_snapshot(conn, program_clean, "programs",
#'                   snapshot_date = "2025-06-11", is_current = TRUE)
#' db_close(conn)
#' }
#'
#' @export
db_write_snapshot <- function(conn, obj, table_name,
                               snapshot_date = NULL,
                               is_current = FALSE,
                               overwrite = FALSE,
                               verbose = TRUE) {
  .check_suggested("duckdb", "DuckDB database")
  .check_suggested("DBI", "Database connectivity")
  .db_validate_conn(conn)

  # --- Extract data ----
  df <- if (is.data.frame(obj)) {
    tibble::as_tibble(obj)
  } else if (!is.null(obj$data)) {
    tibble::as_tibble(obj$data)
  } else {
    cli::cli_abort("Cannot extract data from {.cls {class(obj)[1]}} object.")
  }

  # --- Resolve snapshot_date ----
  if (is.null(snapshot_date)) {
    if (!is.data.frame(obj) && !is.null(obj$meta$snapshot_date)) {
      snapshot_date <- obj$meta$snapshot_date
    } else {
      snapshot_date <- Sys.Date()
    }
  }
  if (is.character(snapshot_date)) {
    snapshot_date <- as.Date(snapshot_date)
  }

  # --- Add snapshot_date column if not present ----
  if (!"snapshot_date" %in% names(df)) {
    df$snapshot_date <- snapshot_date
  }

  if (verbose) {
    .msg_step("Writing {nrow(df)} rows to table {.val {table_name}} (snapshot: {snapshot_date})")
  }

  # --- Handle existing data ----
  table_exists <- table_name %in% DBI::dbListTables(conn)

  if (table_exists && overwrite) {
    # Remove existing data for this snapshot_date from the table
    if ("snapshot_date" %in% DBI::dbListFields(conn, table_name)) {
      n_deleted <- DBI::dbExecute(conn, glue::glue(
        "DELETE FROM \"{table_name}\" WHERE snapshot_date = '{snapshot_date}'"
      ))
      if (verbose && n_deleted > 0) {
        .msg_step("Removed {n_deleted} existing rows for snapshot {snapshot_date}")
      }
    }
  }

  # --- Register column types ----
  .db_register_columns(conn, table_name, df)

  # --- Prepare and write data ----
  write_df <- .db_prepare_for_write(df)

  if (table_exists) {
    # Check column compatibility
    existing_cols <- DBI::dbListFields(conn, table_name)
    new_cols <- names(write_df)
    common_cols <- intersect(new_cols, existing_cols)
    missing_in_db <- setdiff(new_cols, existing_cols)

    if (length(missing_in_db) > 0 && verbose) {
      .msg_warn("New data has {length(missing_in_db)} column(s) not in existing table: {paste(utils::head(missing_in_db, 5), collapse = ', ')}")
    }

    # Write only common columns
    write_df <- write_df[, common_cols, drop = FALSE]
    DBI::dbAppendTable(conn, table_name, write_df)
  } else {
    DBI::dbWriteTable(conn, table_name, write_df)
  }

  # --- Register snapshot ----
  .db_register_snapshot(conn, table_name, snapshot_date,
                         n_rows = nrow(df), is_current = is_current)

  # --- Update metadata ----
  .db_upsert_meta(conn, "last_modified_at",
                    format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

  if (verbose) {
    .msg_success("Wrote {nrow(df)} rows x {ncol(df)} cols to {.val {table_name}}")
  }

  invisible(table_name)
}


#' Write Melissa geocoding data to DuckDB
#'
#' Convenience function for writing Melissa geocoding results (programs or
#' households) to the database.
#'
#' @param conn A DBI connection object from \code{\link{db_init}}.
#' @param obj An \code{alccdf_melissa_programs} or
#'   \code{alccdf_melissa_households} object.
#' @param table_name Character. Table name. Default auto-detected from object
#'   class: \code{"programs_melissa"} or \code{"subsidy_melissa"}.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible table name.
#'
#' @export
db_write_melissa <- function(conn, obj,
                              table_name = NULL,
                              verbose = TRUE) {
  .check_suggested("duckdb", "DuckDB database")
  .check_suggested("DBI", "Database connectivity")

  # --- Auto-detect table name ----
  if (is.null(table_name)) {
    if (inherits(obj, "alccdf_melissa_programs")) {
      table_name <- "programs_melissa"
    } else if (inherits(obj, "alccdf_melissa_households")) {
      table_name <- "subsidy_melissa"
    } else {
      cli::cli_abort(c(
        "Cannot auto-detect table name.",
        "i" = "Expected {.cls alccdf_melissa_programs} or {.cls alccdf_melissa_households}.",
        "i" = "Got {.cls {class(obj)[1]}}."
      ))
    }
  }

  # --- Resolve snapshot_date ----
  snapshot_date <- if (!is.null(obj$meta$snapshot_date)) {
    obj$meta$snapshot_date
  } else {
    Sys.Date()
  }

  db_write_snapshot(conn, obj, table_name,
                     snapshot_date = snapshot_date,
                     is_current = TRUE,
                     verbose = verbose)
}


#' Write linked dataset to DuckDB
#'
#' Convenience function for writing linkage results (program-QRIS,
#' subsidy-program, or master) to the database.
#'
#' @param conn A DBI connection object from \code{\link{db_init}}.
#' @param obj A linkage S3 object (\code{alccdf_linkage_program_qris},
#'   \code{alccdf_linkage_subsidy_program}, or \code{alccdf_linkage_master}).
#' @param table_name Character. Table name. Default auto-detected from object
#'   class: \code{"linked_program_qris"}, \code{"linked_subsidy_program"}, or
#'   \code{"master_linked"}.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible table name.
#'
#' @export
db_write_linked <- function(conn, obj,
                             table_name = NULL,
                             verbose = TRUE) {
  .check_suggested("duckdb", "DuckDB database")
  .check_suggested("DBI", "Database connectivity")

  # --- Auto-detect table name ----
  if (is.null(table_name)) {
    if (inherits(obj, "alccdf_linkage_master")) {
      table_name <- "master_linked"
    } else if (inherits(obj, "alccdf_linkage_program_qris")) {
      table_name <- "linked_program_qris"
    } else if (inherits(obj, "alccdf_linkage_subsidy_program")) {
      table_name <- "linked_subsidy_program"
    } else {
      cli::cli_abort(c(
        "Cannot auto-detect table name.",
        "i" = "Expected a linkage S3 object.",
        "i" = "Got {.cls {class(obj)[1]}}."
      ))
    }
  }

  # --- Resolve snapshot_date ----
  snapshot_date <- if (!is.null(obj$meta$snapshot_date)) {
    obj$meta$snapshot_date
  } else {
    Sys.Date()
  }

  db_write_snapshot(conn, obj, table_name,
                     snapshot_date = snapshot_date,
                     is_current = TRUE,
                     overwrite = TRUE,
                     verbose = verbose)
}
