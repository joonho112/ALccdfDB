#' @title Database Module: Read
#' @description Read data from a DuckDB database with snapshot filtering, type
#'   reconstruction, and convenience accessors for Melissa and linked data.
#' @name db-read
NULL

#' Read a data snapshot from DuckDB
#'
#' Reads data for a specific snapshot date from a DuckDB table. If
#' \code{snapshot_date} is NULL, reads the current (is_current = TRUE) snapshot
#' or falls back to the latest available.
#'
#' @param conn A DBI connection object from \code{\link{db_init}}.
#' @param table_name Character. Name of the DuckDB table to read.
#' @param snapshot_date Date or character. Snapshot date to read. If NULL
#'   (default), reads the current or latest snapshot.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return A tibble with the requested data.
#'
#' @examples
#' \dontrun{
#' conn <- db_init("output/alccdfdb.duckdb", read_only = TRUE)
#' programs <- db_read_snapshot(conn, "programs")
#' programs_jun <- db_read_snapshot(conn, "programs",
#'                                   snapshot_date = "2025-06-11")
#' db_close(conn)
#' }
#'
#' @export
db_read_snapshot <- function(conn, table_name,
                              snapshot_date = NULL,
                              verbose = TRUE) {
  .check_suggested("duckdb", "DuckDB database")
  .check_suggested("DBI", "Database connectivity")
  .db_validate_conn(conn)

  if (!table_name %in% DBI::dbListTables(conn)) {
    cli::cli_abort("Table {.val {table_name}} not found in database.")
  }

  # --- Resolve snapshot_date ----
  has_snapshot_col <- "snapshot_date" %in% DBI::dbListFields(conn, table_name)

  if (is.null(snapshot_date) && has_snapshot_col) {
    # Try to find the current or latest snapshot from the registry
    snapshot_date <- .db_get_current_snapshot(conn, table_name)

    if (is.null(snapshot_date)) {
      # No registry entry; get latest from the actual data
      latest <- DBI::dbGetQuery(conn, glue::glue(
        "SELECT MAX(snapshot_date) AS snap FROM \"{table_name}\""
      ))
      if (!is.na(latest$snap[1])) {
        snapshot_date <- as.Date(latest$snap[1])
      }
    }
  }

  if (is.character(snapshot_date)) {
    snapshot_date <- as.Date(snapshot_date)
  }

  # --- Build query ----
  sql <- glue::glue("SELECT * FROM \"{table_name}\"")

  if (!is.null(snapshot_date) && has_snapshot_col) {
    sql <- glue::glue("{sql} WHERE snapshot_date = '{snapshot_date}'")
    if (verbose) {
      .msg_step("Reading {.val {table_name}} for snapshot {snapshot_date}")
    }
  } else {
    if (verbose) {
      .msg_step("Reading all rows from {.val {table_name}}")
    }
  }

  df <- tibble::as_tibble(DBI::dbGetQuery(conn, sql))

  # --- Reconstruct R types ----
  df <- .db_reconstruct_types(df, conn, table_name)

  if (nrow(df) == 0 && verbose) {
    .msg_warn("No data found in {.val {table_name}}")
    if (!is.null(snapshot_date)) {
      .msg_step("Requested snapshot_date: {snapshot_date}")
    }
  } else if (verbose) {
    .msg_success("Read {nrow(df)} rows x {ncol(df)} cols from {.val {table_name}}")
  }

  df
}


#' Read Melissa geocoding data from DuckDB
#'
#' Convenience function for reading Melissa geocoding results from the database.
#'
#' @param conn A DBI connection object from \code{\link{db_init}}.
#' @param type Character. Type of Melissa data to read: \code{"programs"} or
#'   \code{"households"}.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return A tibble with Melissa geocoding data.
#'
#' @export
db_read_melissa <- function(conn, type = c("programs", "households"),
                             verbose = TRUE) {
  .check_suggested("duckdb", "DuckDB database")
  .check_suggested("DBI", "Database connectivity")
  type <- match.arg(type)

  table_name <- switch(type,
    programs   = "programs_melissa",
    households = "subsidy_melissa"
  )

  if (!table_name %in% DBI::dbListTables(conn)) {
    cli::cli_abort(c(
      "Melissa table {.val {table_name}} not found in database.",
      "i" = "Write Melissa data first with {.fn db_write_melissa}."
    ))
  }

  db_read_snapshot(conn, table_name, verbose = verbose)
}


#' Read the latest snapshot from DuckDB
#'
#' Convenience function that reads the most recent snapshot of a table.
#'
#' @param conn A DBI connection object from \code{\link{db_init}}.
#' @param table_name Character. Name of the DuckDB table to read.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return A tibble with the latest snapshot data.
#'
#' @export
db_read_latest <- function(conn, table_name, verbose = TRUE) {
  .check_suggested("duckdb", "DuckDB database")
  .check_suggested("DBI", "Database connectivity")
  .db_validate_conn(conn)

  if (!table_name %in% DBI::dbListTables(conn)) {
    cli::cli_abort("Table {.val {table_name}} not found in database.")
  }

  has_snapshot_col <- "snapshot_date" %in% DBI::dbListFields(conn, table_name)

  if (has_snapshot_col) {
    # Get the latest snapshot_date
    latest <- DBI::dbGetQuery(conn, glue::glue(
      "SELECT MAX(snapshot_date) AS snap FROM \"{table_name}\""
    ))

    if (is.na(latest$snap[1])) {
      if (verbose) .msg_warn("No data found in {.val {table_name}}")
      return(tibble::tibble())
    }

    snapshot_date <- as.Date(latest$snap[1])
    db_read_snapshot(conn, table_name,
                      snapshot_date = snapshot_date,
                      verbose = verbose)
  } else {
    # No snapshot column -- read all data
    db_read_snapshot(conn, table_name, verbose = verbose)
  }
}


#' List available snapshots for a table
#'
#' Returns a summary of all snapshot dates registered for a given table,
#' including row counts and which snapshot is marked as current.
#'
#' @param conn A DBI connection object from \code{\link{db_init}}.
#' @param table_name Character. Name of the DuckDB table.
#' @return A tibble with columns: \code{snapshot_date}, \code{table_name},
#'   \code{n_rows}, \code{is_current}, \code{loaded_at}.
#'
#' @export
db_list_snapshots <- function(conn, table_name) {
  .check_suggested("duckdb", "DuckDB database")
  .check_suggested("DBI", "Database connectivity")
  .db_validate_conn(conn)

  .db_get_snapshots(conn, table_name)
}
