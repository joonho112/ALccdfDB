#' @title Database Module: Schema (Internal)
#' @description Internal functions for managing the DuckDB database schema,
#'   metadata, column registry, and snapshot tracking tables.
#' @name db-schema
#' @keywords internal
NULL

#' Create ALccdfDB database schema
#'
#' Creates the three internal system tables (_alccdfdb_meta, _alccdfdb_columns,
#' _alccdfdb_snapshots) and writes initial metadata.
#'
#' @param conn A DBI connection object
#' @keywords internal
.db_create_schema <- function(conn) {
  # --- Metadata table ----
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS _alccdfdb_meta (
      key   VARCHAR PRIMARY KEY,
      value VARCHAR
    )
  ")

  # --- Column registry ----
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS _alccdfdb_columns (
      table_name  VARCHAR NOT NULL,
      column_name VARCHAR NOT NULL,
      column_type VARCHAR,
      description VARCHAR,
      PRIMARY KEY (table_name, column_name)
    )
  ")

  # --- Snapshot registry ----
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS _alccdfdb_snapshots (
      snapshot_date DATE NOT NULL,
      table_name    VARCHAR NOT NULL,
      n_rows        INTEGER,
      is_current    BOOLEAN DEFAULT FALSE,
      loaded_at     TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (snapshot_date, table_name)
    )
  ")

  # --- Write initial metadata ----
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  pkg_version <- tryCatch(
    as.character(utils::packageVersion("ALccdfDB")),
    error = function(e) "unknown"
  )

  .db_upsert_meta(conn, "schema_version", "1.0")
  .db_upsert_meta(conn, "package", "ALccdfDB")
  .db_upsert_meta(conn, "created_at", now)
  .db_upsert_meta(conn, "last_modified_at", now)
  .db_upsert_meta(conn, "alccdfdb_version", pkg_version)
}


#' Register column metadata for a table
#'
#' Records column names, types, and optional descriptions in the column registry
#' for future reference and type reconstruction.
#'
#' @param conn A DBI connection object
#' @param table_name Character. Table name.
#' @param df Data frame whose columns to register.
#' @keywords internal
.db_register_columns <- function(conn, table_name, df) {
  # Remove existing entries for this table
  DBI::dbExecute(conn,
    "DELETE FROM _alccdfdb_columns WHERE table_name = ?",
    params = list(table_name)
  )

  if (ncol(df) == 0) return(invisible(NULL))

  # Build registry entries
  entries <- lapply(names(df), function(col_name) {
    col <- df[[col_name]]
    r_type <- .db_detect_r_type(col)
    data.frame(
      table_name  = table_name,
      column_name = col_name,
      column_type = r_type,
      description = NA_character_,
      stringsAsFactors = FALSE
    )
  })

  registry <- do.call(rbind, entries)
  DBI::dbAppendTable(conn, "_alccdfdb_columns", registry)
}


#' Register a snapshot in the snapshot registry
#'
#' @param conn A DBI connection object
#' @param table_name Character. Table name.
#' @param snapshot_date Date. Snapshot date.
#' @param n_rows Integer. Number of rows.
#' @param is_current Logical. Whether this is the current snapshot.
#' @keywords internal
.db_register_snapshot <- function(conn, table_name, snapshot_date,
                                   n_rows, is_current = FALSE) {
  # Format snapshot_date
  snap_str <- as.character(snapshot_date)

  # Remove existing entry for this table + date combination
  DBI::dbExecute(conn,
    "DELETE FROM _alccdfdb_snapshots WHERE snapshot_date = ? AND table_name = ?",
    params = list(snap_str, table_name)
  )

  # If marking as current, unset current for this table
  if (is_current) {
    DBI::dbExecute(conn,
      "UPDATE _alccdfdb_snapshots SET is_current = FALSE WHERE table_name = ?",
      params = list(table_name)
    )
  }

  # Insert new snapshot entry
  DBI::dbExecute(conn, glue::glue(
    "INSERT INTO _alccdfdb_snapshots (snapshot_date, table_name, n_rows, is_current) ",
    "VALUES ('{snap_str}', '{table_name}', {as.integer(n_rows)}, {tolower(as.character(is_current))})"
  ))
}


#' Get list of snapshots for a table
#'
#' @param conn A DBI connection object
#' @param table_name Character. Table name.
#' @return A tibble with snapshot dates and metadata.
#' @keywords internal
.db_get_snapshots <- function(conn, table_name) {
  tibble::as_tibble(DBI::dbGetQuery(conn,
    "SELECT snapshot_date, table_name, n_rows, is_current, loaded_at
     FROM _alccdfdb_snapshots
     WHERE table_name = ?
     ORDER BY snapshot_date DESC",
    params = list(table_name)
  ))
}


#' Get the current or latest snapshot date for a table
#'
#' @param conn A DBI connection object
#' @param table_name Character. Table name.
#' @return Date value, or NULL if no snapshots exist.
#' @keywords internal
.db_get_current_snapshot <- function(conn, table_name) {
  # Try current first
  result <- DBI::dbGetQuery(conn,
    "SELECT snapshot_date FROM _alccdfdb_snapshots
     WHERE table_name = ? AND is_current = TRUE
     LIMIT 1",
    params = list(table_name)
  )

  if (nrow(result) > 0) {
    return(as.Date(result$snapshot_date[1]))
  }

  # Fall back to latest
  result <- DBI::dbGetQuery(conn,
    "SELECT snapshot_date FROM _alccdfdb_snapshots
     WHERE table_name = ?
     ORDER BY snapshot_date DESC
     LIMIT 1",
    params = list(table_name)
  )

  if (nrow(result) > 0) {
    return(as.Date(result$snapshot_date[1]))
  }

  NULL
}


#' Prepare data frame for DuckDB write
#'
#' Converts factors to character, POSIXct to character, and Date to character
#' for reliable DuckDB storage.
#'
#' @param df A data frame
#' @return A plain data frame suitable for DBI::dbWriteTable
#' @keywords internal
.db_prepare_for_write <- function(df) {
  for (col in names(df)) {
    if (is.factor(df[[col]])) {
      df[[col]] <- as.character(df[[col]])
    }
    if (inherits(df[[col]], "POSIXct")) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  as.data.frame(df, stringsAsFactors = FALSE)
}


#' Reconstruct R types from column registry after reading from DuckDB
#'
#' @param df Data frame read from DuckDB
#' @param conn DBI connection
#' @param table_name Table name for looking up column types
#' @return Data frame with reconstructed R types
#' @keywords internal
.db_reconstruct_types <- function(df, conn, table_name) {
  if (!"_alccdfdb_columns" %in% DBI::dbListTables(conn)) {
    return(df)
  }

  type_reg <- DBI::dbGetQuery(conn,
    "SELECT column_name, column_type FROM _alccdfdb_columns WHERE table_name = ?",
    params = list(table_name)
  )

  if (nrow(type_reg) == 0) return(df)

  for (i in seq_len(nrow(type_reg))) {
    col_name <- type_reg$column_name[i]
    col_type <- type_reg$column_type[i]

    if (!col_name %in% names(df)) next

    col <- df[[col_name]]

    if (col_type == "Date" && !inherits(col, "Date")) {
      df[[col_name]] <- as.Date(col)
    } else if (col_type == "integer" && !is.integer(col)) {
      df[[col_name]] <- as.integer(col)
    } else if (col_type == "logical" && !is.logical(col)) {
      df[[col_name]] <- as.logical(col)
    } else if (col_type == "numeric" && !is.numeric(col)) {
      df[[col_name]] <- as.numeric(col)
    }
    # character columns are already correct from DuckDB
  }

  df
}


#' Detect R type of a column
#' @keywords internal
.db_detect_r_type <- function(x) {
  if (is.factor(x)) return("factor")
  if (inherits(x, "Date")) return("Date")
  if (inherits(x, "POSIXct")) return("POSIXct")
  if (is.logical(x)) return("logical")
  if (is.integer(x)) return("integer")
  if (is.numeric(x)) return("numeric")
  "character"
}
