#' @title Database Module: Core
#' @description Initialize, close, query, and inspect a DuckDB database for
#'   persistent storage of ALccdfDB data with snapshot tracking.
#' @name db-core
NULL

#' Initialize DuckDB database
#'
#' Creates a new or opens an existing DuckDB database for storing ALccdfDB
#' snapshot data. New databases are initialized with the ALccdfDB schema
#' (metadata, column registry, and snapshot registry tables). Existing databases
#' are validated for schema compatibility.
#'
#' @param path Path to the DuckDB file. Default
#'   \code{"output/alccdfdb.duckdb"}. Use \code{":memory:"} for an in-memory
#'   database (data lost when connection is closed).
#' @param read_only Logical. Open in read-only mode? Default FALSE.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return A DBI connection object to the DuckDB database.
#'
#' @examples
#' \dontrun{
#' conn <- db_init("output/alccdfdb.duckdb")
#' db_list_tables(conn)
#' db_close(conn)
#' }
#'
#' @export
db_init <- function(path = "output/alccdfdb.duckdb",
                    read_only = FALSE,
                    verbose = TRUE) {
  .check_suggested("duckdb", "DuckDB database")
  .check_suggested("DBI", "Database connectivity")

  is_new <- !file.exists(path) || path == ":memory:"

  if (is_new && path != ":memory:") {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  }

  conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = read_only)

  if (is_new) {
    .db_create_schema(conn)
    if (verbose) .msg_success("Created new ALccdfDB database: {path}")
  } else {
    .db_validate_schema(conn)
    if (verbose) .msg_step("Opened existing ALccdfDB database: {path}")
  }

  conn
}


#' Close database connection
#'
#' Properly closes the DuckDB database connection and shuts down the database
#' driver.
#'
#' @param conn A DBI connection object from \code{\link{db_init}}.
#' @return Invisible TRUE on success.
#'
#' @export
db_close <- function(conn) {
  .check_suggested("DBI", "Database connectivity")

  if (DBI::dbIsValid(conn)) {
    DBI::dbDisconnect(conn, shutdown = TRUE)
  }
  .msg_step("Database connection closed")
  invisible(TRUE)
}


#' Execute arbitrary SQL query
#'
#' Executes a SQL query and returns the result as a tibble. Useful for custom
#' aggregation, filtering, and analysis directly in the database.
#'
#' @param conn A DBI connection object from \code{\link{db_init}}.
#' @param sql Character. SQL query string.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return A tibble with the query results.
#'
#' @examples
#' \dontrun{
#' conn <- db_init("output/alccdfdb.duckdb", read_only = TRUE)
#' result <- db_query(conn, "
#'   SELECT snapshot_date, COUNT(*) as n
#'   FROM programs
#'   GROUP BY snapshot_date
#'   ORDER BY snapshot_date
#' ")
#' db_close(conn)
#' }
#'
#' @export
db_query <- function(conn, sql, verbose = TRUE) {
  .check_suggested("duckdb", "DuckDB database")
  .check_suggested("DBI", "Database connectivity")
  .db_validate_conn(conn)

  result <- tibble::as_tibble(DBI::dbGetQuery(conn, sql))

  if (verbose) {
    .msg_step("Query returned {nrow(result)} rows x {ncol(result)} cols")
  }

  result
}


#' List all data tables in the database
#'
#' Lists all user data tables, excluding internal metadata tables (those
#' prefixed with \code{_alccdfdb_}).
#'
#' @param conn A DBI connection object from \code{\link{db_init}}.
#' @return Character vector of table names.
#'
#' @export
db_list_tables <- function(conn) {
  .check_suggested("DBI", "Database connectivity")
  .db_validate_conn(conn)

  all_tables <- DBI::dbListTables(conn)
  # Exclude internal ALccdfDB system tables
  user_tables <- all_tables[!grepl("^_alccdfdb_", all_tables)]
  sort(user_tables)
}


#' Get column metadata for a database table
#'
#' Returns column names and DuckDB types for a given table, along with
#' descriptions from the column registry if available.
#'
#' @param conn A DBI connection object from \code{\link{db_init}}.
#' @param table_name Character. Table name.
#' @return A tibble with columns: \code{column_name}, \code{column_type},
#'   \code{description}.
#'
#' @export
db_table_info <- function(conn, table_name) {
  .check_suggested("duckdb", "DuckDB database")
  .check_suggested("DBI", "Database connectivity")
  .db_validate_conn(conn)

  if (!table_name %in% DBI::dbListTables(conn)) {
    cli::cli_abort("Table {.val {table_name}} not found in database.")
  }

  # Get DuckDB column info
  col_info <- DBI::dbGetQuery(conn, glue::glue(
    "SELECT column_name, data_type AS column_type
     FROM information_schema.columns
     WHERE table_name = '{table_name}'
     ORDER BY ordinal_position"
  ))

  result <- tibble::as_tibble(col_info)

  # Get descriptions from column registry if available
  if ("_alccdfdb_columns" %in% DBI::dbListTables(conn)) {
    reg <- DBI::dbGetQuery(conn,
      "SELECT column_name, description FROM _alccdfdb_columns WHERE table_name = ?",
      params = list(table_name)
    )
    if (nrow(reg) > 0) {
      desc_lookup <- stats::setNames(reg$description, reg$column_name)
      result$description <- unname(desc_lookup[result$column_name])
    } else {
      result$description <- NA_character_
    }
  } else {
    result$description <- NA_character_
  }

  result
}


# ---- Internal helpers --------------------------------------------------------

#' Validate database connection
#' @keywords internal
.db_validate_conn <- function(conn) {
  if (!DBI::dbIsValid(conn)) {
    cli::cli_abort("Database connection is not valid. Use {.fn db_init} to connect.")
  }
}


#' Validate existing database schema
#' @keywords internal
.db_validate_schema <- function(conn) {
  tables <- DBI::dbListTables(conn)
  if (!"_alccdfdb_meta" %in% tables) {
    cli::cli_abort(c(
      "Database does not contain ALccdfDB schema.",
      "i" = "Use {.fn db_init} to create a new database."
    ))
  }

  result <- DBI::dbGetQuery(conn,
    "SELECT value FROM _alccdfdb_meta WHERE key = 'schema_version'"
  )
  if (nrow(result) == 0) {
    cli::cli_abort("Database schema version not found.")
  }
  if (result$value[1] != "1.0") {
    cli::cli_abort(c(
      "Database schema version mismatch.",
      "i" = "Expected {.val 1.0}, got {.val {result$value[1]}}.",
      "i" = "Database may have been created by a different version of ALccdfDB."
    ))
  }

  invisible(TRUE)
}


#' Upsert a metadata key-value pair
#' @keywords internal
.db_upsert_meta <- function(conn, key, value) {
  # Delete then insert (DuckDB-compatible upsert)
  DBI::dbExecute(conn,
    "DELETE FROM _alccdfdb_meta WHERE key = ?",
    params = list(key)
  )
  DBI::dbExecute(conn,
    "INSERT INTO _alccdfdb_meta (key, value) VALUES (?, ?)",
    params = list(key, as.character(value))
  )
}
