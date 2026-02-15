# tests/testthat/test-database.R
# Comprehensive unit tests for the ALccdfDB database module (DuckDB).
# Uses testthat edition 3.

SNAPSHOT_DATE <- as.Date("2025-09-12")
fixture_path <- function(filename) testthat::test_path("fixtures", filename)

# Helper: skip if DuckDB or DBI are not available
skip_if_no_duckdb <- function() {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    skip("duckdb not installed")
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    skip("DBI not installed")
  }
}

# Helper: create a test data frame with diverse column types
make_test_df <- function(n = 5) {
  tibble::tibble(
    id        = seq_len(n),
    name      = paste0("item_", seq_len(n)),
    value     = as.numeric(seq_len(n)) * 1.5,
    count     = as.integer(seq_len(n)),
    active    = rep(c(TRUE, FALSE), length.out = n),
    created   = as.Date("2025-01-01") + seq_len(n) - 1L
  )
}

# Helper: create a test data frame with factor and POSIXct columns
make_test_df_special <- function(n = 4) {
  tibble::tibble(
    id       = seq_len(n),
    category = factor(rep(c("A", "B"), length.out = n)),
    ts       = as.POSIXct("2025-06-15 12:00:00") + seq_len(n) * 3600,
    label    = paste0("label_", seq_len(n))
  )
}

# Helper: create a mock S3 object resembling program_clean output
make_test_s3_obj <- function(n = 3) {
  df <- tibble::tibble(
    facility_id   = paste0("FAC", sprintf("%03d", seq_len(n))),
    facility_name = paste0("Facility ", seq_len(n)),
    capacity      = as.integer(c(50, 75, 100)[seq_len(n)]),
    tier          = factor(c("Star 1", "Star 3", "Star 5")[seq_len(n)],
                           levels = paste("Star", 1:5), ordered = TRUE),
    open_date     = as.Date("2020-01-15") + seq_len(n) * 30L
  )
  obj <- list(
    data = df,
    meta = list(
      module = "program",
      stage = "clean",
      snapshot_date = SNAPSHOT_DATE,
      n_rows = nrow(df),
      n_cols = ncol(df)
    ),
    diagnostics = list()
  )
  class(obj) <- "alccdf_program_clean"
  obj
}


# ==============================================================================
# 1. db_init / db_close
# ==============================================================================

test_that("db_init creates in-memory database successfully", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))


  expect_true(DBI::dbIsValid(conn))
})

test_that("db_init creates schema tables", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  all_tables <- DBI::dbListTables(conn)
  expect_true("_alccdfdb_meta" %in% all_tables)
  expect_true("_alccdfdb_columns" %in% all_tables)
  expect_true("_alccdfdb_snapshots" %in% all_tables)
})

test_that("db_init writes initial metadata", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  meta <- DBI::dbGetQuery(conn, "SELECT * FROM _alccdfdb_meta ORDER BY key")
  expect_true("schema_version" %in% meta$key)
  expect_equal(meta$value[meta$key == "schema_version"], "1.0")
  expect_true("package" %in% meta$key)
  expect_equal(meta$value[meta$key == "package"], "ALccdfDB")
  expect_true("created_at" %in% meta$key)
  expect_true("last_modified_at" %in% meta$key)
})

test_that("db_close returns TRUE", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  result <- db_close(conn)
  expect_true(result)
})

test_that("db_close on already-closed connection does not error", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  db_close(conn)
  # Second close should not error
  expect_no_error(db_close(conn))
})


# ==============================================================================
# 2. Write and Read roundtrip
# ==============================================================================

test_that("write-read roundtrip preserves data values", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(5)
  db_write_snapshot(conn, df, "test_table",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)
  result <- db_read_snapshot(conn, "test_table",
                             snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  # Should have all original columns plus snapshot_date
  expect_true(all(names(df) %in% names(result)))
  expect_true("snapshot_date" %in% names(result))

  # Compare values (excluding snapshot_date which was added)
  for (col in names(df)) {
    expect_equal(result[[col]], df[[col]], info = paste("Column:", col))
  }
})

test_that("write-read roundtrip preserves integer type", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(5)
  db_write_snapshot(conn, df, "test_int",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)
  result <- db_read_snapshot(conn, "test_int",
                             snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  expect_true(is.integer(result$count))
  expect_equal(result$count, df$count)
})

test_that("write-read roundtrip preserves character type", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(5)
  db_write_snapshot(conn, df, "test_chr",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)
  result <- db_read_snapshot(conn, "test_chr",
                             snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  expect_true(is.character(result$name))
  expect_equal(result$name, df$name)
})

test_that("write-read roundtrip preserves numeric type", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(5)
  db_write_snapshot(conn, df, "test_num",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)
  result <- db_read_snapshot(conn, "test_num",
                             snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  expect_true(is.numeric(result$value))
  expect_equal(result$value, df$value)
})

test_that("write-read roundtrip preserves logical type", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(5)
  db_write_snapshot(conn, df, "test_lgl",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)
  result <- db_read_snapshot(conn, "test_lgl",
                             snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  expect_true(is.logical(result$active))
  expect_equal(result$active, df$active)
})

test_that("write-read roundtrip preserves Date type", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(5)
  db_write_snapshot(conn, df, "test_date",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)
  result <- db_read_snapshot(conn, "test_date",
                             snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  expect_true(inherits(result$created, "Date"))
  expect_equal(result$created, df$created)
})

test_that("factor columns are converted to character on write and read back as character", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df_special(4)
  db_write_snapshot(conn, df, "test_factor",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)
  result <- db_read_snapshot(conn, "test_factor",
                             snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  # Factors are stored as character and read back as character
  # (the column registry records original type as "factor")
  expect_true(is.character(result$category))
  expect_equal(result$category, as.character(df$category))
})

test_that("POSIXct columns are converted to character on write", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df_special(4)
  db_write_snapshot(conn, df, "test_posixct",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)
  result <- db_read_snapshot(conn, "test_posixct",
                             snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  # POSIXct is stored and returned as character
  expect_true(is.character(result$ts))
  expect_equal(result$ts, as.character(df$ts))
})

test_that("write-read roundtrip returns tibble", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(3)
  db_write_snapshot(conn, df, "test_tibble",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)
  result <- db_read_snapshot(conn, "test_tibble",
                             snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  expect_s3_class(result, "tbl_df")
})


# ==============================================================================
# 3. Snapshot management
# ==============================================================================

test_that("db_write_snapshot with explicit snapshot_date stores correctly", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(3)
  db_write_snapshot(conn, df, "snap_test",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  snapshots <- db_list_snapshots(conn, "snap_test")
  expect_equal(nrow(snapshots), 1)
  expect_equal(as.Date(snapshots$snapshot_date[1]), SNAPSHOT_DATE)
})

test_that("db_write_snapshot with is_current=TRUE marks snapshot as current", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(3)
  db_write_snapshot(conn, df, "current_test",
                    snapshot_date = SNAPSHOT_DATE,
                    is_current = TRUE, verbose = FALSE)

  snapshots <- db_list_snapshots(conn, "current_test")
  expect_equal(nrow(snapshots), 1)
  expect_true(snapshots$is_current[1])
})

test_that("multiple snapshots for the same table are tracked", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df1 <- make_test_df(3)
  df2 <- make_test_df(5)
  date1 <- as.Date("2025-06-01")
  date2 <- as.Date("2025-09-01")

  db_write_snapshot(conn, df1, "multi_snap",
                    snapshot_date = date1, verbose = FALSE)
  db_write_snapshot(conn, df2, "multi_snap",
                    snapshot_date = date2, verbose = FALSE)

  snapshots <- db_list_snapshots(conn, "multi_snap")
  expect_equal(nrow(snapshots), 2)
  expect_true(all(as.Date(snapshots$snapshot_date) %in% c(date1, date2)))
})

test_that("db_list_snapshots shows row counts", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df1 <- make_test_df(3)
  df2 <- make_test_df(7)
  date1 <- as.Date("2025-06-01")
  date2 <- as.Date("2025-09-01")

  db_write_snapshot(conn, df1, "count_snap",
                    snapshot_date = date1, verbose = FALSE)
  db_write_snapshot(conn, df2, "count_snap",
                    snapshot_date = date2, verbose = FALSE)

  snapshots <- db_list_snapshots(conn, "count_snap")
  # Rows include the added snapshot_date column, so n_rows reflects original
  expect_true("n_rows" %in% names(snapshots))
  # Row counts should be 3 and 7
  expect_true(3 %in% snapshots$n_rows)
  expect_true(7 %in% snapshots$n_rows)
})

test_that("db_read_snapshot reads specific snapshot_date", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df1 <- make_test_df(3)
  df2 <- make_test_df(5)
  date1 <- as.Date("2025-06-01")
  date2 <- as.Date("2025-09-01")

  db_write_snapshot(conn, df1, "read_spec",
                    snapshot_date = date1, verbose = FALSE)
  db_write_snapshot(conn, df2, "read_spec",
                    snapshot_date = date2, verbose = FALSE)

  result1 <- db_read_snapshot(conn, "read_spec",
                              snapshot_date = date1, verbose = FALSE)
  result2 <- db_read_snapshot(conn, "read_spec",
                              snapshot_date = date2, verbose = FALSE)

  expect_equal(nrow(result1), 3)
  expect_equal(nrow(result2), 5)
})

test_that("db_read_snapshot without date reads current/latest snapshot", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df1 <- make_test_df(3)
  df2 <- make_test_df(5)
  date1 <- as.Date("2025-06-01")
  date2 <- as.Date("2025-09-01")

  db_write_snapshot(conn, df1, "auto_read",
                    snapshot_date = date1, verbose = FALSE)
  db_write_snapshot(conn, df2, "auto_read",
                    snapshot_date = date2, is_current = TRUE, verbose = FALSE)

  # Without specifying date, should read the "current" (date2) snapshot
  result <- db_read_snapshot(conn, "auto_read", verbose = FALSE)
  expect_equal(nrow(result), 5)
})

test_that("db_read_latest returns the most recent snapshot", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df_old <- make_test_df(2)
  df_new <- make_test_df(8)
  date_old <- as.Date("2025-01-01")
  date_new <- as.Date("2025-12-01")

  db_write_snapshot(conn, df_old, "latest_test",
                    snapshot_date = date_old, verbose = FALSE)
  db_write_snapshot(conn, df_new, "latest_test",
                    snapshot_date = date_new, verbose = FALSE)

  result <- db_read_latest(conn, "latest_test", verbose = FALSE)
  expect_equal(nrow(result), 8)
  # All rows should have the latest snapshot_date
  expect_true(all(as.Date(result$snapshot_date) == date_new))
})

test_that("is_current=TRUE unsets previous current for the same table", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(3)
  date1 <- as.Date("2025-06-01")
  date2 <- as.Date("2025-09-01")

  db_write_snapshot(conn, df, "current_swap",
                    snapshot_date = date1, is_current = TRUE, verbose = FALSE)
  db_write_snapshot(conn, df, "current_swap",
                    snapshot_date = date2, is_current = TRUE, verbose = FALSE)

  snapshots <- db_list_snapshots(conn, "current_swap")
  current_snaps <- snapshots[snapshots$is_current == TRUE, ]
  expect_equal(nrow(current_snaps), 1)
  expect_equal(as.Date(current_snaps$snapshot_date), date2)
})


# ==============================================================================
# 4. db_list_tables
# ==============================================================================

test_that("db_list_tables returns empty vector for fresh database", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  tables <- db_list_tables(conn)
  expect_length(tables, 0)
  expect_true(is.character(tables))
})

test_that("db_list_tables shows user tables after writing", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(3)
  db_write_snapshot(conn, df, "my_table",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  tables <- db_list_tables(conn)
  expect_true("my_table" %in% tables)
})

test_that("db_list_tables excludes system tables", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(3)
  db_write_snapshot(conn, df, "user_data",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  tables <- db_list_tables(conn)
  internal <- tables[grepl("^_alccdfdb_", tables)]
  expect_length(internal, 0)
})

test_that("db_list_tables returns sorted table names", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(2)
  db_write_snapshot(conn, df, "zebra",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)
  db_write_snapshot(conn, df, "alpha",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  tables <- db_list_tables(conn)
  expect_equal(tables, sort(tables))
})


# ==============================================================================
# 5. db_table_info
# ==============================================================================

test_that("db_table_info returns correct column names and types", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(3)
  db_write_snapshot(conn, df, "info_test",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  info <- db_table_info(conn, "info_test")

  expect_s3_class(info, "tbl_df")
  expect_true("column_name" %in% names(info))
  expect_true("column_type" %in% names(info))
  expect_true("description" %in% names(info))

  # All original columns plus snapshot_date should be listed
  expected_cols <- c(names(df), "snapshot_date")
  expect_true(all(expected_cols %in% info$column_name))
})

test_that("db_table_info errors for non-existent table", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  expect_error(db_table_info(conn, "nonexistent"), "not found")
})


# ==============================================================================
# 6. db_query
# ==============================================================================

test_that("db_query executes simple SELECT", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(5)
  db_write_snapshot(conn, df, "query_test",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  result <- db_query(conn, "SELECT * FROM query_test", verbose = FALSE)
  expect_equal(nrow(result), 5)
  expect_true("id" %in% names(result))
})

test_that("db_query returns tibble", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(3)
  db_write_snapshot(conn, df, "query_tibble",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  result <- db_query(conn, "SELECT COUNT(*) AS n FROM query_tibble",
                     verbose = FALSE)
  expect_s3_class(result, "tbl_df")
  expect_equal(result$n, 3)
})

test_that("db_query handles aggregation", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(5)
  db_write_snapshot(conn, df, "query_agg",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  result <- db_query(conn,
    "SELECT SUM(count) AS total, AVG(value) AS mean_val FROM query_agg",
    verbose = FALSE
  )
  expect_equal(result$total, sum(df$count))
  expect_equal(result$mean_val, mean(df$value), tolerance = 1e-10)
})


# ==============================================================================
# 7. Overwrite mode
# ==============================================================================

test_that("overwrite=TRUE replaces existing snapshot data", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df1 <- make_test_df(3)
  df2 <- make_test_df(7)

  db_write_snapshot(conn, df1, "overwrite_test",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)
  db_write_snapshot(conn, df2, "overwrite_test",
                    snapshot_date = SNAPSHOT_DATE,
                    overwrite = TRUE, verbose = FALSE)

  result <- db_read_snapshot(conn, "overwrite_test",
                             snapshot_date = SNAPSHOT_DATE, verbose = FALSE)
  expect_equal(nrow(result), 7)
})

test_that("overwrite=FALSE (default) appends data for same snapshot_date", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df1 <- make_test_df(3)
  df2 <- make_test_df(5)

  db_write_snapshot(conn, df1, "append_test",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)
  db_write_snapshot(conn, df2, "append_test",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  result <- db_read_snapshot(conn, "append_test",
                             snapshot_date = SNAPSHOT_DATE, verbose = FALSE)
  # Without overwrite, both writes are appended

  expect_equal(nrow(result), 3 + 5)
})


# ==============================================================================
# 8. S3 object support
# ==============================================================================

test_that("db_write_snapshot extracts data from S3 object with $data", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  obj <- make_test_s3_obj(3)
  db_write_snapshot(conn, obj, "s3_test",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  result <- db_read_snapshot(conn, "s3_test",
                             snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  expect_equal(nrow(result), 3)
  expect_true("facility_id" %in% names(result))
  expect_true("facility_name" %in% names(result))
  expect_true("capacity" %in% names(result))
})

test_that("S3 object data integrity preserved after roundtrip", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  obj <- make_test_s3_obj(3)
  original_data <- obj$data

  db_write_snapshot(conn, obj, "s3_integrity",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  result <- db_read_snapshot(conn, "s3_integrity",
                             snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  # Character columns preserved
  expect_equal(result$facility_id, original_data$facility_id)
  expect_equal(result$facility_name, original_data$facility_name)

  # Integer column preserved via type reconstruction
  expect_true(is.integer(result$capacity))
  expect_equal(result$capacity, original_data$capacity)

  # Date column preserved via type reconstruction
  expect_true(inherits(result$open_date, "Date"))
  expect_equal(result$open_date, original_data$open_date)

  # Factor column is stored as character
  expect_true(is.character(result$tier))
  expect_equal(result$tier, as.character(original_data$tier))
})

test_that("S3 object snapshot_date from meta is used when snapshot_date=NULL", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  obj <- make_test_s3_obj(3)
  # obj$meta$snapshot_date is SNAPSHOT_DATE

  db_write_snapshot(conn, obj, "s3_meta_date", verbose = FALSE)

  snapshots <- db_list_snapshots(conn, "s3_meta_date")
  expect_equal(as.Date(snapshots$snapshot_date[1]), SNAPSHOT_DATE)
})


# ==============================================================================
# 9. Error handling
# ==============================================================================

test_that("db_read_snapshot errors for non-existent table", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  expect_error(
    db_read_snapshot(conn, "does_not_exist", verbose = FALSE),
    "not found"
  )
})

test_that("db_read_latest errors for non-existent table", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  expect_error(
    db_read_latest(conn, "no_such_table", verbose = FALSE),
    "not found"
  )
})

test_that("db_query errors on invalid connection", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  db_close(conn)

  expect_error(
    db_query(conn, "SELECT 1", verbose = FALSE),
    "not valid"
  )
})

test_that("db_write_snapshot errors when obj has no data", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  bad_obj <- list(meta = list(module = "test"))
  class(bad_obj) <- "bad_class"

  expect_error(
    db_write_snapshot(conn, bad_obj, "fail_table",
                      snapshot_date = SNAPSHOT_DATE, verbose = FALSE),
    "Cannot extract"
  )
})


# ==============================================================================
# 10. Internal functions
# ==============================================================================

test_that(".db_detect_r_type identifies integer", {
  fn <- ALccdfDB:::.db_detect_r_type
  expect_equal(fn(1L), "integer")
})

test_that(".db_detect_r_type identifies numeric", {
  fn <- ALccdfDB:::.db_detect_r_type
  expect_equal(fn(1.5), "numeric")
})

test_that(".db_detect_r_type identifies character", {
  fn <- ALccdfDB:::.db_detect_r_type
  expect_equal(fn("hello"), "character")
})

test_that(".db_detect_r_type identifies logical", {
  fn <- ALccdfDB:::.db_detect_r_type
  expect_equal(fn(TRUE), "logical")
})

test_that(".db_detect_r_type identifies Date", {
  fn <- ALccdfDB:::.db_detect_r_type
  expect_equal(fn(Sys.Date()), "Date")
})

test_that(".db_detect_r_type identifies POSIXct", {
  fn <- ALccdfDB:::.db_detect_r_type
  expect_equal(fn(Sys.time()), "POSIXct")
})

test_that(".db_detect_r_type identifies factor", {
  fn <- ALccdfDB:::.db_detect_r_type
  expect_equal(fn(factor("a")), "factor")
})

test_that(".db_prepare_for_write converts factors to character", {
  fn <- ALccdfDB:::.db_prepare_for_write
  df <- data.frame(
    x = factor(c("a", "b", "c")),
    y = 1:3,
    stringsAsFactors = FALSE
  )
  result <- fn(df)
  expect_true(is.character(result$x))
  expect_equal(result$x, c("a", "b", "c"))
})

test_that(".db_prepare_for_write converts POSIXct to character", {
  fn <- ALccdfDB:::.db_prepare_for_write
  df <- data.frame(
    ts = as.POSIXct(c("2025-01-01 12:00:00", "2025-01-02 13:00:00")),
    id = 1:2,
    stringsAsFactors = FALSE
  )
  result <- fn(df)
  expect_true(is.character(result$ts))
})

test_that(".db_prepare_for_write returns plain data.frame", {
  fn <- ALccdfDB:::.db_prepare_for_write
  df <- tibble::tibble(x = 1:3, y = c("a", "b", "c"))
  result <- fn(df)
  expect_true(is.data.frame(result))
  # Should not be a tibble (plain data.frame)
  expect_false(inherits(result, "tbl_df"))
})

test_that(".db_reconstruct_types restores Date from registry", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- tibble::tibble(
    d = as.Date("2025-06-15"),
    n = 42L,
    flag = TRUE
  )
  db_write_snapshot(conn, df, "recon_test",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  # Read raw without reconstruction
  raw <- tibble::as_tibble(DBI::dbGetQuery(conn,
    "SELECT * FROM recon_test"
  ))

  # Apply reconstruction
  fn <- ALccdfDB:::.db_reconstruct_types
  restored <- fn(raw, conn, "recon_test")

  expect_true(inherits(restored$d, "Date"))
  expect_true(is.integer(restored$n))
  expect_true(is.logical(restored$flag))
})

test_that(".db_create_schema is idempotent (IF NOT EXISTS)", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  # Call create schema again -- should not error
  fn <- ALccdfDB:::.db_create_schema
  expect_no_error(fn(conn))

  # Schema tables still exist
  all_tables <- DBI::dbListTables(conn)
  expect_true("_alccdfdb_meta" %in% all_tables)
})

test_that(".db_validate_schema passes on valid schema", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  fn <- ALccdfDB:::.db_validate_schema
  expect_true(fn(conn))
})

test_that(".db_upsert_meta updates existing key", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  fn <- ALccdfDB:::.db_upsert_meta
  fn(conn, "test_key", "value_1")
  fn(conn, "test_key", "value_2")

  result <- DBI::dbGetQuery(conn,
    "SELECT value FROM _alccdfdb_meta WHERE key = 'test_key'"
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$value, "value_2")
})

test_that(".db_register_columns stores column types", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- tibble::tibble(
    a = 1L,
    b = "hello",
    c = as.Date("2025-01-01"),
    d = TRUE
  )

  fn <- ALccdfDB:::.db_register_columns
  fn(conn, "reg_test", df)

  reg <- DBI::dbGetQuery(conn,
    "SELECT * FROM _alccdfdb_columns WHERE table_name = 'reg_test' ORDER BY column_name"
  )

  expect_equal(nrow(reg), 4)
  expect_true("a" %in% reg$column_name)

  a_type <- reg$column_type[reg$column_name == "a"]
  expect_equal(a_type, "integer")

  d_type <- reg$column_type[reg$column_name == "d"]
  expect_equal(d_type, "logical")
})

test_that(".db_get_current_snapshot returns current when set", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(3)
  db_write_snapshot(conn, df, "current_test_2",
                    snapshot_date = SNAPSHOT_DATE,
                    is_current = TRUE, verbose = FALSE)

  fn <- ALccdfDB:::.db_get_current_snapshot
  result <- fn(conn, "current_test_2")
  expect_equal(result, SNAPSHOT_DATE)
})

test_that(".db_get_current_snapshot returns latest when no current set", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(3)
  date1 <- as.Date("2025-01-01")
  date2 <- as.Date("2025-12-01")

  db_write_snapshot(conn, df, "no_current",
                    snapshot_date = date1,
                    is_current = FALSE, verbose = FALSE)
  db_write_snapshot(conn, df, "no_current",
                    snapshot_date = date2,
                    is_current = FALSE, verbose = FALSE)

  fn <- ALccdfDB:::.db_get_current_snapshot
  result <- fn(conn, "no_current")
  expect_equal(result, date2)
})

test_that(".db_get_current_snapshot returns NULL for unknown table", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  fn <- ALccdfDB:::.db_get_current_snapshot
  result <- fn(conn, "nonexistent_table")
  expect_null(result)
})


# ==============================================================================
# 11. Edge cases
# ==============================================================================

test_that("db_write_snapshot accepts character snapshot_date", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(3)
  db_write_snapshot(conn, df, "char_date_test",
                    snapshot_date = "2025-09-12", verbose = FALSE)

  snapshots <- db_list_snapshots(conn, "char_date_test")
  expect_equal(nrow(snapshots), 1)
  expect_equal(as.Date(snapshots$snapshot_date[1]), SNAPSHOT_DATE)
})

test_that("db_read_snapshot accepts character snapshot_date", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(3)
  db_write_snapshot(conn, df, "read_char_date",
                    snapshot_date = SNAPSHOT_DATE, verbose = FALSE)

  result <- db_read_snapshot(conn, "read_char_date",
                             snapshot_date = "2025-09-12", verbose = FALSE)
  expect_equal(nrow(result), 3)
})

test_that("db_list_snapshots returns empty tibble for table with no snapshots", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  result <- db_list_snapshots(conn, "no_such_table")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("db_write_snapshot returns invisible table name", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- make_test_df(2)
  result <- db_write_snapshot(conn, df, "return_test",
                              snapshot_date = SNAPSHOT_DATE, verbose = FALSE)
  expect_equal(result, "return_test")
})

test_that("writing an empty data frame works without error", {
  skip_if_no_duckdb()

  conn <- db_init(":memory:", verbose = FALSE)
  withr::defer(db_close(conn))

  df <- tibble::tibble(id = integer(0), name = character(0))
  expect_no_error(
    db_write_snapshot(conn, df, "empty_test",
                      snapshot_date = SNAPSHOT_DATE, verbose = FALSE)
  )

  result <- db_read_snapshot(conn, "empty_test",
                             snapshot_date = SNAPSHOT_DATE, verbose = FALSE)
  expect_equal(nrow(result), 0)
})
