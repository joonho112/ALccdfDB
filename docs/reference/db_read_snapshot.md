# Read a data snapshot from DuckDB

Reads data for a specific snapshot date from a DuckDB table. If
`snapshot_date` is NULL, reads the current (is_current = TRUE) snapshot
or falls back to the latest available.

## Usage

``` r
db_read_snapshot(conn, table_name, snapshot_date = NULL, verbose = TRUE)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init`](https://joonho112.github.io/ALccdfDB/reference/db_init.md).

- table_name:

  Character. Name of the DuckDB table to read.

- snapshot_date:

  Date or character. Snapshot date to read. If NULL (default), reads the
  current or latest snapshot.

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

A tibble with the requested data.

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- db_init("output/alccdfdb.duckdb", read_only = TRUE)
programs <- db_read_snapshot(conn, "programs")
programs_jun <- db_read_snapshot(conn, "programs",
                                  snapshot_date = "2025-06-11")
db_close(conn)
} # }
```
