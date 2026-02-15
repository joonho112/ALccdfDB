# Write a data snapshot to DuckDB

Extracts data from an ALccdfDB S3 object, adds a `snapshot_date` column
if not present, writes to a DuckDB table (append mode), and registers
the snapshot in the internal tracking table.

## Usage

``` r
db_write_snapshot(
  conn,
  obj,
  table_name,
  snapshot_date = NULL,
  is_current = FALSE,
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init`](https://joonho112.github.io/ALccdfDB/reference/db_init.md).

- obj:

  An ALccdfDB S3 object (any module) or a data frame.

- table_name:

  Character. Name of the target DuckDB table.

- snapshot_date:

  Date. Snapshot date. If NULL, extracted from the object's metadata or
  defaults to today.

- is_current:

  Logical. Mark this snapshot as the current (active) one? Default
  FALSE.

- overwrite:

  Logical. If TRUE and the table already exists with data for this
  snapshot_date, the old data is removed first. Default FALSE.

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

Invisible table name.

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- db_init("output/alccdfdb.duckdb")
db_write_snapshot(conn, program_clean, "programs",
                  snapshot_date = "2025-06-11", is_current = TRUE)
db_close(conn)
} # }
```
