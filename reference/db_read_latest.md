# Read the latest snapshot from DuckDB

Convenience function that reads the most recent snapshot of a table.

## Usage

``` r
db_read_latest(conn, table_name, verbose = TRUE)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init`](https://joonho112.github.io/ALccdfDB/reference/db_init.md).

- table_name:

  Character. Name of the DuckDB table to read.

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

A tibble with the latest snapshot data.
