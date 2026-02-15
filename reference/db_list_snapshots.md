# List available snapshots for a table

Returns a summary of all snapshot dates registered for a given table,
including row counts and which snapshot is marked as current.

## Usage

``` r
db_list_snapshots(conn, table_name)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init`](https://joonho112.github.io/ALccdfDB/reference/db_init.md).

- table_name:

  Character. Name of the DuckDB table.

## Value

A tibble with columns: `snapshot_date`, `table_name`, `n_rows`,
`is_current`, `loaded_at`.
