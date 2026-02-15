# Get list of snapshots for a table

Get list of snapshots for a table

## Usage

``` r
.db_get_snapshots(conn, table_name)
```

## Arguments

- conn:

  A DBI connection object

- table_name:

  Character. Table name.

## Value

A tibble with snapshot dates and metadata.
