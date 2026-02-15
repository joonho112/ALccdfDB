# Get the current or latest snapshot date for a table

Get the current or latest snapshot date for a table

## Usage

``` r
.db_get_current_snapshot(conn, table_name)
```

## Arguments

- conn:

  A DBI connection object

- table_name:

  Character. Table name.

## Value

Date value, or NULL if no snapshots exist.
