# Get column metadata for a database table

Returns column names and DuckDB types for a given table, along with
descriptions from the column registry if available.

## Usage

``` r
db_table_info(conn, table_name)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init`](https://joonho112.github.io/ALccdfDB/reference/db_init.md).

- table_name:

  Character. Table name.

## Value

A tibble with columns: `column_name`, `column_type`, `description`.
