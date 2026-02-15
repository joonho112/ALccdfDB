# Register column metadata for a table

Records column names, types, and optional descriptions in the column
registry for future reference and type reconstruction.

## Usage

``` r
.db_register_columns(conn, table_name, df)
```

## Arguments

- conn:

  A DBI connection object

- table_name:

  Character. Table name.

- df:

  Data frame whose columns to register.
