# List all data tables in the database

Lists all user data tables, excluding internal metadata tables (those
prefixed with `_alccdfdb_`).

## Usage

``` r
db_list_tables(conn)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init`](https://joonho112.github.io/ALccdfDB/reference/db_init.md).

## Value

Character vector of table names.
