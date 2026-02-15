# Create ALccdfDB database schema

Creates the three internal system tables (\_alccdfdb_meta,
\_alccdfdb_columns, \_alccdfdb_snapshots) and writes initial metadata.

## Usage

``` r
.db_create_schema(conn)
```

## Arguments

- conn:

  A DBI connection object
