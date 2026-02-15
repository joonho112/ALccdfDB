# Reconstruct R types from column registry after reading from DuckDB

Reconstruct R types from column registry after reading from DuckDB

## Usage

``` r
.db_reconstruct_types(df, conn, table_name)
```

## Arguments

- df:

  Data frame read from DuckDB

- conn:

  DBI connection

- table_name:

  Table name for looking up column types

## Value

Data frame with reconstructed R types
