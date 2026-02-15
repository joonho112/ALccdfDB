# Write Melissa geocoding data to DuckDB

Convenience function for writing Melissa geocoding results (programs or
households) to the database.

## Usage

``` r
db_write_melissa(conn, obj, table_name = NULL, verbose = TRUE)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init`](https://joonho112.github.io/ALccdfDB/reference/db_init.md).

- obj:

  An `alccdf_melissa_programs` or `alccdf_melissa_households` object.

- table_name:

  Character. Table name. Default auto-detected from object class:
  `"programs_melissa"` or `"subsidy_melissa"`.

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

Invisible table name.
