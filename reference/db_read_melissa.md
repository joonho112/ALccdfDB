# Read Melissa geocoding data from DuckDB

Convenience function for reading Melissa geocoding results from the
database.

## Usage

``` r
db_read_melissa(conn, type = c("programs", "households"), verbose = TRUE)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init`](https://joonho112.github.io/ALccdfDB/reference/db_init.md).

- type:

  Character. Type of Melissa data to read: `"programs"` or
  `"households"`.

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

A tibble with Melissa geocoding data.
