# Execute arbitrary SQL query

Executes a SQL query and returns the result as a tibble. Useful for
custom aggregation, filtering, and analysis directly in the database.

## Usage

``` r
db_query(conn, sql, verbose = TRUE)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init`](https://joonho112.github.io/ALccdfDB/reference/db_init.md).

- sql:

  Character. SQL query string.

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

A tibble with the query results.

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- db_init("output/alccdfdb.duckdb", read_only = TRUE)
result <- db_query(conn, "
  SELECT snapshot_date, COUNT(*) as n
  FROM programs
  GROUP BY snapshot_date
  ORDER BY snapshot_date
")
db_close(conn)
} # }
```
