# Initialize DuckDB database

Creates a new or opens an existing DuckDB database for storing ALccdfDB
snapshot data. New databases are initialized with the ALccdfDB schema
(metadata, column registry, and snapshot registry tables). Existing
databases are validated for schema compatibility.

## Usage

``` r
db_init(path = "output/alccdfdb.duckdb", read_only = FALSE, verbose = TRUE)
```

## Arguments

- path:

  Path to the DuckDB file. Default `"output/alccdfdb.duckdb"`. Use
  `":memory:"` for an in-memory database (data lost when connection is
  closed).

- read_only:

  Logical. Open in read-only mode? Default FALSE.

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

A DBI connection object to the DuckDB database.

## Examples

``` r
if (FALSE) { # \dontrun{
conn <- db_init("output/alccdfdb.duckdb")
db_list_tables(conn)
db_close(conn)
} # }
```
