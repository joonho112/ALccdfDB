# Apply column mapping to a data frame

Reads a column mapping CSV and renames data frame columns from raw Excel
names to clean standardised names. Matching is case-insensitive and
tolerant of minor whitespace/punctuation differences.

## Usage

``` r
.apply_column_map(df, map_file, verbose = TRUE)
```

## Arguments

- df:

  A data frame with raw column names

- map_file:

  Path to the column mapping CSV (must have `raw_name` and `clean_name`
  columns)

- verbose:

  Logical; print warnings about unmapped columns?

## Value

A tibble with renamed columns
