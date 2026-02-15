# Prepare standardised geocoding columns from Melissa output

Renames Melissa's raw column names (LAT, LNG, CT, etc.) to clean,
snake_case names for the package standard.

## Usage

``` r
.prepare_melissa_geo(melissa_df, prefix = "")
```

## Arguments

- melissa_df:

  Melissa data tibble with raw column names

- prefix:

  Optional prefix for output column names (e.g., "hh\_" for household
  geocoding to avoid collisions with program geocoding)

## Value

A tibble with address + renamed geocoding columns
