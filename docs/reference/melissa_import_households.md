# Import Melissa geocoding results for subsidy households

Reads a post-processed Melissa geocoding Excel file for subsidy
household addresses, applies the `melissa_column_map.csv` column
mapping, converts coordinate fields to numeric, and validates Alabama
bounds.

## Usage

``` r
melissa_import_households(path, snapshot_date, verbose = TRUE)
```

## Arguments

- path:

  Path to the Melissa household geocoding Excel file (e.g.,
  "subsidy_households_geocoded_by_melissa.xlsx")

- snapshot_date:

  Date of the geocoding run (Date or character parseable via
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html))

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_melissa_households` S3 object

## Examples

``` r
if (FALSE) { # \dontrun{
mel_hh <- melissa_import_households(
  path = "data/subsidy_households_geocoded_by_melissa.xlsx",
  snapshot_date = "2025-09-24"
)
mel_hh
} # }
```
