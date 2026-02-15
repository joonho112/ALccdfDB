# Import Melissa geocoding results for programs

Reads a post-processed Melissa geocoding Excel file for child care
programs, applies the `melissa_column_map.csv` column mapping, converts
coordinate fields to numeric, and validates that coordinates fall within
Alabama bounds.

## Usage

``` r
melissa_import_programs(path, snapshot_date, verbose = TRUE)
```

## Arguments

- path:

  Path to the Melissa program geocoding Excel file (e.g.,
  "program_geocoded_by_melissa.xlsx")

- snapshot_date:

  Date of the geocoding run (Date or character parseable via
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html))

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_melissa_programs` S3 object

## Examples

``` r
if (FALSE) { # \dontrun{
mel <- melissa_import_programs(
  path = "data/program_geocoded_by_melissa.xlsx",
  snapshot_date = "2025-09-24"
)
mel
} # }
```
