# Read a staff/professional-level CSV file

Reads an Alabama Pathways EC Professional Level Users CSV file, applies
the `staff_column_map.csv` column mapping, and returns a standardised
raw object.

## Usage

``` r
staff_read(path, snapshot_date, verbose = TRUE)
```

## Arguments

- path:

  Path to the CSV file (e.g.,
  "AlabamaPathways_ECProfessionalLevelUsers_2025-06-04.csv")

- snapshot_date:

  Date of the administrative snapshot

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_staff_raw` S3 object

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- staff_read(
  path = "data/AlabamaPathways_ECProfessionalLevelUsers_2025-06-04.csv",
  snapshot_date = as.Date("2025-06-04")
)
} # }
```
