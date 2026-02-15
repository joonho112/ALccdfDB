# Generate staff summary statistics

Computes summary statistics for a staff-module S3 object, including
counts by career lattice level, provider type, county, and operating
status.

## Usage

``` r
staff_summary_stats(obj, verbose = TRUE)
```

## Arguments

- obj:

  A staff-module S3 object (any stage from clean onward)

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

A named list of summary tibbles:

- overview:

  One-row tibble with high-level counts

- by_career_level:

  Counts by career_lattice_level

- by_provider_type:

  Counts by provider_type

- by_user_county:

  Counts by user_county

- by_facility_county:

  Counts by facility_county

- by_position:

  Counts by position

- operating_status:

  Counts of currently_operating TRUE/FALSE/NA

## Examples

``` r
if (FALSE) { # \dontrun{
stats <- staff_summary_stats(staff_clean)
stats$overview
stats$by_career_level
} # }
```
