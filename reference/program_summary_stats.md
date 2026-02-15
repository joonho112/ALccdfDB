# Generate program summary statistics

Computes summary statistics for a program-module S3 object, including
counts by facility type, county, tier, and capacity distributions.

## Usage

``` r
program_summary_stats(obj, verbose = TRUE)
```

## Arguments

- obj:

  A program-module S3 object (any stage from clean onward)

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

A named list of summary tibbles:

- overview:

  One-row tibble with high-level counts

- by_type:

  Counts by facility_type

- by_county:

  Counts by county

- by_tier:

  Counts by facility_tier

- capacity:

  Capacity distribution statistics

## Examples

``` r
if (FALSE) { # \dontrun{
stats <- program_summary_stats(unified)
stats$overview
stats$by_county
} # }
```
