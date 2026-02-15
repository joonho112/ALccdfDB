# Generate subsidy summary statistics

Computes summary statistics for a subsidy-module S3 object, including
counts by county, age distributions, copayment distributions, and
provider counts.

## Usage

``` r
subsidy_summary_stats(obj, verbose = TRUE)
```

## Arguments

- obj:

  A subsidy-module S3 object (any stage from clean onward)

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

A named list of summary tibbles:

- overview:

  One-row tibble with high-level counts

- by_county:

  Counts by county

- age_distribution:

  Age distribution statistics

- copay_distribution:

  Copayment distribution (clients only)

- by_eligibility:

  Counts by eligibility category or funding type

## Examples

``` r
if (FALSE) { # \dontrun{
stats <- subsidy_summary_stats(clean_enrolled)
stats$overview
stats$by_county
} # }
```
