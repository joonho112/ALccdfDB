# Export subsidy data to all supported formats

Convenience function that exports to CSV, Excel, Stata, Parquet, and
RDS.

## Usage

``` r
subsidy_export_all(
  obj,
  dir,
  basename,
  formats = c("csv", "excel", "stata", "parquet", "rds"),
  include_pii = FALSE,
  verbose = TRUE
)
```

## Arguments

- obj:

  A subsidy-module S3 object (any stage)

- dir:

  Output directory

- basename:

  Base filename (without extension)

- formats:

  Character vector of formats to export. Default exports all:
  `c("csv", "excel", "stata", "parquet", "rds")`.

- include_pii:

  Logical; if TRUE, merge PII fields into exports. Default FALSE (PII
  excluded).

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

Invisible named character vector of file paths
