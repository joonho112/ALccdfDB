# Export staff data to all supported formats

Convenience function that exports to CSV, Excel, Stata, Parquet, and
RDS.

## Usage

``` r
staff_export_all(
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

  A staff-module S3 object (any stage)

- dir:

  Output directory

- basename:

  Base filename (without extension)

- formats:

  Character vector of formats to export. Default exports all:
  `c("csv", "excel", "stata", "parquet", "rds")`.

- include_pii:

  Logical; if TRUE, include PII in exports. Default FALSE.

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

Invisible named character vector of file paths
