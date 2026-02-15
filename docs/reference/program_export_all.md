# Export program data to all supported formats

Convenience function that exports to CSV, Excel, Stata, Parquet, and
RDS.

## Usage

``` r
program_export_all(
  obj,
  dir,
  basename,
  formats = c("csv", "excel", "stata", "parquet", "rds"),
  verbose = TRUE
)
```

## Arguments

- obj:

  A program-module S3 object (any stage)

- dir:

  Output directory

- basename:

  Base filename (without extension)

- formats:

  Character vector of formats to export. Default exports all:
  `c("csv", "excel", "stata", "parquet", "rds")`.

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

Invisible named character vector of file paths
