# Export subsidy data to RDS

Export subsidy data to RDS

## Usage

``` r
subsidy_export_rds(obj, path, include_pii = FALSE, verbose = TRUE)
```

## Arguments

- obj:

  A subsidy-module S3 object (any stage) or a data frame

- path:

  Output file path

- include_pii:

  Logical; if TRUE, export the full object including PII. Default FALSE
  (PII stripped from export).

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

Invisible file path
