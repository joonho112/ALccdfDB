# Export staff data to RDS

Export staff data to RDS

## Usage

``` r
staff_export_rds(obj, path, include_pii = FALSE, verbose = TRUE)
```

## Arguments

- obj:

  A staff-module S3 object (any stage) or a data frame

- path:

  Output file path

- include_pii:

  Logical; if TRUE, the full object (including `$pii`) is saved. If
  FALSE, PII table is stripped before saving. Default FALSE.

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

Invisible file path
