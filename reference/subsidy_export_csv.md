# Export subsidy data to CSV

Export subsidy data to CSV

## Usage

``` r
subsidy_export_csv(obj, path, include_pii = FALSE, verbose = TRUE)
```

## Arguments

- obj:

  A subsidy-module S3 object (any stage) or a data frame

- path:

  Output file path

- include_pii:

  Logical; if TRUE, merge PII fields into the export. Default FALSE (PII
  excluded for safety).

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

Invisible file path
