# Export subsidy data to Excel

Export subsidy data to Excel

## Usage

``` r
subsidy_export_excel(obj, path, include_pii = FALSE, verbose = TRUE)
```

## Arguments

- obj:

  A subsidy-module S3 object (any stage) or a data frame

- path:

  Output file path

- include_pii:

  Logical; if TRUE, merge PII fields into the export. Default FALSE.

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

Invisible file path
