# Export subsidy data to Stata (.dta)

Export subsidy data to Stata (.dta)

## Usage

``` r
subsidy_export_stata(obj, path, include_pii = FALSE, verbose = TRUE)
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
