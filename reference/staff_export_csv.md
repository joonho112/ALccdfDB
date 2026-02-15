# Export staff data to CSV

Export staff data to CSV

## Usage

``` r
staff_export_csv(obj, path, include_pii = FALSE, verbose = TRUE)
```

## Arguments

- obj:

  A staff-module S3 object (any stage) or a data frame

- path:

  Output file path

- include_pii:

  Logical; if TRUE and the object has a `$pii` table, join PII fields
  into the export. Default FALSE.

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

Invisible file path
