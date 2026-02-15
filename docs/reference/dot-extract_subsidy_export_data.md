# Extract export-ready data from subsidy object, optionally merging PII

Extract export-ready data from subsidy object, optionally merging PII

## Usage

``` r
.extract_subsidy_export_data(obj, include_pii = FALSE)
```

## Arguments

- obj:

  A subsidy S3 object or data frame

- include_pii:

  Logical; if TRUE, merge PII fields

## Value

A tibble ready for export
