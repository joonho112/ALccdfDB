# Build a validation result object

Build a validation result object

## Usage

``` r
.build_validation_result(
  checks,
  issues = NULL,
  module,
  stage = "validation",
  snapshot_date = NULL,
  strict = FALSE,
  class_name = NULL
)
```

## Arguments

- checks:

  A tibble of check results (from .make_check)

- issues:

  A tibble of specific problematic rows (optional)

- module:

  Module name

- stage:

  Processing stage

- snapshot_date:

  Snapshot date

- strict:

  Whether strict mode was used

- class_name:

  S3 class name for the validation object

## Value

An S3 validation object
