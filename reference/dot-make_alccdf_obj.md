# Create an ALccdfDB S3 object

Create an ALccdfDB S3 object

## Usage

``` r
.make_alccdf_obj(
  data,
  class_name,
  module,
  stage,
  snapshot_date = NULL,
  extra_meta = list(),
  diagnostics = list()
)
```

## Arguments

- data:

  A data frame or tibble

- class_name:

  Character string for the S3 class name

- module:

  Character string for the module name

- stage:

  Character string for the processing stage

- snapshot_date:

  Date of the snapshot

- extra_meta:

  Named list of additional metadata

- diagnostics:

  Named list of diagnostics

## Value

An S3 object of the specified class
