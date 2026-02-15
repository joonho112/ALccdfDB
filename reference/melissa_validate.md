# Validate Melissa geocoding results

Runs 3 data-quality checks against an `alccdf_melissa_programs` or
`alccdf_melissa_households` object. In strict mode, any ERROR-level
check causes an immediate abort.

## Usage

``` r
melissa_validate(obj, strict = FALSE, verbose = TRUE)
```

## Arguments

- obj:

  An `alccdf_melissa_programs` or `alccdf_melissa_households` object
  from
  [`melissa_import_programs`](https://joonho112.github.io/ALccdfDB/reference/melissa_import_programs.md)
  or
  [`melissa_import_households`](https://joonho112.github.io/ALccdfDB/reference/melissa_import_households.md)

- strict:

  Logical; if TRUE, ERROR-level failures abort with an error message.
  Default FALSE (report only).

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_melissa_validation` S3 object (inherits from
`alccdf_validation`).

## Details

The 3 checks performed are:

1.  **coordinate_bounds** (ERROR): Latitude must be within \[30.14,
    35.01\] and longitude within \[-88.47, -84.89\] (Alabama bounds).

2.  **match_quality** (WARN): Flags if \>10% of rows have poor Melissa
    match quality based on RESULTCODE/STATUSCODE.

3.  **id_match_rate** (WARN): Reports the percentage of random_id values
    that are non-empty.

## Examples

``` r
if (FALSE) { # \dontrun{
validation <- melissa_validate(mel_programs)
validation
validation$checks
} # }
```
