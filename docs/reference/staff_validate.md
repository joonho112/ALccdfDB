# Validate cleaned staff data

Runs 4 data-quality checks against an `alccdf_staff_clean` object. In
strict mode, any ERROR-level check causes an immediate abort.

## Usage

``` r
staff_validate(clean_obj, strict = FALSE, verbose = TRUE)
```

## Arguments

- clean_obj:

  An `alccdf_staff_clean` object from
  [`staff_clean`](https://joonho112.github.io/ALccdfDB/reference/staff_clean.md)

- strict:

  Logical; if TRUE, ERROR-level failures abort with an error message.
  Default FALSE (report only).

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_staff_validation` S3 object (inherits from
`alccdf_validation`).

## Details

The 4 checks performed are:

1.  **required_columns** (ERROR): Core columns must be present in the
    main data.

2.  **duplicate_records** (WARN): Duplicate combinations of staff name +
    facility name.

3.  **valid_professional_levels** (WARN): Career lattice levels must
    match expected set.

4.  **pii_separation** (ERROR): PII fields (staff_name, user_email) must
    not appear in the main data.

## Examples

``` r
if (FALSE) { # \dontrun{
validation <- staff_validate(clean_obj)
validation
validation$checks
} # }
```
