# Validate cleaned subsidy data

Runs 7 data-quality checks against an `alccdf_subsidy_clean` object. In
strict mode, any ERROR-level check causes an immediate abort.

## Usage

``` r
subsidy_validate(clean_obj, strict = FALSE, verbose = TRUE)
```

## Arguments

- clean_obj:

  An `alccdf_subsidy_clean` object from
  [`subsidy_clean`](https://joonho112.github.io/ALccdfDB/reference/subsidy_clean.md)

- strict:

  Logical; if TRUE, ERROR-level failures abort with an error message.
  Default FALSE (report only).

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_subsidy_validation` S3 object (inherits from
`alccdf_validation`).

## Details

The 7 checks performed are:

1.  **required_columns** (ERROR): Core columns must be present.

2.  **valid_address** (WARN): At least one address field
    (`family_address` or `provider_address`) should be non-empty.

3.  **copay_range** (WARN): Weekly copayment in 0–500 range (clients
    type only).

4.  **age_range** (WARN): Child age between 0 and 18.

5.  **provider_reference** (INFO): `provider_id` should be non-empty.

6.  **duplicate_records** (WARN): Exact duplicates on case_id + child
    key columns.

7.  **pii_separation** (ERROR): Verify that SSN columns are NOT present
    in the main data table.

## Examples

``` r
if (FALSE) { # \dontrun{
validation <- subsidy_validate(clean_obj)
validation
validation$checks
} # }
```
