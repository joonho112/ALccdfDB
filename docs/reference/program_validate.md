# Validate cleaned program data

Runs 10 data-quality checks against an `alccdf_program_clean` object. In
strict mode, any ERROR-level check causes an immediate abort.

## Usage

``` r
program_validate(clean_obj, strict = FALSE, verbose = TRUE)
```

## Arguments

- clean_obj:

  An `alccdf_program_clean` object from
  [`program_clean`](https://joonho112.github.io/ALccdfDB/reference/program_clean.md)

- strict:

  Logical; if TRUE, ERROR-level failures abort with an error message.
  Default FALSE (report only).

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_program_validation` S3 object (inherits from
`alccdf_validation`).

## Details

The 10 checks performed are:

1.  **required_columns** (ERROR): Core columns must be present.

2.  **duplicate_facility_id** (ERROR): Facility IDs must be unique.

3.  **positive_capacity** (WARN): Capacities must be positive or NA.

4.  **valid_county** (WARN): County must match one of Alabama's 67.

5.  **expiration_date_range** (WARN): Dates not more than 2 years past.

6.  **operating_hours_range** (WARN): Hours between 0 and 24.

7.  **age_range_logic** (ERROR): age_min must be \< age_max.

8.  **address_completeness** (WARN): Address should not be missing.

9.  **facility_tier_valid** (WARN): Star 1–5 or NA only.

10. **snapshot_date_present** (ERROR): snapshot_date must exist.

## Examples

``` r
if (FALSE) { # \dontrun{
validation <- program_validate(clean_obj)
validation
validation$checks
} # }
```
