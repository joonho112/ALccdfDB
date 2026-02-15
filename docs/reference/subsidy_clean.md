# Clean raw subsidy data

Applies a comprehensive cleaning pipeline to an `alccdf_subsidy_raw`
object. The most critical step is PII separation: parent and child names
and SSNs are extracted into a separate PII table, replaced by random IDs
in the main data.

## Usage

``` r
subsidy_clean(raw_obj, verbose = TRUE)
```

## Arguments

- raw_obj:

  An `alccdf_subsidy_raw` object from
  [`subsidy_read`](https://joonho112.github.io/ALccdfDB/reference/subsidy_read.md)

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_subsidy_clean` S3 object with:

- data:

  Main data tibble with PII fields removed and random IDs added

- pii:

  PII lookup table linking random IDs to names/SSNs

- meta:

  Metadata including cleaning steps

## Details

The cleaning pipeline applies the following operations in order:

1.  **PII Separation**: Generate random IDs for parent and child. Move
    `parent_name`, `child_name`, `parent_ssn`, and `child_ssn` into a
    separate PII table. Main data retains only the random IDs.

2.  **Date Parsing**: Parse `parent_dob`, `child_dob`,
    `eligibility_begin_date`, `eligibility_end_date`,
    `placement_start_date`, `placement_end_date`, `placement_date` from
    character to Date.

3.  **Address Standardisation**: Standardise `family_address` and
    `provider_address` using
    [`.standardize_address()`](https://joonho112.github.io/ALccdfDB/reference/dot-standardize_address.md).

4.  **Copayment Conversion**: Convert `copay_weekly` to numeric (clients
    data only).

5.  **Age Conversion**: Convert `child_age` to numeric.

6.  **County Standardisation**: Title-case county names.

7.  **Snapshot Date**: Add `snapshot_date` column.

## Examples

``` r
if (FALSE) { # \dontrun{
raw   <- subsidy_read("EnrolledChildren.xlsx", "enrolled",
                       as.Date("2025-06-11"))
clean <- subsidy_clean(raw)
clean$pii  # PII lookup table
} # }
```
