# Link programs to staff data

Performs a LEFT JOIN of staff data onto program data by matching
`facility_name` (case-insensitive, whitespace-trimmed). Computes
per-program aggregates: number of staff and career lattice level
distribution.

## Usage

``` r
linkage_programs_staff(program_obj, staff_obj, verbose = TRUE)
```

## Arguments

- program_obj:

  A program-module S3 object (e.g., `alccdf_program_unified`,
  `alccdf_program_deduped`, `alccdf_program_clean`) containing
  `facility_name`

- staff_obj:

  A staff-module S3 object (e.g., `alccdf_staff_clean`) containing
  `facility_name`

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_linked_programs_staff` S3 object with:

- data:

  Program-level tibble with staff aggregates appended

- meta:

  Metadata including linkage key, match statistics

- diagnostics:

  Unmatched programs, unmatched staff records

## Details

Staff data uses "Facility Name" (mapped to `facility_name`) which
matches the `facility_name` column in program data. Matching is
case-insensitive and whitespace-trimmed to handle minor inconsistencies.

The linkage pipeline:

1.  Extract data from both objects

2.  Create normalised facility name keys in both datasets

3.  Aggregate staff data to the facility name level

4.  LEFT JOIN aggregated staff onto programs by normalised name

5.  Compute match diagnostics

6.  Build S3 object

## Examples

``` r
if (FALSE) { # \dontrun{
linked <- linkage_programs_staff(program_obj, staff_obj)
alccdf_data(linked)
linked$diagnostics$unmatched_staff
} # }
```
