# Link programs to subsidy enrolled children

Performs a LEFT JOIN of subsidy enrolled children data onto program data
by `facility_id`, preserving all program rows. Computes per-program
aggregates: number of enrolled children, number of cases, and care level
distribution.

## Usage

``` r
linkage_programs_enrolled(program_obj, enrolled_obj, verbose = TRUE)
```

## Arguments

- program_obj:

  A program-module S3 object (e.g., `alccdf_program_unified`,
  `alccdf_program_deduped`, `alccdf_program_clean`) containing
  `facility_id`

- enrolled_obj:

  A subsidy-module S3 object with `subsidy_type = "enrolled"` (e.g.,
  `alccdf_subsidy_clean`)

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_linked_programs_enrolled` S3 object with:

- data:

  Program-level tibble with enrollment aggregates appended

- meta:

  Metadata including linkage key, match statistics

- diagnostics:

  Unmatched programs, unmatched enrolled records

## Details

The linkage pipeline:

1.  Extract data from both objects

2.  Aggregate enrolled data to the program (facility_id) level

3.  LEFT JOIN aggregated enrollment onto programs by facility_id

4.  Compute match diagnostics

5.  Build S3 object

Programs without any enrolled children receive `NA` for enrollment
columns. All 2,193 programs (or however many exist) are preserved.

## Examples

``` r
if (FALSE) { # \dontrun{
linked <- linkage_programs_enrolled(program_obj, enrolled_obj)
alccdf_data(linked)
linked$diagnostics$unmatched_programs
} # }
```
