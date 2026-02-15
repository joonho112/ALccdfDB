# Clean raw staff data

Applies a comprehensive cleaning pipeline to an `alccdf_staff_raw`
object. PII fields (staff_name, user_email) are separated into a
distinct `$pii` table linked by a randomly generated `random_staff_id`.
All cleaning steps are logged for reproducibility.

## Usage

``` r
staff_clean(raw_obj, verbose = TRUE)
```

## Arguments

- raw_obj:

  An `alccdf_staff_raw` object from
  [`staff_read`](https://joonho112.github.io/ALccdfDB/reference/staff_read.md)

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_staff_clean` S3 object with components:

- data:

  Main data frame with PII removed and random_staff_id added

- pii:

  PII table with random_staff_id, staff_name, user_email

- meta:

  Metadata including processing log

## Details

The cleaning pipeline applies the following operations in order:

1.  Generate random staff IDs for PII linkage

2.  Separate PII fields (staff_name, user_email) into `$pii` table

3.  Standardise county names (user_county, facility_county) to title
    case

4.  Clean career_lattice_level (standardise levels)

5.  Convert currently_operating to logical

6.  Add snapshot_date column

## Examples

``` r
if (FALSE) { # \dontrun{
raw   <- staff_read("AlabamaPathways_ECProfessionalLevelUsers.csv", "2025-06-04")
clean <- staff_clean(raw)
clean$data     # main data without PII
clean$pii      # PII table
} # }
```
