# Clean raw program data

Applies a comprehensive cleaning pipeline to an `alccdf_program_raw`
object. All cleaning steps are logged for reproducibility. Produces
output consistent with the established `all_programs_appended_cleaned`
protocol, including proper R factor levels for categorical variables and
analysis-ready derived variables.

## Usage

``` r
program_clean(raw_obj, verbose = TRUE)
```

## Arguments

- raw_obj:

  An `alccdf_program_raw` object from
  [`program_read`](https://joonho112.github.io/ALccdfDB/reference/program_read.md)

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_program_clean` S3 object

## Details

The cleaning pipeline applies the following operations in order:

1.  Standardise addresses (facility_address, physical_address)

2.  Create facility_address2 (further standardised)

3.  Parse expiration_date and compute days_till_expire

4.  Parse time fields and create datetime versions

5.  Convert capacities to integer + derive capacity variables

6.  Parse age ranges with binary age-group indicators

7.  Compute operating hours (day/night) + operation type

8.  Recode facility_tier as ordered factor (Star 1–5) with quality
    metrics

9.  Standardise county as factor (67 Alabama counties)

10. Set region as factor

11. Assign facility_type factor using raw data values (preserves Family
    Home vs Group Home distinction)

12. Convert children_under_12mo to integer

## Examples

``` r
if (FALSE) { # \dontrun{
raw   <- program_read("Centers.xlsx", "center", as.Date("2025-09-12"))
clean <- program_clean(raw)
} # }
```
