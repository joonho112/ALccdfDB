# Configure a program data processing pipeline

Sets up paths and parameters for processing program-level data from a
DHR snapshot. At least one program file path must be provided.

## Usage

``` r
program_config(
  snapshot_date,
  center_path = NULL,
  home_path = NULL,
  exempt_path = NULL,
  excepted_path = NULL,
  output_dir = NULL,
  verbose = TRUE
)
```

## Arguments

- snapshot_date:

  Date of the administrative snapshot (Date or character parseable to
  Date via [`as.Date()`](https://rdrr.io/r/base/as.Date.html))

- center_path:

  Path to Licensed Day Care Center Excel file (NULL if not available)

- home_path:

  Path to Family and Group Home Excel file (NULL if not available)

- exempt_path:

  Path to Exempt Center (Faith-Based) Excel file (NULL if not available)

- excepted_path:

  Path to License Excepted Programs Excel file (NULL if not available)

- output_dir:

  Output directory for exports (auto-generated from snapshot_date if
  NULL)

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_program_config` S3 object containing paths, snapshot_date,
and pipeline parameters.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- program_config(
  snapshot_date = "2025-06-11",
  center_path   = "data/Centers_06_11_25.xlsx",
  home_path     = "data/FamilyHomes_06_11_25.xlsx"
)
cfg
} # }
```
