# Read a single program Excel file

Reads a DHR program Excel file (Centers, Family/Group Homes, Exempt, or
Excepted), automatically detects the format version (legacy vs.
current), applies the appropriate column mapping, and returns a
standardised raw object.

## Usage

``` r
program_read(
  path,
  type,
  snapshot_date,
  sheet = NULL,
  skip = NULL,
  verbose = TRUE
)
```

## Arguments

- path:

  Path to the Excel file

- type:

  Character string: one of `"center"`, `"family_home"`, `"exempt"`, or
  `"excepted"`

- snapshot_date:

  Date of the administrative snapshot

- sheet:

  Sheet name or index to read (NULL for the first sheet)

- skip:

  Number of header rows to skip. If NULL, auto-detected from type:
  `"exempt"` = 0, all others = 2.

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_program_raw` S3 object

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- program_read(
  path = "data/Centers_06_11_25.xlsx",
  type = "center",
  snapshot_date = as.Date("2025-06-11")
)
} # }
```
