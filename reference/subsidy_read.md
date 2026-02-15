# Read a single subsidy Excel file

Reads a DHR subsidy Excel file (Enrolled Children or Active Clients With
Addresses), automatically detects the format version (legacy vs. current
for enrolled data), applies the appropriate column mapping, and returns
a standardised raw object.

## Usage

``` r
subsidy_read(
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

  Character string: one of `"enrolled"` or `"clients"`

- snapshot_date:

  Date of the administrative snapshot

- sheet:

  Sheet name or index to read (NULL for the first sheet)

- skip:

  Number of header rows to skip. If NULL, auto-detected: `"enrolled"`
  tries skip = 0 first (current format), falls back to skip = 2
  (legacy); `"clients"` always skip = 2.

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_subsidy_raw` S3 object

## Details

Format detection for enrolled data:

- **Current (Version B)**: 20 columns with skip = 0. Contains "Parent
  ID/SSN" and "Child ID/SSN" columns.

- **Legacy (Version A)**: 18 columns with skip = 2. Does NOT contain
  "Parent ID/SSN" or "Child ID/SSN" columns.

For clients data, the format is always 14 columns with skip = 2.

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- subsidy_read(
  path = "data/ActiveEnrolledChildren_06_11_25.xlsx",
  type = "enrolled",
  snapshot_date = as.Date("2025-06-11")
)

raw_clients <- subsidy_read(
  path = "data/ActiveClientsWithAddresses_06_11_25.xlsx",
  type = "clients",
  snapshot_date = as.Date("2025-06-11")
)
} # }
```
