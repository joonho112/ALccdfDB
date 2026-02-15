# Link subsidy clients to program data

Performs a LEFT JOIN of program data onto subsidy client data by
`provider_id`, preserving all individual client rows. Each client row
receives program-level columns (e.g., `facility_type`, `latitude`,
`longitude`, `county`, `facility_tier`) for spatial and analytic use.

## Usage

``` r
linkage_clients_programs(clients_obj, program_obj, verbose = TRUE)
```

## Arguments

- clients_obj:

  A subsidy-module S3 object with `subsidy_type = "clients"` (e.g.,
  `alccdf_subsidy_clean`)

- program_obj:

  A program-module S3 object (e.g., `alccdf_program_unified`,
  `alccdf_program_deduped`, `alccdf_program_clean`) containing
  `provider_id`

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_linked_clients_programs` S3 object with:

- data:

  Client-level tibble with program columns appended

- meta:

  Metadata including linkage key, match statistics

- diagnostics:

  Unmatched clients, unmatched programs

## Details

This linkage is designed for spatial analysis comparing household
locations (from client `family_address`) against program locations (from
program `latitude`/`longitude`).

The linkage pipeline:

1.  Extract data from both objects

2.  Select relevant program columns to append (avoiding name collisions)

3.  LEFT JOIN program data onto clients by provider_id

4.  Compute match diagnostics

5.  Build S3 object

Clients without a matching program receive `NA` for all program columns.
All client rows are preserved.

Note: If a `provider_id` matches multiple programs, client rows will be
duplicated. The diagnostics report any such expansion.

## Examples

``` r
if (FALSE) { # \dontrun{
linked <- linkage_clients_programs(clients_obj, program_obj)
alccdf_data(linked)
# Spatial analysis: household vs program coordinates
linked$data %>%
  dplyr::filter(!is.na(latitude)) %>%
  dplyr::select(family_address, provider_name, latitude, longitude)
} # }
```
