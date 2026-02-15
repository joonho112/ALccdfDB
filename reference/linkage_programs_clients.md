# Link programs to subsidy clients

Performs a LEFT JOIN of subsidy client data onto program data by
`provider_id`, preserving all program rows. Computes per-program
aggregates: number of active clients, average weekly copayment, and
number of unique cases.

## Usage

``` r
linkage_programs_clients(program_obj, clients_obj, verbose = TRUE)
```

## Arguments

- program_obj:

  A program-module S3 object (e.g., `alccdf_program_unified`,
  `alccdf_program_deduped`, `alccdf_program_clean`) containing
  `provider_id`

- clients_obj:

  A subsidy-module S3 object with `subsidy_type = "clients"` (e.g.,
  `alccdf_subsidy_clean`)

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_linked_programs_clients` S3 object with:

- data:

  Program-level tibble with client aggregates appended

- meta:

  Metadata including linkage key, match statistics

- diagnostics:

  Unmatched programs, unmatched client records

## Details

The linkage pipeline:

1.  Extract data from both objects

2.  Aggregate client data to the program (provider_id) level

3.  LEFT JOIN aggregated clients onto programs by provider_id

4.  Compute match diagnostics

5.  Build S3 object

Programs without any active clients receive `0` for count columns and
`NA` for average copay.

## Examples

``` r
if (FALSE) { # \dontrun{
linked <- linkage_programs_clients(program_obj, clients_obj)
alccdf_data(linked)
linked$diagnostics$unmatched_clients
} # }
```
