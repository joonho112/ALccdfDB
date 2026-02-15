# Generate linkage report

Takes a named list of linked objects and generates a markdown report
summarising match statistics, unmatched analyses, and an overall
comparison table across all linkages.

## Usage

``` r
linkage_report(linked_objects, output_dir, verbose = TRUE)
```

## Arguments

- linked_objects:

  A named list of linked S3 objects. Expected names include any
  combination of:

  programs_enrolled

  :   An `alccdf_linked_programs_enrolled` object

  programs_clients

  :   An `alccdf_linked_programs_clients` object

  programs_staff

  :   An `alccdf_linked_programs_staff` object

  clients_programs

  :   An `alccdf_linked_clients_programs` object

- output_dir:

  Directory where the report file will be written.

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

Invisible character string: the path to the generated report file.

## Details

The report includes:

- A header with generation timestamp and package version

- For each linked dataset:

  - Match key used

  - Match count and match rate

  - Unmatched record summary

- A summary comparison table across all linkages

## Examples

``` r
if (FALSE) { # \dontrun{
linked <- list(
  programs_enrolled = linkage_programs_enrolled(prog, enrolled),
  programs_clients  = linkage_programs_clients(prog, clients),
  programs_staff    = linkage_programs_staff(prog, staff),
  clients_programs  = linkage_clients_programs(clients, prog)
)
report_path <- linkage_report(linked, output_dir = "output/reports")
} # }
```
