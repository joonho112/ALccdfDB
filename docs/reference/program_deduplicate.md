# Deduplicate unified program data

Identifies and removes exact duplicate records from an
`alccdf_program_unified` object. By default, duplicates are identified
using `facility_id`, but additional key columns can be specified.

## Usage

``` r
program_deduplicate(
  unified_obj,
  key_cols = "facility_id",
  keep = c("first", "last"),
  verbose = TRUE
)
```

## Arguments

- unified_obj:

  An `alccdf_program_unified` object from
  [`program_append_types`](https://joonho112.github.io/ALccdfDB/reference/program_append_types.md)

- key_cols:

  Character vector of column names used to identify duplicates. Default
  is `"facility_id"`.

- keep:

  Strategy for which duplicate to keep: `"first"` (default) or `"last"`.

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_program_deduped` S3 object

## Details

When duplicates are found on the specified key columns, only the first
(or last) occurrence is retained. The removed duplicates are stored in
`obj$diagnostics$removed_duplicates` for inspection.

## Examples

``` r
if (FALSE) { # \dontrun{
deduped <- program_deduplicate(unified)
deduped <- program_deduplicate(unified, key_cols = c("facility_id", "facility_type"))
} # }
```
