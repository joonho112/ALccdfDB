# Read all program files from a configuration

Convenience wrapper that reads all program files specified in a
`alccdf_program_config` object and returns them as a named list.

## Usage

``` r
program_read_all(config)
```

## Arguments

- config:

  An `alccdf_program_config` object created by
  [`program_config`](https://joonho112.github.io/ALccdfDB/reference/program_config.md)

## Value

A named list of `alccdf_program_raw` objects, one per available type.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- program_config(
  snapshot_date = "2025-06-11",
  center_path   = "data/Centers.xlsx",
  home_path     = "data/FamilyHomes.xlsx"
)
raw_list <- program_read_all(cfg)
names(raw_list)
} # }
```
