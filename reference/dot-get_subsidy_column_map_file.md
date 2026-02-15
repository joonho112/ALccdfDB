# Get the path to the appropriate subsidy column map CSV

Get the path to the appropriate subsidy column map CSV

## Usage

``` r
.get_subsidy_column_map_file(type, format_version)
```

## Arguments

- type:

  Subsidy type ("enrolled" or "clients")

- format_version:

  `"legacy"` or `"current"`

## Value

Full path to the CSV file within `inst/extdata/mappings/`
