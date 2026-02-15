# Export data to multiple formats

Export data to multiple formats

## Usage

``` r
export_all(
  obj,
  dir,
  basename,
  formats = c("csv", "excel", "stata", "parquet", "rds")
)
```

## Arguments

- obj:

  An ALccdfDB S3 object or a data frame

- dir:

  Output directory

- basename:

  Base filename (without extension)

- formats:

  Character vector of formats: "csv", "excel", "stata", "parquet", "rds"

## Value

Invisible named character vector of paths
