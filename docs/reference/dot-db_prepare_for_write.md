# Prepare data frame for DuckDB write

Converts factors to character, POSIXct to character, and Date to
character for reliable DuckDB storage.

## Usage

``` r
.db_prepare_for_write(df)
```

## Arguments

- df:

  A data frame

## Value

A plain data frame suitable for DBI::dbWriteTable
