# Detect subsidy format version

For enrolled data, checks whether "Parent ID/SSN" column is present
(current format) or not (legacy format). For clients data, always
returns "current".

## Usage

``` r
.detect_subsidy_format(df, type)
```

## Arguments

- df:

  A data frame freshly read from Excel

- type:

  Subsidy type ("enrolled" or "clients")

## Value

Character string: `"legacy"` or `"current"`
