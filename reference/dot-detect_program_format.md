# Detect program format version

Checks whether the data frame contains a "Facility Tier" column (current
format) or not (legacy format). The detection is case-insensitive.

## Usage

``` r
.detect_program_format(df, type)
```

## Arguments

- df:

  A data frame freshly read from Excel

- type:

  Program type (used for context in messages)

## Value

Character string: `"legacy"` or `"current"`
