# Prepare data frame for Stata export

Converts factor and Date columns to types that Stata can handle, and
ensures variable names comply with Stata naming rules.

## Usage

``` r
.prepare_for_stata(data)
```

## Arguments

- data:

  A data frame

## Value

A data frame suitable for haven::write_dta
