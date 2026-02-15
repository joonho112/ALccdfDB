# Separate PII fields from subsidy data

Generates random IDs for parents and children, extracts PII fields into
a separate lookup table, and removes PII from the main data.

## Usage

``` r
.separate_subsidy_pii(df, verbose = TRUE)
```

## Arguments

- df:

  Data frame with raw subsidy columns

- verbose:

  Logical; print progress messages?

## Value

A list with `data` (main data with PII removed) and `pii` (PII lookup
table)
