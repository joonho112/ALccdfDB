# Recode facility tier values

Converts facility tier strings to an ordered factor. "None", empty
strings, and similar non-star values are mapped to NA.

## Usage

``` r
.recode_facility_tier(tier_raw)
```

## Arguments

- tier_raw:

  Character vector of raw tier values

## Value

Ordered factor with levels Star 1 through Star 5
