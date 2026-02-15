# Normalise an address for join matching

Creates a canonical form of address strings for robust matching between
DHR program data and Melissa geocoding data. Handles common formatting
differences: case, punctuation, whitespace, abbreviation variants, and
trailing county names appended by Melissa.

## Usage

``` r
.normalise_address_for_join(x)
```

## Arguments

- x:

  Character vector of addresses

## Value

Character vector of normalised addresses
