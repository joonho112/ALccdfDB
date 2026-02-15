# Create further-standardised address (v2)

Applies additional standardisation beyond the basic address cleaning:
normalises abbreviations, fixes directionals consistently. Matches the
`facility_address2` logic from the established cleaning protocol.

## Usage

``` r
.standardize_address_v2(x)
```

## Arguments

- x:

  Character vector of addresses (already through basic standardisation)

## Value

Character vector
