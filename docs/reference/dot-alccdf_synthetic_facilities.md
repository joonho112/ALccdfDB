# Generate shared facility identities

Creates deterministic facility records for all generators to share. When
called with the same seed and n, produces identical facilities.

## Usage

``` r
.alccdf_synthetic_facilities(n, seed)
```

## Arguments

- n:

  Integer. Number of facilities to generate.

- seed:

  Integer. Random seed.

## Value

A tibble with facility_id, facility_name, facility_type, provider_id,
county, region, facility_address.
