# Merge Melissa coordinates into subsidy client household data

Joins Melissa geocoding results to subsidy client data. Matching uses
`family_address` in the subsidy data against `address` in the Melissa
household geocoding data, with both sides normalised for robust
matching.

## Usage

``` r
melissa_merge_households(subsidy_obj, melissa_path, verbose = TRUE)
```

## Arguments

- subsidy_obj:

  A subsidy-module S3 object (clients type)

- melissa_path:

  Path to the Melissa household geocoding file

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

The input subsidy object augmented with geocoding columns
