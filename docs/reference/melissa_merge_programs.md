# Merge Melissa coordinates into program data

Joins Melissa geocoding results (latitude, longitude, census tract,
etc.) to a program-module S3 object. Matching uses `facility_address` in
the program data against `address` in the Melissa data, with both sides
normalised for robust matching.

## Usage

``` r
melissa_merge_programs(program_obj, melissa_path, verbose = TRUE)
```

## Arguments

- program_obj:

  A program-module S3 object (any stage from clean onward)

- melissa_path:

  Path to the Melissa program geocoding file (RDS, Excel, or CSV).
  Expected columns: address, LAT, LNG, CT, CENSUSBLOC, FIPS, COUNTYNAME,
  PLACENAME, PLACECODE, RESULTCODE, STATUSCODE, MD_City, MD_PostalCode,
  etc.

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

The input program object augmented with geocoding columns (latitude,
longitude, census_tract, fips_code, etc.)
