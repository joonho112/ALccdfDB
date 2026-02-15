# Append multiple program types into a unified data set

Takes one or more `alccdf_program_clean` objects and stacks them into a
single data frame, aligning columns and filling type-specific fields
with NA where absent.

## Usage

``` r
program_append_types(..., verbose = TRUE)
```

## Arguments

- ...:

  One or more `alccdf_program_clean` objects, or a single named list of
  such objects (as returned by
  [`program_clean_all`](https://joonho112.github.io/ALccdfDB/reference/program_clean_all.md)).

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

An `alccdf_program_unified` S3 object containing the combined data.

## Details

The function:

1.  Validates that all inputs are `alccdf_program_clean` objects.

2.  Validates that all inputs share the same `snapshot_date`.

3.  Aligns columns: the union of all columns is used; missing columns in
    any type are filled with NA.

4.  Checks for duplicate `facility_id` across types (warns only).

5.  Returns a unified object with provenance metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
unified <- program_append_types(clean_center, clean_home, clean_exempt)
unified <- program_append_types(clean_list)  # also works with a list
} # }
```
