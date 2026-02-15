# Clean all raw program objects from a list

Convenience wrapper that applies
[`program_clean`](https://joonho112.github.io/ALccdfDB/reference/program_clean.md)
to every element of a list of `alccdf_program_raw` objects.

## Usage

``` r
program_clean_all(raw_list, verbose = TRUE)
```

## Arguments

- raw_list:

  A named list of `alccdf_program_raw` objects (as returned by
  [`program_read_all`](https://joonho112.github.io/ALccdfDB/reference/program_read_all.md))

- verbose:

  Logical; print progress messages?

## Value

A named list of `alccdf_program_clean` objects
