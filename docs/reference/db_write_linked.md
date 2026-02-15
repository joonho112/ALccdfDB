# Write linked dataset to DuckDB

Convenience function for writing linkage results (program-QRIS,
subsidy-program, or master) to the database.

## Usage

``` r
db_write_linked(conn, obj, table_name = NULL, verbose = TRUE)
```

## Arguments

- conn:

  A DBI connection object from
  [`db_init`](https://joonho112.github.io/ALccdfDB/reference/db_init.md).

- obj:

  A linkage S3 object (`alccdf_linkage_program_qris`,
  `alccdf_linkage_subsidy_program`, or `alccdf_linkage_master`).

- table_name:

  Character. Table name. Default auto-detected from object class:
  `"linked_program_qris"`, `"linked_subsidy_program"`, or
  `"master_linked"`.

- verbose:

  Logical; print progress messages? Default TRUE.

## Value

Invisible table name.
