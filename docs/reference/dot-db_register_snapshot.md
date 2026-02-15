# Register a snapshot in the snapshot registry

Register a snapshot in the snapshot registry

## Usage

``` r
.db_register_snapshot(
  conn,
  table_name,
  snapshot_date,
  n_rows,
  is_current = FALSE
)
```

## Arguments

- conn:

  A DBI connection object

- table_name:

  Character. Table name.

- snapshot_date:

  Date. Snapshot date.

- n_rows:

  Integer. Number of rows.

- is_current:

  Logical. Whether this is the current snapshot.
