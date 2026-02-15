# Cross-Module Linkage and Database Integration

## Overview

ALccdfDB links four administrative data modules into enriched,
analysis-ready datasets. The linkage layer connects programs to enrolled
children (by `facility_id`), to subsidy clients (by `provider_id`), and
to Alabama Pathways staff records (by `facility_name`,
case-insensitive). A reverse linkage also appends program attributes
onto individual client rows for spatial analysis.

Four linkage types are available:

- **Programs x Enrolled** – aggregate enrollment counts onto programs
- **Programs x Clients** – aggregate client counts and copay onto
  programs
- **Programs x Staff** – aggregate staff counts and career levels onto
  programs
- **Clients x Programs** – append program attributes onto client rows

This vignette also demonstrates Melissa geocoding output and DuckDB
integration for persistent storage and SQL-based querying.

## Setup and Data Generation

All synthetic generators share a common facility pool when called with
the same seed, ensuring that `facility_id`, `provider_id`, and
`facility_name` overlap across modules to enable linkage.

``` r

library(ALccdfDB)
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.5.2
#> 
#> Attaching package: 'dplyr'
#> The following object is masked from 'package:ALccdfDB':
#> 
#>     db_list_tables
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 4.5.2

seed <- 42
programs <- alccdf_synthetic_programs(n = 50, seed = seed)
subsidy  <- alccdf_synthetic_subsidy(n_enrolled = 80, n_clients = 40, seed = seed)
staff    <- alccdf_synthetic_staff(n = 60, seed = seed)
melissa  <- alccdf_synthetic_melissa(programs = programs, seed = seed)
```

## Verify Shared Keys

Before running linkage, verify that the shared join keys overlap across
modules.

``` r

prg_data <- alccdf_data(programs)
enr_data <- alccdf_data(subsidy$enrolled)
cli_data <- alccdf_data(subsidy$clients)
stf_data <- alccdf_data(staff)

# facility_id overlap (programs <-> enrolled)
prg_fids <- unique(prg_data$facility_id)
enr_fids <- unique(enr_data$facility_id)
cat("Programs facility_ids:", length(prg_fids), "\n")
#> Programs facility_ids: 50
cat("Enrolled facility_ids:", length(enr_fids), "\n")
#> Enrolled facility_ids: 50
cat("Overlap (facility_id):", length(intersect(prg_fids, enr_fids)), "\n\n")
#> Overlap (facility_id): 33

# provider_id overlap (programs <-> clients)
prg_pids <- unique(prg_data$provider_id)
cli_pids <- unique(cli_data$provider_id)
cat("Programs provider_ids:", length(prg_pids), "\n")
#> Programs provider_ids: 50
cat("Clients provider_ids:", length(cli_pids), "\n")
#> Clients provider_ids: 32
cat("Overlap (provider_id):", length(intersect(prg_pids, cli_pids)), "\n\n")
#> Overlap (provider_id): 32

# facility_name overlap (programs <-> staff, case-insensitive)
prg_names <- unique(tolower(trimws(prg_data$facility_name)))
stf_names <- unique(tolower(trimws(stf_data$facility_name)))
cat("Programs facility_names:", length(prg_names), "\n")
#> Programs facility_names: 50
cat("Staff facility_names:", length(stf_names), "\n")
#> Staff facility_names: 37
cat("Overlap (facility_name):", length(intersect(prg_names, stf_names)), "\n")
#> Overlap (facility_name): 28
```

## Programs x Enrolled Children

Link programs to enrolled children by `facility_id`. Each program row is
preserved (LEFT JOIN) and enriched with enrollment aggregates: the
number of enrolled children, case counts, and care level distribution
columns.

``` r

linked_pe <- linkage_programs_enrolled(programs, subsidy$enrolled, verbose = FALSE)
linked_pe
#> 
#> ── ALccdfDB Linkage: Programs x Enrolled Children ──
#> 
#> ℹ Join key: facility_id
#> ℹ Backbone: programs
#> ℹ Dimensions: 50 rows x 57 cols
#> ℹ Programs: 50
#> ℹ Enrolled records: 80
#> ℹ Match rate: 33/50 (66%)
#> 
#> ── Processing Log
#> [2026-02-15 15:55:55] Linked programs x enrolled: 33/50 programs matched (66%),
#> 80 enrolled records
```

Inspect the new columns added by the linkage:

``` r

pe_df <- alccdf_data(linked_pe)
new_cols <- c("n_enrolled_children", "n_cases",
              grep("^care_level_", names(pe_df), value = TRUE))
head(pe_df[, intersect(c("facility_id", "facility_name", new_cols), names(pe_df))], 10)
#> # A tibble: 10 × 8
#>    facility_id facility_name  n_enrolled_children n_cases care_level_School Ag…¹
#>    <chr>       <chr>                        <int>   <int>                  <int>
#>  1 FX001       JSU Child Lea…                   0       0                     NA
#>  2 FX002       Gadsden State…                   1       1                      1
#>  3 FC001       Jubilee Child…                   1       1                      1
#>  4 FG001       Lakeside Grou…                   2       2                      1
#>  5 FE001       New Hope Chur…                   1       1                      0
#>  6 FE002       Emmanuel Luth…                   0       0                     NA
#>  7 FF001       Hall Family D…                   0       0                     NA
#>  8 FC002       Happy Hearts …                   0       0                     NA
#>  9 FF002       Walker Home C…                   2       2                      1
#> 10 FF003       Smith Family …                   1       1                      1
#> # ℹ abbreviated name: ¹​`care_level_School Age`
#> # ℹ 3 more variables: care_level_PreSchool <int>, care_level_Toddler <int>,
#> #   care_level_Infant <int>
```

**Figure 1: Distribution of Enrolled Children per Program**

``` r

pe_plot <- pe_df[!is.na(pe_df$n_enrolled_children), ]

ggplot(pe_plot, aes(x = n_enrolled_children)) +
  geom_histogram(binwidth = 1, fill = "#4292C6", color = "white") +
  labs(
    title = "Enrolled Children per Program",
    x = "Number of Enrolled Children",
    y = "Count of Programs"
  ) +
  theme_minimal()
```

![Distribution of enrolled children per
program](data-linkage_files/figure-html/fig-enrolled-hist-1.png)

Distribution of enrolled children per program

**Figure 2: Enrolled Children by Facility Type**

``` r

ggplot(pe_plot, aes(x = facility_type, y = n_enrolled_children)) +
  geom_boxplot(fill = "#9ECAE1", outlier.color = "#084594") +
  labs(
    title = "Enrolled Children by Facility Type",
    x = "Facility Type",
    y = "Number of Enrolled Children"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
```

![Enrolled children by facility
type](data-linkage_files/figure-html/fig-enrolled-boxplot-1.png)

Enrolled children by facility type

## Programs x Clients

Link programs to subsidy clients (Active Clients With Addresses) by
`provider_id`. Aggregated columns include `n_active_clients`,
`avg_copay_weekly`, and `n_unique_cases`.

``` r

linked_pc <- linkage_programs_clients(programs, subsidy$clients, verbose = FALSE)
linked_pc
#> 
#> ── ALccdfDB Linkage: Programs x Subsidy Clients ──
#> 
#> ℹ Join key: provider_id
#> ℹ Backbone: programs
#> ℹ Dimensions: 50 rows x 54 cols
#> ℹ Programs: 50
#> ℹ Client records: 40
#> ℹ Match rate: 32/50 (64%)
#> 
#> ── Processing Log
#> [2026-02-15 15:55:56] Linked programs x clients: 32/50 programs matched (64%),
#> 40 client records
```

``` r

pc_df <- alccdf_data(linked_pc)
head(pc_df[, c("facility_id", "facility_name", "n_active_clients",
               "avg_copay_weekly", "n_unique_cases")], 10)
#> # A tibble: 10 × 5
#>    facility_id facility_name    n_active_clients avg_copay_weekly n_unique_cases
#>    <chr>       <chr>                       <int>            <dbl>          <int>
#>  1 FX001       JSU Child Learn…                3            35.8               3
#>  2 FX002       Gadsden State C…                0            NA                 0
#>  3 FC001       Jubilee Child C…                2            15.2               2
#>  4 FG001       Lakeside Group …                1            31.2               1
#>  5 FE001       New Hope Church…                1            45.4               1
#>  6 FE002       Emmanuel Luther…                1             8.72              1
#>  7 FF001       Hall Family Day…                0            NA                 0
#>  8 FC002       Happy Hearts Ch…                1             6.19              1
#>  9 FF002       Walker Home Chi…                1             9.57              1
#> 10 FF003       Smith Family Ch…                0            NA                 0
```

## Programs x Staff

Link programs to Alabama Pathways staff data by `facility_name`
(case-insensitive, whitespace-trimmed). Adds `n_staff` and career
lattice level distribution columns.

``` r

linked_ps <- linkage_programs_staff(programs, staff, verbose = FALSE)
linked_ps
#> 
#> ── ALccdfDB Linkage: Programs x Staff ──
#> 
#> ℹ Join key: facility_name (case-insensitive, trimmed)
#> ℹ Backbone: programs
#> ℹ Dimensions: 50 rows x 57 cols
#> ℹ Programs: 50
#> ℹ Staff records: 60
#> ℹ Match rate: 28/50 (56%)
#> 
#> ── Processing Log
#> [2026-02-15 15:55:56] Linked programs x staff: 28/50 programs matched (56%), 60
#> staff records
```

**Figure 3: Mean Staff per Program by Facility Type**

``` r

ps_df <- alccdf_data(linked_ps)

staff_summary <- aggregate(n_staff ~ facility_type, data = ps_df, FUN = mean)
staff_summary$n_staff <- round(staff_summary$n_staff, 1)

ggplot(staff_summary, aes(x = reorder(facility_type, n_staff), y = n_staff)) +
  geom_col(fill = "#74C476") +
  geom_text(aes(label = n_staff), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title = "Mean Staff per Program by Facility Type",
    x = "Facility Type",
    y = "Mean Number of Staff"
  ) +
  theme_minimal()
```

![Mean staff count per program by facility
type](data-linkage_files/figure-html/fig-staff-bar-1.png)

Mean staff count per program by facility type

## Clients x Programs

The reverse linkage appends program-level attributes (facility type,
tier, coordinates, capacity, etc.) onto individual client rows. This is
useful for spatial analysis comparing household and program locations.

``` r

linked_cp <- linkage_clients_programs(subsidy$clients, programs, verbose = FALSE)
linked_cp
#> 
#> ── ALccdfDB Linkage: Clients x Programs ──
#> 
#> ℹ Join key: provider_id
#> ℹ Backbone: clients
#> ℹ Dimensions: 40 rows x 33 cols
#> ℹ Clients: 40
#> ℹ Programs: 50
#> ℹ Match rate: 40/40 (100%)
#> ℹ Program columns appended: facility_id, facility_name, facility_type, facility_tier, facility_address, county, region, day_capacity, night_capacity, day_age_range, night_age_range, day_start, day_end, night_start, night_end, expiration_date, days_till_expire, license_number, snapshot_date
#> 
#> ── Processing Log
#> [2026-02-15 15:55:56] Linked clients x programs: 40/40 client rows matched
#> (100%), 19 program columns appended
```

``` r

cp_df <- alccdf_data(linked_cp)
prog_cols <- intersect(
  c("provider_id", "family_address", "facility_name", "facility_type", "county"),
  names(cp_df)
)
head(cp_df[, prog_cols], 8)
#> # A tibble: 8 × 5
#>   provider_id family_address                  facility_name facility_type county
#>   <chr>       <chr>                           <chr>         <fct>         <chr> 
#> 1 P0004       2442 Cypress Circle, Florence,… Lakeside Gro… Group Home    Calho…
#> 2 P0001       2997 Walnut Avenue, Alabaster,… JSU Child Le… Excepted (Un… St. C…
#> 3 P0020       842 Oak Lane, Alabaster, AL 35… Pleasant Hil… Faith-Based   Dale  
#> 4 P0048       8066 Elm Street, Florence, AL … Grace Commun… Faith-Based   Houst…
#> 5 P0044       1669 Pecan Drive, Mobile, AL 3… Calhoun Comm… Excepted (Un… St. C…
#> 6 P0042       6738 Pine Way, Mobile, AL 36602 Dogwood Lear… Center        Montg…
#> 7 P0038       523 Cypress Circle, Huntsville… ABC Learning… Center        St. C…
#> 8 P0023       6690 Birch Road, Dothan, AL 36… Auburn Unive… Excepted (Un… St. C…
```

## Linkage Diagnostics Summary

Compare match rates across all four linkage types. The summary is built
manually since the metadata structure differs between linkage types.

``` r

diag_summary <- tibble::tibble(
  Linkage = c(
    "Programs x Enrolled",
    "Programs x Clients",
    "Programs x Staff",
    "Clients x Programs"
  ),
  Join_Key = c(
    "facility_id",
    "provider_id",
    "facility_name",
    "provider_id"
  ),
  n_matched = c(
    linked_pe$meta$n_matched,
    linked_pc$meta$n_matched,
    linked_ps$meta$n_matched,
    linked_cp$meta$n_matched
  ),
  n_total = c(
    linked_pe$meta$n_programs,
    linked_pc$meta$n_programs,
    linked_ps$meta$n_programs,
    linked_cp$meta$n_linked
  ),
  match_rate_pct = c(
    linked_pe$meta$match_rate,
    linked_pc$meta$match_rate,
    linked_ps$meta$match_rate,
    linked_cp$meta$match_rate
  )
)

diag_summary
#> # A tibble: 4 × 5
#>   Linkage             Join_Key      n_matched n_total match_rate_pct
#>   <chr>               <chr>             <int>   <int>          <dbl>
#> 1 Programs x Enrolled facility_id          33      50             66
#> 2 Programs x Clients  provider_id          32      50             64
#> 3 Programs x Staff    facility_name        28      50             56
#> 4 Clients x Programs  provider_id          40      40            100
```

## Melissa Geocoding

The Melissa geocoding module provides latitude, longitude, census tract,
FIPS code, and address standardization for both program and household
addresses. The synthetic generator produces realistic Alabama
coordinates within the state bounding box.

``` r

head(melissa$programs, 8)
#> # A tibble: 8 × 15
#>   facility_address        latitude longitude census_tract census_block fips_code
#>   <chr>                      <dbl>     <dbl>        <dbl>        <dbl>     <dbl>
#> 1 4991 MCFARLAND BOULEVA…     31.7     -85.7       816614         9727      1097
#> 2 6873 MCFARLAND BOULEVA…     30.3     -86.4       574227         2495      1069
#> 3 3208 HIGHLAND AVENUE, …     31.9     -85.4       767203         4334      1103
#> 4 9453 QUINTARD AVENUE, …     31.9     -86.3       691108         2268      1081
#> 5 2920 HIGHLAND AVENUE, …     32.1     -85.5       859017         1865      1073
#> 6 4273 MEMORIAL PARKWAY,…     32.6     -87.9       621691         4348      1003
#> 7 4721 DEXTER AVENUE, AL…     34.3     -84.9       777938         1043      1055
#> 8 3839 FAIRVIEW AVENUE, …     30.4     -85.4       667786         6534      1003
#> # ℹ 9 more variables: melissa_county <chr>, melissa_place <chr>,
#> #   melissa_place_code <int>, melissa_city <chr>, melissa_state <chr>,
#> #   melissa_zip <chr>, melissa_geo_zip <chr>, melissa_result_code <chr>,
#> #   melissa_status_code <chr>
```

**Figure 4: Geocoded Program Locations**

``` r

ggplot(melissa$programs, aes(x = longitude, y = latitude)) +
  geom_point(color = "#E6550D", alpha = 0.7, size = 2.5) +
  labs(
    title = "Geocoded Program Locations",
    subtitle = "Synthetic Melissa output for Alabama CCDF programs",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()
```

![Scatter plot of geocoded program locations across
Alabama](data-linkage_files/figure-html/fig-melissa-scatter-1.png)

Scatter plot of geocoded program locations across Alabama

## DuckDB Integration

Store processed data in a DuckDB database for efficient querying and
persistent snapshots. The database module supports writing any S3 object
via
[`db_write_snapshot()`](https://joonho112.github.io/ALccdfDB/reference/db_write_snapshot.md)
and querying with standard SQL.

``` r

db_path <- tempfile(fileext = ".duckdb")
conn <- db_init(db_path, verbose = FALSE)
```

``` r

# Write program, enrolled, and staff snapshots
db_write_snapshot(conn, programs, "programs", verbose = FALSE)
db_write_snapshot(conn, subsidy$enrolled, "enrolled", verbose = FALSE)
db_write_snapshot(conn, staff, "staff", verbose = FALSE)

ALccdfDB::db_list_tables(conn)
#> [1] "enrolled" "programs" "staff"
```

### SQL Queries

``` r

# Programs by county: count and average day capacity
db_query(conn, "
  SELECT county, COUNT(*) AS n_programs,
         ROUND(AVG(day_capacity), 0) AS avg_day_capacity
  FROM programs
  GROUP BY county
  ORDER BY n_programs DESC
  LIMIT 10
", verbose = FALSE)
#> # A tibble: 10 × 3
#>    county     n_programs avg_day_capacity
#>    <chr>           <dbl>            <dbl>
#>  1 St. Clair           5               97
#>  2 Morgan              4               43
#>  3 Calhoun             4               48
#>  4 Mobile              4               50
#>  5 Tuscaloosa          3               68
#>  6 DeKalb              3               36
#>  7 Lauderdale          3              114
#>  8 Jefferson           3               63
#>  9 Lee                 3               34
#> 10 Shelby              2                6
```

``` r

# Enrollment by care level: count and distinct cases
db_query(conn, "
  SELECT care_level, COUNT(*) AS n_records,
         COUNT(DISTINCT case_id) AS n_cases
  FROM enrolled
  GROUP BY care_level
  ORDER BY n_records DESC
", verbose = FALSE)
#> # A tibble: 4 × 3
#>   care_level n_records n_cases
#>   <chr>          <dbl>   <dbl>
#> 1 School Age        46      46
#> 2 Toddler           15      15
#> 3 PreSchool         15      15
#> 4 Infant             4       4
```

``` r

# Column metadata for the programs table
db_table_info(conn, "programs")
#> # A tibble: 51 × 3
#>    column_name       column_type description
#>    <chr>             <chr>       <chr>      
#>  1 facility_id       VARCHAR     NA         
#>  2 facility_name     VARCHAR     NA         
#>  3 facility_type_raw VARCHAR     NA         
#>  4 facility_type     VARCHAR     NA         
#>  5 facility_tier     VARCHAR     NA         
#>  6 county            VARCHAR     NA         
#>  7 region            VARCHAR     NA         
#>  8 facility_address  VARCHAR     NA         
#>  9 facility_address2 VARCHAR     NA         
#> 10 provider_id       VARCHAR     NA         
#> # ℹ 41 more rows
```

``` r

db_close(conn)
#> ℹ Database connection closed
unlink(db_path)
```

## Export

Export linked datasets to multiple formats for use in Stata, Excel, or
other tools:

``` r

# CSV
linkage_programs_enrolled_export_csv(linked_pe, "output/linkage/programs_enrolled.csv")

# Excel
linkage_programs_enrolled_export_excel(linked_pe, "output/linkage/programs_enrolled.xlsx")

# Stata
linkage_programs_enrolled_export_stata(linked_pe, "output/linkage/programs_enrolled.dta")

# RDS (preserves S3 classes and factor levels)
linkage_programs_enrolled_export_rds(linked_pe, "output/linkage/programs_enrolled.rds")

# All formats at once
linkage_programs_enrolled_export_all(linked_pe, "output/linkage/", "programs_enrolled")
```
