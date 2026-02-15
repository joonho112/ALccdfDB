# Changelog

## ALccdfDB (development version)

## ALccdfDB 1.0.0 (2026-02-15)

### New features

#### Program module

- Read, clean, validate, export for 4 facility types (Centers,
  Family/Group Homes, Faith-Based Exempt, License Excepted).
- Auto-detection of Excel format versions with data-driven column
  mapping (11 CSV mapping files).
- Added
  [`program_append_types()`](https://joonho112.github.io/ALccdfDB/reference/program_append_types.md)
  to unify all facility types with proper factor levels (Center, Family
  Home, Group Home, Faith-Based, Excepted).
- Added
  [`program_deduplicate()`](https://joonho112.github.io/ALccdfDB/reference/program_deduplicate.md)
  for facility_id-based deduplication.
- Quality tier recoding: “None” -\> NA, numeric -\> ordered factor “Star
  1” through “Star 5”.
- 3 quality-adjusted capacity schemes: linear (0.2-1.0), binary
  (0.0/1.0), exponential (0.1-1.0).
- 28 derived analysis variables (capacity flags, age group indicators,
  operating hours, quality weights).
- Added
  [`program_summary_stats()`](https://joonho112.github.io/ALccdfDB/reference/program_summary_stats.md)
  for comprehensive descriptive statistics.
- Added
  [`program_validate()`](https://joonho112.github.io/ALccdfDB/reference/program_validate.md)
  with 10 quality checks.
- 5 export formats: CSV, Excel, Stata (.dta), Parquet, RDS.

#### Subsidy module

- Added
  [`subsidy_read()`](https://joonho112.github.io/ALccdfDB/reference/subsidy_read.md)
  and
  [`subsidy_clean()`](https://joonho112.github.io/ALccdfDB/reference/subsidy_clean.md)
  for enrolled children and client household records.
- PII separation: sensitive fields (names, SSNs) stored in `$pii`, never
  in `$data`.
- Random ID generation linking `$data` to `$pii`.
- Added
  [`subsidy_validate()`](https://joonho112.github.io/ALccdfDB/reference/subsidy_validate.md),
  [`subsidy_summary_stats()`](https://joonho112.github.io/ALccdfDB/reference/subsidy_summary_stats.md).
- 5 export formats (PII excluded by default).

#### Staff module

- Added
  [`staff_read()`](https://joonho112.github.io/ALccdfDB/reference/staff_read.md)
  and
  [`staff_clean()`](https://joonho112.github.io/ALccdfDB/reference/staff_clean.md)
  for Alabama Pathways workforce registry.
- Career lattice level recoding (5 ordered levels).
- PII separation for staff names and emails.
- Added
  [`staff_validate()`](https://joonho112.github.io/ALccdfDB/reference/staff_validate.md),
  [`staff_summary_stats()`](https://joonho112.github.io/ALccdfDB/reference/staff_summary_stats.md).
- 5 export formats (PII excluded by default).

#### Melissa geocoding module

- Added
  [`melissa_import_programs()`](https://joonho112.github.io/ALccdfDB/reference/melissa_import_programs.md)
  and
  [`melissa_import_households()`](https://joonho112.github.io/ALccdfDB/reference/melissa_import_households.md)
  for Melissa.com output.
- Added
  [`melissa_merge_programs()`](https://joonho112.github.io/ALccdfDB/reference/melissa_merge_programs.md)
  and
  [`melissa_merge_households()`](https://joonho112.github.io/ALccdfDB/reference/melissa_merge_households.md)
  for address-based geocoding join.
- Address normalization pipeline for matching.
- Added
  [`melissa_validate()`](https://joonho112.github.io/ALccdfDB/reference/melissa_validate.md)
  for geocoding quality checks.
- Provides latitude, longitude, census tract, FIPS code, address
  standardization.

#### Linkage module

- Added
  [`linkage_programs_enrolled()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_enrolled.md)
  – join by `facility_id`, aggregate enrollment counts and care levels.
- Added
  [`linkage_programs_clients()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_clients.md)
  – join by `provider_id`, aggregate client counts and copay.
- Added
  [`linkage_programs_staff()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_staff.md)
  – join by `facility_name` (case-insensitive), aggregate staff counts
  and career levels.
- Added
  [`linkage_clients_programs()`](https://joonho112.github.io/ALccdfDB/reference/linkage_clients_programs.md)
  – reverse join, append program attributes to client rows.
- Added
  [`linkage_report()`](https://joonho112.github.io/ALccdfDB/reference/linkage_report.md)
  for cross-module match diagnostics.
- Each linkage type has its own 5 export formats (20 export functions
  total).

#### Database module (DuckDB)

- Added
  [`db_init()`](https://joonho112.github.io/ALccdfDB/reference/db_init.md)
  and
  [`db_close()`](https://joonho112.github.io/ALccdfDB/reference/db_close.md)
  for creating/opening DuckDB databases with schema versioning.
- Added
  [`db_write_snapshot()`](https://joonho112.github.io/ALccdfDB/reference/db_write_snapshot.md)
  for persisting processed data with timestamp tracking.
- Added
  [`db_read_snapshot()`](https://joonho112.github.io/ALccdfDB/reference/db_read_snapshot.md)
  and
  [`db_read_latest()`](https://joonho112.github.io/ALccdfDB/reference/db_read_latest.md)
  for retrieving stored data.
- Added
  [`db_list_tables()`](https://joonho112.github.io/ALccdfDB/reference/db_list_tables.md),
  [`db_list_snapshots()`](https://joonho112.github.io/ALccdfDB/reference/db_list_snapshots.md),
  [`db_table_info()`](https://joonho112.github.io/ALccdfDB/reference/db_table_info.md),
  [`db_query()`](https://joonho112.github.io/ALccdfDB/reference/db_query.md)
  for inspection and SQL queries.
- DuckDB and DBI are optional dependencies (Suggests).

#### Synthetic data generators

- Added
  [`alccdf_synthetic_programs()`](https://joonho112.github.io/ALccdfDB/reference/synthetic-data.md),
  [`alccdf_synthetic_subsidy()`](https://joonho112.github.io/ALccdfDB/reference/synthetic-data.md),
  [`alccdf_synthetic_staff()`](https://joonho112.github.io/ALccdfDB/reference/synthetic-data.md),
  [`alccdf_synthetic_melissa()`](https://joonho112.github.io/ALccdfDB/reference/synthetic-data.md)
  for generating realistic datasets without confidential DHR files.
- All generators share a common facility pool via same seed, enabling
  cross-module linkage.
- Uses
  [`withr::with_seed()`](https://withr.r-lib.org/reference/with_seed.html)
  for reproducibility.

#### Vignettes

- Added “Getting Started with ALccdfDB” vignette – overview, synthetic
  data, facility types, quality tiers.
- Added “Complete Data Processing Pipeline” vignette – all 3 modules
  with 6 figures and quality weight comparison.
- Added “Cross-Module Linkage and Database Integration” vignette – 4
  linkage types, Melissa geocoding, DuckDB/SQL.

#### Utilities

- S3 object system: `$data`, `$meta`, `$diagnostics` structure with
  [`alccdf_data()`](https://joonho112.github.io/ALccdfDB/reference/alccdf_data.md)
  and
  [`alccdf_meta()`](https://joonho112.github.io/ALccdfDB/reference/alccdf_meta.md)
  accessors.
- Codebook functions:
  [`alccdf_facility_type_levels()`](https://joonho112.github.io/ALccdfDB/reference/alccdf_facility_type_levels.md),
  [`alccdf_facility_tier_levels()`](https://joonho112.github.io/ALccdfDB/reference/alccdf_facility_tier_levels.md).
- Generic export functions:
  [`export_csv()`](https://joonho112.github.io/ALccdfDB/reference/export_csv.md),
  [`export_excel()`](https://joonho112.github.io/ALccdfDB/reference/export_excel.md),
  [`export_stata()`](https://joonho112.github.io/ALccdfDB/reference/export_stata.md),
  [`export_parquet()`](https://joonho112.github.io/ALccdfDB/reference/export_parquet.md),
  [`export_rds()`](https://joonho112.github.io/ALccdfDB/reference/export_rds.md),
  [`export_all()`](https://joonho112.github.io/ALccdfDB/reference/export_all.md).
- Data-driven column mappings in `inst/extdata/` (11 mapping CSVs + 4
  codebook CSVs).

### Package stats

- 95 exported functions, 34 R source files, ~9,765 lines of code.
- 1,088 test expectations across 8 test files.
- 3 vignettes with 12 inline ggplot2 figures.
- `R CMD check`: 0 errors, 0 warnings, 0 notes.
