# ALccdfDB (development version)

# ALccdfDB 1.0.0 (2026-02-15)

## New features

### Program module
* Read, clean, validate, export for 4 facility types (Centers, Family/Group Homes, Faith-Based Exempt, License Excepted).
* Auto-detection of Excel format versions with data-driven column mapping (11 CSV mapping files).
* Added `program_append_types()` to unify all facility types with proper factor levels (Center, Family Home, Group Home, Faith-Based, Excepted).
* Added `program_deduplicate()` for facility_id-based deduplication.
* Quality tier recoding: "None" -> NA, numeric -> ordered factor "Star 1" through "Star 5".
* 3 quality-adjusted capacity schemes: linear (0.2-1.0), binary (0.0/1.0), exponential (0.1-1.0).
* 28 derived analysis variables (capacity flags, age group indicators, operating hours, quality weights).
* Added `program_summary_stats()` for comprehensive descriptive statistics.
* Added `program_validate()` with 10 quality checks.
* 5 export formats: CSV, Excel, Stata (.dta), Parquet, RDS.

### Subsidy module
* Added `subsidy_read()` and `subsidy_clean()` for enrolled children and client household records.
* PII separation: sensitive fields (names, SSNs) stored in `$pii`, never in `$data`.
* Random ID generation linking `$data` to `$pii`.
* Added `subsidy_validate()`, `subsidy_summary_stats()`.
* 5 export formats (PII excluded by default).

### Staff module
* Added `staff_read()` and `staff_clean()` for Alabama Pathways workforce registry.
* Career lattice level recoding (5 ordered levels).
* PII separation for staff names and emails.
* Added `staff_validate()`, `staff_summary_stats()`.
* 5 export formats (PII excluded by default).

### Melissa geocoding module
* Added `melissa_import_programs()` and `melissa_import_households()` for Melissa.com output.
* Added `melissa_merge_programs()` and `melissa_merge_households()` for address-based geocoding join.
* Address normalization pipeline for matching.
* Added `melissa_validate()` for geocoding quality checks.
* Provides latitude, longitude, census tract, FIPS code, address standardization.

### Linkage module
* Added `linkage_programs_enrolled()` -- join by `facility_id`, aggregate enrollment counts and care levels.
* Added `linkage_programs_clients()` -- join by `provider_id`, aggregate client counts and copay.
* Added `linkage_programs_staff()` -- join by `facility_name` (case-insensitive), aggregate staff counts and career levels.
* Added `linkage_clients_programs()` -- reverse join, append program attributes to client rows.
* Added `linkage_report()` for cross-module match diagnostics.
* Each linkage type has its own 5 export formats (20 export functions total).

### Database module (DuckDB)
* Added `db_init()` and `db_close()` for creating/opening DuckDB databases with schema versioning.
* Added `db_write_snapshot()` for persisting processed data with timestamp tracking.
* Added `db_read_snapshot()` and `db_read_latest()` for retrieving stored data.
* Added `db_list_tables()`, `db_list_snapshots()`, `db_table_info()`, `db_query()` for inspection and SQL queries.
* DuckDB and DBI are optional dependencies (Suggests).

### Synthetic data generators
* Added `alccdf_synthetic_programs()`, `alccdf_synthetic_subsidy()`, `alccdf_synthetic_staff()`, `alccdf_synthetic_melissa()` for generating realistic datasets without confidential DHR files.
* All generators share a common facility pool via same seed, enabling cross-module linkage.
* Uses `withr::with_seed()` for reproducibility.

### Vignettes
* Added "Getting Started with ALccdfDB" vignette -- overview, synthetic data, facility types, quality tiers.
* Added "Complete Data Processing Pipeline" vignette -- all 3 modules with 6 figures and quality weight comparison.
* Added "Cross-Module Linkage and Database Integration" vignette -- 4 linkage types, Melissa geocoding, DuckDB/SQL.

### Utilities
* S3 object system: `$data`, `$meta`, `$diagnostics` structure with `alccdf_data()` and `alccdf_meta()` accessors.
* Codebook functions: `alccdf_facility_type_levels()`, `alccdf_facility_tier_levels()`.
* Generic export functions: `export_csv()`, `export_excel()`, `export_stata()`, `export_parquet()`, `export_rds()`, `export_all()`.
* Data-driven column mappings in `inst/extdata/` (11 mapping CSVs + 4 codebook CSVs).

## Package stats
* 95 exported functions, 34 R source files, ~9,765 lines of code.
* 1,088 test expectations across 8 test files.
* 3 vignettes with 12 inline ggplot2 figures.
* `R CMD check`: 0 errors, 0 warnings, 0 notes.
