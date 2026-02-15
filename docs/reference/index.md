# Package index

## Program Module

Licensed childcare facility data processing

- [`program_config()`](https://joonho112.github.io/ALccdfDB/reference/program_config.md)
  : Configure a program data processing pipeline
- [`program_read()`](https://joonho112.github.io/ALccdfDB/reference/program_read.md)
  : Read a single program Excel file
- [`program_read_all()`](https://joonho112.github.io/ALccdfDB/reference/program_read_all.md)
  : Read all program files from a configuration
- [`program_clean()`](https://joonho112.github.io/ALccdfDB/reference/program_clean.md)
  : Clean raw program data
- [`program_clean_all()`](https://joonho112.github.io/ALccdfDB/reference/program_clean_all.md)
  : Clean all raw program objects from a list
- [`program_validate()`](https://joonho112.github.io/ALccdfDB/reference/program_validate.md)
  : Validate cleaned program data
- [`program_append_types()`](https://joonho112.github.io/ALccdfDB/reference/program_append_types.md)
  : Append multiple program types into a unified data set
- [`program_deduplicate()`](https://joonho112.github.io/ALccdfDB/reference/program_deduplicate.md)
  : Deduplicate unified program data
- [`program_summary_stats()`](https://joonho112.github.io/ALccdfDB/reference/program_summary_stats.md)
  : Generate program summary statistics
- [`program_export_csv()`](https://joonho112.github.io/ALccdfDB/reference/program_export_csv.md)
  : Export program data to CSV
- [`program_export_excel()`](https://joonho112.github.io/ALccdfDB/reference/program_export_excel.md)
  : Export program data to Excel
- [`program_export_stata()`](https://joonho112.github.io/ALccdfDB/reference/program_export_stata.md)
  : Export program data to Stata (.dta)
- [`program_export_parquet()`](https://joonho112.github.io/ALccdfDB/reference/program_export_parquet.md)
  : Export program data to Parquet
- [`program_export_rds()`](https://joonho112.github.io/ALccdfDB/reference/program_export_rds.md)
  : Export program data to RDS
- [`program_export_all()`](https://joonho112.github.io/ALccdfDB/reference/program_export_all.md)
  : Export program data to all supported formats

## Subsidy Module

Enrolled children and client household records

- [`subsidy_read()`](https://joonho112.github.io/ALccdfDB/reference/subsidy_read.md)
  : Read a single subsidy Excel file
- [`subsidy_clean()`](https://joonho112.github.io/ALccdfDB/reference/subsidy_clean.md)
  : Clean raw subsidy data
- [`subsidy_validate()`](https://joonho112.github.io/ALccdfDB/reference/subsidy_validate.md)
  : Validate cleaned subsidy data
- [`subsidy_summary_stats()`](https://joonho112.github.io/ALccdfDB/reference/subsidy_summary_stats.md)
  : Generate subsidy summary statistics
- [`subsidy_export_csv()`](https://joonho112.github.io/ALccdfDB/reference/subsidy_export_csv.md)
  : Export subsidy data to CSV
- [`subsidy_export_excel()`](https://joonho112.github.io/ALccdfDB/reference/subsidy_export_excel.md)
  : Export subsidy data to Excel
- [`subsidy_export_stata()`](https://joonho112.github.io/ALccdfDB/reference/subsidy_export_stata.md)
  : Export subsidy data to Stata (.dta)
- [`subsidy_export_parquet()`](https://joonho112.github.io/ALccdfDB/reference/subsidy_export_parquet.md)
  : Export subsidy data to Parquet
- [`subsidy_export_rds()`](https://joonho112.github.io/ALccdfDB/reference/subsidy_export_rds.md)
  : Export subsidy data to RDS
- [`subsidy_export_all()`](https://joonho112.github.io/ALccdfDB/reference/subsidy_export_all.md)
  : Export subsidy data to all supported formats

## Staff Module

Alabama Pathways staff registry

- [`staff_read()`](https://joonho112.github.io/ALccdfDB/reference/staff_read.md)
  : Read a staff/professional-level CSV file
- [`staff_clean()`](https://joonho112.github.io/ALccdfDB/reference/staff_clean.md)
  : Clean raw staff data
- [`staff_validate()`](https://joonho112.github.io/ALccdfDB/reference/staff_validate.md)
  : Validate cleaned staff data
- [`staff_summary_stats()`](https://joonho112.github.io/ALccdfDB/reference/staff_summary_stats.md)
  : Generate staff summary statistics
- [`staff_export_csv()`](https://joonho112.github.io/ALccdfDB/reference/staff_export_csv.md)
  : Export staff data to CSV
- [`staff_export_excel()`](https://joonho112.github.io/ALccdfDB/reference/staff_export_excel.md)
  : Export staff data to Excel
- [`staff_export_stata()`](https://joonho112.github.io/ALccdfDB/reference/staff_export_stata.md)
  : Export staff data to Stata (.dta)
- [`staff_export_parquet()`](https://joonho112.github.io/ALccdfDB/reference/staff_export_parquet.md)
  : Export staff data to Parquet
- [`staff_export_rds()`](https://joonho112.github.io/ALccdfDB/reference/staff_export_rds.md)
  : Export staff data to RDS
- [`staff_export_all()`](https://joonho112.github.io/ALccdfDB/reference/staff_export_all.md)
  : Export staff data to all supported formats

## Melissa Geocoding Module

Melissa.com geocoding integration

- [`melissa_import_programs()`](https://joonho112.github.io/ALccdfDB/reference/melissa_import_programs.md)
  : Import Melissa geocoding results for programs
- [`melissa_import_households()`](https://joonho112.github.io/ALccdfDB/reference/melissa_import_households.md)
  : Import Melissa geocoding results for subsidy households
- [`melissa_merge_programs()`](https://joonho112.github.io/ALccdfDB/reference/melissa_merge_programs.md)
  : Merge Melissa coordinates into program data
- [`melissa_merge_households()`](https://joonho112.github.io/ALccdfDB/reference/melissa_merge_households.md)
  : Merge Melissa coordinates into subsidy client household data
- [`melissa_validate()`](https://joonho112.github.io/ALccdfDB/reference/melissa_validate.md)
  : Validate Melissa geocoding results

## Linkage Module

Cross-module joins and diagnostics

- [`linkage_programs_enrolled()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_enrolled.md)
  : Link programs to subsidy enrolled children
- [`linkage_programs_clients()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_clients.md)
  : Link programs to subsidy clients
- [`linkage_programs_staff()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_staff.md)
  : Link programs to staff data
- [`linkage_clients_programs()`](https://joonho112.github.io/ALccdfDB/reference/linkage_clients_programs.md)
  : Link subsidy clients to program data
- [`linkage_report()`](https://joonho112.github.io/ALccdfDB/reference/linkage_report.md)
  : Generate linkage report
- [`linkage_programs_enrolled_export_all()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_enrolled_export_all.md)
  : Export linked programs-enrolled data to all supported formats
- [`linkage_programs_enrolled_export_csv()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_enrolled_export_csv.md)
  : Export linked programs-enrolled data to CSV
- [`linkage_programs_enrolled_export_excel()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_enrolled_export_excel.md)
  : Export linked programs-enrolled data to Excel
- [`linkage_programs_enrolled_export_parquet()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_enrolled_export_parquet.md)
  : Export linked programs-enrolled data to Parquet
- [`linkage_programs_enrolled_export_rds()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_enrolled_export_rds.md)
  : Export linked programs-enrolled data to RDS
- [`linkage_programs_enrolled_export_stata()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_enrolled_export_stata.md)
  : Export linked programs-enrolled data to Stata (.dta)
- [`linkage_programs_clients_export_all()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_clients_export_all.md)
  : Export linked programs-clients data to all supported formats
- [`linkage_programs_clients_export_csv()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_clients_export_csv.md)
  : Export linked programs-clients data to CSV
- [`linkage_programs_clients_export_excel()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_clients_export_excel.md)
  : Export linked programs-clients data to Excel
- [`linkage_programs_clients_export_parquet()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_clients_export_parquet.md)
  : Export linked programs-clients data to Parquet
- [`linkage_programs_clients_export_rds()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_clients_export_rds.md)
  : Export linked programs-clients data to RDS
- [`linkage_programs_clients_export_stata()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_clients_export_stata.md)
  : Export linked programs-clients data to Stata (.dta)
- [`linkage_programs_staff_export_all()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_staff_export_all.md)
  : Export linked programs-staff data to all supported formats
- [`linkage_programs_staff_export_csv()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_staff_export_csv.md)
  : Export linked programs-staff data to CSV
- [`linkage_programs_staff_export_excel()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_staff_export_excel.md)
  : Export linked programs-staff data to Excel
- [`linkage_programs_staff_export_parquet()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_staff_export_parquet.md)
  : Export linked programs-staff data to Parquet
- [`linkage_programs_staff_export_rds()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_staff_export_rds.md)
  : Export linked programs-staff data to RDS
- [`linkage_programs_staff_export_stata()`](https://joonho112.github.io/ALccdfDB/reference/linkage_programs_staff_export_stata.md)
  : Export linked programs-staff data to Stata (.dta)
- [`linkage_clients_programs_export_all()`](https://joonho112.github.io/ALccdfDB/reference/linkage_clients_programs_export_all.md)
  : Export linked clients-programs data to all supported formats
- [`linkage_clients_programs_export_csv()`](https://joonho112.github.io/ALccdfDB/reference/linkage_clients_programs_export_csv.md)
  : Export linked clients-programs data to CSV
- [`linkage_clients_programs_export_excel()`](https://joonho112.github.io/ALccdfDB/reference/linkage_clients_programs_export_excel.md)
  : Export linked clients-programs data to Excel
- [`linkage_clients_programs_export_parquet()`](https://joonho112.github.io/ALccdfDB/reference/linkage_clients_programs_export_parquet.md)
  : Export linked clients-programs data to Parquet
- [`linkage_clients_programs_export_rds()`](https://joonho112.github.io/ALccdfDB/reference/linkage_clients_programs_export_rds.md)
  : Export linked clients-programs data to RDS
- [`linkage_clients_programs_export_stata()`](https://joonho112.github.io/ALccdfDB/reference/linkage_clients_programs_export_stata.md)
  : Export linked clients-programs data to Stata (.dta)

## Database Module

DuckDB persistent storage and SQL queries

- [`db_init()`](https://joonho112.github.io/ALccdfDB/reference/db_init.md)
  : Initialize DuckDB database
- [`db_close()`](https://joonho112.github.io/ALccdfDB/reference/db_close.md)
  : Close database connection
- [`db_write_snapshot()`](https://joonho112.github.io/ALccdfDB/reference/db_write_snapshot.md)
  : Write a data snapshot to DuckDB
- [`db_write_linked()`](https://joonho112.github.io/ALccdfDB/reference/db_write_linked.md)
  : Write linked dataset to DuckDB
- [`db_write_melissa()`](https://joonho112.github.io/ALccdfDB/reference/db_write_melissa.md)
  : Write Melissa geocoding data to DuckDB
- [`db_read_snapshot()`](https://joonho112.github.io/ALccdfDB/reference/db_read_snapshot.md)
  : Read a data snapshot from DuckDB
- [`db_read_latest()`](https://joonho112.github.io/ALccdfDB/reference/db_read_latest.md)
  : Read the latest snapshot from DuckDB
- [`db_read_melissa()`](https://joonho112.github.io/ALccdfDB/reference/db_read_melissa.md)
  : Read Melissa geocoding data from DuckDB
- [`db_list_tables()`](https://joonho112.github.io/ALccdfDB/reference/db_list_tables.md)
  : List all data tables in the database
- [`db_list_snapshots()`](https://joonho112.github.io/ALccdfDB/reference/db_list_snapshots.md)
  : List available snapshots for a table
- [`db_table_info()`](https://joonho112.github.io/ALccdfDB/reference/db_table_info.md)
  : Get column metadata for a database table
- [`db_query()`](https://joonho112.github.io/ALccdfDB/reference/db_query.md)
  : Execute arbitrary SQL query

## Synthetic Data

Generators for examples and vignettes

- [`alccdf_synthetic_programs()`](https://joonho112.github.io/ALccdfDB/reference/synthetic-data.md)
  [`alccdf_synthetic_subsidy()`](https://joonho112.github.io/ALccdfDB/reference/synthetic-data.md)
  [`alccdf_synthetic_staff()`](https://joonho112.github.io/ALccdfDB/reference/synthetic-data.md)
  [`alccdf_synthetic_melissa()`](https://joonho112.github.io/ALccdfDB/reference/synthetic-data.md)
  : Generate Synthetic ALccdfDB Data for Demonstrations

## Codebooks & Accessors

Factor levels, data extraction, and codebook system

- [`alccdf_data()`](https://joonho112.github.io/ALccdfDB/reference/alccdf_data.md)
  : Extract data from an ALccdfDB S3 object
- [`alccdf_meta()`](https://joonho112.github.io/ALccdfDB/reference/alccdf_meta.md)
  : Extract metadata from an ALccdfDB S3 object
- [`alccdf_facility_type_levels()`](https://joonho112.github.io/ALccdfDB/reference/alccdf_facility_type_levels.md)
  : Return the canonical program type factor levels (v3)
- [`alccdf_facility_tier_levels()`](https://joonho112.github.io/ALccdfDB/reference/alccdf_facility_tier_levels.md)
  : Return the canonical facility tier levels

## Generic Export Functions

Module-agnostic export helpers

- [`export_csv()`](https://joonho112.github.io/ALccdfDB/reference/export_csv.md)
  : Export data to CSV
- [`export_excel()`](https://joonho112.github.io/ALccdfDB/reference/export_excel.md)
  : Export data to Excel
- [`export_stata()`](https://joonho112.github.io/ALccdfDB/reference/export_stata.md)
  : Export data to Stata (.dta)
- [`export_parquet()`](https://joonho112.github.io/ALccdfDB/reference/export_parquet.md)
  : Export data to Parquet
- [`export_rds()`](https://joonho112.github.io/ALccdfDB/reference/export_rds.md)
  : Export data to RDS
- [`export_all()`](https://joonho112.github.io/ALccdfDB/reference/export_all.md)
  : Export data to multiple formats

## S3 Print Methods

Display methods for ALccdfDB objects

- [`print(`*`<alccdf_linked_clients_programs>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_linked_clients_programs.md)
  : Print method for linked clients-programs objects
- [`print(`*`<alccdf_linked_programs_clients>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_linked_programs_clients.md)
  : Print method for linked programs-clients objects
- [`print(`*`<alccdf_linked_programs_enrolled>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_linked_programs_enrolled.md)
  : Print method for linked programs-enrolled objects
- [`print(`*`<alccdf_linked_programs_staff>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_linked_programs_staff.md)
  : Print method for linked programs-staff objects
- [`print(`*`<alccdf_melissa_households>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_melissa_households.md)
  : Print method for Melissa household objects
- [`print(`*`<alccdf_melissa_programs>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_melissa_programs.md)
  : Print method for Melissa program objects
- [`print(`*`<alccdf_melissa_validation>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_melissa_validation.md)
  : Print method for Melissa validation objects
- [`print(`*`<alccdf_program_clean>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_program_clean.md)
  : Print method for clean program objects
- [`print(`*`<alccdf_program_config>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_program_config.md)
  : Print method for program configuration objects
- [`print(`*`<alccdf_program_deduped>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_program_deduped.md)
  : Print method for deduplicated program objects
- [`print(`*`<alccdf_program_raw>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_program_raw.md)
  : Print method for raw program objects
- [`print(`*`<alccdf_program_unified>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_program_unified.md)
  : Print method for unified program objects
- [`print(`*`<alccdf_program_validation>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_program_validation.md)
  : Print method for program validation objects
- [`print(`*`<alccdf_staff_clean>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_staff_clean.md)
  : Print method for clean staff objects
- [`print(`*`<alccdf_staff_raw>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_staff_raw.md)
  : Print method for raw staff objects
- [`print(`*`<alccdf_staff_validation>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_staff_validation.md)
  : Print method for staff validation objects
- [`print(`*`<alccdf_subsidy_clean>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_subsidy_clean.md)
  : Print method for clean subsidy objects
- [`print(`*`<alccdf_subsidy_raw>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_subsidy_raw.md)
  : Print method for raw subsidy objects
- [`print(`*`<alccdf_subsidy_validation>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_subsidy_validation.md)
  : Print method for subsidy validation objects
- [`print(`*`<alccdf_validation>`*`)`](https://joonho112.github.io/ALccdfDB/reference/print.alccdf_validation.md)
  : Print method for validation objects
