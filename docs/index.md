# ALccdfDB

**Alabama CCDF Administrative Data Cleaning and Management** (v1.0.0)

A modular R toolkit for processing, cleaning, validating, linking, and
managing Alabama CCDF (Child Care and Development Fund) administrative
records from the Alabama Department of Human Resources (DHR). Transforms
raw Excel files covering licensed childcare facilities, subsidy
enrollment, client households, staff registries, and geocoding results
into analysis-ready linked datasets with DuckDB persistent storage.

- **95 exported functions** + 23 S3 methods
- **34 R source files**, ~9,765 lines of code
- **1,088 tests** across 8 test files
- **3 vignettes** with 12 inline figures
- R CMD check: 0 errors, 0 warnings, 0 notes

## Author

JoonHo Lee, Ph.D. Assistant Professor, The University of Alabama
<jlee296@ua.edu>

## Modules

| Module | Purpose | Key Output |
|----|----|----|
| Program | Licensed childcare facility data (Center, Family Home, Group Home, Faith-Based, Excepted); cleaning, tier recoding, derived variables | Unified program tibble with factor tiers |
| Subsidy | Enrolled children and client household records; PII separation | Clean enrolled + client tibbles |
| Staff | Alabama Pathways staff registry; career lattice levels | Standardized staff tibble |
| Melissa | Melissa.com geocoding integration (lat/lng, census tract, FIPS) | Geocoded program and household data |
| Linkage | Cross-module joins (programs x enrolled, clients, staff; clients x programs) | Linked analysis-ready tibbles |
| Database | DuckDB persistent storage with snapshot management | Versioned `.duckdb` database |
| Utils | Shared helpers, export functions (CSV, Excel, Stata, Parquet, RDS) | Multi-format file output |

## Data Sources

DHR Excel files ingested by the package:

- Licensed Centers
- Family / Group Homes
- Faith-Based Exempt facilities
- License Excepted programs
- Subsidy Enrollment records
- Active Clients with Addresses
- Alabama Pathways staff registry
- Melissa.com geocoding results

**5 facility types:** Center, Family Home, Group Home, Faith-Based,
Excepted **5 quality tiers:** Star 1 through Star 5 (ordered factor) **3
quality weight schemes:** linear (0.2–1.0), binary (0.0/1.0),
exponential (0.1–1.0)

## Installation

``` r

# Install from GitHub
# install.packages("pak")
pak::pak("joonho112/ALccdfDB")
```

## Quick Start

### Synthetic Data

Every module ships with a synthetic-data generator so documentation and
demos run without access to restricted DHR files.

``` r

library(ALccdfDB)

# ---- Generate synthetic datasets ----
programs <- alccdf_synthetic_programs(n = 50, seed = 42)
subsidy  <- alccdf_synthetic_subsidy(n_enrolled = 80, n_clients = 40, seed = 42)
staff    <- alccdf_synthetic_staff(n = 60, seed = 42)
melissa  <- alccdf_synthetic_melissa(programs = programs, seed = 42)

# ---- Inspect ----
programs
subsidy$enrolled
subsidy$clients
staff
melissa
```

### Real Data Pipeline

``` r

library(ALccdfDB)

# 1 - Configure paths and snapshot date
cfg <- program_config(
  snapshot_date = "2025-06-11",
  center_path   = "data/Centers_06_11_25.xlsx",
  home_path     = "data/FamilyGroupHomes_06_11_25.xlsx",
  exempt_path   = "data/FaithBasedExempt_06_11_25.xlsx",
  excepted_path = "data/LicenseExcepted_06_11_25.xlsx"
)

# 2 - Read, clean, validate each facility type
raw_center  <- program_read(cfg$paths$center, type = "center",
                            snapshot_date = cfg$snapshot_date)
clean_center <- program_clean(raw_center)
# ... repeat for home, exempt, excepted ...

# 3 - Append all types and deduplicate
unified  <- program_append_types(clean_center, clean_home,
                                  clean_exempt, clean_excepted)
programs <- program_deduplicate(unified)

# 4 - Subsidy pipeline (enrolled + clients)
enrolled_raw <- subsidy_read("data/enrollment.xlsx", type = "enrolled",
                              snapshot_date = "2025-06-11")
enrolled     <- subsidy_clean(enrolled_raw)

# 5 - Staff pipeline
staff_raw <- staff_read("data/pathways.csv", snapshot_date = "2025-06-11")
stf       <- staff_clean(staff_raw)

# 6 - Melissa geocoding merge
melissa_prg <- melissa_import_programs("data/melissa_programs.xlsx")
programs    <- melissa_merge_programs(programs, melissa_prg)

# 7 - Cross-module linkage
linked_pe <- linkage_programs_enrolled(programs, enrolled)
linked_ps <- linkage_programs_staff(programs, stf)

# 8 - Export all formats
program_export_all(programs, dir = "output/", basename = "programs_2025")
```

### DuckDB Storage

``` r

library(ALccdfDB)

# Initialize database
conn <- db_init("alccdf.duckdb")

# Write snapshots
db_write_snapshot(conn, programs, "programs")
db_write_snapshot(conn, enrolled, "enrolled")

# Inspect tables
db_list_tables(conn)
db_table_info(conn, "programs")

# SQL queries
db_query(conn, "
  SELECT county, COUNT(*) AS n_programs,
         ROUND(AVG(day_capacity), 0) AS avg_capacity
  FROM programs
  GROUP BY county
  ORDER BY n_programs DESC
  LIMIT 10
")

# Read back
snap <- db_read_snapshot(conn, "programs")
db_close(conn)
```

## Pipeline Architecture

    DHR Excel Files
         |
         v
    *_read()
         |
         v
    *_clean()
         |
         v
    *_validate()
         |
         +--[ Program ]---> program_append_types()
         |                         |
         |                         v
         |                  program_deduplicate()
         |                         |
         +--[ Subsidy ]-----------+|
         |                         ||
         +--[ Staff ]-------------+||
                                   |||
                                   vvv
                         melissa_merge_*()
                                   |
                                   v
                           linkage_*()
                            /           \
                           v             v
                  db_write_snapshot()   *_export_*()
                           |             |
                           v             v
                     .duckdb file    CSV / Excel /
                                     Stata / Parquet / RDS

## Key Features

- **Data-driven column mapping** – handles heterogeneous Excel report
  formats across facility types without hard-coded column positions.
- **Five facility types** – Center, Family Home, Group Home,
  Faith-Based, and Excepted facilities processed through a single
  unified pipeline.
- **Quality tier recoding** – Star 1 through Star 5 as an ordered factor
  with three weighting schemes (linear, binary, exponential).
- **PII separation** – Subsidy module cleanly splits personally
  identifiable information from analytic variables.
- **Melissa.com geocoding** – imports and merges lat/lng, census tract,
  and FIPS codes for programs and households.
- **Cross-module linkage** – purpose-built joins by `facility_id`,
  `provider_id`, and `facility_name` (case-insensitive) with match-rate
  diagnostics.
- **DuckDB persistent storage** – snapshot-based versioning lets you
  store, query, and retrieve point-in-time datasets.
- **Multi-format export** – one-call export to CSV, Excel, Stata `.dta`,
  Parquet, and RDS.
- **Synthetic data generators** – every vignette and demo runs without
  restricted data via `alccdf_synthetic_*()` functions.
- **23 custom S3 print methods** – informative, color-coded console
  summaries for every major object type.
- **Comprehensive test suite** – 1,088 expectations across 8 test files;
  R CMD check passes cleanly.

## Vignettes

``` r

vignette("getting-started", package = "ALccdfDB")
vignette("full-pipeline",   package = "ALccdfDB")
vignette("data-linkage",    package = "ALccdfDB")
```

## License

MIT
