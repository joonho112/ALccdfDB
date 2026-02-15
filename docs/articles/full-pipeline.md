# Complete Data Processing Pipeline

## Overview

This vignette demonstrates the end-to-end ALccdfDB workflow across all
three data modules: **programs**, **subsidy**, and **staff**. Starting
from synthetic data generation, we walk through data inspection,
visualization, validation, summary statistics, and export. Every
function call shown here mirrors the exact steps you would use with real
Alabama CCDF administrative data files.

## Setup

``` r

library(ALccdfDB)
library(dplyr)
library(tidyr)
library(ggplot2)
```

## Generate Synthetic Data

ALccdfDB provides synthetic data generators that produce realistic
datasets matching the structure and column types of real administrative
data. Using a shared seed ensures facility identities are consistent
across modules, enabling cross-module linkage.

``` r

programs <- alccdf_synthetic_programs(n = 50, seed = 42)
subsidy  <- alccdf_synthetic_subsidy(n_enrolled = 80, n_clients = 40, seed = 42)
staff    <- alccdf_synthetic_staff(n = 60, seed = 42)
```

------------------------------------------------------------------------

## Program Module

The program module is the backbone of the CCDF data system. It contains
facility-level information including type, tier rating, capacity,
operating hours, age ranges served, and quality-adjusted capacity
measures.

### Data Structure

The program object is an S3 object with `$data`, `$meta`, and
`$diagnostics` components. Use
[`alccdf_data()`](https://joonho112.github.io/ALccdfDB/reference/alccdf_data.md)
to extract the underlying tibble.

``` r

programs
#> 
#> ── ALccdfDB Program Data (Clean) ──
#> 
#> ℹ Type: synthetic
#> ℹ Snapshot: 2026-02-15
#> ℹ Dimensions: 50 rows x 51 cols
#> ℹ Raw rows: 50
#> ℹ Columns: facility_id, facility_name, facility_type_raw, facility_type, facility_tier, county, region, facility_address, facility_address2, provider_id, license_number, day_start, day_end, night_start, night_end, day_capacity, night_capacity, has_day_capacity, has_night_capacity, total_capacity, night_to_day_ratio, day_age_range, day_age_min, day_age_max, day_age_range_num, day_age_infants_toddlers, day_age_preschool, day_age_school_aged, night_age_range, night_age_min, night_age_max, day_operating_hours, night_operating_hours, operation_type, expiration_date, current_date, days_till_expire, facility_tier_original, was_originally_rated, is_high_quality, tier_weight_linear, tier_weight_binary, tier_weight_exp, capacity_qa_linear, capacity_qa_binary, capacity_qa_exp, capacity_flag, snapshot_date, phone_number, director_name, consultant
```

``` r

str(alccdf_data(programs)[, 1:15], give.attr = FALSE)
#> tibble [50 × 15] (S3: tbl_df/tbl/data.frame)
#>  $ facility_id      : chr [1:50] "FX001" "FX002" "FC001" "FG001" ...
#>  $ facility_name    : chr [1:50] "JSU Child Learning Center" "Gadsden State Child Center" "Jubilee Child Care Center" "Lakeside Group Home" ...
#>  $ facility_type_raw: chr [1:50] "Excepted (University/Other)" "Excepted (University/Other)" "Center" "Group Home" ...
#>  $ facility_type    : Factor w/ 5 levels "Center","Family Home",..: 5 5 1 3 4 4 2 1 2 2 ...
#>  $ facility_tier    : Ord.factor w/ 5 levels "Star 1"<"Star 2"<..: NA NA 3 4 NA NA 4 1 2 3 ...
#>  $ county           : Factor w/ 23 levels "Calhoun","Coffee",..: 20 17 10 1 5 6 19 17 17 10 ...
#>  $ region           : Factor w/ 7 levels "Region 1","Region 2",..: 3 7 1 6 6 7 6 7 3 6 ...
#>  $ facility_address : chr [1:50] "4991 McFarland Boulevard, Prattville, AL 36067" "6873 McFarland Boulevard, Decatur, AL 35601" "3208 Highland Avenue, Birmingham, AL 35203" "9453 Quintard Avenue, Anniston, AL 36201" ...
#>  $ facility_address2: chr [1:50] "4991 MCFARLAND BLVD, PRATTVILLE, AL 36067" "6873 MCFARLAND BLVD, DECATUR, AL 35601" "3208 HIGHLAND AVE, BIRMINGHAM, AL 35203" "9453 QUINTARD AVE, ANNISTON, AL 36201" ...
#>  $ provider_id      : chr [1:50] "P0001" "P0002" "P0003" "P0004" ...
#>  $ license_number   : chr [1:50] "LIC-41746-9094" "LIC-93842-4518" "LIC-65011-2674" "LIC-52079-6986" ...
#>  $ day_start        : chr [1:50] "06:00" "07:00" "07:00" "07:30" ...
#>  $ day_end          : chr [1:50] "17:30" "19:00" "18:30" "18:30" ...
#>  $ night_start      : chr [1:50] NA NA NA NA ...
#>  $ night_end        : chr [1:50] NA NA NA NA ...
```

### Facility Type Distribution

Alabama child care programs fall into five categories: Centers (the most
common), Family Homes, Group Homes, Faith-Based (exempt), and Excepted
(university/other) programs.

``` r

prog_df <- alccdf_data(programs)

type_counts <- prog_df %>%
  count(facility_type) %>%
  arrange(n)

ggplot(type_counts, aes(x = reorder(facility_type, n), y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title = "Number of Facilities by Type",
    x = NULL,
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
```

![Figure 1: Distribution of facility
types](full-pipeline_files/figure-html/fig-1-facility-types-1.png)

Figure 1: Distribution of facility types

### Day Capacity by Facility Type

Capacity varies substantially by facility type. Centers tend to have the
largest capacities, while Family Homes are capped at much smaller
numbers.

``` r

ggplot(prog_df, aes(x = facility_type, y = day_capacity)) +
  geom_boxplot(fill = "lightblue", outlier.colour = "red", outlier.size = 2) +
  labs(
    title = "Day Capacity by Facility Type",
    x = "Facility Type",
    y = "Day Capacity"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(face = "bold")
  )
```

![Figure 2: Day capacity distribution by facility
type](full-pipeline_files/figure-html/fig-2-capacity-boxplot-1.png)

Figure 2: Day capacity distribution by facility type

### Tier Distribution by Facility Type

The Alabama Quality STARS rating system assigns tiers (Star 1 through
Star 5) to eligible programs. Faith-Based and Excepted programs are not
rated, so we exclude NA tiers for this view.

``` r

tier_df <- prog_df %>%
  filter(!is.na(facility_tier))

ggplot(tier_df, aes(x = facility_type, fill = facility_tier)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "YlOrRd", name = "Tier") +
  labs(
    title = "Tier Proportions by Facility Type",
    x = "Facility Type",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(face = "bold")
  )
```

![Figure 3: Tier proportions by facility
type](full-pipeline_files/figure-html/fig-3-tier-proportions-1.png)

Figure 3: Tier proportions by facility type

### Quality Weight Comparison by Tier

ALccdfDB computes three quality-adjusted capacity measures using
different weighting schemes: linear (proportional to tier), binary
(high-quality vs. not), and exponential (accelerating reward for higher
tiers).

``` r

quality_table <- prog_df %>%
  filter(!is.na(facility_tier)) %>%
  group_by(facility_tier) %>%
  summarise(
    n = n(),
    mean_capacity   = round(mean(day_capacity, na.rm = TRUE), 1),
    mean_qa_linear  = round(mean(capacity_qa_linear, na.rm = TRUE), 1),
    mean_qa_binary  = round(mean(capacity_qa_binary, na.rm = TRUE), 1),
    .groups = "drop"
  )

knitr::kable(
  quality_table,
  caption = "Quality-adjusted capacity by tier",
  col.names = c("Tier", "N", "Mean Capacity", "QA Linear", "QA Binary")
)
```

| Tier   |   N | Mean Capacity | QA Linear | QA Binary |
|:-------|----:|--------------:|----------:|----------:|
| Star 1 |   2 |          99.0 |      19.8 |       0.0 |
| Star 2 |   4 |          51.5 |      20.6 |       0.0 |
| Star 3 |   7 |          85.4 |      51.3 |      85.4 |
| Star 4 |   8 |          68.5 |      54.8 |      68.5 |
| Star 5 |   6 |          51.3 |      51.3 |      51.3 |

Quality-adjusted capacity by tier {.table}

### Age Group Coverage

Each program serves one or more age groups. The three binary indicators
(`day_age_infants_toddlers`, `day_age_preschool`, `day_age_school_aged`)
identify which populations each facility covers.

``` r

age_long <- prog_df %>%
  select(facility_id, day_age_infants_toddlers, day_age_preschool,
         day_age_school_aged) %>%
  pivot_longer(
    cols = c(day_age_infants_toddlers, day_age_preschool, day_age_school_aged),
    names_to = "age_group",
    values_to = "serves"
  ) %>%
  mutate(
    age_group = case_when(
      age_group == "day_age_infants_toddlers" ~ "Infants & Toddlers",
      age_group == "day_age_preschool"        ~ "Preschool",
      age_group == "day_age_school_aged"      ~ "School Aged"
    )
  )

age_summary <- age_long %>%
  group_by(age_group) %>%
  summarise(n_serving = sum(serves, na.rm = TRUE), .groups = "drop")

ggplot(age_summary, aes(x = reorder(age_group, n_serving), y = n_serving)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = n_serving), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title = "Number of Facilities Serving Each Age Group",
    x = NULL,
    y = "Number of Facilities"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
```

![Figure 4: Age group coverage across
facilities](full-pipeline_files/figure-html/fig-4-age-coverage-1.png)

Figure 4: Age group coverage across facilities

------------------------------------------------------------------------

## Subsidy Module

The subsidy module handles two related datasets: **enrolled children**
(placement-level records linking children to providers) and **clients**
(family-level records with funding and copayment details). Both use PII
separation to keep sensitive fields isolated from analytic data.

### Enrolled Children

``` r

enrolled <- subsidy$enrolled
enrolled
#> 
#> ── ALccdfDB Subsidy Data (Clean) ──
#> 
#> ℹ Type: enrolled
#> ℹ Snapshot: 2026-02-15
#> ℹ Dimensions: 80 rows x 17 cols
#> ℹ Raw rows: 80
#> ℹ PII records: 80
#> ℹ Main columns: parent_id, child_id, case_id, child_age, care_level, eligibility_category, eligibility_begin_date, eligibility_end_date, placement_start_date, placement_end_date, facility_id, facility_name, provider_id, county, region, unit_of_care, snapshot_date
#> ℹ PII columns: parent_id, child_id, parent_name, child_name, parent_ssn, child_ssn
```

``` r

head(alccdf_data(enrolled))
#> # A tibble: 6 × 17
#>   parent_id child_id case_id child_age care_level eligibility_category
#>   <chr>     <chr>    <chr>       <dbl> <chr>      <chr>               
#> 1 P696670   C241532  C001          6.3 School Age Category A          
#> 2 P185518   C884977  C002          6.9 School Age Category A          
#> 3 P643674   C471663  C003          2.1 Toddler    Category A          
#> 4 P736083   C217926  C004          0.9 Infant     Category B          
#> 5 P315327   C531532  C005          9.6 School Age Category A          
#> 6 P671551   C493130  C006          8.9 School Age Category C          
#> # ℹ 11 more variables: eligibility_begin_date <date>,
#> #   eligibility_end_date <date>, placement_start_date <date>,
#> #   placement_end_date <date>, facility_id <chr>, facility_name <chr>,
#> #   provider_id <chr>, county <chr>, region <chr>, unit_of_care <chr>,
#> #   snapshot_date <date>
```

### PII Separation

A key design principle in ALccdfDB is that personally identifiable
information (names, SSNs) is stored in a separate `$pii` table, never in
the main `$data` tibble. This allows analysts to work with the data
without inadvertent exposure of sensitive fields.

``` r

cat("Data columns:", ncol(alccdf_data(enrolled)), "\n")
#> Data columns: 17
cat("PII columns: ", ncol(enrolled$pii), "\n")
#> PII columns:  6
cat("PII field names:", paste(names(enrolled$pii), collapse = ", "), "\n")
#> PII field names: parent_id, child_id, parent_name, child_name, parent_ssn, child_ssn
```

### Care Level Distribution

The `care_level` field classifies children into four developmental
categories based on age: Infant, Toddler, PreSchool, and School Age.

``` r

enrolled_df <- alccdf_data(enrolled)

care_counts <- enrolled_df %>%
  count(care_level) %>%
  arrange(desc(n))

ggplot(care_counts, aes(x = reorder(care_level, n), y = n)) +
  geom_col(fill = "mediumpurple") +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title = "Enrolled Children by Care Level",
    x = NULL,
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
```

![Figure 5: Care level distribution among enrolled
children](full-pipeline_files/figure-html/fig-5-care-level-1.png)

Figure 5: Care level distribution among enrolled children

------------------------------------------------------------------------

## Staff Module

The staff module tracks child care workforce data including position,
career lattice level, provider affiliation, and residential location.
Like the subsidy module, it separates PII (staff names, email addresses)
from analytic fields.

### Staff Data Overview

``` r

staff
#> 
#> ── ALccdfDB Staff Data (Clean) ──
#> 
#> ℹ Snapshot: 2026-02-15
#> ℹ Main data: 60 rows x 10 cols
#> ℹ PII table: 60 rows x 3 cols
#> ℹ Raw rows: 60
#> ℹ PII fields separated:
#> ℹ Career levels: Level 1: 12, Level 2: 15, Level 3: 15, Level 4: 12, Level 5: 6
#> ℹ Main columns: random_staff_id, user_zip, user_county, facility_name, facility_county, provider_type, currently_operating, position, career_lattice_level, snapshot_date
#> ℹ PII columns: random_staff_id, staff_name, user_email
```

``` r

head(alccdf_data(staff))
#> # A tibble: 6 × 10
#>   random_staff_id user_zip user_county facility_name             facility_county
#>   <chr>           <chr>    <chr>       <chr>                     <chr>          
#> 1 STF445867       36604    Russell     Bright Future Childcare   Jefferson      
#> 2 STF613086       36560    Jefferson   Huntingdon College Presc… Colbert        
#> 3 STF741524       36111    Morgan      Jubilee Child Care Center Jefferson      
#> 4 STF230771       35967    Dekalb      Sunrise Preschool         Mobile         
#> 5 STF305283       35805    Calhoun     Northside Group Home      Talladega      
#> 6 STF741791       35758    Russell     Emmanuel Lutheran Presch… Elmore         
#> # ℹ 5 more variables: provider_type <chr>, currently_operating <lgl>,
#> #   position <chr>, career_lattice_level <chr>, snapshot_date <date>
```

### Career Lattice Level Distribution

Alabama’s career lattice system classifies child care professionals into
five levels reflecting education, training, and experience.

``` r

staff_df <- alccdf_data(staff)

career_counts <- staff_df %>%
  count(career_lattice_level) %>%
  arrange(career_lattice_level)

ggplot(career_counts, aes(x = career_lattice_level, y = n)) +
  geom_col(fill = "seagreen") +
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  labs(
    title = "Staff by Career Lattice Level",
    x = "Career Lattice Level",
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
```

![Figure 6: Staff distribution by career lattice
level](full-pipeline_files/figure-html/fig-6-career-level-1.png)

Figure 6: Staff distribution by career lattice level

### Position Distribution

``` r

position_table <- staff_df %>%
  count(position) %>%
  arrange(desc(n)) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

knitr::kable(
  position_table,
  caption = "Staff distribution by position",
  col.names = c("Position", "N", "Percent")
)
```

| Position     |   N | Percent |
|:-------------|----:|--------:|
| Assistant    |  22 |    36.7 |
| Teacher      |  18 |    30.0 |
| Lead Teacher |   8 |    13.3 |
| Aide         |   6 |    10.0 |
| Director     |   6 |    10.0 |

Staff distribution by position {.table}

------------------------------------------------------------------------

## Validation Framework

ALccdfDB includes validation functions for each module that run a
battery of data quality checks. Each validator returns a structured
result object reporting PASS, WARN, ERROR, and INFO statuses across
multiple checks.

``` r

prog_validation <- program_validate(programs, verbose = FALSE)
prog_validation
#> 
#> ── ALccdfDB Validation Report: program ──
#> 
#> ℹ Status: ✔ PASSED
#> ℹ Snapshot: 2026-02-15
#> ℹ Checks: 10 total
#> ! Warnings: 1
#> ℹ Info: 1
#> ✔ [PASS] All required columns present
#> ✔ [PASS] No duplicate facility IDs
#> ⚠ [WARN] Non-positive capacity values found (48 issues)
#> ✔ [PASS] County: 23 unique values (factor)
#> ✔ [PASS] Expiration dates within expected range
#> ✔ [PASS] Operating hours within 0-24 range
#> ℹ [INFO] Age range columns not present (skipped)
#> ✔ [PASS] All facility addresses present
#> ✔ [PASS] Facility tiers valid (Star 1-5 or NA)
#> ✔ [PASS] Snapshot date present on all rows
```

Subsidy and staff modules have their own validators with checks tailored
to their data structures.

``` r

sub_validation   <- subsidy_validate(enrolled, verbose = FALSE)
staff_validation <- staff_validate(staff, verbose = FALSE)

sub_validation
#> 
#> ── ALccdfDB Validation Report: subsidy ──
#> 
#> ℹ Status: ✔ PASSED
#> ℹ Snapshot: 2026-02-15
#> ℹ Checks: 7 total
#> ✖ Errors: 1
#> ℹ Info: 2
#> ✘ [ERROR] Missing required columns (3 issues)
#> ℹ [INFO] No address columns present (skipped)
#> ℹ [INFO] Copayment check not applicable for enrolled data
#> ✔ [PASS] All child ages in reasonable range (0-18)
#> ✔ [PASS] All records have provider_id
#> ✔ [PASS] No duplicate records on [case_id, facility_id]
#> ✔ [PASS] PII fields properly separated from main data
staff_validation
#> 
#> ── ALccdfDB Validation Report: staff ──
#> 
#> ℹ Status: ✔ PASSED
#> ℹ Snapshot: 2026-02-15
#> ℹ Checks: 4 total
#> ! Warnings: 1
#> ✔ [PASS] All required columns present
#> ✔ [PASS] No duplicate name + facility combinations
#> ⚠ [WARN] Unexpected career lattice levels found (60 issues)
#> ✔ [PASS] PII fields (staff_name, user_email) not in main data
```

------------------------------------------------------------------------

## Export

ALccdfDB supports five output formats for each module: CSV, Excel, Stata
(.dta), Parquet, and RDS. Subsidy and staff exports default to
PII-excluded mode for safety.

``` r

# Program exports
program_export_csv(programs, "output/programs.csv")
program_export_excel(programs, "output/programs.xlsx")
program_export_stata(programs, "output/programs.dta")
program_export_parquet(programs, "output/programs.parquet")
program_export_rds(programs, "output/programs.rds")

# Subsidy exports (PII excluded by default)
subsidy_export_csv(enrolled, "output/enrolled.csv")
subsidy_export_excel(enrolled, "output/enrolled.xlsx")

# Staff exports (PII excluded by default)
staff_export_csv(staff, "output/staff.csv")
staff_export_excel(staff, "output/staff.xlsx")

# Export all formats at once
program_export_all(programs, dir = "output", basename = "programs_2025")
```

------------------------------------------------------------------------

## Real Data Workflow

When working with actual DHR administrative files, the workflow starts
with configuration and reads from Excel source files. The pipeline
follows the sequence: **configure –\> read –\> clean –\> append –\>
validate –\> export**.

``` r

# 1. Configure the pipeline with file paths and snapshot date
cfg <- program_config(
  snapshot_date = "2025-06-11",
  center_path   = "data-raw/Centers_06_11_25.xlsx",
  home_path     = "data-raw/FamilyGroupHomes_06_11_25.xlsx",
  exempt_path   = "data-raw/ExemptCenters_06_11_25.xlsx",
  excepted_path = "data-raw/ExceptedPrograms_06_11_25.xlsx",
  output_dir    = "output/2025-06-11"
)

# 2. Read all program types from Excel
raw_list <- program_read_all(cfg)

# 3. Clean all program types
clean_list <- program_clean_all(raw_list)

# 4. Append into a unified dataset
unified <- program_append_types(clean_list)

# 5. Validate
validation <- program_validate(unified)

# 6. Summary statistics
stats <- program_summary_stats(unified)
stats$overview
stats$by_type

# 7. Export to all formats
program_export_all(
  unified,
  dir      = "output/2025-06-11",
  basename = "all_programs_2025-06-11"
)
```
