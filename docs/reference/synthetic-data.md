# Generate Synthetic ALccdfDB Data for Demonstrations

These functions create realistic synthetic datasets that mirror the
structure and types of real Alabama CCDF administrative data. They are
useful for demonstrations, vignettes, and testing without requiring
access to confidential data files.

All four generators share facility identities when called with the same
`seed`, enabling cross-module linkage (programs \<-\> enrolled \<-\>
clients, programs \<-\> staff, programs \<-\> melissa).

Creates a synthetic `alccdf_program_clean` object with realistic
facility characteristics, capacity, operating hours, tier ratings, and
derived quality-adjusted capacity fields.

Creates synthetic enrolled-children and clients subsidy data with PII
separation, realistic dates, and cross-module facility linkage.

Creates a synthetic `alccdf_staff_clean` object with staff professional
data, career lattice levels, and PII separation. Uses facility names as
the linkage key back to programs.

Creates synthetic Melissa.com geocoding results for both program
addresses and household (client) addresses. Returns plain tibbles (not
S3 objects) matching the structure produced by
[`melissa_import_programs`](https://joonho112.github.io/ALccdfDB/reference/melissa_import_programs.md)
and
[`melissa_import_households`](https://joonho112.github.io/ALccdfDB/reference/melissa_import_households.md).

## Usage

``` r
alccdf_synthetic_programs(n = 50, seed = 42)

alccdf_synthetic_subsidy(n_enrolled = 80, n_clients = 40, seed = 42)

alccdf_synthetic_staff(n = 60, seed = 42)

alccdf_synthetic_melissa(programs = NULL, seed = 42)
```

## Arguments

- n:

  Integer. Number of staff records. Default 60.

- seed:

  Integer. Random seed for reproducibility. Default 42.

- n_enrolled:

  Integer. Number of enrolled-children records. Default 80.

- n_clients:

  Integer. Number of client records. Default 40.

- programs:

  An optional `alccdf_program_clean` object. If `NULL` (default), uses
  `alccdf_synthetic_programs(seed = seed)` to generate addresses.

## Value

An `alccdf_program_clean` S3 object.

A named list with `$enrolled` and `$clients`, each an
`alccdf_subsidy_clean` S3 object with `$data`, `$pii`, and `$meta`
components.

An `alccdf_staff_clean` S3 object with `$data`, `$pii`, and `$meta`
components.

A named list with:

- programs:

  A tibble with geocoded program addresses

- households:

  A tibble with geocoded household addresses

## Examples

``` r
prog <- alccdf_synthetic_programs(n = 30, seed = 123)
prog
#> 
#> ── ALccdfDB Program Data (Clean) ──
#> 
#> ℹ Type: synthetic
#> ℹ Snapshot: 2026-02-15
#> ℹ Dimensions: 30 rows x 51 cols
#> ℹ Raw rows: 30
#> ℹ Columns: facility_id, facility_name, facility_type_raw, facility_type, facility_tier, county, region, facility_address, facility_address2, provider_id, license_number, day_start, day_end, night_start, night_end, day_capacity, night_capacity, has_day_capacity, has_night_capacity, total_capacity, night_to_day_ratio, day_age_range, day_age_min, day_age_max, day_age_range_num, day_age_infants_toddlers, day_age_preschool, day_age_school_aged, night_age_range, night_age_min, night_age_max, day_operating_hours, night_operating_hours, operation_type, expiration_date, current_date, days_till_expire, facility_tier_original, was_originally_rated, is_high_quality, tier_weight_linear, tier_weight_binary, tier_weight_exp, capacity_qa_linear, capacity_qa_binary, capacity_qa_exp, capacity_flag, snapshot_date, phone_number, director_name, consultant
head(prog$data)
#> # A tibble: 6 × 51
#>   facility_id facility_name facility_type_raw facility_type facility_tier county
#>   <chr>       <chr>         <chr>             <fct>         <ord>         <fct> 
#> 1 FC001       Creative Kid… Center            Center        Star 2        Colbe…
#> 2 FF001       Thompson Fam… Family Home       Family Home   Star 4        Colbe…
#> 3 FC002       Tender Care … Center            Center        Star 2        Russe…
#> 4 FG001       Oak Grove Gr… Group Home        Group Home    Star 5        Shelby
#> 5 FX001       ASU Early Le… Excepted (Univer… Excepted (Un… NA            Elmore
#> 6 FC003       Growing Mind… Center            Center        Star 4        Calho…
#> # ℹ 45 more variables: region <fct>, facility_address <chr>,
#> #   facility_address2 <chr>, provider_id <chr>, license_number <chr>,
#> #   day_start <chr>, day_end <chr>, night_start <chr>, night_end <chr>,
#> #   day_capacity <int>, night_capacity <int>, has_day_capacity <int>,
#> #   has_night_capacity <int>, total_capacity <int>, night_to_day_ratio <dbl>,
#> #   day_age_range <chr>, day_age_min <dbl>, day_age_max <dbl>,
#> #   day_age_range_num <dbl>, day_age_infants_toddlers <int>, …

sub <- alccdf_synthetic_subsidy(n_enrolled = 40, n_clients = 20, seed = 99)
sub$enrolled
#> 
#> ── ALccdfDB Subsidy Data (Clean) ──
#> 
#> ℹ Type: enrolled
#> ℹ Snapshot: 2026-02-15
#> ℹ Dimensions: 40 rows x 17 cols
#> ℹ Raw rows: 40
#> ℹ PII records: 40
#> ℹ Main columns: parent_id, child_id, case_id, child_age, care_level, eligibility_category, eligibility_begin_date, eligibility_end_date, placement_start_date, placement_end_date, facility_id, facility_name, provider_id, county, region, unit_of_care, snapshot_date
#> ℹ PII columns: parent_id, child_id, parent_name, child_name, parent_ssn, child_ssn
head(sub$enrolled$data)
#> # A tibble: 6 × 17
#>   parent_id child_id case_id child_age care_level eligibility_category
#>   <chr>     <chr>    <chr>       <dbl> <chr>      <chr>               
#> 1 P968338   C716818  C001         10   School Age Category A          
#> 2 P165618   C956665  C002          5.9 School Age Category A          
#> 3 P134300   C952086  C003          5.2 School Age Category C          
#> 4 P626625   C428265  C004          8.1 School Age Category C          
#> 5 P780832   C887151  C005          6.3 School Age Category E          
#> 6 P994810   C852913  C006          6.2 School Age Category D          
#> # ℹ 11 more variables: eligibility_begin_date <date>,
#> #   eligibility_end_date <date>, placement_start_date <date>,
#> #   placement_end_date <date>, facility_id <chr>, facility_name <chr>,
#> #   provider_id <chr>, county <chr>, region <chr>, unit_of_care <chr>,
#> #   snapshot_date <date>
head(sub$enrolled$pii)
#> # A tibble: 6 × 6
#>   parent_id child_id parent_name       child_name       parent_ssn  child_ssn  
#>   <chr>     <chr>    <chr>             <chr>            <chr>       <chr>      
#> 1 P968338   C716818  Barbara Smith     Evelyn Davis     825-15-7323 743-71-6120
#> 2 P165618   C956665  Thomas Rodriguez  Mason Thomas     528-30-2749 503-49-7733
#> 3 P134300   C952086  David Thomas      Olivia Taylor    604-24-3619 692-60-4717
#> 4 P626625   C428265  Linda Martinez    Lucas Lopez      748-33-1582 651-87-5943
#> 5 P780832   C887151  Linda Brown       Lucas Jones      691-82-3407 490-41-5374
#> 6 P994810   C852913  Patricia Martinez Isabella Johnson 478-77-5436 856-39-6381

staff <- alccdf_synthetic_staff(n = 30, seed = 99)
staff
#> 
#> ── ALccdfDB Staff Data (Clean) ──
#> 
#> ℹ Snapshot: 2026-02-15
#> ℹ Main data: 30 rows x 10 cols
#> ℹ PII table: 30 rows x 3 cols
#> ℹ Raw rows: 30
#> ℹ PII fields separated: 
#> ℹ Career levels: Level 1: 7, Level 2: 10, Level 3: 7, Level 4: 5, Level 5: 1
#> ℹ Main columns: random_staff_id, user_zip, user_county, facility_name, facility_county, provider_type, currently_operating, position, career_lattice_level, snapshot_date
#> ℹ PII columns: random_staff_id, staff_name, user_email
head(staff$data)
#> # A tibble: 6 × 10
#>   random_staff_id user_zip user_county facility_name           facility_county
#>   <chr>           <chr>    <chr>       <chr>                   <chr>          
#> 1 STF840825       36507    Calhoun     Good Shepherd Academy   Lauderdale     
#> 2 STF677327       35824    Houston     Brown's Little Ones     Baldwin        
#> 3 STF494028       35613    Colbert     Restoration Church Care Morgan         
#> 4 STF567143       35645    Mobile      Kidz Zone Learning      Mobile         
#> 5 STF187738       35749    Russell     Smart Start Academy     Shelby         
#> 6 STF429045       36869    Russell     Tiny Angels Day Care    Jefferson      
#> # ℹ 5 more variables: provider_type <chr>, currently_operating <lgl>,
#> #   position <chr>, career_lattice_level <chr>, snapshot_date <date>

mel <- alccdf_synthetic_melissa(seed = 42)
head(mel$programs)
#> # A tibble: 6 × 15
#>   facility_address        latitude longitude census_tract census_block fips_code
#>   <chr>                      <dbl>     <dbl>        <dbl>        <dbl>     <dbl>
#> 1 4991 MCFARLAND BOULEVA…     31.7     -85.7       816614         9727      1097
#> 2 6873 MCFARLAND BOULEVA…     30.3     -86.4       574227         2495      1069
#> 3 3208 HIGHLAND AVENUE, …     31.9     -85.4       767203         4334      1103
#> 4 9453 QUINTARD AVENUE, …     31.9     -86.3       691108         2268      1081
#> 5 2920 HIGHLAND AVENUE, …     32.1     -85.5       859017         1865      1073
#> 6 4273 MEMORIAL PARKWAY,…     32.6     -87.9       621691         4348      1003
#> # ℹ 9 more variables: melissa_county <chr>, melissa_place <chr>,
#> #   melissa_place_code <int>, melissa_city <chr>, melissa_state <chr>,
#> #   melissa_zip <chr>, melissa_geo_zip <chr>, melissa_result_code <chr>,
#> #   melissa_status_code <chr>
head(mel$households)
#> # A tibble: 6 × 15
#>   facility_address        latitude longitude census_tract census_block fips_code
#>   <chr>                      <dbl>     <dbl>        <dbl>        <dbl>     <dbl>
#> 1 2338 MAPLE DRIVE, DECA…     33.4     -85.3       664610         9482      1069
#> 2 6210 IRIS CIRCLE, AUBU…     33.3     -88.2        57339         8061      1103
#> 3 7110 WALNUT AVENUE, GA…     35.0     -88.1       142152         4041      1089
#> 4 5047 MAGNOLIA PLACE, M…     33.9     -85.8       216421         9788      1003
#> 5 8396 PECAN DRIVE, DOTH…     30.9     -85.6       565360         3224      1003
#> 6 916 HICKORY LANE, FLOR…     30.8     -88.3       179799         9206      1125
#> # ℹ 9 more variables: melissa_county <chr>, melissa_place <chr>,
#> #   melissa_place_code <int>, melissa_city <chr>, melissa_state <chr>,
#> #   melissa_zip <chr>, melissa_geo_zip <chr>, melissa_result_code <chr>,
#> #   melissa_status_code <chr>
```
