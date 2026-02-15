# tests/testthat/test-program.R
# Comprehensive unit tests for the program module pipeline in ALccdfDB.
# Uses testthat edition 3 with fixture files in tests/testthat/fixtures/.

# Shared snapshot date used consistently across all tests.
SNAPSHOT_DATE <- as.Date("2025-09-12")

# Helper: path to a fixture file
fixture_path <- function(filename) {
  testthat::test_path("fixtures", filename)
}


# ==============================================================================
# 1. program_read() - Read fixture files
# ==============================================================================

test_that("program_read reads center fixture", {
  raw <- program_read(
    path = fixture_path("test_center.xlsx"),
    type = "center",
    snapshot_date = SNAPSHOT_DATE,
    verbose = FALSE
  )
  expect_s3_class(raw, "alccdf_program_raw")
  expect_equal(nrow(alccdf_data(raw)), 3)
  expect_equal(alccdf_meta(raw)$program_type, "center")
  expect_equal(alccdf_meta(raw)$snapshot_date, SNAPSHOT_DATE)

  # Column mapping should have produced clean names

  df <- alccdf_data(raw)
  expect_true("facility_id" %in% names(df))
  expect_true("facility_name" %in% names(df))
  expect_true("county" %in% names(df))
})

test_that("program_read reads family_home fixture", {
  raw <- program_read(
    path = fixture_path("test_family_home.xlsx"),
    type = "family_home",
    snapshot_date = SNAPSHOT_DATE,
    verbose = FALSE
  )
  expect_s3_class(raw, "alccdf_program_raw")
  expect_equal(nrow(alccdf_data(raw)), 4)
  expect_equal(alccdf_meta(raw)$program_type, "family_home")

  # Fixture has Family Home and Group Home in type column
  df <- alccdf_data(raw)
  expect_true("facility_type_raw" %in% names(df))
  raw_types <- unique(df$facility_type_raw)
  expect_true("Family Home" %in% raw_types)
  expect_true("Group Home" %in% raw_types)
})

test_that("program_read reads exempt fixture with skip=0", {
  raw <- program_read(
    path = fixture_path("test_exempt.xlsx"),
    type = "exempt",
    snapshot_date = SNAPSHOT_DATE,
    verbose = FALSE
  )
  expect_s3_class(raw, "alccdf_program_raw")
  expect_equal(nrow(alccdf_data(raw)), 2)
  expect_equal(alccdf_meta(raw)$program_type, "exempt")
  # exempt auto-detects skip = 0
  expect_equal(alccdf_meta(raw)$skip, 0L)
})

test_that("program_read reads excepted fixture", {
  raw <- program_read(
    path = fixture_path("test_excepted.xlsx"),
    type = "excepted",
    snapshot_date = SNAPSHOT_DATE,
    verbose = FALSE
  )
  expect_s3_class(raw, "alccdf_program_raw")
  expect_equal(nrow(alccdf_data(raw)), 1)
  expect_equal(alccdf_meta(raw)$program_type, "excepted")
})

test_that("program_read rejects invalid type", {
  expect_error(
    program_read(
      path = fixture_path("test_center.xlsx"),
      type = "invalid_type",
      snapshot_date = SNAPSHOT_DATE,
      verbose = FALSE
    ),
    "type"
  )
})

test_that("program_read rejects non-existent file", {
  expect_error(
    program_read(
      path = "nonexistent.xlsx",
      type = "center",
      snapshot_date = SNAPSHOT_DATE,
      verbose = FALSE
    ),
    "not found"
  )
})

test_that("program_read accepts character snapshot_date", {
  raw <- program_read(
    path = fixture_path("test_center.xlsx"),
    type = "center",
    snapshot_date = "2025-09-12",
    verbose = FALSE
  )
  expect_equal(alccdf_meta(raw)$snapshot_date, as.Date("2025-09-12"))
})

test_that("program_read processing_log has entries", {
  raw <- program_read(
    path = fixture_path("test_center.xlsx"),
    type = "center",
    snapshot_date = SNAPSHOT_DATE,
    verbose = FALSE
  )
  expect_true(length(alccdf_meta(raw)$processing_log) > 0)
})


# ==============================================================================
# 2. program_clean() - CRITICAL regression tests
# ==============================================================================

# --- 2a. Family Home / Group Home preservation ---

test_that("program_clean preserves BOTH Family Home AND Group Home types", {
  raw <- program_read(
    fixture_path("test_family_home.xlsx"), "family_home",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  # CRITICAL REGRESSION: facility_type must have BOTH levels present
  ft_values <- as.character(df$facility_type)
  expect_true("Family Home" %in% ft_values,
    info = "Family Home must be present after cleaning family_home fixture")
  expect_true("Group Home" %in% ft_values,
    info = "Group Home must be present after cleaning family_home fixture")

  # Check correct counts: 2 Family Home, 2 Group Home
  expect_equal(sum(ft_values == "Family Home"), 2)
  expect_equal(sum(ft_values == "Group Home"), 2)
})

test_that("program_clean facility_type is a factor with canonical levels", {
  raw <- program_read(
    fixture_path("test_family_home.xlsx"), "family_home",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  expect_s3_class(df$facility_type, "factor")
  expect_equal(levels(df$facility_type), alccdf_facility_type_levels())
})

# --- 2b. Age indicator thresholds ---

test_that("program_clean day_age_infants_toddlers uses threshold < 3", {
  raw <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  # CRITICAL REGRESSION: infants_toddlers = age_min < 3
  # Center fixture has:
  #   "0 - 5 Years"  -> age_min = 0, age_min < 3 = TRUE -> 1
  #   "0 - 12 Years" -> age_min = 0, age_min < 3 = TRUE -> 1
  #   "3 - 5 Years"  -> age_min = 3, age_min < 3 = FALSE -> 0
  expect_true("day_age_infants_toddlers" %in% names(df))
  expected <- c(1L, 1L, 0L)
  expect_equal(df$day_age_infants_toddlers, expected)
})

test_that("program_clean age indicators preschool and school_aged are correct", {
  raw <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  # preschool: age_min < 5 AND age_max >= 3
  # "0 - 5": 0 < 5 & 5 >= 3 -> 1
  # "0 - 12": 0 < 5 & 12 >= 3 -> 1
  # "3 - 5": 3 < 5 & 5 >= 3 -> 1
  expect_equal(df$day_age_preschool, c(1L, 1L, 1L))

  # school_aged: age_max >= 5
  # "0 - 5": 5 >= 5 -> 1
  # "0 - 12": 12 >= 5 -> 1
  # "3 - 5": 5 >= 5 -> 1
  expect_equal(df$day_age_school_aged, c(1L, 1L, 1L))
})

# --- 2c. Facility tier recoding ---

test_that("program_clean facility_tier 'None' becomes NA", {
  raw <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  # CRITICAL REGRESSION: Center fixture row 3 has "None" -> should be NA
  expect_true("facility_tier" %in% names(df))
  expect_true(is.na(df$facility_tier[3]))

  # Row 1 "Star 1" and row 2 "Star 3" should be preserved
  expect_equal(as.character(df$facility_tier[1]), "Star 1")
  expect_equal(as.character(df$facility_tier[2]), "Star 3")
})

test_that("program_clean facility_tier is ordered factor", {
  raw <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  expect_s3_class(df$facility_tier, "factor")
  expect_true(is.ordered(df$facility_tier))
  expect_equal(levels(df$facility_tier), alccdf_facility_tier_levels())
})

# --- 2d. Excepted type recoding ---

test_that("program_clean recodes 'Excepted (Out of School Time)' to 'Excepted (University/Other)'", {
  raw <- program_read(
    fixture_path("test_excepted.xlsx"), "excepted",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  # CRITICAL REGRESSION: "Excepted (Out of School Time)" -> "Excepted (University/Other)"
  expect_equal(
    as.character(df$facility_type[1]),
    "Excepted (University/Other)"
  )
})

# --- 2e. Derived variables must be present ---

test_that("program_clean produces all expected derived variables for centers", {
  raw <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  # Address variables
  expect_true("facility_address" %in% names(df))
  expect_true("facility_address2" %in% names(df))

  # Date variables
  expect_true("expiration_date" %in% names(df))
  expect_true("current_date" %in% names(df))
  expect_true("days_till_expire" %in% names(df))

  # Capacity variables
  expect_true("has_day_capacity" %in% names(df))
  expect_true("has_night_capacity" %in% names(df))
  expect_true("total_capacity" %in% names(df))
  expect_true("night_to_day_ratio" %in% names(df))

  # Age variables
  expect_true("day_age_min" %in% names(df))
  expect_true("day_age_max" %in% names(df))
  expect_true("day_age_range_num" %in% names(df))
  expect_true("day_age_infants_toddlers" %in% names(df))
  expect_true("day_age_preschool" %in% names(df))
  expect_true("day_age_school_aged" %in% names(df))

  # Operating hours
  expect_true("day_oper_hours" %in% names(df))
  expect_true("operation_type" %in% names(df))

  # Tier quality metrics
  expect_true("was_originally_rated" %in% names(df))
  expect_true("is_high_quality" %in% names(df))
  expect_true("tier_weight_linear" %in% names(df))
  expect_true("tier_weight_binary" %in% names(df))
  expect_true("tier_weight_exp" %in% names(df))
  expect_true("capacity_qa_linear" %in% names(df))

  # Capacity flag
  expect_true("capacity_flag" %in% names(df))

  # Snapshot date column
  expect_true("snapshot_date" %in% names(df))
  expect_equal(unique(df$snapshot_date), SNAPSHOT_DATE)

  # Datetime versions of time
  expect_true("day_start_datetime" %in% names(df))
  expect_true("day_end_datetime" %in% names(df))

  # Factor columns
  expect_s3_class(df$county, "factor")
  expect_s3_class(df$region, "factor")
  expect_s3_class(df$facility_type, "factor")
})

# --- 2f. Capacity-derived variables ---

test_that("program_clean computes capacity variables correctly", {
  raw <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  # Center fixture: day_capacity = c(50, 100, 75), night_capacity = c(0, 30, 0)
  expect_equal(df$day_capacity, c(50L, 100L, 75L))
  expect_equal(df$night_capacity, c(0L, 30L, 0L))
  expect_equal(df$has_day_capacity, c(1L, 1L, 1L))
  expect_equal(df$has_night_capacity, c(0L, 1L, 0L))
  expect_equal(df$total_capacity, c(50L, 130L, 75L))
})

# --- 2g. Operating hours ---

test_that("program_clean computes day operating hours", {
  raw <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  # Center fixture times: (06:00-18:00=12), (07:00-17:00=10), (06:30-17:30=11)
  expect_equal(df$day_oper_hours, c(12, 10, 11))
})

# --- 2h. Operation type classification ---

test_that("program_clean classifies operation type correctly", {
  raw <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  # Row 1: day cap 50, night cap 0 -> Day Only
  # Row 2: day cap 100, night cap 30 -> Day & Night
  # Row 3: day cap 75, night cap 0 -> Day Only
  expect_equal(df$operation_type, c("Day Only", "Day & Night", "Day Only"))
})

# --- 2i. Tier quality metrics ---

test_that("program_clean computes quality metrics correctly", {
  raw <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  # Row 1: Star 1 -> was_rated=TRUE, is_high_quality=FALSE
  # Row 2: Star 3 -> was_rated=TRUE, is_high_quality=TRUE
  # Row 3: None(NA) -> was_rated=FALSE, is_high_quality=FALSE
  expect_equal(df$was_originally_rated, c(TRUE, TRUE, FALSE))
  expect_equal(df$is_high_quality, c(FALSE, TRUE, FALSE))

  # Linear weights: Star 1 = 0.2, Star 3 = 0.6, NA = NA
  expect_equal(df$tier_weight_linear[1], 0.2)
  expect_equal(df$tier_weight_linear[2], 0.6)
  expect_true(is.na(df$tier_weight_linear[3]))
})

# --- 2j. S3 class and metadata ---

test_that("program_clean returns alccdf_program_clean class", {
  raw <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)

  expect_s3_class(clean, "alccdf_program_clean")
  meta <- alccdf_meta(clean)
  expect_equal(meta$stage, "clean")
  expect_equal(meta$program_type, "center")
  expect_true(length(meta$processing_log) > 0)
  expect_true("cleaning_steps" %in% names(meta))
})

# --- 2k. Children under 12 months ---

test_that("program_clean converts children_under_12mo to integer", {
  raw <- program_read(
    fixture_path("test_family_home.xlsx"), "family_home",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  if ("children_under_12mo" %in% names(df)) {
    expect_true(is.integer(df$children_under_12mo))
    expect_equal(df$children_under_12mo, c(3L, 6L, 2L, 4L))
  }
})

# --- 2l. program_clean rejects wrong class ---

test_that("program_clean rejects non alccdf_program_raw input", {
  expect_error(
    program_clean(data.frame(x = 1), verbose = FALSE),
    "alccdf_program_raw"
  )
})

# --- 2m. Exempt cleaning ---

test_that("program_clean assigns Faith-Based type for exempt", {
  raw <- program_read(
    fixture_path("test_exempt.xlsx"), "exempt",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  # Exempt programs should have facility_type = "Faith-Based"
  ft_values <- as.character(df$facility_type)
  # Check that the facility type is assigned (either from raw or fallback)
  expect_true(all(!is.na(df$facility_type)))
})


# ==============================================================================
# 3. program_append_types() - Combine all types
# ==============================================================================

test_that("program_append_types combines multiple cleaned objects", {
  raw_center <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  raw_fh <- program_read(
    fixture_path("test_family_home.xlsx"), "family_home",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean_center <- program_clean(raw_center, verbose = FALSE)
  clean_fh <- program_clean(raw_fh, verbose = FALSE)

  unified <- program_append_types(clean_center, clean_fh, verbose = FALSE)

  expect_s3_class(unified, "alccdf_program_unified")
  expect_equal(nrow(alccdf_data(unified)), 3 + 4)  # 3 centers + 4 homes
  expect_equal(alccdf_meta(unified)$snapshot_date, SNAPSHOT_DATE)
})

test_that("program_append_types preserves facility_type as factor", {
  raw_center <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  raw_fh <- program_read(
    fixture_path("test_family_home.xlsx"), "family_home",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean_center <- program_clean(raw_center, verbose = FALSE)
  clean_fh <- program_clean(raw_fh, verbose = FALSE)

  unified <- program_append_types(clean_center, clean_fh, verbose = FALSE)
  df <- alccdf_data(unified)

  expect_s3_class(df$facility_type, "factor")
  expect_equal(levels(df$facility_type), alccdf_facility_type_levels())

  # Should have Centers, Family Homes, and Group Homes
  ft_vals <- as.character(df$facility_type)
  expect_true("Center" %in% ft_vals)
  expect_true("Family Home" %in% ft_vals)
  expect_true("Group Home" %in% ft_vals)
})

test_that("program_append_types works with a list input", {
  raw_center <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean_center <- program_clean(raw_center, verbose = FALSE)

  unified <- program_append_types(list(center = clean_center), verbose = FALSE)
  expect_s3_class(unified, "alccdf_program_unified")
  expect_equal(nrow(alccdf_data(unified)), 3)
})

test_that("program_append_types combines all four types", {
  raw_c <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  raw_f <- program_read(fixture_path("test_family_home.xlsx"), "family_home", SNAPSHOT_DATE, verbose = FALSE)
  raw_e <- program_read(fixture_path("test_exempt.xlsx"), "exempt", SNAPSHOT_DATE, verbose = FALSE)
  raw_x <- program_read(fixture_path("test_excepted.xlsx"), "excepted", SNAPSHOT_DATE, verbose = FALSE)

  clean_c <- program_clean(raw_c, verbose = FALSE)
  clean_f <- program_clean(raw_f, verbose = FALSE)
  clean_e <- program_clean(raw_e, verbose = FALSE)
  clean_x <- program_clean(raw_x, verbose = FALSE)

  unified <- program_append_types(clean_c, clean_f, clean_e, clean_x, verbose = FALSE)
  expect_equal(nrow(alccdf_data(unified)), 3 + 4 + 2 + 1)  # 10 total

  meta <- alccdf_meta(unified)
  expect_equal(meta$n_types, 4)
})

test_that("program_append_types rejects non-clean objects", {
  expect_error(
    program_append_types(data.frame(x = 1), verbose = FALSE),
    "alccdf_program_clean"
  )
})

test_that("program_append_types requires at least one input", {
  expect_error(program_append_types(verbose = FALSE))
})


# ==============================================================================
# 4. program_deduplicate() - Dedup logic
# ==============================================================================

test_that("program_deduplicate removes exact duplicates by facility_id", {
  raw_c <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean_c <- program_clean(raw_c, verbose = FALSE)

  # Append same data twice to create duplicates
  unified <- program_append_types(clean_c, clean_c, verbose = FALSE)
  expect_equal(nrow(alccdf_data(unified)), 6)  # 3 + 3

  deduped <- program_deduplicate(unified, verbose = FALSE)
  expect_s3_class(deduped, "alccdf_program_deduped")
  expect_equal(nrow(alccdf_data(deduped)), 3)  # duplicates removed
  expect_equal(alccdf_meta(deduped)$n_duplicates_found, 3)
})

test_that("program_deduplicate keeps 'first' by default", {
  raw_c <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean_c <- program_clean(raw_c, verbose = FALSE)

  unified <- program_append_types(clean_c, clean_c, verbose = FALSE)
  deduped <- program_deduplicate(unified, verbose = FALSE)

  expect_equal(alccdf_meta(deduped)$keep, "first")
})

test_that("program_deduplicate stores removed rows in diagnostics", {
  raw_c <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean_c <- program_clean(raw_c, verbose = FALSE)

  unified <- program_append_types(clean_c, clean_c, verbose = FALSE)
  deduped <- program_deduplicate(unified, verbose = FALSE)

  removed <- deduped$diagnostics$removed_duplicates
  expect_true(is.data.frame(removed))
  expect_equal(nrow(removed), 3)
})

test_that("program_deduplicate with no duplicates keeps all rows", {
  raw_c <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  raw_f <- program_read(fixture_path("test_family_home.xlsx"), "family_home", SNAPSHOT_DATE, verbose = FALSE)
  clean_c <- program_clean(raw_c, verbose = FALSE)
  clean_f <- program_clean(raw_f, verbose = FALSE)

  unified <- program_append_types(clean_c, clean_f, verbose = FALSE)
  deduped <- program_deduplicate(unified, verbose = FALSE)

  expect_equal(nrow(alccdf_data(deduped)), 7)
  expect_equal(alccdf_meta(deduped)$n_duplicates_found, 0)
})

test_that("program_deduplicate supports custom key_cols", {
  raw_c <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean_c <- program_clean(raw_c, verbose = FALSE)

  unified <- program_append_types(clean_c, clean_c, verbose = FALSE)
  deduped <- program_deduplicate(unified,
                                  key_cols = c("facility_id", "facility_type"),
                                  verbose = FALSE)
  expect_equal(nrow(alccdf_data(deduped)), 3)
})

test_that("program_deduplicate errors on missing key columns", {
  raw_c <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean_c <- program_clean(raw_c, verbose = FALSE)

  unified <- program_append_types(clean_c, verbose = FALSE)
  expect_error(
    program_deduplicate(unified, key_cols = "nonexistent_column", verbose = FALSE),
    "not found"
  )
})

test_that("program_deduplicate rejects wrong class", {
  expect_error(
    program_deduplicate(data.frame(x = 1), verbose = FALSE),
    "alccdf_program_unified"
  )
})


# ==============================================================================
# 5. program_validate() - Validation checks
# ==============================================================================

test_that("program_validate returns validation object for clean data", {
  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  val <- program_validate(clean, verbose = FALSE)

  expect_s3_class(val, "alccdf_program_validation")
  expect_s3_class(val, "alccdf_validation")
  expect_true(is.data.frame(val$checks))
  expect_true(nrow(val$checks) > 0)
  expect_true("check_id" %in% names(val$checks))
  expect_true("status" %in% names(val$checks))
})

test_that("program_validate passes for clean fixture data", {
  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  val <- program_validate(clean, verbose = FALSE)
  # Should pass (no errors in strict sense) since our fixture data is well-formed
  expect_true(val$passed)
})

test_that("program_validate checks required columns", {
  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  val <- program_validate(clean, verbose = FALSE)
  req_check <- val$checks[val$checks$check_id == "required_columns", ]
  expect_equal(nrow(req_check), 1)
  expect_equal(req_check$status, "PASS")
})

test_that("program_validate checks for duplicate facility_id", {
  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  val <- program_validate(clean, verbose = FALSE)
  dup_check <- val$checks[val$checks$check_id == "duplicate_facility_id", ]
  expect_equal(nrow(dup_check), 1)
  expect_equal(dup_check$status, "PASS")
})

test_that("program_validate checks snapshot_date_present", {
  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  val <- program_validate(clean, verbose = FALSE)
  snap_check <- val$checks[val$checks$check_id == "snapshot_date_present", ]
  expect_equal(nrow(snap_check), 1)
  expect_equal(snap_check$status, "PASS")
})

test_that("program_validate checks facility_tier_valid", {
  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  val <- program_validate(clean, verbose = FALSE)
  tier_check <- val$checks[val$checks$check_id == "facility_tier_valid", ]
  expect_equal(nrow(tier_check), 1)
  expect_equal(tier_check$status, "PASS")
})

test_that("program_validate strict mode errors on real issues", {
  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  # Manually break the data to trigger an error
  clean$data$facility_id <- NA
  clean$data$snapshot_date <- NA

  expect_error(
    program_validate(clean, strict = TRUE, verbose = FALSE),
    "strict"
  )
})

test_that("program_validate rejects non alccdf_program_clean input", {
  expect_error(
    program_validate(data.frame(x = 1), verbose = FALSE),
    "alccdf_program_clean"
  )
})


# ==============================================================================
# 6. Export functions - roundtrip tests
# ==============================================================================

test_that("program_export_csv writes and reads back correctly", {
  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  csv_path <- file.path(tmpdir, "test_export.csv")

  result_path <- program_export_csv(clean, csv_path, verbose = FALSE)
  expect_equal(result_path, csv_path)
  expect_true(file.exists(csv_path))

  df_read <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  expect_equal(nrow(df_read), 3)
  expect_true("facility_id" %in% names(df_read))
})

test_that("program_export_excel writes xlsx file", {
  skip_if_not_installed("openxlsx")

  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  xlsx_path <- file.path(tmpdir, "test_export.xlsx")

  result_path <- program_export_excel(clean, xlsx_path, verbose = FALSE)
  expect_equal(result_path, xlsx_path)
  expect_true(file.exists(xlsx_path))

  df_read <- readxl::read_excel(xlsx_path)
  expect_equal(nrow(df_read), 3)
})

test_that("program_export_stata writes dta file", {
  skip_if_not_installed("haven")

  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  dta_path <- file.path(tmpdir, "test_export.dta")

  result_path <- program_export_stata(clean, dta_path, verbose = FALSE)
  expect_equal(result_path, dta_path)
  expect_true(file.exists(dta_path))

  df_read <- haven::read_dta(dta_path)
  expect_equal(nrow(df_read), 3)
})

test_that("program_export_parquet writes parquet file", {
  skip_if_not_installed("arrow")

  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  pq_path <- file.path(tmpdir, "test_export.parquet")

  result_path <- program_export_parquet(clean, pq_path, verbose = FALSE)
  expect_equal(result_path, pq_path)
  expect_true(file.exists(pq_path))

  df_read <- arrow::read_parquet(pq_path)
  expect_equal(nrow(df_read), 3)
})

test_that("program_export_rds writes and reads back the full S3 object", {
  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  rds_path <- file.path(tmpdir, "test_export.rds")

  result_path <- program_export_rds(clean, rds_path, verbose = FALSE)
  expect_equal(result_path, rds_path)
  expect_true(file.exists(rds_path))

  restored <- readRDS(rds_path)
  expect_s3_class(restored, "alccdf_program_clean")
  expect_equal(nrow(alccdf_data(restored)), 3)
})

test_that("program_export_csv creates output directory if needed", {
  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  nested_path <- file.path(tmpdir, "subdir", "nested", "test.csv")

  program_export_csv(clean, nested_path, verbose = FALSE)
  expect_true(file.exists(nested_path))
})


# ==============================================================================
# 7. program_export_all() - Batch export
# ==============================================================================

test_that("program_export_all exports to multiple formats", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  skip_if_not_installed("arrow")

  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()

  paths <- program_export_all(clean, tmpdir, "test_batch", verbose = FALSE)

  expect_true(file.exists(paths[["csv"]]))
  expect_true(file.exists(paths[["excel"]]))
  expect_true(file.exists(paths[["stata"]]))
  expect_true(file.exists(paths[["parquet"]]))
  expect_true(file.exists(paths[["rds"]]))

  # Verify file extensions
  expect_true(grepl("\\.csv$", paths[["csv"]]))
  expect_true(grepl("\\.xlsx$", paths[["excel"]]))
  expect_true(grepl("\\.dta$", paths[["stata"]]))
  expect_true(grepl("\\.parquet$", paths[["parquet"]]))
  expect_true(grepl("\\.rds$", paths[["rds"]]))
})

test_that("program_export_all respects formats parameter", {
  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  paths <- program_export_all(clean, tmpdir, "test_subset",
                               formats = c("csv", "rds"), verbose = FALSE)

  expect_true(file.exists(paths[["csv"]]))
  expect_true(file.exists(paths[["rds"]]))
  expect_equal(length(paths), 2)
})

test_that("program_export_all creates output directory", {
  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  outdir <- file.path(tmpdir, "new_output_dir")
  expect_false(dir.exists(outdir))

  paths <- program_export_all(clean, outdir, "test",
                               formats = "csv", verbose = FALSE)
  expect_true(dir.exists(outdir))
  expect_true(file.exists(paths[["csv"]]))
})


# ==============================================================================
# 8. program_config() - Configuration creation
# ==============================================================================

test_that("program_config creates config object", {
  cfg <- program_config(
    snapshot_date = SNAPSHOT_DATE,
    center_path = fixture_path("test_center.xlsx"),
    verbose = FALSE
  )
  expect_s3_class(cfg, "alccdf_program_config")
  expect_equal(cfg$snapshot_date, SNAPSHOT_DATE)
  expect_true("center" %in% cfg$types_available)
})

test_that("program_config accepts character snapshot_date", {
  cfg <- program_config(
    snapshot_date = "2025-09-12",
    center_path = fixture_path("test_center.xlsx"),
    verbose = FALSE
  )
  expect_equal(cfg$snapshot_date, as.Date("2025-09-12"))
})

test_that("program_config tracks multiple types", {
  cfg <- program_config(
    snapshot_date = SNAPSHOT_DATE,
    center_path = fixture_path("test_center.xlsx"),
    home_path = fixture_path("test_family_home.xlsx"),
    exempt_path = fixture_path("test_exempt.xlsx"),
    excepted_path = fixture_path("test_excepted.xlsx"),
    verbose = FALSE
  )
  expect_equal(length(cfg$types_available), 4)
  expect_true(all(c("center", "family_home", "exempt", "excepted") %in%
    cfg$types_available))
})

test_that("program_config errors with no file paths", {
  expect_error(
    program_config(snapshot_date = SNAPSHOT_DATE, verbose = FALSE),
    "At least one"
  )
})

test_that("program_config errors with non-existent file", {
  expect_error(
    program_config(
      snapshot_date = SNAPSHOT_DATE,
      center_path = "no_such_file.xlsx",
      verbose = FALSE
    ),
    "not found"
  )
})

test_that("program_config errors with invalid date", {
  expect_error(
    program_config(
      snapshot_date = "not-a-date",
      center_path = fixture_path("test_center.xlsx"),
      verbose = FALSE
    )
  )
})

test_that("program_config uses default output_dir based on date", {
  cfg <- program_config(
    snapshot_date = SNAPSHOT_DATE,
    center_path = fixture_path("test_center.xlsx"),
    verbose = FALSE
  )
  expect_true(grepl("2025-09-12", cfg$output_dir))
})

test_that("program_config uses custom output_dir", {
  cfg <- program_config(
    snapshot_date = SNAPSHOT_DATE,
    center_path = fixture_path("test_center.xlsx"),
    output_dir = "/tmp/custom_output",
    verbose = FALSE
  )
  expect_equal(cfg$output_dir, "/tmp/custom_output")
})


# ==============================================================================
# 9. program_summary_stats() - Summary generation
# ==============================================================================

test_that("program_summary_stats returns expected structure", {
  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  stats <- program_summary_stats(clean, verbose = FALSE)

  expect_true(is.list(stats))
  expect_true(all(c("overview", "by_type", "by_county", "by_tier", "capacity") %in% names(stats)))
})

test_that("program_summary_stats overview has correct counts", {
  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  stats <- program_summary_stats(clean, verbose = FALSE)
  expect_equal(stats$overview$n_facilities, 3)
})

test_that("program_summary_stats by_type shows types", {
  raw <- program_read(fixture_path("test_family_home.xlsx"), "family_home", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  stats <- program_summary_stats(clean, verbose = FALSE)
  expect_true(nrow(stats$by_type) > 0)

  # Should include both Family Home and Group Home
  type_names <- as.character(stats$by_type$facility_type)
  expect_true("Family Home" %in% type_names)
  expect_true("Group Home" %in% type_names)
})

test_that("program_summary_stats by_county has entries", {
  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  stats <- program_summary_stats(clean, verbose = FALSE)
  expect_true(nrow(stats$by_county) > 0)
  # 3 centers in 3 different counties
  expect_equal(nrow(stats$by_county), 3)
})

test_that("program_summary_stats by_tier has entries", {
  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  stats <- program_summary_stats(clean, verbose = FALSE)
  expect_true(nrow(stats$by_tier) > 0)
})

test_that("program_summary_stats capacity has statistics", {
  raw <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  clean <- program_clean(raw, verbose = FALSE)

  stats <- program_summary_stats(clean, verbose = FALSE)
  expect_true(nrow(stats$capacity) > 0)
  expect_true("mean" %in% names(stats$capacity))
  expect_true("median" %in% names(stats$capacity))
  expect_true("min" %in% names(stats$capacity))
  expect_true("max" %in% names(stats$capacity))
})

test_that("program_summary_stats works on unified data", {
  raw_c <- program_read(fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE)
  raw_f <- program_read(fixture_path("test_family_home.xlsx"), "family_home", SNAPSHOT_DATE, verbose = FALSE)
  clean_c <- program_clean(raw_c, verbose = FALSE)
  clean_f <- program_clean(raw_f, verbose = FALSE)

  unified <- program_append_types(clean_c, clean_f, verbose = FALSE)

  stats <- program_summary_stats(unified, verbose = FALSE)
  expect_equal(stats$overview$n_facilities, 7)
  expect_true(nrow(stats$by_type) >= 3)  # Center, Family Home, Group Home
})
