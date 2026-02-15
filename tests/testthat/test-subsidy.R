# tests/testthat/test-subsidy.R
# Comprehensive unit tests for the subsidy module pipeline in ALccdfDB.
# Uses testthat edition 3 with fixture files in tests/testthat/fixtures/.

# Shared snapshot date used consistently across all tests.
SNAPSHOT_DATE <- as.Date("2025-09-12")

# Helper: path to a fixture file
fixture_path <- function(filename) {
  testthat::test_path("fixtures", filename)
}


# ==============================================================================
# 1. subsidy_read() - Read fixture files
# ==============================================================================

# --- 1a. Enrolled reading ---

test_that("subsidy_read reads enrolled fixture", {
  raw <- subsidy_read(
    path = fixture_path("test_enrolled.xlsx"),
    type = "enrolled",
    snapshot_date = SNAPSHOT_DATE,
    verbose = FALSE
  )
  expect_s3_class(raw, "alccdf_subsidy_raw")
  expect_equal(nrow(alccdf_data(raw)), 3)
  expect_equal(alccdf_meta(raw)$subsidy_type, "enrolled")
  expect_equal(alccdf_meta(raw)$snapshot_date, SNAPSHOT_DATE)
})

test_that("subsidy_read enrolled auto-detects skip=0 for current format", {
  raw <- subsidy_read(
    path = fixture_path("test_enrolled.xlsx"),
    type = "enrolled",
    snapshot_date = SNAPSHOT_DATE,
    verbose = FALSE
  )
  # The fixture has skip=0 and contains Parent ID/SSN, so detected as "current"
  expect_equal(alccdf_meta(raw)$skip, 0L)
  expect_equal(alccdf_meta(raw)$format_version, "current")
})

test_that("subsidy_read enrolled applies column mapping", {
  raw <- subsidy_read(
    path = fixture_path("test_enrolled.xlsx"),
    type = "enrolled",
    snapshot_date = SNAPSHOT_DATE,
    verbose = FALSE
  )
  df <- alccdf_data(raw)

  # Columns should be mapped to clean names
  expect_true("case_id" %in% names(df))
  expect_true("parent_name" %in% names(df))
  expect_true("parent_ssn" %in% names(df))
  expect_true("child_name" %in% names(df))
  expect_true("child_dob" %in% names(df))
  expect_true("provider_id" %in% names(df))
  expect_true("facility_id" %in% names(df))
  expect_true("eligibility_category" %in% names(df))
})

# --- 1b. Clients reading ---

test_that("subsidy_read reads clients fixture", {
  raw <- subsidy_read(
    path = fixture_path("test_clients.xlsx"),
    type = "clients",
    snapshot_date = SNAPSHOT_DATE,
    verbose = FALSE
  )
  expect_s3_class(raw, "alccdf_subsidy_raw")
  expect_equal(nrow(alccdf_data(raw)), 2)
  expect_equal(alccdf_meta(raw)$subsidy_type, "clients")
  expect_equal(alccdf_meta(raw)$snapshot_date, SNAPSHOT_DATE)
})

test_that("subsidy_read clients always uses skip=2", {
  raw <- subsidy_read(
    path = fixture_path("test_clients.xlsx"),
    type = "clients",
    snapshot_date = SNAPSHOT_DATE,
    verbose = FALSE
  )
  expect_equal(alccdf_meta(raw)$skip, 2L)
  expect_equal(alccdf_meta(raw)$format_version, "current")
})

test_that("subsidy_read clients applies column mapping", {
  raw <- subsidy_read(
    path = fixture_path("test_clients.xlsx"),
    type = "clients",
    snapshot_date = SNAPSHOT_DATE,
    verbose = FALSE
  )
  df <- alccdf_data(raw)

  expect_true("county" %in% names(df))
  expect_true("parent_name" %in% names(df))
  expect_true("case_id" %in% names(df))
  expect_true("child_name" %in% names(df))
  expect_true("child_dob" %in% names(df))
  expect_true("provider_id" %in% names(df))
  expect_true("copay_weekly" %in% names(df))
  expect_true("family_address" %in% names(df))
  expect_true("provider_address" %in% names(df))
})

# --- 1c. Error handling ---

test_that("subsidy_read rejects invalid type", {
  expect_error(
    subsidy_read(
      path = fixture_path("test_enrolled.xlsx"),
      type = "invalid_type",
      snapshot_date = SNAPSHOT_DATE,
      verbose = FALSE
    ),
    "type"
  )
})

test_that("subsidy_read rejects non-existent file", {
  expect_error(
    subsidy_read(
      path = "nonexistent.xlsx",
      type = "enrolled",
      snapshot_date = SNAPSHOT_DATE,
      verbose = FALSE
    ),
    "not found"
  )
})

test_that("subsidy_read accepts character snapshot_date", {
  raw <- subsidy_read(
    path = fixture_path("test_enrolled.xlsx"),
    type = "enrolled",
    snapshot_date = "2025-09-12",
    verbose = FALSE
  )
  expect_equal(alccdf_meta(raw)$snapshot_date, as.Date("2025-09-12"))
})

test_that("subsidy_read processing_log has entries", {
  raw <- subsidy_read(
    path = fixture_path("test_enrolled.xlsx"),
    type = "enrolled",
    snapshot_date = SNAPSHOT_DATE,
    verbose = FALSE
  )
  expect_true(length(alccdf_meta(raw)$processing_log) > 0)
})


# ==============================================================================
# 2. subsidy_clean() - Cleaning pipeline
# ==============================================================================

# --- 2a. PII Separation ---

test_that("subsidy_clean returns alccdf_subsidy_clean class for enrolled", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  expect_s3_class(clean, "alccdf_subsidy_clean")
  expect_equal(alccdf_meta(clean)$stage, "clean")
  expect_equal(alccdf_meta(clean)$subsidy_type, "enrolled")
})

test_that("subsidy_clean returns alccdf_subsidy_clean class for clients", {
  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  expect_s3_class(clean, "alccdf_subsidy_clean")
  expect_equal(alccdf_meta(clean)$subsidy_type, "clients")
})

test_that("subsidy_clean generates random parent and child IDs", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  # Random IDs should be present in main data

  expect_true("random_parent_id" %in% names(df))
  expect_true("random_child_id" %in% names(df))

  # IDs should start with correct prefix and be unique
  expect_true(all(grepl("^P\\d{7}$", df$random_parent_id)))
  expect_true(all(grepl("^C\\d{7}$", df$random_child_id)))
  expect_equal(length(unique(df$random_child_id)), nrow(df))
})

test_that("subsidy_clean PII table has correct structure", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  pii <- clean$pii
  expect_true(is.data.frame(pii))
  expect_equal(nrow(pii), 3)

  # PII table should contain the random IDs and actual PII fields
  expect_true("random_parent_id" %in% names(pii))
  expect_true("random_child_id" %in% names(pii))
  expect_true("parent_name" %in% names(pii))
  expect_true("child_name" %in% names(pii))
})

test_that("subsidy_clean PII table contains parent_ssn for enrolled current format", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  pii <- clean$pii
  # The enrolled fixture is current format with Parent ID/SSN
  expect_true("parent_ssn" %in% names(pii))
  expect_true("child_ssn" %in% names(pii))
})

test_that("subsidy_clean removes PII from main data", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  # PII fields must NOT be in main data
  expect_false("parent_name" %in% names(df))
  expect_false("child_name" %in% names(df))
  expect_false("parent_ssn" %in% names(df))
  expect_false("child_ssn" %in% names(df))
})

test_that("subsidy_clean PII removed from main data for clients", {
  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  # PII fields must NOT be in main data
  expect_false("parent_name" %in% names(df))
  expect_false("child_name" %in% names(df))

  # But random IDs should be present
  expect_true("random_parent_id" %in% names(df))
  expect_true("random_child_id" %in% names(df))
})

test_that("subsidy_clean PII random IDs link back to PII table", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  df <- alccdf_data(clean)
  pii <- clean$pii

  # Random IDs in main data should match those in PII table
  expect_equal(df$random_parent_id, pii$random_parent_id)
  expect_equal(df$random_child_id, pii$random_child_id)
})

# --- 2b. Date Parsing ---

test_that("subsidy_clean parses date fields for enrolled", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  # parent_dob should be moved to PII, but child_dob should be in main data
  expect_true("child_dob" %in% names(df))
  expect_true(inherits(df$child_dob, "Date"))

  # Check specific parsed dates
  expect_equal(df$child_dob[1], as.Date("2020-01-10"))
  expect_equal(df$child_dob[2], as.Date("2022-03-15"))
  expect_equal(df$child_dob[3], as.Date("2021-07-20"))

  # Eligibility dates
  expect_true("eligibility_begin_date" %in% names(df))
  expect_true(inherits(df$eligibility_begin_date, "Date"))
  expect_equal(df$eligibility_begin_date[1], as.Date("2024-01-01"))

  expect_true("eligibility_end_date" %in% names(df))
  expect_true(inherits(df$eligibility_end_date, "Date"))

  # Placement dates
  expect_true("placement_start_date" %in% names(df))
  expect_true(inherits(df$placement_start_date, "Date"))
  expect_equal(df$placement_start_date[1], as.Date("2024-02-01"))
})

test_that("subsidy_clean parses date fields for clients", {
  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  expect_true("child_dob" %in% names(df))
  expect_true(inherits(df$child_dob, "Date"))
  expect_equal(df$child_dob[1], as.Date("2020-01-10"))

  # Clients have placement_date (not placement_start_date)
  expect_true("placement_date" %in% names(df))
  expect_true(inherits(df$placement_date, "Date"))
  expect_equal(df$placement_date[1], as.Date("2024-02-01"))
})

# --- 2c. Address Standardization ---

test_that("subsidy_clean standardizes family_address for clients", {
  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  expect_true("family_address" %in% names(df))
  # Address should be standardized: all-caps abbreviations, title case, etc.
  # Original: "10 Oak Street, Montgomery, AL 36104"
  # After standardization, "Street" -> "ST", title case + uppercase abbrevs
  addr1 <- df$family_address[1]
  expect_true(grepl("AL", addr1))  # State stays uppercase
  expect_true(is.character(addr1))
  expect_true(nchar(addr1) > 0)
})

test_that("subsidy_clean standardizes provider_address for clients", {
  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  expect_true("provider_address" %in% names(df))
  addr <- df$provider_address[1]
  expect_true(grepl("AL", addr))
  expect_true(is.character(addr))
})

# --- 2d. County Standardization ---

test_that("subsidy_clean standardizes county to title case", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  expect_true("county" %in% names(df))
  # Original values are "Montgomery" and "Mobile" (already title case)
  expect_equal(df$county[1], "Montgomery")
  expect_equal(df$county[3], "Mobile")
})

# --- 2e. Copay Conversion (clients only) ---

test_that("subsidy_clean converts copay_weekly to numeric for clients", {
  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  expect_true("copay_weekly" %in% names(df))
  expect_true(is.numeric(df$copay_weekly))
  expect_equal(df$copay_weekly[1], 25.50)
  expect_equal(df$copay_weekly[2], 0)
})

# --- 2f. Age Conversion ---

test_that("subsidy_clean converts child_age to numeric", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  expect_true("child_age" %in% names(df))
  expect_true(is.numeric(df$child_age))
  expect_equal(df$child_age, c(5, 3, 4))
})

test_that("subsidy_clean converts child_age to numeric for clients", {
  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  expect_true(is.numeric(df$child_age))
  expect_equal(df$child_age, c(5, 4))
})

# --- 2g. Snapshot date column ---

test_that("subsidy_clean adds snapshot_date column", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  df <- alccdf_data(clean)

  expect_true("snapshot_date" %in% names(df))
  expect_equal(unique(df$snapshot_date), SNAPSHOT_DATE)
})

# --- 2h. Metadata ---

test_that("subsidy_clean metadata records cleaning steps", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  meta <- alccdf_meta(clean)

  expect_true("cleaning_steps" %in% names(meta))
  expect_true("pii_separation" %in% meta$cleaning_steps)
  expect_true("date_parsing" %in% meta$cleaning_steps)
  expect_true("address_standardisation" %in% meta$cleaning_steps)
  expect_true("county_standardisation" %in% meta$cleaning_steps)
  expect_true(meta$has_pii)
  expect_equal(meta$n_pii_records, 3)
  expect_true(length(meta$processing_log) > 0)
})

# --- 2i. Wrong class rejection ---

test_that("subsidy_clean rejects non alccdf_subsidy_raw input", {
  expect_error(
    subsidy_clean(data.frame(x = 1), verbose = FALSE),
    "alccdf_subsidy_raw"
  )
})


# ==============================================================================
# 3. subsidy_validate() - Validation checks
# ==============================================================================

# --- 3a. Validation returns correct class ---

test_that("subsidy_validate returns validation object for enrolled", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  val <- subsidy_validate(clean, verbose = FALSE)

  expect_s3_class(val, "alccdf_subsidy_validation")
  expect_s3_class(val, "alccdf_validation")
  expect_true(is.data.frame(val$checks))
  expect_true(nrow(val$checks) > 0)
  expect_true("check_id" %in% names(val$checks))
  expect_true("status" %in% names(val$checks))
})

test_that("subsidy_validate returns validation object for clients", {
  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  val <- subsidy_validate(clean, verbose = FALSE)

  expect_s3_class(val, "alccdf_subsidy_validation")
  expect_true(val$passed)
})

# --- 3b. Validation passes for clean fixture data ---

test_that("subsidy_validate passes for clean enrolled fixture data", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  val <- subsidy_validate(clean, verbose = FALSE)

  expect_true(val$passed)
})

test_that("subsidy_validate passes for clean clients fixture data", {
  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  val <- subsidy_validate(clean, verbose = FALSE)

  expect_true(val$passed)
})

# --- 3c. Individual check verification ---

test_that("subsidy_validate checks required_columns", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  val <- subsidy_validate(clean, verbose = FALSE)

  req_check <- val$checks[val$checks$check_id == "required_columns", ]
  expect_equal(nrow(req_check), 1)
  expect_equal(req_check$status, "PASS")
})

test_that("subsidy_validate checks pii_separation", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  val <- subsidy_validate(clean, verbose = FALSE)

  pii_check <- val$checks[val$checks$check_id == "pii_separation", ]
  expect_equal(nrow(pii_check), 1)
  expect_equal(pii_check$status, "PASS")
})

test_that("subsidy_validate checks age_range", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  val <- subsidy_validate(clean, verbose = FALSE)

  age_check <- val$checks[val$checks$check_id == "age_range", ]
  expect_equal(nrow(age_check), 1)
  expect_equal(age_check$status, "PASS")
})

test_that("subsidy_validate checks copay_range for clients", {
  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  val <- subsidy_validate(clean, verbose = FALSE)

  copay_check <- val$checks[val$checks$check_id == "copay_range", ]
  expect_equal(nrow(copay_check), 1)
  expect_equal(copay_check$status, "PASS")
})

test_that("subsidy_validate copay_range is INFO for enrolled (not applicable)", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  val <- subsidy_validate(clean, verbose = FALSE)

  copay_check <- val$checks[val$checks$check_id == "copay_range", ]
  expect_equal(nrow(copay_check), 1)
  expect_equal(copay_check$status, "INFO")
})

test_that("subsidy_validate checks duplicate_records", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  val <- subsidy_validate(clean, verbose = FALSE)

  dup_check <- val$checks[val$checks$check_id == "duplicate_records", ]
  expect_equal(nrow(dup_check), 1)
  expect_equal(dup_check$status, "PASS")
})

test_that("subsidy_validate checks provider_reference", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  val <- subsidy_validate(clean, verbose = FALSE)

  prov_check <- val$checks[val$checks$check_id == "provider_reference", ]
  expect_equal(nrow(prov_check), 1)
  expect_equal(prov_check$status, "PASS")
})

# --- 3d. Strict mode ---

test_that("subsidy_validate strict mode errors when required columns missing", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  # Manually break the data to trigger an ERROR check
  clean$data$case_id <- NULL
  clean$data$snapshot_date <- NULL

  expect_error(
    subsidy_validate(clean, strict = TRUE, verbose = FALSE),
    "strict"
  )
})

test_that("subsidy_validate strict mode errors when PII leaked to main data", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  # Manually inject PII back into main data
  clean$data$parent_ssn <- "leaked"

  expect_error(
    subsidy_validate(clean, strict = TRUE, verbose = FALSE),
    "strict"
  )
})

test_that("subsidy_validate non-strict mode does not error on issues", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  # Break data but run in non-strict mode
  clean$data$case_id <- NULL

  val <- subsidy_validate(clean, strict = FALSE, verbose = FALSE)
  expect_s3_class(val, "alccdf_subsidy_validation")
  # Should still have errors recorded
  expect_true(val$n_errors > 0)
})

# --- 3e. Wrong class rejection ---

test_that("subsidy_validate rejects non alccdf_subsidy_clean input", {
  expect_error(
    subsidy_validate(data.frame(x = 1), verbose = FALSE),
    "alccdf_subsidy_clean"
  )
})


# ==============================================================================
# 4. Export functions - roundtrip tests
# ==============================================================================

# --- 4a. CSV export ---

test_that("subsidy_export_csv writes and reads back enrolled data", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  csv_path <- file.path(tmpdir, "test_enrolled.csv")

  result_path <- subsidy_export_csv(clean, csv_path, verbose = FALSE)
  expect_equal(result_path, csv_path)
  expect_true(file.exists(csv_path))

  df_read <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  expect_equal(nrow(df_read), 3)
  expect_true("random_parent_id" %in% names(df_read))
  expect_true("case_id" %in% names(df_read))

  # PII should NOT be in the export by default
  expect_false("parent_name" %in% names(df_read))
  expect_false("child_name" %in% names(df_read))
  expect_false("parent_ssn" %in% names(df_read))
})

test_that("subsidy_export_csv with include_pii=TRUE includes PII", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  csv_path <- file.path(tmpdir, "test_enrolled_pii.csv")

  subsidy_export_csv(clean, csv_path, include_pii = TRUE, verbose = FALSE)
  df_read <- utils::read.csv(csv_path, stringsAsFactors = FALSE)

  # PII fields should be present when include_pii=TRUE
  expect_true("parent_name" %in% names(df_read))
  expect_true("child_name" %in% names(df_read))
})

test_that("subsidy_export_csv writes clients data", {
  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  csv_path <- file.path(tmpdir, "test_clients.csv")

  subsidy_export_csv(clean, csv_path, verbose = FALSE)
  expect_true(file.exists(csv_path))

  df_read <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  expect_equal(nrow(df_read), 2)
  expect_true("copay_weekly" %in% names(df_read))
})

# --- 4b. Excel export ---

test_that("subsidy_export_excel writes xlsx file", {
  skip_if_not_installed("openxlsx")

  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  xlsx_path <- file.path(tmpdir, "test_export.xlsx")

  result_path <- subsidy_export_excel(clean, xlsx_path, verbose = FALSE)
  expect_equal(result_path, xlsx_path)
  expect_true(file.exists(xlsx_path))

  df_read <- readxl::read_excel(xlsx_path)
  expect_equal(nrow(df_read), 3)
})

# --- 4c. Stata export ---

test_that("subsidy_export_stata writes dta file", {
  skip_if_not_installed("haven")

  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  dta_path <- file.path(tmpdir, "test_export.dta")

  result_path <- subsidy_export_stata(clean, dta_path, verbose = FALSE)
  expect_equal(result_path, dta_path)
  expect_true(file.exists(dta_path))

  df_read <- haven::read_dta(dta_path)
  expect_equal(nrow(df_read), 3)
})

# --- 4d. Parquet export ---

test_that("subsidy_export_parquet writes parquet file", {
  skip_if_not_installed("arrow")

  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  pq_path <- file.path(tmpdir, "test_export.parquet")

  result_path <- subsidy_export_parquet(clean, pq_path, verbose = FALSE)
  expect_equal(result_path, pq_path)
  expect_true(file.exists(pq_path))

  df_read <- arrow::read_parquet(pq_path)
  expect_equal(nrow(df_read), 3)
})

# --- 4e. RDS export ---

test_that("subsidy_export_rds writes and reads back full S3 object", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  rds_path <- file.path(tmpdir, "test_export.rds")

  result_path <- subsidy_export_rds(clean, rds_path, verbose = FALSE)
  expect_equal(result_path, rds_path)
  expect_true(file.exists(rds_path))

  restored <- readRDS(rds_path)
  expect_s3_class(restored, "alccdf_subsidy_clean")
  # PII should be stripped by default
  expect_null(restored$pii)
})

test_that("subsidy_export_rds with include_pii=TRUE preserves PII", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  rds_path <- file.path(tmpdir, "test_export_pii.rds")

  subsidy_export_rds(clean, rds_path, include_pii = TRUE, verbose = FALSE)

  restored <- readRDS(rds_path)
  expect_s3_class(restored, "alccdf_subsidy_clean")
  expect_true(!is.null(restored$pii))
  expect_true("parent_name" %in% names(restored$pii))
})

# --- 4f. CSV creates output directory if needed ---

test_that("subsidy_export_csv creates output directory if needed", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  nested_path <- file.path(tmpdir, "subdir", "nested", "test.csv")

  subsidy_export_csv(clean, nested_path, verbose = FALSE)
  expect_true(file.exists(nested_path))
})


# ==============================================================================
# 5. subsidy_export_all() - Batch export
# ==============================================================================

test_that("subsidy_export_all exports to multiple formats", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  skip_if_not_installed("arrow")

  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()

  paths <- subsidy_export_all(clean, tmpdir, "test_batch", verbose = FALSE)

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

test_that("subsidy_export_all respects formats parameter", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  paths <- subsidy_export_all(clean, tmpdir, "test_subset",
                               formats = c("csv", "rds"), verbose = FALSE)

  expect_true(file.exists(paths[["csv"]]))
  expect_true(file.exists(paths[["rds"]]))
  expect_equal(length(paths), 2)
})

test_that("subsidy_export_all creates output directory", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  outdir <- file.path(tmpdir, "new_output_dir")
  expect_false(dir.exists(outdir))

  paths <- subsidy_export_all(clean, outdir, "test",
                               formats = "csv", verbose = FALSE)
  expect_true(dir.exists(outdir))
  expect_true(file.exists(paths[["csv"]]))
})

test_that("subsidy_export_all PII exclusion by default", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  paths <- subsidy_export_all(clean, tmpdir, "test_no_pii",
                               formats = "csv", verbose = FALSE)

  df_read <- utils::read.csv(paths[["csv"]], stringsAsFactors = FALSE)
  expect_false("parent_name" %in% names(df_read))
  expect_false("parent_ssn" %in% names(df_read))
})


# ==============================================================================
# 6. subsidy_summary_stats() - Summary generation
# ==============================================================================

test_that("subsidy_summary_stats returns expected structure for enrolled", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  stats <- subsidy_summary_stats(clean, verbose = FALSE)

  expect_true(is.list(stats))
  expect_true(all(c("overview", "by_county", "age_distribution",
                     "copay_distribution", "by_eligibility") %in% names(stats)))
})

test_that("subsidy_summary_stats overview has correct counts for enrolled", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  stats <- subsidy_summary_stats(clean, verbose = FALSE)

  expect_equal(stats$overview$n_records, 3)
  expect_true(stats$overview$n_cases > 0)
  expect_true(stats$overview$n_children > 0)
  expect_true(stats$overview$n_counties > 0)
})

test_that("subsidy_summary_stats returns expected structure for clients", {
  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  stats <- subsidy_summary_stats(clean, verbose = FALSE)

  expect_true(is.list(stats))
  expect_equal(stats$overview$n_records, 2)
})

test_that("subsidy_summary_stats by_county has entries", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  stats <- subsidy_summary_stats(clean, verbose = FALSE)

  expect_true(nrow(stats$by_county) > 0)
  expect_true("county" %in% names(stats$by_county))
  expect_true("n" %in% names(stats$by_county))
  expect_true("pct" %in% names(stats$by_county))
})

test_that("subsidy_summary_stats age_distribution has statistics", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  stats <- subsidy_summary_stats(clean, verbose = FALSE)

  expect_true(nrow(stats$age_distribution) > 0)
  expect_true("mean" %in% names(stats$age_distribution))
  expect_true("median" %in% names(stats$age_distribution))
  expect_true("min" %in% names(stats$age_distribution))
  expect_true("max" %in% names(stats$age_distribution))
})

test_that("subsidy_summary_stats copay_distribution populated for clients", {
  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  stats <- subsidy_summary_stats(clean, verbose = FALSE)

  expect_true(nrow(stats$copay_distribution) > 0)
  expect_true("mean" %in% names(stats$copay_distribution))
  expect_true("median" %in% names(stats$copay_distribution))
})

test_that("subsidy_summary_stats copay_distribution empty for enrolled", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  stats <- subsidy_summary_stats(clean, verbose = FALSE)

  # Enrolled data has no copay_weekly column, so copay_distribution should be empty
  expect_equal(nrow(stats$copay_distribution), 0)
})

test_that("subsidy_summary_stats by_eligibility has entries for enrolled", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  stats <- subsidy_summary_stats(clean, verbose = FALSE)

  # Enrolled has eligibility_category
  expect_true(nrow(stats$by_eligibility) > 0)
})

test_that("subsidy_summary_stats by_eligibility uses funding_type for clients", {
  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  stats <- subsidy_summary_stats(clean, verbose = FALSE)

  # Clients has funding_type column
  expect_true(nrow(stats$by_eligibility) > 0)
})


# ==============================================================================
# 7. Full pipeline integration - enrolled
# ==============================================================================

test_that("full enrolled pipeline: read -> clean -> validate -> export", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  expect_s3_class(raw, "alccdf_subsidy_raw")

  clean <- subsidy_clean(raw, verbose = FALSE)
  expect_s3_class(clean, "alccdf_subsidy_clean")
  expect_equal(nrow(alccdf_data(clean)), 3)

  val <- subsidy_validate(clean, verbose = FALSE)
  expect_true(val$passed)

  tmpdir <- withr::local_tempdir()
  csv_path <- file.path(tmpdir, "pipeline_enrolled.csv")
  subsidy_export_csv(clean, csv_path, verbose = FALSE)
  expect_true(file.exists(csv_path))

  # Verify roundtrip
  df_round <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  expect_equal(nrow(df_round), 3)
  expect_true("random_parent_id" %in% names(df_round))
  expect_true("random_child_id" %in% names(df_round))
})


# ==============================================================================
# 8. Full pipeline integration - clients
# ==============================================================================

test_that("full clients pipeline: read -> clean -> validate -> export", {
  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  expect_s3_class(raw, "alccdf_subsidy_raw")

  clean <- subsidy_clean(raw, verbose = FALSE)
  expect_s3_class(clean, "alccdf_subsidy_clean")
  expect_equal(nrow(alccdf_data(clean)), 2)

  val <- subsidy_validate(clean, verbose = FALSE)
  expect_true(val$passed)

  tmpdir <- withr::local_tempdir()
  csv_path <- file.path(tmpdir, "pipeline_clients.csv")
  subsidy_export_csv(clean, csv_path, verbose = FALSE)
  expect_true(file.exists(csv_path))

  df_round <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  expect_equal(nrow(df_round), 2)
  expect_true("copay_weekly" %in% names(df_round))
  expect_false("parent_name" %in% names(df_round))
})


# ==============================================================================
# 9. Export roundtrip - PII merge verification
# ==============================================================================

test_that("CSV export with include_pii merges PII correctly", {
  raw <- subsidy_read(
    fixture_path("test_enrolled.xlsx"), "enrolled",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  csv_pii <- file.path(tmpdir, "enrolled_with_pii.csv")
  csv_no_pii <- file.path(tmpdir, "enrolled_no_pii.csv")

  subsidy_export_csv(clean, csv_pii, include_pii = TRUE, verbose = FALSE)
  subsidy_export_csv(clean, csv_no_pii, include_pii = FALSE, verbose = FALSE)

  df_pii <- utils::read.csv(csv_pii, stringsAsFactors = FALSE)
  df_no_pii <- utils::read.csv(csv_no_pii, stringsAsFactors = FALSE)

  # PII version should have more columns
  expect_true(ncol(df_pii) > ncol(df_no_pii))

  # Both should have the same number of rows
  expect_equal(nrow(df_pii), nrow(df_no_pii))

  # PII version should contain PII fields
  expect_true("parent_name" %in% names(df_pii))
  expect_true("child_name" %in% names(df_pii))

  # Non-PII version should not
  expect_false("parent_name" %in% names(df_no_pii))
})
