# tests/testthat/test-staff.R
# Comprehensive unit tests for the staff module pipeline in ALccdfDB.
# Uses testthat edition 3 with fixture files in tests/testthat/fixtures/.

# Shared snapshot date used consistently across all tests.
SNAPSHOT_DATE <- as.Date("2025-09-12")

# Helper: path to a fixture file
fixture_path <- function(filename) {
  testthat::test_path("fixtures", filename)
}


# ==============================================================================
# 1. staff_read() - Read CSV and verify column mapping
# ==============================================================================

test_that("staff_read reads fixture CSV and returns correct class", {
  raw <- staff_read(
    path = fixture_path("test_staff.csv"),
    snapshot_date = SNAPSHOT_DATE,
    verbose = FALSE
  )
  expect_s3_class(raw, "alccdf_staff_raw")
  expect_equal(nrow(raw$data), 5)
  expect_equal(raw$meta$snapshot_date, SNAPSHOT_DATE)
})

test_that("staff_read applies column mapping to produce snake_case names", {
  raw <- staff_read(
    path = fixture_path("test_staff.csv"),
    snapshot_date = SNAPSHOT_DATE,
    verbose = FALSE
  )
  df <- raw$data

  expected_cols <- c("staff_name", "user_zip", "user_county", "facility_name",
                     "facility_county", "provider_type", "currently_operating",
                     "position", "career_lattice_level", "user_email")
  for (col in expected_cols) {
    expect_true(col %in% names(df),
                info = paste("Expected column", col, "not found in raw data"))
  }
})

test_that("staff_read accepts character snapshot_date", {
  raw <- staff_read(
    path = fixture_path("test_staff.csv"),
    snapshot_date = "2025-09-12",
    verbose = FALSE
  )
  expect_equal(raw$meta$snapshot_date, as.Date("2025-09-12"))
})

test_that("staff_read errors on non-existent file", {
  expect_error(
    staff_read(
      path = "nonexistent_staff_file.csv",
      snapshot_date = SNAPSHOT_DATE,
      verbose = FALSE
    ),
    "not found"
  )
})

test_that("staff_read stores source_file in metadata", {
  raw <- staff_read(
    path = fixture_path("test_staff.csv"),
    snapshot_date = SNAPSHOT_DATE,
    verbose = FALSE
  )
  expect_true(!is.null(raw$meta$source_file))
  expect_true(grepl("test_staff\\.csv$", raw$meta$source_file))
})

test_that("staff_read processing_log has entries", {
  raw <- staff_read(
    path = fixture_path("test_staff.csv"),
    snapshot_date = SNAPSHOT_DATE,
    verbose = FALSE
  )
  expect_true(length(raw$meta$processing_log) > 0)
})

test_that("staff_read preserves all 5 rows of fixture data", {
  raw <- staff_read(
    path = fixture_path("test_staff.csv"),
    snapshot_date = SNAPSHOT_DATE,
    verbose = FALSE
  )
  expect_equal(raw$meta$n_rows, 5)
})


# ==============================================================================
# 2. staff_clean() - Comprehensive cleaning pipeline tests
# ==============================================================================

# --- 2a. PII separation ---

test_that("staff_clean returns alccdf_staff_clean class with $data, $pii, $meta", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  expect_s3_class(clean, "alccdf_staff_clean")
  expect_true("data" %in% names(clean))
  expect_true("pii" %in% names(clean))
  expect_true("meta" %in% names(clean))
  expect_s3_class(clean$data, "tbl_df")
  expect_s3_class(clean$pii, "tbl_df")
})

test_that("staff_clean separates staff_name and user_email into $pii", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  # PII columns must be in $pii
  expect_true("staff_name" %in% names(clean$pii))
  expect_true("user_email" %in% names(clean$pii))

  # PII columns must NOT be in $data

  expect_false("staff_name" %in% names(clean$data))
  expect_false("user_email" %in% names(clean$data))
})

test_that("staff_clean $pii table has random_staff_id linking key", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  expect_true("random_staff_id" %in% names(clean$pii))
  expect_true("random_staff_id" %in% names(clean$data))

  # random_staff_id should link $data and $pii (same set of IDs)
  expect_equal(sort(clean$data$random_staff_id), sort(clean$pii$random_staff_id))
})

test_that("staff_clean $pii preserves all 5 records", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  expect_equal(nrow(clean$pii), 5)
  expect_equal(nrow(clean$data), 5)
})

# --- 2b. Random staff ID generation ---

test_that("staff_clean generates random_staff_id with STF prefix and 6 digits", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  ids <- clean$data$random_staff_id
  expect_length(ids, 5)

  # All IDs should start with "STF" followed by exactly 6 digits
  expect_true(all(grepl("^STF\\d{6}$", ids)),
              info = "All random_staff_id values should match pattern STF followed by 6 digits")
})

test_that("staff_clean generates unique random_staff_id values", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  ids <- clean$data$random_staff_id
  expect_equal(length(unique(ids)), length(ids))
})

# --- 2c. County standardisation ---

test_that("staff_clean standardises user_county to title case", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  # Fixture has lowercase: montgomery, jefferson, madison, houston, mobile
  expected <- c("Montgomery", "Jefferson", "Madison", "Houston", "Mobile")
  expect_equal(clean$data$user_county, expected)
})

test_that("staff_clean standardises facility_county to title case", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  # Fixture facility counties: Montgomery, Montgomery, Jefferson, Houston, Mobile
  # These are already title case in the fixture, but the function applies
  # str_to_title which should leave them unchanged
  expected <- c("Montgomery", "Montgomery", "Jefferson", "Houston", "Mobile")
  expect_equal(clean$data$facility_county, expected)
})

# --- 2d. currently_operating conversion to logical ---

test_that("staff_clean converts currently_operating to logical", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  expect_true(is.logical(clean$data$currently_operating))
  # Fixture: "Yes", "Yes", "Yes", "No", "Yes"
  expect_equal(clean$data$currently_operating, c(TRUE, TRUE, TRUE, FALSE, TRUE))
})

# --- 2e. Career lattice level cleaning ---

test_that("staff_clean cleans career_lattice_level values", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  # Fixture has: "Level 3", "Level 5", "Level 2", "Level 1", "Level 4"
  # After .clean_career_lattice_level via str_to_title: "Level 3", "Level 5", ...
  # These go through the case_when regex matching
  # "Level 3" -> str_to_title -> "Level 3" -> grepl("^Level\\s*III$") no
  # Actually "Level 3" does not match roman numeral patterns.
  # The function uses str_to_title first, then matches against roman numerals.
  # "Level 3" in title case = "Level 3". This does NOT match any of the
  # roman numeral patterns, so it falls through to TRUE ~ x (keep as-is).
  levels <- clean$data$career_lattice_level
  expect_true(is.character(levels))
  expect_length(levels, 5)

  # All values should be non-NA character
  expect_true(all(!is.na(levels)))
})

# --- 2f. Snapshot date column ---

test_that("staff_clean adds snapshot_date column", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  expect_true("snapshot_date" %in% names(clean$data))
  expect_equal(unique(clean$data$snapshot_date), SNAPSHOT_DATE)
})

# --- 2g. Metadata ---

test_that("staff_clean metadata has expected fields", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  meta <- clean$meta
  expect_equal(meta$module, "staff")
  expect_equal(meta$stage, "clean")
  expect_equal(meta$snapshot_date, SNAPSHOT_DATE)
  expect_equal(meta$n_rows, 5)
  expect_equal(meta$n_rows_raw, 5)
  expect_equal(meta$n_pii_fields, 2)
  expect_true("cleaning_steps" %in% names(meta))
  expect_true(length(meta$processing_log) > 0)
})

# --- 2h. Class rejection ---

test_that("staff_clean rejects non alccdf_staff_raw input", {
  expect_error(
    staff_clean(data.frame(x = 1), verbose = FALSE),
    "alccdf_staff_raw"
  )
})


# ==============================================================================
# 3. staff_validate() - Validation checks
# ==============================================================================

test_that("staff_validate returns validation object for clean data", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  val <- staff_validate(clean, verbose = FALSE)

  expect_s3_class(val, "alccdf_staff_validation")
  expect_s3_class(val, "alccdf_validation")
  expect_true(is.data.frame(val$checks))
  expect_true(nrow(val$checks) > 0)
  expect_true("check_id" %in% names(val$checks))
  expect_true("status" %in% names(val$checks))
})

test_that("staff_validate passes for clean fixture data", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  val <- staff_validate(clean, verbose = FALSE)
  expect_true(val$passed)
})

test_that("staff_validate runs all 4 checks", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  val <- staff_validate(clean, verbose = FALSE)

  expected_checks <- c("required_columns", "duplicate_records",
                        "valid_professional_levels", "pii_separation")
  for (ck in expected_checks) {
    matching <- val$checks[val$checks$check_id == ck, ]
    expect_equal(nrow(matching), 1,
                 info = paste("Expected check", ck, "to be present in validation"))
  }
})

test_that("staff_validate required_columns passes for fixture data", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  val <- staff_validate(clean, verbose = FALSE)
  req_check <- val$checks[val$checks$check_id == "required_columns", ]
  expect_equal(req_check$status, "PASS")
})

test_that("staff_validate pii_separation passes for clean data", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  val <- staff_validate(clean, verbose = FALSE)
  pii_check <- val$checks[val$checks$check_id == "pii_separation", ]
  expect_equal(pii_check$status, "PASS")
})

test_that("staff_validate duplicate_records detects no duplicates in fixture", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  val <- staff_validate(clean, verbose = FALSE)
  dup_check <- val$checks[val$checks$check_id == "duplicate_records", ]
  expect_equal(dup_check$status, "PASS")
})

test_that("staff_validate strict mode errors when issues exist", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  # Inject PII field back into main data to trigger pii_separation ERROR
  clean$data$staff_name <- "Injected"

  expect_error(
    staff_validate(clean, strict = TRUE, verbose = FALSE),
    "strict"
  )
})

test_that("staff_validate rejects non alccdf_staff_clean input", {
  expect_error(
    staff_validate(data.frame(x = 1), verbose = FALSE),
    "alccdf_staff_clean"
  )
})


# ==============================================================================
# 4. Export functions - roundtrip tests
# ==============================================================================

test_that("staff_export_csv writes and reads back correctly", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  csv_path <- file.path(tmpdir, "staff_export.csv")

  result_path <- staff_export_csv(clean, csv_path, verbose = FALSE)
  expect_equal(result_path, csv_path)
  expect_true(file.exists(csv_path))

  df_read <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  expect_equal(nrow(df_read), 5)
  expect_true("random_staff_id" %in% names(df_read))

  # Without PII: staff_name and user_email should NOT be present
  expect_false("staff_name" %in% names(df_read))
  expect_false("user_email" %in% names(df_read))
})

test_that("staff_export_csv with include_pii includes PII columns", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  csv_path <- file.path(tmpdir, "staff_export_pii.csv")

  staff_export_csv(clean, csv_path, include_pii = TRUE, verbose = FALSE)
  df_read <- utils::read.csv(csv_path, stringsAsFactors = FALSE)

  expect_true("staff_name" %in% names(df_read))
  expect_true("user_email" %in% names(df_read))
})

test_that("staff_export_excel writes xlsx file", {
  skip_if_not_installed("openxlsx")

  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  xlsx_path <- file.path(tmpdir, "staff_export.xlsx")

  result_path <- staff_export_excel(clean, xlsx_path, verbose = FALSE)
  expect_equal(result_path, xlsx_path)
  expect_true(file.exists(xlsx_path))

  df_read <- readxl::read_excel(xlsx_path)
  expect_equal(nrow(df_read), 5)
})

test_that("staff_export_stata writes dta file", {
  skip_if_not_installed("haven")

  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  dta_path <- file.path(tmpdir, "staff_export.dta")

  result_path <- staff_export_stata(clean, dta_path, verbose = FALSE)
  expect_equal(result_path, dta_path)
  expect_true(file.exists(dta_path))

  df_read <- haven::read_dta(dta_path)
  expect_equal(nrow(df_read), 5)
})

test_that("staff_export_parquet writes parquet file", {
  skip_if_not_installed("arrow")

  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  pq_path <- file.path(tmpdir, "staff_export.parquet")

  result_path <- staff_export_parquet(clean, pq_path, verbose = FALSE)
  expect_equal(result_path, pq_path)
  expect_true(file.exists(pq_path))

  df_read <- arrow::read_parquet(pq_path)
  expect_equal(nrow(df_read), 5)
})

test_that("staff_export_rds writes and reads back the full S3 object", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  rds_path <- file.path(tmpdir, "staff_export.rds")

  result_path <- staff_export_rds(clean, rds_path, verbose = FALSE)
  expect_equal(result_path, rds_path)
  expect_true(file.exists(rds_path))

  restored <- readRDS(rds_path)
  expect_s3_class(restored, "alccdf_staff_clean")
  expect_equal(nrow(restored$data), 5)
})

test_that("staff_export_rds without PII strips PII content", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  rds_path <- file.path(tmpdir, "staff_no_pii.rds")

  staff_export_rds(clean, rds_path, include_pii = FALSE, verbose = FALSE)
  restored <- readRDS(rds_path)

  # PII table should exist but contain placeholder note instead of real names
  expect_true("random_staff_id" %in% names(restored$pii))
  expect_false("staff_name" %in% names(restored$pii))
  expect_false("user_email" %in% names(restored$pii))
})

test_that("staff_export_rds with PII preserves full PII table", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  rds_path <- file.path(tmpdir, "staff_with_pii.rds")

  staff_export_rds(clean, rds_path, include_pii = TRUE, verbose = FALSE)
  restored <- readRDS(rds_path)

  expect_true("staff_name" %in% names(restored$pii))
  expect_true("user_email" %in% names(restored$pii))
  expect_equal(nrow(restored$pii), 5)
})

test_that("staff_export_csv creates output directory if needed", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  nested_path <- file.path(tmpdir, "subdir", "nested", "staff.csv")

  staff_export_csv(clean, nested_path, verbose = FALSE)
  expect_true(file.exists(nested_path))
})


# ==============================================================================
# 5. staff_export_all() - Batch export
# ==============================================================================

test_that("staff_export_all exports to multiple formats", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("haven")
  skip_if_not_installed("arrow")

  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  paths <- staff_export_all(clean, tmpdir, "staff_batch", verbose = FALSE)

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

test_that("staff_export_all respects formats parameter", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  paths <- staff_export_all(clean, tmpdir, "staff_subset",
                             formats = c("csv", "rds"), verbose = FALSE)

  expect_true(file.exists(paths[["csv"]]))
  expect_true(file.exists(paths[["rds"]]))
  expect_equal(length(paths), 2)
})

test_that("staff_export_all creates output directory", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  outdir <- file.path(tmpdir, "new_staff_output")
  expect_false(dir.exists(outdir))

  paths <- staff_export_all(clean, outdir, "staff_test",
                             formats = "csv", verbose = FALSE)
  expect_true(dir.exists(outdir))
  expect_true(file.exists(paths[["csv"]]))
})


# ==============================================================================
# 6. staff_summary_stats() - Summary statistics
# ==============================================================================

test_that("staff_summary_stats returns expected structure", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  stats <- staff_summary_stats(clean, verbose = FALSE)

  expect_true(is.list(stats))
  expected_names <- c("overview", "by_career_level", "by_provider_type",
                      "by_user_county", "by_facility_county",
                      "by_position", "operating_status")
  expect_true(all(expected_names %in% names(stats)))
})

test_that("staff_summary_stats overview has correct counts", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  stats <- staff_summary_stats(clean, verbose = FALSE)
  expect_equal(stats$overview$n_staff, 5)
})

test_that("staff_summary_stats by_career_level has entries", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  stats <- staff_summary_stats(clean, verbose = FALSE)
  expect_true(nrow(stats$by_career_level) > 0)
  expect_true("career_lattice_level" %in% names(stats$by_career_level))
  expect_true("n" %in% names(stats$by_career_level))
  expect_true("pct" %in% names(stats$by_career_level))

  # 5 staff with 5 different career levels -> 5 rows
  expect_equal(nrow(stats$by_career_level), 5)
})

test_that("staff_summary_stats by_provider_type has entries", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  stats <- staff_summary_stats(clean, verbose = FALSE)
  expect_true(nrow(stats$by_provider_type) > 0)
  expect_true("provider_type" %in% names(stats$by_provider_type))

  # Fixture has "Center" (3 staff) and "Group Home" (2 staff)
  types <- stats$by_provider_type$provider_type
  expect_true("Center" %in% types)
  expect_true("Group Home" %in% types)
})

test_that("staff_summary_stats by_user_county has entries for all 5 counties", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  stats <- staff_summary_stats(clean, verbose = FALSE)
  # 5 unique user counties
  expect_equal(nrow(stats$by_user_county), 5)
})

test_that("staff_summary_stats by_position has entries", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  stats <- staff_summary_stats(clean, verbose = FALSE)
  expect_true(nrow(stats$by_position) > 0)
  # Fixture positions: Teacher (3), Director (1), Assistant (1)
  positions <- stats$by_position$position
  expect_true("Teacher" %in% positions)
  expect_true("Director" %in% positions)
  expect_true("Assistant" %in% positions)
})

test_that("staff_summary_stats operating_status has correct counts", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  stats <- staff_summary_stats(clean, verbose = FALSE)
  expect_true(nrow(stats$operating_status) == 3)

  # Fixture: 4 "Yes" and 1 "No"
  operating_row <- stats$operating_status[stats$operating_status$status == "Operating", ]
  not_operating_row <- stats$operating_status[stats$operating_status$status == "Not Operating", ]
  expect_equal(operating_row$n, 4)
  expect_equal(not_operating_row$n, 1)
})


# ==============================================================================
# 7. Internal functions: .clean_career_lattice_level()
# ==============================================================================

test_that(".clean_career_lattice_level standardises roman numeral levels", {
  fn <- ALccdfDB:::.clean_career_lattice_level

  # Roman numerals should be standardised
  expect_equal(fn("Level I"), "Level I")
  expect_equal(fn("Level II"), "Level II")
  expect_equal(fn("Level III"), "Level III")
  expect_equal(fn("Level IV"), "Level IV")
  expect_equal(fn("Level V"), "Level V")
  expect_equal(fn("Level VI"), "Level VI")
  expect_equal(fn("Level VII"), "Level VII")
  expect_equal(fn("Level VIII"), "Level VIII")
  expect_equal(fn("Level IX"), "Level IX")
  expect_equal(fn("Level X"), "Level X")
})

test_that(".clean_career_lattice_level handles case insensitivity", {
  fn <- ALccdfDB:::.clean_career_lattice_level

  expect_equal(fn("level i"), "Level I")
  expect_equal(fn("LEVEL II"), "Level II")
  expect_equal(fn("level iii"), "Level III")
})

test_that(".clean_career_lattice_level handles named levels", {
  fn <- ALccdfDB:::.clean_career_lattice_level

  expect_equal(fn("beginning"), "Beginning")
  expect_equal(fn("INITIAL"), "Initial")
  expect_equal(fn("associate"), "Associate")
  expect_equal(fn("professional"), "Professional")
  expect_equal(fn("master"), "Master")
  expect_equal(fn("advanced"), "Advanced")
})

test_that(".clean_career_lattice_level handles NA and empty strings", {
  fn <- ALccdfDB:::.clean_career_lattice_level

  expect_true(is.na(fn(NA_character_)))
  expect_true(is.na(fn("")))
})

test_that(".clean_career_lattice_level squishes whitespace", {
  fn <- ALccdfDB:::.clean_career_lattice_level

  expect_equal(fn("  Level   I  "), "Level I")
  expect_equal(fn("Level  III"), "Level III")
})

test_that(".clean_career_lattice_level is vectorized", {
  fn <- ALccdfDB:::.clean_career_lattice_level

  result <- fn(c("Level I", "beginning", NA, "Level V"))
  expect_equal(result, c("Level I", "Beginning", NA, "Level V"))
})


# ==============================================================================
# 8. Internal functions: .parse_logical_field()
# ==============================================================================

test_that(".parse_logical_field converts Yes/No correctly", {
  fn <- ALccdfDB:::.parse_logical_field

  expect_true(fn("Yes"))
  expect_false(fn("No"))
  expect_true(fn("yes"))
  expect_false(fn("no"))
})

test_that(".parse_logical_field converts True/False correctly", {
  fn <- ALccdfDB:::.parse_logical_field

  expect_true(fn("True"))
  expect_false(fn("False"))
  expect_true(fn("TRUE"))
  expect_false(fn("FALSE"))
})

test_that(".parse_logical_field converts 1/0 correctly", {
  fn <- ALccdfDB:::.parse_logical_field

  expect_true(fn("1"))
  expect_false(fn("0"))
})

test_that(".parse_logical_field converts Y/N and T/F correctly", {
  fn <- ALccdfDB:::.parse_logical_field

  expect_true(fn("Y"))
  expect_false(fn("N"))
  expect_true(fn("T"))
  expect_false(fn("F"))
})

test_that(".parse_logical_field handles NA and empty strings", {
  fn <- ALccdfDB:::.parse_logical_field

  expect_true(is.na(fn(NA_character_)))
  expect_true(is.na(fn("")))
})

test_that(".parse_logical_field returns NA for unrecognised values", {
  fn <- ALccdfDB:::.parse_logical_field

  expect_true(is.na(fn("maybe")))
  expect_true(is.na(fn("unknown")))
})

test_that(".parse_logical_field is vectorized", {
  fn <- ALccdfDB:::.parse_logical_field

  result <- fn(c("Yes", "No", "True", "False", "1", "0", NA))
  expect_equal(result, c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, NA))
})

test_that(".parse_logical_field handles leading/trailing whitespace", {
  fn <- ALccdfDB:::.parse_logical_field

  expect_true(fn("  Yes  "))
  expect_false(fn("  No  "))
})


# ==============================================================================
# 9. Error handling and edge cases
# ==============================================================================

test_that("staff_validate detects PII leak when staff_name is in main data", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  # Deliberately inject PII into main data
  clean$data$staff_name <- "Leaked Name"

  val <- staff_validate(clean, strict = FALSE, verbose = FALSE)
  pii_check <- val$checks[val$checks$check_id == "pii_separation", ]
  expect_equal(pii_check$status, "ERROR")
  expect_true(pii_check$n_issues >= 1)
})

test_that("staff_validate detects PII leak when user_email is in main data", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  # Deliberately inject PII into main data
  clean$data$user_email <- "leaked@test.com"

  val <- staff_validate(clean, strict = FALSE, verbose = FALSE)
  pii_check <- val$checks[val$checks$check_id == "pii_separation", ]
  expect_equal(pii_check$status, "ERROR")
})

test_that("staff_summary_stats works on clean object", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  # Should not error
  stats <- staff_summary_stats(clean, verbose = FALSE)
  expect_true(is.list(stats))
  expect_equal(stats$overview$n_staff, 5)
})


# ==============================================================================
# 10. Export roundtrip with data integrity verification
# ==============================================================================

test_that("staff_export_csv roundtrip preserves column count (without PII)", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  csv_path <- file.path(tmpdir, "roundtrip.csv")
  staff_export_csv(clean, csv_path, include_pii = FALSE, verbose = FALSE)

  df_read <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  expect_equal(nrow(df_read), nrow(clean$data))
  expect_equal(ncol(df_read), ncol(clean$data))
})

test_that("staff_export_csv roundtrip with PII has extra columns", {
  raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE, verbose = FALSE)
  clean <- staff_clean(raw, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  csv_path <- file.path(tmpdir, "roundtrip_pii.csv")
  staff_export_csv(clean, csv_path, include_pii = TRUE, verbose = FALSE)

  df_read <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  # With PII: should have main data cols + PII cols (staff_name, user_email)
  expect_equal(nrow(df_read), 5)
  n_pii_cols <- clean$meta$n_pii_fields
  expect_equal(ncol(df_read), ncol(clean$data) + n_pii_cols)
})
