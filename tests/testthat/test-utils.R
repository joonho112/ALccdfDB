# tests/testthat/test-utils.R
# Comprehensive unit tests for all internal utility functions in ALccdfDB.
# Uses testthat edition 3.

# ==============================================================================
# 1. .standardize_address()
# ==============================================================================

test_that(".standardize_address standardizes road types", {
  fn <- ALccdfDB:::.standardize_address

  # Full road type words should be abbreviated and uppercased

  result <- fn("123 Main Street, Montgomery, AL 36101")
  expect_true(grepl("ST", result))
  expect_false(grepl("Street", result, ignore.case = FALSE))

  result_ave <- fn("456 Oak Avenue, Birmingham, AL 35201")
  expect_true(grepl("AVE", result_ave))

  result_rd <- fn("789 Pine Road, Huntsville, AL 35801")
  expect_true(grepl("RD", result_rd))

  result_blvd <- fn("100 Elm Boulevard, Selma, AL 36701")
  expect_true(grepl("BLVD", result_blvd))

  result_dr <- fn("200 Cherry Drive, Auburn, AL 36830")
  expect_true(grepl("DR", result_dr))

  result_ln <- fn("300 Birch Lane, Dothan, AL 36301")
  expect_true(grepl("LN", result_ln))
})

test_that(".standardize_address handles directionals", {
  fn <- ALccdfDB:::.standardize_address

  result_nw <- fn("100 NW 1st Street, City")
  expect_true(grepl("NW", result_nw))

  result_ne <- fn("200 NE Oak Avenue, City")
  expect_true(grepl("NE", result_ne))

  result_sw <- fn("300 SW Pine Road, City")
  expect_true(grepl("SW", result_sw))
})

test_that(".standardize_address handles empty input", {
  fn <- ALccdfDB:::.standardize_address
  expect_identical(fn(character(0)), character(0))
})

test_that(".standardize_address handles trailing county removal", {
  fn <- ALccdfDB:::.standardize_address

  # Trailing ", CountyName" at end should be removed

  result <- fn("123 Main Street, Montgomery, Jefferson")
  # "Jefferson" (trailing county name) should be removed
  expect_false(grepl("Jefferson", result))
})

test_that(".standardize_address squishes whitespace", {
  fn <- ALccdfDB:::.standardize_address
  result <- fn("  123   Main    Street  ,  City  ")
  expect_false(grepl("  ", result))
})

test_that(".standardize_address applies title case then uppercases abbreviations", {
  fn <- ALccdfDB:::.standardize_address
  result <- fn("123 main street, montgomery")
  # Should have title case but with abbreviation uppercased
  expect_true(grepl("ST", result))
  expect_true(grepl("123 Main", result))
})

test_that(".standardize_address uppercases AL state abbreviation", {
  fn <- ALccdfDB:::.standardize_address
  result <- fn("123 Main Street, Montgomery, AL 36101")
  expect_true(grepl("AL", result))
})

test_that(".standardize_address is vectorized", {
  fn <- ALccdfDB:::.standardize_address
  result <- fn(c("123 Main Street", "456 Oak Avenue"))
  expect_length(result, 2)
})


# ==============================================================================
# 2. .parse_time_string()
# ==============================================================================

test_that(".parse_time_string parses HH:MM:SS format", {
  fn <- ALccdfDB:::.parse_time_string
  result <- fn("06:00:00")
  expect_equal(result, "06:00:00")
})

test_that(".parse_time_string parses HH:MM format", {
  fn <- ALccdfDB:::.parse_time_string
  result <- fn("14:30")
  expect_equal(result, "14:30:00")
})

test_that(".parse_time_string parses AM/PM format", {
  fn <- ALccdfDB:::.parse_time_string
  result <- fn("6:00 AM")
  expect_equal(result, "06:00:00")

  result_pm <- fn("6:00 PM")
  expect_equal(result_pm, "18:00:00")
})

test_that(".parse_time_string handles all NA input", {
  fn <- ALccdfDB:::.parse_time_string
  result <- fn(c(NA, NA))
  expect_true(all(is.na(result)))
})

test_that(".parse_time_string is vectorized", {
  fn <- ALccdfDB:::.parse_time_string
  result <- fn(c("06:00:00", "18:00:00"))
  expect_length(result, 2)
  expect_equal(result, c("06:00:00", "18:00:00"))
})


# ==============================================================================
# 3. .parse_age_range()
# ==============================================================================

test_that(".parse_age_range extracts min and max from standard text", {
  fn <- ALccdfDB:::.parse_age_range
  result <- fn("0.25 - 12 Years")
  expect_s3_class(result, "tbl_df")
  expect_equal(result$age_min, 0.25)
  expect_equal(result$age_max, 12)
})

test_that(".parse_age_range handles whole numbers", {
  fn <- ALccdfDB:::.parse_age_range
  result <- fn("0 - 5 Years")
  expect_equal(result$age_min, 0)
  expect_equal(result$age_max, 5)
})

test_that(".parse_age_range handles age range without 'Years'", {
  fn <- ALccdfDB:::.parse_age_range
  result <- fn("3 - 5")
  expect_equal(result$age_min, 3)
  expect_equal(result$age_max, 5)
})

test_that(".parse_age_range handles NA input", {
  fn <- ALccdfDB:::.parse_age_range
  result <- fn(NA_character_)
  expect_true(is.na(result$age_min))
  expect_true(is.na(result$age_max))
})

test_that(".parse_age_range is vectorized", {
  fn <- ALccdfDB:::.parse_age_range
  result <- fn(c("0 - 5 Years", "3 - 12 Years"))
  expect_equal(nrow(result), 2)
  expect_equal(result$age_min, c(0, 3))
  expect_equal(result$age_max, c(5, 12))
})


# ==============================================================================
# 4. .make_alccdf_obj()
# ==============================================================================

test_that(".make_alccdf_obj creates S3 object with correct structure", {
  fn <- ALccdfDB:::.make_alccdf_obj
  df <- data.frame(x = 1:3, y = letters[1:3])
  obj <- fn(df, class_name = "test_class", module = "test", stage = "raw")

  expect_s3_class(obj, "test_class")
  expect_true(is.list(obj))
  expect_true("data" %in% names(obj))
  expect_true("meta" %in% names(obj))
  expect_true("diagnostics" %in% names(obj))
})

test_that(".make_alccdf_obj data is converted to tibble", {
  fn <- ALccdfDB:::.make_alccdf_obj
  df <- data.frame(x = 1:3)
  obj <- fn(df, class_name = "tc", module = "test", stage = "raw")
  expect_s3_class(obj$data, "tbl_df")
})

test_that(".make_alccdf_obj meta contains expected fields", {
  fn <- ALccdfDB:::.make_alccdf_obj
  df <- data.frame(x = 1:5)
  snap <- as.Date("2025-09-12")
  obj <- fn(df, class_name = "tc", module = "prog", stage = "clean",
            snapshot_date = snap, extra_meta = list(program_type = "center"))

  expect_equal(obj$meta$module, "prog")
  expect_equal(obj$meta$stage, "clean")
  expect_equal(obj$meta$snapshot_date, snap)
  expect_equal(obj$meta$n_rows, 5)
  expect_equal(obj$meta$n_cols, 1)
  expect_equal(obj$meta$program_type, "center")
  expect_true(!is.null(obj$meta$created_at))
  expect_true(!is.null(obj$meta$package_version))
  expect_equal(obj$meta$processing_log, character())
})

test_that(".make_alccdf_obj stores diagnostics", {
  fn <- ALccdfDB:::.make_alccdf_obj
  df <- data.frame(x = 1)
  diag <- list(removed = data.frame(a = 1))
  obj <- fn(df, "tc", "test", "raw", diagnostics = diag)
  expect_equal(obj$diagnostics, diag)
})


# ==============================================================================
# 5. .assert_class()
# ==============================================================================

test_that(".assert_class passes for correct class", {
  fn <- ALccdfDB:::.assert_class
  obj <- structure(list(), class = "my_class")
  expect_true(fn(obj, "my_class"))
})

test_that(".assert_class errors for wrong class", {
  fn <- ALccdfDB:::.assert_class
  obj <- structure(list(), class = "wrong_class")
  expect_error(fn(obj, "expected_class"), class = "rlang_error")
})

test_that(".assert_class error message includes fn_name when provided", {
  fn <- ALccdfDB:::.assert_class
  obj <- structure(list(), class = "wrong_class")
  expect_error(
    fn(obj, "expected_class", fn_name = "my_function"),
    "my_function"
  )
})

test_that(".assert_class error message includes actual class", {
  fn <- ALccdfDB:::.assert_class
  obj <- structure(list(), class = "actual_class")
  expect_error(fn(obj, "expected_class"), "actual_class")
})


# ==============================================================================
# 6. .log_step()
# ==============================================================================

test_that(".log_step appends message to processing_log", {
  fn_make <- ALccdfDB:::.make_alccdf_obj
  fn_log <- ALccdfDB:::.log_step

  obj <- fn_make(data.frame(x = 1), "tc", "test", "raw")
  expect_length(obj$meta$processing_log, 0)

  obj <- fn_log(obj, "Step 1 done")
  expect_length(obj$meta$processing_log, 1)
  expect_true(grepl("Step 1 done", obj$meta$processing_log[1]))
})

test_that(".log_step prepends timestamp", {
  fn_make <- ALccdfDB:::.make_alccdf_obj
  fn_log <- ALccdfDB:::.log_step

  obj <- fn_make(data.frame(x = 1), "tc", "test", "raw")
  obj <- fn_log(obj, "test message")
  expect_true(grepl("^\\[\\d{4}-\\d{2}-\\d{2}", obj$meta$processing_log[1]))
})

test_that(".log_step accumulates multiple entries", {
  fn_make <- ALccdfDB:::.make_alccdf_obj
  fn_log <- ALccdfDB:::.log_step

  obj <- fn_make(data.frame(x = 1), "tc", "test", "raw")
  obj <- fn_log(obj, "Step 1")
  obj <- fn_log(obj, "Step 2")
  obj <- fn_log(obj, "Step 3")
  expect_length(obj$meta$processing_log, 3)
})


# ==============================================================================
# 7. .generate_random_ids()
# ==============================================================================

test_that(".generate_random_ids returns correct length", {
  fn <- ALccdfDB:::.generate_random_ids
  result <- fn(prefix = "ID", n = 10, width = 5)
  expect_length(result, 10)
})

test_that(".generate_random_ids applies prefix", {
  fn <- ALccdfDB:::.generate_random_ids
  result <- fn(prefix = "FAC", n = 3, width = 4)
  expect_true(all(grepl("^FAC", result)))
})

test_that(".generate_random_ids generates unique values", {
  fn <- ALccdfDB:::.generate_random_ids
  result <- fn(prefix = "ID", n = 50, width = 5)
  expect_equal(length(unique(result)), 50)
})

test_that(".generate_random_ids respects width", {
  fn <- ALccdfDB:::.generate_random_ids
  result <- fn(prefix = "", n = 5, width = 3)
  # Each should have exactly 3 digits
  expect_true(all(nchar(result) == 3))
})

test_that(".generate_random_ids single ID", {
  fn <- ALccdfDB:::.generate_random_ids
  result <- fn()
  expect_length(result, 1)
  expect_true(grepl("^ID", result))
})


# ==============================================================================
# 8. alccdf_data() / alccdf_meta()
# ==============================================================================

test_that("alccdf_data extracts data tibble from S3 object", {
  obj <- ALccdfDB:::.make_alccdf_obj(
    data.frame(a = 1:3), "tc", "test", "raw"
  )
  result <- alccdf_data(obj)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
})

test_that("alccdf_data errors on invalid input", {
  expect_error(alccdf_data(list(wrong = 1)), "data")
  expect_error(alccdf_data("not a list"), "data")
})

test_that("alccdf_meta extracts meta list from S3 object", {
  obj <- ALccdfDB:::.make_alccdf_obj(
    data.frame(a = 1), "tc", "test", "raw",
    snapshot_date = as.Date("2025-01-01")
  )
  result <- alccdf_meta(obj)
  expect_true(is.list(result))
  expect_equal(result$module, "test")
  expect_equal(result$stage, "raw")
  expect_equal(result$snapshot_date, as.Date("2025-01-01"))
})

test_that("alccdf_meta errors on invalid input", {
  expect_error(alccdf_meta(list(wrong = 1)), "meta")
  expect_error(alccdf_meta(42), "meta")
})


# ==============================================================================
# 9. .make_check() / .build_validation_result()
# ==============================================================================

test_that(".make_check returns one-row tibble with correct columns", {
  fn <- ALccdfDB:::.make_check
  result <- fn("test_check", "A test check", "PASS")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$check_id, "test_check")
  expect_equal(result$description, "A test check")
  expect_equal(result$status, "PASS")
  expect_equal(result$n_issues, 0L)
  expect_true(is.na(result$detail))
})

test_that(".make_check stores n_issues and detail", {
  fn <- ALccdfDB:::.make_check
  result <- fn("ck1", "Check 1", "ERROR", n_issues = 5L, detail = "col_a, col_b")
  expect_equal(result$n_issues, 5L)
  expect_equal(result$detail, "col_a, col_b")
})

test_that(".make_check rejects invalid status", {
  fn <- ALccdfDB:::.make_check
  expect_error(fn("ck1", "desc", "INVALID"))
})

test_that(".make_check accepts all valid statuses", {
  fn <- ALccdfDB:::.make_check
  for (s in c("PASS", "ERROR", "WARN", "INFO")) {
    result <- fn("ck", "desc", s)
    expect_equal(result$status, s)
  }
})

test_that(".build_validation_result creates correct structure", {
  fn_check <- ALccdfDB:::.make_check
  fn_build <- ALccdfDB:::.build_validation_result

  checks <- dplyr::bind_rows(
    fn_check("ck1", "Check 1", "PASS"),
    fn_check("ck2", "Check 2", "WARN", n_issues = 2L)
  )
  result <- fn_build(checks, module = "test", snapshot_date = as.Date("2025-01-01"))

  expect_true(result$passed)
  expect_equal(result$n_errors, 0)
  expect_equal(result$n_warnings, 1)
  expect_true(is.list(result$meta))
  expect_equal(result$meta$module, "test")
})

test_that(".build_validation_result passes when errors exist but strict=FALSE", {
  fn_check <- ALccdfDB:::.make_check
  fn_build <- ALccdfDB:::.build_validation_result

  checks <- dplyr::bind_rows(
    fn_check("ck1", "Error check", "ERROR", n_issues = 1L)
  )
  result <- fn_build(checks, module = "test", strict = FALSE)
  # passed should be TRUE because strict is FALSE
  expect_true(result$passed)
  expect_equal(result$n_errors, 1)
})

test_that(".build_validation_result errors in strict mode with errors", {
  fn_check <- ALccdfDB:::.make_check
  fn_build <- ALccdfDB:::.build_validation_result

  checks <- dplyr::bind_rows(
    fn_check("ck1", "Error check", "ERROR", n_issues = 1L)
  )
  expect_error(
    fn_build(checks, module = "test", strict = TRUE),
    "strict"
  )
})

test_that(".build_validation_result uses custom class_name", {
  fn_check <- ALccdfDB:::.make_check
  fn_build <- ALccdfDB:::.build_validation_result

  checks <- fn_check("ck1", "desc", "PASS")
  result <- fn_build(checks, module = "test",
                     class_name = "my_custom_validation")
  expect_s3_class(result, "my_custom_validation")
})

test_that(".build_validation_result generates default class_name", {
  fn_check <- ALccdfDB:::.make_check
  fn_build <- ALccdfDB:::.build_validation_result

  checks <- fn_check("ck1", "desc", "PASS")
  result <- fn_build(checks, module = "program")
  expect_s3_class(result, "alccdf_program_validation")
})


# ==============================================================================
# 10. Messaging functions
# ==============================================================================

test_that(".msg_step produces output", {
  fn <- ALccdfDB:::.msg_step
  expect_message(fn("test step"), regexp = NULL)
})

test_that(".msg_success produces output", {
  fn <- ALccdfDB:::.msg_success
  expect_message(fn("success msg"), regexp = NULL)
})

test_that(".msg_warn produces output", {
  fn <- ALccdfDB:::.msg_warn
  expect_message(fn("warning msg"), regexp = NULL)
})

test_that(".msg_error produces output", {
  fn <- ALccdfDB:::.msg_error
  expect_message(fn("error msg"), regexp = NULL)
})

test_that(".msg_header produces output at each level", {
  fn <- ALccdfDB:::.msg_header
  for (lvl in 1:3) {
    expect_message(fn("header msg", level = lvl), regexp = NULL)
  }
})


# ==============================================================================
# 11. .standardize_address_v2()
# ==============================================================================

test_that(".standardize_address_v2 further standardizes address", {
  fn <- ALccdfDB:::.standardize_address_v2

  # Input is already through basic standardization (title-cased, abbreviated)
  result <- fn("123 Main ST, Montgomery, AL 36101")
  expect_true(grepl("ST", result))
  # Should be further standardized
  expect_true(is.character(result))
})

test_that(".standardize_address_v2 removes trailing county", {
  fn <- ALccdfDB:::.standardize_address_v2
  result <- fn("123 Main ST, Montgomery")
  # Trailing county name removed
  expect_false(grepl("Montgomery$", result))
})

test_that(".standardize_address_v2 handles NULL", {
  fn <- ALccdfDB:::.standardize_address_v2
  expect_null(fn(NULL))
})

test_that(".standardize_address_v2 uppercases road types and directions", {
  fn <- ALccdfDB:::.standardize_address_v2
  result <- fn("100 Oak Drive NW, Mobile")
  expect_true(grepl("DR", result))
  expect_true(grepl("NW", result))
})

test_that(".standardize_address_v2 standardizes suite", {
  fn <- ALccdfDB:::.standardize_address_v2
  result <- fn("100 Main Street Suite 200")
  expect_true(grepl("STE 200", result))
})


# ==============================================================================
# 12. .time_diff_hours()
# ==============================================================================

test_that(".time_diff_hours computes simple time difference", {
  fn <- ALccdfDB:::.time_diff_hours
  result <- fn("06:00:00", "18:00:00")
  expect_equal(result, 12)
})

test_that(".time_diff_hours handles same time", {
  fn <- ALccdfDB:::.time_diff_hours
  result <- fn("12:00:00", "12:00:00")
  expect_equal(result, 0)
})

test_that(".time_diff_hours applies overnight correction", {
  fn <- ALccdfDB:::.time_diff_hours
  # Night shift: 22:00 to 06:00 = 8 hours
  result <- fn("22:00:00", "06:00:00")
  expect_equal(result, 8)
})

test_that(".time_diff_hours handles another overnight scenario", {
  fn <- ALccdfDB:::.time_diff_hours
  # 19:00 to 06:00 = 11 hours
  result <- fn("19:00:00", "06:00:00")
  expect_equal(result, 11)
})

test_that(".time_diff_hours handles minutes and seconds", {
  fn <- ALccdfDB:::.time_diff_hours
  result <- fn("06:30:00", "17:30:00")
  expect_equal(result, 11)
})

test_that(".time_diff_hours handles NA", {
  fn <- ALccdfDB:::.time_diff_hours
  result <- fn(NA_character_, "18:00:00")
  expect_true(is.na(result))
})

test_that(".time_diff_hours is vectorized", {
  fn <- ALccdfDB:::.time_diff_hours
  result <- fn(c("06:00:00", "22:00:00"), c("18:00:00", "06:00:00"))
  expect_equal(result, c(12, 8))
})


# ==============================================================================
# 13. .recode_facility_tier()
# ==============================================================================

test_that(".recode_facility_tier recodes star values to ordered factor", {
  fn <- ALccdfDB:::.recode_facility_tier
  result <- fn(c("Star 1", "Star 3", "Star 5"))
  expect_s3_class(result, "factor")
  expect_true(is.ordered(result))
  expect_equal(levels(result), c("Star 1", "Star 2", "Star 3", "Star 4", "Star 5"))
  expect_equal(as.character(result), c("Star 1", "Star 3", "Star 5"))
})

test_that(".recode_facility_tier maps 'None' to NA", {
  fn <- ALccdfDB:::.recode_facility_tier
  result <- fn(c("None", "Star 1"))
  expect_true(is.na(result[1]))
  expect_equal(as.character(result[2]), "Star 1")
})

test_that(".recode_facility_tier maps empty string to NA", {
  fn <- ALccdfDB:::.recode_facility_tier
  result <- fn(c("", "Star 2"))
  expect_true(is.na(result[1]))
})

test_that(".recode_facility_tier maps plain numbers to Star levels", {
  fn <- ALccdfDB:::.recode_facility_tier
  result <- fn(c("1", "2", "3", "4", "5"))
  expect_equal(as.character(result), paste("Star", 1:5))
})

test_that(".recode_facility_tier handles NA input", {
  fn <- ALccdfDB:::.recode_facility_tier
  result <- fn(c(NA, "Star 3"))
  expect_true(is.na(result[1]))
  expect_equal(as.character(result[2]), "Star 3")
})


# ==============================================================================
# 14. .parse_program_date()
# ==============================================================================

test_that(".parse_program_date parses ISO date strings", {
  fn <- ALccdfDB:::.parse_program_date
  result <- fn("2026-06-15")
  expect_s3_class(result, "Date")
  expect_equal(result, as.Date("2026-06-15"))
})

test_that(".parse_program_date parses Excel serial numbers", {
  fn <- ALccdfDB:::.parse_program_date
  # Excel serial 45814 = 2025-06-15
  result <- fn("45814")
  expect_s3_class(result, "Date")
  # Serial 45814 from origin 1899-12-30
  expect_equal(result, as.Date(45814, origin = "1899-12-30"))
})

test_that(".parse_program_date handles already Date input", {
  fn <- ALccdfDB:::.parse_program_date
  d <- as.Date("2025-06-15")
  result <- fn(d)
  expect_equal(result, d)
})

test_that(".parse_program_date handles already POSIXct input", {
  fn <- ALccdfDB:::.parse_program_date
  dt <- as.POSIXct("2025-06-15 12:00:00")
  result <- fn(dt)
  expect_equal(result, as.Date("2025-06-15"))
})

test_that(".parse_program_date handles all NA", {
  fn <- ALccdfDB:::.parse_program_date
  result <- fn(c(NA, NA))
  expect_true(all(is.na(result)))
})

test_that(".parse_program_date is vectorized", {
  fn <- ALccdfDB:::.parse_program_date
  result <- fn(c("2026-06-15", "2026-03-22"))
  expect_length(result, 2)
  expect_equal(result, as.Date(c("2026-06-15", "2026-03-22")))
})


# ==============================================================================
# 15. alccdf_facility_type_levels() and alccdf_facility_tier_levels()
# ==============================================================================

test_that("alccdf_facility_type_levels returns correct canonical levels", {
  result <- alccdf_facility_type_levels()
  expect_equal(result, c("Center", "Family Home", "Group Home",
                          "Faith-Based", "Excepted (University/Other)"))
  expect_length(result, 5)
})

test_that("alccdf_facility_tier_levels returns correct ordered levels", {
  result <- alccdf_facility_tier_levels()
  expect_equal(result, c("Star 1", "Star 2", "Star 3", "Star 4", "Star 5"))
  expect_length(result, 5)
})
