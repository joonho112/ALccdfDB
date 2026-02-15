# tests/testthat/test-melissa.R
# Unit tests for the melissa module: address normalisation, program merge,
# and household merge.
# Uses testthat edition 3 with fixture files in tests/testthat/fixtures/.

# Shared snapshot date used consistently across all tests.
SNAPSHOT_DATE <- as.Date("2025-09-12")

# Helper: path to a fixture file
fixture_path <- function(filename) {
  testthat::test_path("fixtures", filename)
}


# ==============================================================================
# 1. .normalise_address_for_join() - Internal address normalisation
# ==============================================================================

test_that(".normalise_address_for_join uppercases everything", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn("123 main street, montgomery, al 36101")
  expect_true(result == toupper(result))
  expect_true(grepl("123 MAIN", result))
})

test_that(".normalise_address_for_join removes trailing county name", {
  fn <- ALccdfDB:::.normalise_address_for_join
  # Melissa format: "123 MAIN STREET,MONTGOMERY,AL 36101,Montgomery"
  result <- fn("123 MAIN STREET,MONTGOMERY,AL 36101,Montgomery")
  expect_false(grepl("Montgomery$", result))
  # Should end with zip code portion
  expect_true(grepl("36101", result))
})

test_that(".normalise_address_for_join removes trailing county with various names", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result_jeff <- fn("456 OAK AVENUE,BIRMINGHAM,AL 35201,Jefferson")
  expect_false(grepl("JEFFERSON$", result_jeff))

  result_mad <- fn("789 PINE ROAD,HUNTSVILLE,AL 35801,Madison")
  expect_false(grepl("MADISON$", result_mad))
})

test_that(".normalise_address_for_join abbreviates STREET to ST", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn("123 Main Street, Montgomery, AL 36101")
  expect_true(grepl("\\bST\\b", result))
  expect_false(grepl("STREET", result))
})

test_that(".normalise_address_for_join abbreviates AVENUE to AVE", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn("456 Oak Avenue, Birmingham, AL 35201")
  expect_true(grepl("\\bAVE\\b", result))
  expect_false(grepl("AVENUE", result))
})

test_that(".normalise_address_for_join abbreviates ROAD to RD", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn("789 Pine Road, Huntsville, AL 35801")
  expect_true(grepl("\\bRD\\b", result))
  expect_false(grepl("ROAD", result))
})

test_that(".normalise_address_for_join abbreviates DRIVE to DR", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn("200 Cherry Drive, Auburn, AL 36830")
  expect_true(grepl("\\bDR\\b", result))
  expect_false(grepl("DRIVE", result))
})

test_that(".normalise_address_for_join abbreviates LANE to LN", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn("300 Birch Lane, Dothan, AL 36301")
  expect_true(grepl("\\bLN\\b", result))
  expect_false(grepl("LANE", result))
})

test_that(".normalise_address_for_join abbreviates BOULEVARD to BLVD", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn("100 Elm Boulevard, Selma, AL 36701")
  expect_true(grepl("\\bBLVD\\b", result))
  expect_false(grepl("BOULEVARD", result))
})

test_that(".normalise_address_for_join abbreviates COURT to CT", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn("400 Maple Court, Decatur, AL 35601")
  expect_true(grepl("\\bCT\\b", result))
  expect_false(grepl("COURT", result))
})

test_that(".normalise_address_for_join abbreviates CIRCLE to CIR", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn("500 Oak Circle, Florence, AL 35630")
  expect_true(grepl("\\bCIR\\b", result))
  expect_false(grepl("CIRCLE", result))
})

test_that(".normalise_address_for_join abbreviates HIGHWAY to HWY", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn("600 Highway 31, Decatur, AL 35601")
  expect_true(grepl("\\bHWY\\b", result))
  expect_false(grepl("HIGHWAY", result))
})

test_that(".normalise_address_for_join abbreviates SUITE to STE", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn("100 Main Street Suite 200, Montgomery, AL 36101")
  expect_true(grepl("\\bSTE\\b", result))
  expect_false(grepl("SUITE", result))
})

test_that(".normalise_address_for_join abbreviates APARTMENT to APT", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn("100 Main Street Apartment 3A, Montgomery, AL 36101")
  expect_true(grepl("\\bAPT\\b", result))
  expect_false(grepl("APARTMENT", result))
})

test_that(".normalise_address_for_join abbreviates BUILDING to BLDG", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn("100 Main Street Building C, Montgomery, AL 36101")
  expect_true(grepl("\\bBLDG\\b", result))
  expect_false(grepl("BUILDING", result))
})

test_that(".normalise_address_for_join abbreviates cardinal directions", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result_n <- fn("100 North Main Street")
  expect_true(grepl("\\bN\\b", result_n))
  expect_false(grepl("NORTH", result_n))

  result_s <- fn("100 South Main Street")
  expect_true(grepl("\\bS\\b", result_s))
  expect_false(grepl("SOUTH", result_s))

  result_e <- fn("100 East Main Street")
  expect_true(grepl("\\bE\\b", result_e))
  expect_false(grepl("EAST", result_e))

  result_w <- fn("100 West Main Street")
  expect_true(grepl("\\bW\\b", result_w))
  expect_false(grepl("WEST", result_w))
})

test_that(".normalise_address_for_join preserves directional abbreviations", {
  fn <- ALccdfDB:::.normalise_address_for_join
  # NW, NE, SW, SE should pass through (they are not full words)
  result_nw <- fn("100 NW 1st Street, City, AL 36101")
  expect_true(grepl("NW", result_nw))

  result_ne <- fn("200 NE Oak Avenue, City, AL 36101")
  expect_true(grepl("NE", result_ne))

  result_sw <- fn("300 SW Pine Road, City, AL 36101")
  expect_true(grepl("SW", result_sw))

  result_se <- fn("400 SE Elm Drive, City, AL 36101")
  expect_true(grepl("SE", result_se))
})

test_that(".normalise_address_for_join collapses whitespace", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn("  123   Main   Street ,  Montgomery ,  AL   36101  ")
  # No double spaces

  expect_false(grepl("  ", result))
  # Leading/trailing whitespace trimmed
  expect_equal(result, trimws(result))
})

test_that(".normalise_address_for_join removes punctuation", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn("123 Main St., Montgomery, AL 36101")
  # Periods and commas become spaces
  expect_false(grepl("[.,]", result))
})

test_that(".normalise_address_for_join handles NA input", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn(NA_character_)
  expect_true(is.na(result))
})

test_that(".normalise_address_for_join handles vector with NAs", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn(c("123 Main Street", NA, "456 Oak Avenue"))
  expect_length(result, 3)
  expect_false(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_false(is.na(result[3]))
})

test_that(".normalise_address_for_join handles empty string", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn("")
  expect_equal(result, "")
})

test_that(".normalise_address_for_join handles NULL input", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn(NULL)
  expect_null(result)
})

test_that(".normalise_address_for_join is vectorized", {
  fn <- ALccdfDB:::.normalise_address_for_join
  result <- fn(c("123 Main Street", "456 Oak Avenue"))
  expect_length(result, 2)
})

test_that(".normalise_address_for_join produces same key for DHR and Melissa formats", {
  fn <- ALccdfDB:::.normalise_address_for_join

  # DHR format (program data): "123 Main Street, Montgomery, AL 36101"
  dhr <- fn("123 Main Street, Montgomery, AL 36101")

  # Melissa format: "123 MAIN STREET,MONTGOMERY,AL 36101,Montgomery"
  mel <- fn("123 MAIN STREET,MONTGOMERY,AL 36101,Montgomery")

  expect_equal(dhr, mel,
    info = "DHR and Melissa addresses must normalise to the same key")
})

test_that(".normalise_address_for_join matches all fixture addresses", {
  fn <- ALccdfDB:::.normalise_address_for_join

  # Center fixture addresses (DHR format)
  dhr_addrs <- c(
    "123 Main Street, Montgomery, AL 36101",
    "456 Oak Avenue, Birmingham, AL 35201",
    "789 Pine Road, Huntsville, AL 35801"
  )

  # Corresponding Melissa fixture addresses
  mel_addrs <- c(
    "123 MAIN STREET,MONTGOMERY,AL 36101,Montgomery",
    "456 OAK AVENUE,BIRMINGHAM,AL 35201,Jefferson",
    "789 PINE ROAD,HUNTSVILLE,AL 35801,Madison"
  )

  dhr_keys <- fn(dhr_addrs)
  mel_keys <- fn(mel_addrs)

  expect_equal(dhr_keys, mel_keys,
    info = "All 3 fixture program addresses must normalise to matching keys")
})


# ==============================================================================
# 2. melissa_merge_programs() - Happy path
# ==============================================================================

# Helper: create a temporary Melissa RDS in the raw format expected by
# .prepare_melissa_geo() (columns: address, LAT, LNG, CT, etc.)
make_melissa_programs_rds <- function(tmpdir) {
  mel_df <- data.frame(
    address = c(
      "123 MAIN STREET,MONTGOMERY,AL 36101,Montgomery",
      "456 OAK AVENUE,BIRMINGHAM,AL 35201,Jefferson",
      "789 PINE ROAD,HUNTSVILLE,AL 35801,Madison"
    ),
    LAT = c(32.3668, 33.5207, 34.7304),
    LNG = c(-86.2999, -86.8025, -86.5861),
    CT = c(10101, 10201, 10301),
    CENSUSBLOC = c(1001, 1002, 1003),
    FIPS = c(1101, 1073, 1089),
    COUNTYNAME = c("Montgomery", "Jefferson", "Madison"),
    PLACENAME = c("Montgomery", "Birmingham", "Huntsville"),
    PLACECODE = c(51000, 7000, 37000),
    MD_City = c("Montgomery", "Birmingham", "Huntsville"),
    MD_State = c("AL", "AL", "AL"),
    MD_PostalCode = c("36101-1234", "35201-5678", "35801-9012"),
    GEOZIP = c(36101, 35201, 35801),
    RESULTCODE = c("GS05", "GS05", "GS03"),
    STATUSCODE = c("B", "B", "5"),
    stringsAsFactors = FALSE
  )
  path <- file.path(tmpdir, "melissa_programs.rds")
  saveRDS(mel_df, path)
  path
}

# Helper: create a temporary Melissa RDS for households
make_melissa_households_rds <- function(tmpdir) {
  mel_df <- data.frame(
    address = c(
      "10 OAK STREET,MONTGOMERY,AL 36104,Montgomery",
      "20 PINE AVENUE,MOBILE,AL 36602,Mobile"
    ),
    LAT = c(32.37, 30.69),
    LNG = c(-86.30, -88.04),
    CT = c(20101, 20201),
    CENSUSBLOC = c(2001, 2002),
    FIPS = c(1101, 1097),
    COUNTYNAME = c("Montgomery", "Mobile"),
    PLACENAME = c("Montgomery", "Mobile"),
    PLACECODE = c(51000, 50000),
    MD_City = c("Montgomery", "Mobile"),
    MD_State = c("AL", "AL"),
    MD_PostalCode = c("36104-0001", "36602-0001"),
    GEOZIP = c(36104, 36602),
    RESULTCODE = c("GS05", "GS05"),
    STATUSCODE = c("B", "B"),
    stringsAsFactors = FALSE
  )
  path <- file.path(tmpdir, "melissa_households.rds")
  saveRDS(mel_df, path)
  path
}

test_that("melissa_merge_programs merges geocoding into deduped program data", {
  tmpdir <- withr::local_tempdir()
  mel_path <- make_melissa_programs_rds(tmpdir)

  # Build a program object through the pipeline: read -> clean -> append -> dedup
  raw <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  unified <- program_append_types(clean, verbose = FALSE)
  deduped <- program_deduplicate(unified, verbose = FALSE)

  # Merge
  result <- melissa_merge_programs(deduped, mel_path, verbose = FALSE)

  # Return type is the same input object (augmented), not a new class
  expect_true(inherits(result, "alccdf_program_deduped"))

  # Geocoding columns should now be present
  df <- alccdf_data(result)
  expect_true("latitude" %in% names(df))
  expect_true("longitude" %in% names(df))
  expect_true("census_tract" %in% names(df))
  expect_true("fips_code" %in% names(df))
  expect_true("melissa_county" %in% names(df))
  expect_true("melissa_city" %in% names(df))
  expect_true("melissa_result_code" %in% names(df))
})

test_that("melissa_merge_programs matched rows have non-NA latitude", {
  tmpdir <- withr::local_tempdir()
  mel_path <- make_melissa_programs_rds(tmpdir)

  raw <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  unified <- program_append_types(clean, verbose = FALSE)
  deduped <- program_deduplicate(unified, verbose = FALSE)

  result <- melissa_merge_programs(deduped, mel_path, verbose = FALSE)
  df <- alccdf_data(result)

  # All 3 center addresses should match the 3 melissa addresses
  n_matched <- sum(!is.na(df$latitude))
  expect_equal(n_matched, 3)

  # Latitudes should be within Alabama bounds
  lats <- df$latitude[!is.na(df$latitude)]
  expect_true(all(lats >= 30.14 & lats <= 35.01))
})

test_that("melissa_merge_programs records match stats in diagnostics", {
  tmpdir <- withr::local_tempdir()
  mel_path <- make_melissa_programs_rds(tmpdir)

  raw <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  unified <- program_append_types(clean, verbose = FALSE)
  deduped <- program_deduplicate(unified, verbose = FALSE)

  result <- melissa_merge_programs(deduped, mel_path, verbose = FALSE)

  expect_equal(result$diagnostics$melissa_n_matched, 3)
  expect_equal(result$diagnostics$melissa_n_unmatched, 0)
  expect_equal(result$diagnostics$melissa_match_rate, 1.0)
})

test_that("melissa_merge_programs preserves row count (left join)", {
  tmpdir <- withr::local_tempdir()
  mel_path <- make_melissa_programs_rds(tmpdir)

  raw <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  unified <- program_append_types(clean, verbose = FALSE)
  deduped <- program_deduplicate(unified, verbose = FALSE)

  n_before <- nrow(alccdf_data(deduped))
  result <- melissa_merge_programs(deduped, mel_path, verbose = FALSE)
  n_after <- nrow(alccdf_data(result))

  expect_equal(n_after, n_before,
    info = "Left join must not change the number of program rows")
})

test_that("melissa_merge_programs works with unified (non-deduped) input", {
  tmpdir <- withr::local_tempdir()
  mel_path <- make_melissa_programs_rds(tmpdir)

  raw <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  unified <- program_append_types(clean, verbose = FALSE)

  # Should work with alccdf_program_unified too
  result <- melissa_merge_programs(unified, mel_path, verbose = FALSE)
  df <- alccdf_data(result)
  expect_true("latitude" %in% names(df))
  expect_equal(sum(!is.na(df$latitude)), 3)
})


# ==============================================================================
# 3. melissa_merge_households() - Happy path
# ==============================================================================

test_that("melissa_merge_households merges geocoding into client data", {
  tmpdir <- withr::local_tempdir()
  mel_path <- make_melissa_households_rds(tmpdir)

  # Build subsidy client object: read -> clean
  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  result <- melissa_merge_households(clean, mel_path, verbose = FALSE)

  # Should be same class as input (augmented)
  expect_true(inherits(result, "alccdf_subsidy_clean"))

  # Household geocoding columns should be present (hh_ prefix)
  df <- alccdf_data(result)
  expect_true("hh_latitude" %in% names(df))
  expect_true("hh_longitude" %in% names(df))
  expect_true("hh_census_tract" %in% names(df))
  expect_true("hh_fips_code" %in% names(df))
  expect_true("hh_melissa_county" %in% names(df))
})

test_that("melissa_merge_households matched rows have non-NA hh_latitude", {
  tmpdir <- withr::local_tempdir()
  mel_path <- make_melissa_households_rds(tmpdir)

  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  result <- melissa_merge_households(clean, mel_path, verbose = FALSE)
  df <- alccdf_data(result)

  # Both client addresses should match
  n_matched <- sum(!is.na(df$hh_latitude))
  expect_equal(n_matched, 2)
})

test_that("melissa_merge_households records match stats in diagnostics", {
  tmpdir <- withr::local_tempdir()
  mel_path <- make_melissa_households_rds(tmpdir)

  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  result <- melissa_merge_households(clean, mel_path, verbose = FALSE)

  expect_equal(result$diagnostics$melissa_n_matched, 2)
  expect_equal(result$diagnostics$melissa_n_unmatched, 0)
  expect_equal(result$diagnostics$melissa_match_rate, 1.0)
})

test_that("melissa_merge_households preserves row count (left join)", {
  tmpdir <- withr::local_tempdir()
  mel_path <- make_melissa_households_rds(tmpdir)

  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  n_before <- nrow(alccdf_data(clean))

  result <- melissa_merge_households(clean, mel_path, verbose = FALSE)
  n_after <- nrow(alccdf_data(result))

  expect_equal(n_after, n_before,
    info = "Left join must not change the number of household rows")
})


# ==============================================================================
# 4. Edge cases
# ==============================================================================

# --- 4a. No matches found (non-matching addresses) ---

test_that("melissa_merge_programs handles zero matches gracefully", {
  tmpdir <- withr::local_tempdir()

  # Create a melissa RDS with addresses that don't match the fixture
  mel_df <- data.frame(
    address = c("999 NONEXISTENT ST,NOWHERE,AL 99999,Fake"),
    LAT = 32.0,
    LNG = -86.0,
    CT = 99999,
    CENSUSBLOC = 9999,
    FIPS = 9999,
    COUNTYNAME = "Fake",
    PLACENAME = "Fake",
    PLACECODE = 99999,
    MD_City = "Fake",
    MD_State = "AL",
    MD_PostalCode = "99999",
    GEOZIP = 99999,
    RESULTCODE = "GS05",
    STATUSCODE = "B",
    stringsAsFactors = FALSE
  )
  mel_path <- file.path(tmpdir, "mel_no_match.rds")
  saveRDS(mel_df, mel_path)

  raw <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  unified <- program_append_types(clean, verbose = FALSE)
  deduped <- program_deduplicate(unified, verbose = FALSE)

  result <- melissa_merge_programs(deduped, mel_path, verbose = FALSE)
  df <- alccdf_data(result)

  # All latitudes should be NA (no matches)
  expect_true(all(is.na(df$latitude)))
  expect_equal(result$diagnostics$melissa_n_matched, 0)
  expect_equal(result$diagnostics$melissa_n_unmatched, 3)
  expect_equal(result$diagnostics$melissa_match_rate, 0)

  # Row count should still be preserved
  expect_equal(nrow(df), 3)
})

test_that("melissa_merge_households handles zero matches gracefully", {
  tmpdir <- withr::local_tempdir()

  mel_df <- data.frame(
    address = c("999 NOWHERE AVE,FAKETOWN,AL 99999,Fake"),
    LAT = 32.0,
    LNG = -86.0,
    CT = 99999,
    CENSUSBLOC = 9999,
    FIPS = 9999,
    COUNTYNAME = "Fake",
    PLACENAME = "Fake",
    PLACECODE = 99999,
    MD_City = "Fake",
    MD_State = "AL",
    MD_PostalCode = "99999",
    GEOZIP = 99999,
    RESULTCODE = "GS05",
    STATUSCODE = "B",
    stringsAsFactors = FALSE
  )
  mel_path <- file.path(tmpdir, "mel_no_match_hh.rds")
  saveRDS(mel_df, mel_path)

  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)
  result <- melissa_merge_households(clean, mel_path, verbose = FALSE)
  df <- alccdf_data(result)

  expect_true(all(is.na(df$hh_latitude)))
  expect_equal(result$diagnostics$melissa_n_matched, 0)
  expect_equal(nrow(df), 2)
})

# --- 4b. All matches found ---

test_that("melissa_merge_programs achieves 100% match with aligned fixtures", {
  tmpdir <- withr::local_tempdir()
  mel_path <- make_melissa_programs_rds(tmpdir)

  raw <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  unified <- program_append_types(clean, verbose = FALSE)
  deduped <- program_deduplicate(unified, verbose = FALSE)

  result <- melissa_merge_programs(deduped, mel_path, verbose = FALSE)

  expect_equal(result$diagnostics$melissa_match_rate, 1.0)
  expect_equal(result$diagnostics$melissa_n_unmatched, 0)
})

# --- 4c. File not found ---

test_that("melissa_merge_programs errors on nonexistent file", {
  raw <- program_read(
    fixture_path("test_center.xlsx"), "center",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- program_clean(raw, verbose = FALSE)
  unified <- program_append_types(clean, verbose = FALSE)
  deduped <- program_deduplicate(unified, verbose = FALSE)

  expect_error(
    melissa_merge_programs(deduped, "/nonexistent/path.rds", verbose = FALSE),
    "not found"
  )
})

test_that("melissa_merge_households errors on nonexistent file", {
  raw <- subsidy_read(
    fixture_path("test_clients.xlsx"), "clients",
    SNAPSHOT_DATE, verbose = FALSE
  )
  clean <- subsidy_clean(raw, verbose = FALSE)

  expect_error(
    melissa_merge_households(clean, "/nonexistent/path.rds", verbose = FALSE),
    "not found"
  )
})

# --- 4d. Works with plain data frame input ---

test_that("melissa_merge_programs accepts a plain data frame", {
  tmpdir <- withr::local_tempdir()
  mel_path <- make_melissa_programs_rds(tmpdir)

  # Build a bare data frame with a facility_address column
  df_input <- data.frame(
    facility_address = c(
      "123 Main Street, Montgomery, AL 36101",
      "456 Oak Avenue, Birmingham, AL 35201"
    ),
    facility_name = c("Center A", "Center B"),
    stringsAsFactors = FALSE
  )

  result <- melissa_merge_programs(df_input, mel_path, verbose = FALSE)

  # With data frame input, returns a tibble (not an S3 object)
  expect_s3_class(result, "tbl_df")
  expect_true("latitude" %in% names(result))
  expect_equal(sum(!is.na(result$latitude)), 2)
})

test_that("melissa_merge_households accepts a plain data frame", {
  tmpdir <- withr::local_tempdir()
  mel_path <- make_melissa_households_rds(tmpdir)

  df_input <- data.frame(
    family_address = c(
      "10 Oak Street, Montgomery, AL 36104",
      "20 Pine Avenue, Mobile, AL 36602"
    ),
    client_name = c("Client A", "Client B"),
    stringsAsFactors = FALSE
  )

  result <- melissa_merge_households(df_input, mel_path, verbose = FALSE)
  expect_s3_class(result, "tbl_df")
  expect_true("hh_latitude" %in% names(result))
  expect_equal(sum(!is.na(result$hh_latitude)), 2)
})

# --- 4e. Melissa households rejects input missing family_address ---

test_that("melissa_merge_households errors when family_address is missing", {
  tmpdir <- withr::local_tempdir()
  mel_path <- make_melissa_households_rds(tmpdir)

  df_no_addr <- data.frame(
    some_column = c("A", "B"),
    stringsAsFactors = FALSE
  )

  expect_error(
    melissa_merge_households(df_no_addr, mel_path, verbose = FALSE),
    "address"
  )
})
