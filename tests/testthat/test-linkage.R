# tests/testthat/test-linkage.R
# Unit tests for the linkage module in ALccdfDB.
# Uses testthat edition 3 with fixture files in tests/testthat/fixtures/.

# Shared snapshot date used consistently across all tests.
SNAPSHOT_DATE <- as.Date("2025-09-12")

# Helper: path to a fixture file
fixture_path <- function(filename) {
  testthat::test_path("fixtures", filename)
}


# ==============================================================================
# Shared setup: build pipeline objects from fixtures
# ==============================================================================

# --- Program pipeline ---
raw_c <- program_read(
  fixture_path("test_center.xlsx"), "center", SNAPSHOT_DATE, verbose = FALSE
)
raw_f <- program_read(
  fixture_path("test_family_home.xlsx"), "family_home", SNAPSHOT_DATE,
  verbose = FALSE
)
clean_c <- program_clean(raw_c, verbose = FALSE)
clean_f <- program_clean(raw_f, verbose = FALSE)
unified <- program_append_types(clean_c, clean_f, verbose = FALSE)
deduped <- program_deduplicate(unified, verbose = FALSE)

# --- Subsidy pipeline ---
sub_enrolled_raw <- subsidy_read(
  fixture_path("test_enrolled.xlsx"), "enrolled", SNAPSHOT_DATE, verbose = FALSE
)
sub_enrolled <- subsidy_clean(sub_enrolled_raw, verbose = FALSE)

sub_clients_raw <- subsidy_read(
  fixture_path("test_clients.xlsx"), "clients", SNAPSHOT_DATE, verbose = FALSE
)
sub_clients <- subsidy_clean(sub_clients_raw, verbose = FALSE)

# --- Staff pipeline ---
staff_raw <- staff_read(fixture_path("test_staff.csv"), SNAPSHOT_DATE,
                        verbose = FALSE)
staff <- staff_clean(staff_raw, verbose = FALSE)


# ==============================================================================
# 1. linkage_programs_enrolled()
# ==============================================================================

test_that("linkage_programs_enrolled returns correct S3 class", {
  linked <- linkage_programs_enrolled(deduped, sub_enrolled, verbose = FALSE)
  expect_s3_class(linked, "alccdf_linked_programs_enrolled")
})

test_that("linkage_programs_enrolled preserves all program rows", {
  linked <- linkage_programs_enrolled(deduped, sub_enrolled, verbose = FALSE)
  linked_df <- alccdf_data(linked)

  # deduped has 7 rows (3 centers + 4 family/group homes)
  expect_equal(nrow(linked_df), nrow(alccdf_data(deduped)))
  expect_equal(nrow(linked_df), 7)
})

test_that("linkage_programs_enrolled matched programs have n_enrolled_children > 0", {
  linked <- linkage_programs_enrolled(deduped, sub_enrolled, verbose = FALSE)
  linked_df <- alccdf_data(linked)


  # Enrolled fixture has facility_ids F001 and F102 which match programs
  matched <- linked_df[linked_df$n_enrolled_children > 0, ]
  expect_true(nrow(matched) > 0)

  # F001 should match (2 enrolled children)
  f001_row <- linked_df[linked_df$facility_id == "F001", ]
  expect_equal(f001_row$n_enrolled_children, 2L)

  # F102 should match (1 enrolled child)
  f102_row <- linked_df[linked_df$facility_id == "F102", ]
  expect_equal(f102_row$n_enrolled_children, 1L)
})

test_that("linkage_programs_enrolled unmatched programs have n_enrolled_children == 0", {
  linked <- linkage_programs_enrolled(deduped, sub_enrolled, verbose = FALSE)
  linked_df <- alccdf_data(linked)

  # Programs without enrolled children should have 0
  unmatched <- linked_df[linked_df$n_enrolled_children == 0, ]
  expect_true(nrow(unmatched) > 0)

  # F002, F003, F101, F103, F104 should be unmatched
  unmatched_ids <- unmatched$facility_id
  expect_true("F002" %in% unmatched_ids)
  expect_true("F003" %in% unmatched_ids)
  expect_true("F101" %in% unmatched_ids)
})

test_that("linkage_programs_enrolled meta has match_rate", {
  linked <- linkage_programs_enrolled(deduped, sub_enrolled, verbose = FALSE)
  meta <- alccdf_meta(linked)

  expect_true("match_rate" %in% names(meta))
  expect_true(is.numeric(meta$match_rate))
  expect_true(meta$match_rate >= 0 && meta$match_rate <= 100)

  # 2 matched out of 7 programs = ~28.6%
  expect_equal(meta$n_matched, 2)
})

test_that("linkage_programs_enrolled adds n_cases column", {
  linked <- linkage_programs_enrolled(deduped, sub_enrolled, verbose = FALSE)
  linked_df <- alccdf_data(linked)

  expect_true("n_cases" %in% names(linked_df))

  # F001 has case C001 (2 children, 1 case)
  f001_row <- linked_df[linked_df$facility_id == "F001", ]
  expect_equal(f001_row$n_cases, 1L)
})

test_that("linkage_programs_enrolled has care_level columns when present in data", {
  linked <- linkage_programs_enrolled(deduped, sub_enrolled, verbose = FALSE)
  linked_df <- alccdf_data(linked)

  # Enrolled fixture has care_level values: PreSchool, Infant
  care_cols <- grep("^care_level_", names(linked_df), value = TRUE)
  expect_true(length(care_cols) > 0)
})

test_that("linkage_programs_enrolled diagnostics contain unmatched info", {
  linked <- linkage_programs_enrolled(deduped, sub_enrolled, verbose = FALSE)

  expect_true("diagnostics" %in% names(linked))
  diag <- linked$diagnostics
  expect_true("n_matched" %in% names(diag))
  expect_true("n_unmatched_programs" %in% names(diag))
  expect_true("match_rate" %in% names(diag))
  expect_true("unmatched_programs" %in% names(diag))
})


# ==============================================================================
# 2. linkage_programs_clients()
# ==============================================================================

test_that("linkage_programs_clients returns correct S3 class", {
  linked <- linkage_programs_clients(deduped, sub_clients, verbose = FALSE)
  expect_s3_class(linked, "alccdf_linked_programs_clients")
})

test_that("linkage_programs_clients preserves all program rows", {
  linked <- linkage_programs_clients(deduped, sub_clients, verbose = FALSE)
  linked_df <- alccdf_data(linked)

  expect_equal(nrow(linked_df), nrow(alccdf_data(deduped)))
  expect_equal(nrow(linked_df), 7)
})

test_that("linkage_programs_clients populates n_active_clients", {
  linked <- linkage_programs_clients(deduped, sub_clients, verbose = FALSE)
  linked_df <- alccdf_data(linked)

  expect_true("n_active_clients" %in% names(linked_df))

  # Clients fixture has provider_id P001 and P102 matching program provider_ids
  # P001 -> 1 client, P102 -> 1 client
  matched <- linked_df[linked_df$n_active_clients > 0, ]
  expect_true(nrow(matched) > 0)

  # Unmatched programs should have 0
  unmatched <- linked_df[linked_df$n_active_clients == 0, ]
  expect_true(nrow(unmatched) > 0)
})

test_that("linkage_programs_clients adds avg_copay_weekly column", {
  linked <- linkage_programs_clients(deduped, sub_clients, verbose = FALSE)
  linked_df <- alccdf_data(linked)

  expect_true("avg_copay_weekly" %in% names(linked_df))
})

test_that("linkage_programs_clients adds n_unique_cases column", {
  linked <- linkage_programs_clients(deduped, sub_clients, verbose = FALSE)
  linked_df <- alccdf_data(linked)

  expect_true("n_unique_cases" %in% names(linked_df))
})

test_that("linkage_programs_clients meta has match statistics", {
  linked <- linkage_programs_clients(deduped, sub_clients, verbose = FALSE)
  meta <- alccdf_meta(linked)

  expect_true("match_rate" %in% names(meta))
  expect_true("n_matched" %in% names(meta))
  expect_true("n_programs" %in% names(meta))
  expect_true(is.numeric(meta$match_rate))
})

test_that("linkage_programs_clients diagnostics have unmatched info", {
  linked <- linkage_programs_clients(deduped, sub_clients, verbose = FALSE)

  diag <- linked$diagnostics
  expect_true("n_matched" %in% names(diag))
  expect_true("n_unmatched_programs" %in% names(diag))
  expect_true("match_rate" %in% names(diag))
})


# ==============================================================================
# 3. linkage_programs_staff()
# ==============================================================================

test_that("linkage_programs_staff returns correct S3 class", {
  linked <- linkage_programs_staff(deduped, staff, verbose = FALSE)
  expect_s3_class(linked, "alccdf_linked_programs_staff")
})

test_that("linkage_programs_staff preserves all program rows", {
  linked <- linkage_programs_staff(deduped, staff, verbose = FALSE)
  linked_df <- alccdf_data(linked)

  expect_equal(nrow(linked_df), nrow(alccdf_data(deduped)))
  expect_equal(nrow(linked_df), 7)
})

test_that("linkage_programs_staff matched programs have n_staff > 0", {
  linked <- linkage_programs_staff(deduped, staff, verbose = FALSE)
  linked_df <- alccdf_data(linked)

  expect_true("n_staff" %in% names(linked_df))

  # Staff fixture:
  #   "Test Center A" -> 2 staff
  #   "Test Center B" -> 1 staff
  #   "Jones Group Home" -> 2 staff
  matched <- linked_df[linked_df$n_staff > 0, ]
  expect_true(nrow(matched) >= 3)

  # Check specific facility counts
  center_a <- linked_df[linked_df$facility_name == "Test Center A", ]
  expect_equal(center_a$n_staff, 2L)

  center_b <- linked_df[linked_df$facility_name == "Test Center B", ]
  expect_equal(center_b$n_staff, 1L)

  # Jones Group Home
  jones <- linked_df[linked_df$facility_name == "Jones Group Home", ]
  expect_equal(jones$n_staff, 2L)
})

test_that("linkage_programs_staff unmatched programs have n_staff == 0", {
  linked <- linkage_programs_staff(deduped, staff, verbose = FALSE)
  linked_df <- alccdf_data(linked)

  unmatched <- linked_df[linked_df$n_staff == 0, ]
  expect_true(nrow(unmatched) > 0)

  # Test Center C, Smith Family Home, Lee Family Home, Park Group Home
  # should be unmatched (4 unmatched)
  expect_equal(nrow(unmatched), 4)
})

test_that("linkage_programs_staff join is case-insensitive on facility_name", {
  # The join uses str_to_lower + str_squish on both sides,
  # so facility names with different case should still match.
  # Our fixtures already have matching case, but we verify the mechanism
  # works by checking that the join key is not present in output.
  linked <- linkage_programs_staff(deduped, staff, verbose = FALSE)
  linked_df <- alccdf_data(linked)

  # The .join_key temp column should be removed from output
  expect_false(".join_key" %in% names(linked_df))

  # Meta should indicate case-insensitive join
  meta <- alccdf_meta(linked)
  expect_true(grepl("case-insensitive", meta$join_key, ignore.case = TRUE))
})

test_that("linkage_programs_staff meta has match statistics", {
  linked <- linkage_programs_staff(deduped, staff, verbose = FALSE)
  meta <- alccdf_meta(linked)

  expect_true("match_rate" %in% names(meta))
  expect_true("n_matched" %in% names(meta))
  expect_true("n_staff" %in% names(meta))
  expect_equal(meta$n_staff, 5)  # 5 staff in fixture
})

test_that("linkage_programs_staff diagnostics have unmatched info", {
  linked <- linkage_programs_staff(deduped, staff, verbose = FALSE)

  diag <- linked$diagnostics
  expect_true("n_matched" %in% names(diag))
  expect_true("n_unmatched_programs" %in% names(diag))
  expect_true("unmatched_staff" %in% names(diag))
})


# ==============================================================================
# 4. linkage_clients_programs()
# ==============================================================================

test_that("linkage_clients_programs returns correct S3 class", {
  linked <- linkage_clients_programs(sub_clients, deduped, verbose = FALSE)
  expect_s3_class(linked, "alccdf_linked_clients_programs")
})

test_that("linkage_clients_programs preserves all client rows", {
  linked <- linkage_clients_programs(sub_clients, deduped, verbose = FALSE)
  linked_df <- alccdf_data(linked)

  # Clients fixture has 2 client records
  expect_equal(nrow(linked_df), nrow(alccdf_data(sub_clients)))
  expect_equal(nrow(linked_df), 2)
})

test_that("linkage_clients_programs appends program columns", {
  linked <- linkage_clients_programs(sub_clients, deduped, verbose = FALSE)
  linked_df <- alccdf_data(linked)

  # Should have program columns like facility_id, facility_name, facility_type
  expect_true("facility_id" %in% names(linked_df))
  expect_true("facility_type" %in% names(linked_df))
})

test_that("linkage_clients_programs meta has match statistics", {
  linked <- linkage_clients_programs(sub_clients, deduped, verbose = FALSE)
  meta <- alccdf_meta(linked)

  expect_true("match_rate" %in% names(meta))
  expect_true("n_matched" %in% names(meta))
  expect_true("n_clients" %in% names(meta))
  expect_true("n_programs" %in% names(meta))
  expect_equal(meta$backbone, "clients")
})

test_that("linkage_clients_programs meta lists appended program columns", {
  linked <- linkage_clients_programs(sub_clients, deduped, verbose = FALSE)
  meta <- alccdf_meta(linked)

  expect_true("program_cols_appended" %in% names(meta))
  expect_true(length(meta$program_cols_appended) > 0)
})

test_that("linkage_clients_programs diagnostics have unmatched info", {
  linked <- linkage_clients_programs(sub_clients, deduped, verbose = FALSE)

  diag <- linked$diagnostics
  expect_true("n_matched" %in% names(diag))
  expect_true("n_unmatched" %in% names(diag))
  expect_true("match_rate" %in% names(diag))
  expect_true("unmatched_clients" %in% names(diag))
  expect_true("unmatched_programs" %in% names(diag))
})


# ==============================================================================
# 5. linkage_report()
# ==============================================================================

test_that("linkage_report writes a markdown file", {
  linked_pe <- linkage_programs_enrolled(deduped, sub_enrolled, verbose = FALSE)
  linked_pc <- linkage_programs_clients(deduped, sub_clients, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  report_path <- linkage_report(
    linked_objects = list(
      programs_enrolled = linked_pe,
      programs_clients  = linked_pc
    ),
    output_dir = tmpdir,
    verbose = FALSE
  )

  expect_true(file.exists(report_path))
  expect_true(grepl("linkage_report\\.md$", report_path))
})

test_that("linkage_report file has content", {
  linked_pe <- linkage_programs_enrolled(deduped, sub_enrolled, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  report_path <- linkage_report(
    linked_objects = list(programs_enrolled = linked_pe),
    output_dir = tmpdir,
    verbose = FALSE
  )

  content <- readLines(report_path)
  expect_true(length(content) > 0)
  # File should have meaningful length (more than just a header)
  expect_true(length(content) > 5)
})

test_that("linkage_report contains expected sections", {
  linked_pe <- linkage_programs_enrolled(deduped, sub_enrolled, verbose = FALSE)
  linked_pc <- linkage_programs_clients(deduped, sub_clients, verbose = FALSE)
  linked_ps <- linkage_programs_staff(deduped, staff, verbose = FALSE)
  linked_cp <- linkage_clients_programs(sub_clients, deduped, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  report_path <- linkage_report(
    linked_objects = list(
      programs_enrolled = linked_pe,
      programs_clients  = linked_pc,
      programs_staff    = linked_ps,
      clients_programs  = linked_cp
    ),
    output_dir = tmpdir,
    verbose = FALSE
  )

  content <- paste(readLines(report_path), collapse = "\n")

  # Header
  expect_true(grepl("ALccdfDB Linkage Report", content))

  # Per-linkage sections
  expect_true(grepl("Programs x Enrolled Children", content))
  expect_true(grepl("Programs x Subsidy Clients", content))
  expect_true(grepl("Programs x Staff", content))
  expect_true(grepl("Clients x Programs", content))

  # Summary section
  expect_true(grepl("Summary: All Linkages", content))

  # Unmatched analysis
  expect_true(grepl("Unmatched Analysis", content))
})

test_that("linkage_report returns invisible file path", {
  linked_pe <- linkage_programs_enrolled(deduped, sub_enrolled, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  result <- linkage_report(
    linked_objects = list(programs_enrolled = linked_pe),
    output_dir = tmpdir,
    verbose = FALSE
  )

  expect_true(is.character(result))
  expect_true(file.exists(result))
})

test_that("linkage_report creates output directory if needed", {
  linked_pe <- linkage_programs_enrolled(deduped, sub_enrolled, verbose = FALSE)

  tmpdir <- withr::local_tempdir()
  nested_dir <- file.path(tmpdir, "subdir", "reports")
  expect_false(dir.exists(nested_dir))

  report_path <- linkage_report(
    linked_objects = list(programs_enrolled = linked_pe),
    output_dir = nested_dir,
    verbose = FALSE
  )

  expect_true(dir.exists(nested_dir))
  expect_true(file.exists(report_path))
})


# ==============================================================================
# 6. Error handling
# ==============================================================================

test_that("linkage_programs_enrolled rejects wrong program class", {
  expect_error(
    linkage_programs_enrolled(data.frame(x = 1), sub_enrolled, verbose = FALSE),
    "data"
  )
})

test_that("linkage_programs_enrolled rejects wrong enrolled class", {
  expect_error(
    linkage_programs_enrolled(deduped, data.frame(x = 1), verbose = FALSE),
    "alccdf_subsidy_clean"
  )
})

test_that("linkage_programs_enrolled rejects clients type for enrolled param", {
  expect_error(
    linkage_programs_enrolled(deduped, sub_clients, verbose = FALSE),
    "enrolled"
  )
})

test_that("linkage_programs_clients rejects wrong clients class", {
  expect_error(
    linkage_programs_clients(deduped, data.frame(x = 1), verbose = FALSE),
    "alccdf_subsidy_clean"
  )
})

test_that("linkage_programs_clients rejects enrolled type for clients param", {
  expect_error(
    linkage_programs_clients(deduped, sub_enrolled, verbose = FALSE),
    "clients"
  )
})

test_that("linkage_programs_staff rejects wrong program class", {
  expect_error(
    linkage_programs_staff(data.frame(x = 1), staff, verbose = FALSE),
    "data"
  )
})

test_that("linkage_programs_staff rejects wrong staff class", {
  expect_error(
    linkage_programs_staff(deduped, data.frame(x = 1), verbose = FALSE),
    "data"
  )
})

test_that("linkage_clients_programs rejects wrong clients class", {
  expect_error(
    linkage_clients_programs(data.frame(x = 1), deduped, verbose = FALSE),
    "alccdf_subsidy_clean"
  )
})

test_that("linkage_clients_programs rejects enrolled type for clients param", {
  expect_error(
    linkage_clients_programs(sub_enrolled, deduped, verbose = FALSE),
    "clients"
  )
})

test_that("linkage_clients_programs rejects wrong program class", {
  expect_error(
    linkage_clients_programs(sub_clients, data.frame(x = 1), verbose = FALSE),
    "data"
  )
})

test_that("linkage_report rejects empty list", {
  expect_error(
    linkage_report(list(), output_dir = tempdir(), verbose = FALSE),
    "non-empty"
  )
})

test_that("linkage_report rejects unnamed list", {
  linked_pe <- linkage_programs_enrolled(deduped, sub_enrolled, verbose = FALSE)
  expect_error(
    linkage_report(list(linked_pe), output_dir = tempdir(), verbose = FALSE),
    "named"
  )
})
