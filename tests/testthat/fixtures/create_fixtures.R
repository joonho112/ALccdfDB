#!/usr/bin/env Rscript
# Create minimal test fixtures for ALccdfDB unit tests
library(openxlsx)

FIXTURE_DIR <- "/Users/joonholee/Documents/ALccdfDB/tests/testthat/fixtures"

# 1. Center file (skip=2)
center_df <- data.frame(
  "Facility ID" = c("F001", "F002", "F003"),
  "Facility Name" = c("Test Center A", "Test Center B", "Test Center C"),
  "Type of the Facility/Provider" = c("Center", "Center", "Center"),
  "Facility Tier" = c("Star 1", "Star 3", "None"),
  "Facility Address" = c("123 Main Street, Montgomery, AL 36101",
                          "456 Oak Avenue, Birmingham, AL 35201",
                          "789 Pine Road, Huntsville, AL 35801"),
  County = c("Montgomery", "Jefferson", "Madison"),
  Region = c("Region 1", "Region 2", "Region 3"),
  "Day Start" = c("06:00:00", "07:00:00", "06:30:00"),
  "Day End" = c("18:00:00", "17:00:00", "17:30:00"),
  "Day Capacity" = c(50, 100, 75),
  "Night Start" = c(NA, "19:00:00", NA),
  "Night End" = c(NA, "06:00:00", NA),
  "Night Capacity" = c(0, 30, 0),
  "Day Age Range" = c("0 - 5 Years", "0 - 12 Years", "3 - 5 Years"),
  "Night Age Range" = c(NA, "0 - 8 Years", NA),
  "Expiration Date" = c("2026-06-15", "2026-03-22", "2025-01-15"),
  "Provider ID" = c("P001", "P002", "P003"),
  "License Number" = c("L001", "L002", "L003"),
  "Phone Number" = c("(334) 555-1234", "(205) 555-5678", "(256) 555-9012"),
  "Director Name" = c("SMITH, JOHN", "DOE, JANE", "BROWN, BOB"),
  Consultant = c("Jones, Mary", "Jones, Mary", "Smith, Tom"),
  check.names = FALSE, stringsAsFactors = FALSE
)
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", "REPORT TITLE", startRow = 1)
writeData(wb, "Sheet1", "Generated on 2025-09-12", startRow = 2)
writeData(wb, "Sheet1", center_df, startRow = 3)
saveWorkbook(wb, file.path(FIXTURE_DIR, "test_center.xlsx"), overwrite = TRUE)
cat("Created test_center.xlsx\n")

# 2. Family/Group Home file (skip=2)
fh_df <- data.frame(
  "Facility ID" = c("F101", "F102", "F103", "F104"),
  "Facility Name" = c("Smith Family Home", "Jones Group Home", "Lee Family Home", "Park Group Home"),
  "Type of the Facility/Provider" = c("Family Home", "Group Home", "Family Home", "Group Home"),
  "Facility Tier" = c("Star 2", "Star 1", "Star 4", "Star 1"),
  "Facility Address" = c("111 Elm ST, Dothan, AL 36301", "222 Maple DR, Mobile, AL 36602",
                          "333 Cedar LN, Tuscaloosa, AL 35401", "444 Birch CT, Auburn, AL 36830"),
  County = c("Houston", "Mobile", "Tuscaloosa", "Lee"),
  Region = c("Region 4", "Region 5", "Region 6", "Region 7"),
  "Day Start" = c("06:00:00", "07:00:00", "06:00:00", "06:30:00"),
  "Day End" = c("17:00:00", "18:00:00", "17:30:00", "18:00:00"),
  "Day Capacity" = c(6, 12, 8, 10),
  "Night Start" = c(NA, NA, NA, NA),
  "Night End" = c(NA, NA, NA, NA),
  "Night Capacity" = c(0, 0, 0, 0),
  "Day Age Range" = c("0 - 5 Years", "0 - 12 Years", "0.25 - 3 Years", "0 - 8 Years"),
  "Night Age Range" = c(NA, NA, NA, NA),
  "Expiration Date" = c("2026-05-01", "2026-08-15", "2026-02-28", "2026-11-30"),
  "Provider ID" = c("P101", "P102", "P103", "P104"),
  "License Number" = c("L101", "L102", "L103", "L104"),
  "Phone Number" = c("(334) 555-1111", "(251) 555-2222", "(205) 555-3333", "(334) 555-4444"),
  "Director Name" = c(NA, NA, NA, NA),
  Consultant = c("Adams, Pat", "Adams, Pat", "Baker, Chris", "Baker, Chris"),
  "Children Under 12 Months" = c(3, 6, 2, 4),
  check.names = FALSE, stringsAsFactors = FALSE
)
wb2 <- createWorkbook()
addWorksheet(wb2, "Sheet1")
writeData(wb2, "Sheet1", "REPORT TITLE", startRow = 1)
writeData(wb2, "Sheet1", "Generated on 2025-09-12", startRow = 2)
writeData(wb2, "Sheet1", fh_df, startRow = 3)
saveWorkbook(wb2, file.path(FIXTURE_DIR, "test_family_home.xlsx"), overwrite = TRUE)
cat("Created test_family_home.xlsx\n")

# 3. Exempt file (skip=0)
exempt_df <- data.frame(
  "Facility ID" = c("F201", "F202"),
  "Facility Name" = c("First Baptist CDC", "Grace Church Preschool"),
  "Facility Type" = c("Faith-Based Center", "Faith-Based Center"),
  "Facility Address" = c("100 Church ST, Decatur, AL 35601", "200 Faith RD, Florence, AL 35630"),
  "Physical Address" = c("100 Church St, Decatur, AL 35601", "200 Faith Rd, Florence, AL 35630"),
  County = c("Morgan", "Lauderdale"),
  Region = c("Region 3", "Region 3"),
  "Day Start" = c("07:00:00", "08:00:00"),
  "Day End" = c("17:00:00", "14:00:00"),
  "Day Capacity" = c(40, 25),
  "Night Start" = c(NA, NA), "Night End" = c(NA, NA), "Night Capacity" = c(0, 0),
  "Day Age Range" = c(NA, NA), "Night Age Range" = c(NA, NA),
  "Expiration Date" = c("2026-12-31", "2026-06-30"),
  "Provider ID" = c(NA, NA), "License Number" = c("E201", "E202"),
  "Contact Person" = c("Pastor Smith", "Rev. Johnson"),
  "Exempt Worker" = c("Trice, Shakira", "Vice, Jessica"),
  "Pastor Name" = c("Pastor Smith", "Rev. Johnson"),
  check.names = FALSE, stringsAsFactors = FALSE
)
wb3 <- createWorkbook()
addWorksheet(wb3, "Sheet1")
writeData(wb3, "Sheet1", exempt_df)
saveWorkbook(wb3, file.path(FIXTURE_DIR, "test_exempt.xlsx"), overwrite = TRUE)
cat("Created test_exempt.xlsx\n")

# 4. Excepted file (skip=2)
excepted_df <- data.frame(
  "Facility ID" = "F301", "Facility Name" = "UA Child Dev Center",
  "Facility Type" = "Excepted (Out of School Time)",
  "Facility Address" = "500 University BLVD, Tuscaloosa, AL 35401",
  "Physical Address" = "500 University Blvd, Tuscaloosa, AL 35401",
  County = "Tuscaloosa", Region = "Region 6",
  "Day Start" = "07:30:00", "Day End" = "17:30:00", "Day Capacity" = 60,
  "Night Start" = NA, "Night End" = NA, "Night Capacity" = 0,
  "Day Age Range" = "5 - 12 Years", "Night Age Range" = NA,
  "Expiration Date" = "2026-09-30", "Provider ID" = "P301",
  "License Number" = "X301", "Facility Tier" = "Star 5",
  "Contact Person" = "Dr. Williams", "Exempt Worker" = NA, "Pastor Name" = NA,
  check.names = FALSE, stringsAsFactors = FALSE
)
wb4 <- createWorkbook()
addWorksheet(wb4, "Sheet1")
writeData(wb4, "Sheet1", "REPORT", startRow = 1)
writeData(wb4, "Sheet1", "Date", startRow = 2)
writeData(wb4, "Sheet1", excepted_df, startRow = 3)
saveWorkbook(wb4, file.path(FIXTURE_DIR, "test_excepted.xlsx"), overwrite = TRUE)
cat("Created test_excepted.xlsx\n")

# 5. Staff CSV
staff_df <- data.frame(
  "Staff Name" = c("Alice Johnson", "Bob Williams", "Carol Davis", "Dan Miller", "Eve Brown"),
  "User ZIP" = c("36101", "35201", "35801", "36301", "36602"),
  "User County" = c("montgomery", "jefferson", "madison", "houston", "mobile"),
  "Facility Name" = c("Test Center A", "Test Center A", "Test Center B", "Jones Group Home", "Jones Group Home"),
  "Facility County" = c("Montgomery", "Montgomery", "Jefferson", "Houston", "Mobile"),
  "Provider Type" = c("Center", "Center", "Center", "Group Home", "Group Home"),
  "Currently Operating" = c("Yes", "Yes", "Yes", "No", "Yes"),
  Position = c("Teacher", "Director", "Teacher", "Assistant", "Teacher"),
  "Career Lattice Level" = c("Level 3", "Level 5", "Level 2", "Level 1", "Level 4"),
  "User Email" = c("alice@test.com", "bob@test.com", "carol@test.com", "dan@test.com", "eve@test.com"),
  check.names = FALSE, stringsAsFactors = FALSE
)
write.csv(staff_df, file.path(FIXTURE_DIR, "test_staff.csv"), row.names = FALSE)
cat("Created test_staff.csv\n")

# 6. Melissa programs RDS
melissa_prog <- data.frame(
  facility_address = c("123 MAIN STREET,MONTGOMERY,AL 36101,Montgomery",
                        "456 OAK AVENUE,BIRMINGHAM,AL 35201,Jefferson",
                        "789 PINE ROAD,HUNTSVILLE,AL 35801,Madison"),
  latitude = c(32.3668, 33.5207, 34.7304),
  longitude = c(-86.2999, -86.8025, -86.5861),
  census_tract = c(10101, 10201, 10301), census_block = c(1001, 1002, 1003),
  fips_code = c(1101, 1073, 1089),
  melissa_county = c("Montgomery", "Jefferson", "Madison"),
  melissa_place = c("Montgomery", "Birmingham", "Huntsville"),
  melissa_place_code = c(51000, 7000, 37000),
  melissa_city = c("Montgomery", "Birmingham", "Huntsville"),
  melissa_state = c("AL", "AL", "AL"),
  melissa_zip = c("36101-1234", "35201-5678", "35801-9012"),
  melissa_geo_zip = c(36101, 35201, 35801),
  melissa_result_code = c("GS05", "GS05", "GS03"),
  melissa_status_code = c("B", "B", "5"),
  stringsAsFactors = FALSE
)
saveRDS(melissa_prog, file.path(FIXTURE_DIR, "test_melissa_programs.rds"))
cat("Created test_melissa_programs.rds\n")

# 7. Melissa households RDS
melissa_hh <- data.frame(
  family_address = c("10 OAK STREET,MONTGOMERY,AL 36104,Montgomery",
                      "20 PINE AVENUE,MOBILE,AL 36602,Mobile"),
  latitude = c(32.37, 30.69), longitude = c(-86.30, -88.04),
  census_tract = c(20101, 20201), census_block = c(2001, 2002),
  fips_code = c(1101, 1097),
  melissa_county = c("Montgomery", "Mobile"),
  melissa_place = c("Montgomery", "Mobile"),
  melissa_place_code = c(51000, 50000),
  melissa_city = c("Montgomery", "Mobile"),
  melissa_state = c("AL", "AL"),
  melissa_zip = c("36104-0001", "36602-0001"),
  melissa_geo_zip = c(36104, 36602),
  melissa_result_code = c("GS05", "GS05"),
  melissa_status_code = c("B", "B"),
  stringsAsFactors = FALSE
)
saveRDS(melissa_hh, file.path(FIXTURE_DIR, "test_melissa_households.rds"))
cat("Created test_melissa_households.rds\n")

# 8. Enrolled (skip=0)
enrolled_df <- data.frame(
  Region = c("Region 1", "Region 1", "Region 5"),
  County = c("Montgomery", "Montgomery", "Mobile"),
  "Case ID" = c("C001", "C001", "C002"),
  "Parent Name" = c("Parent A", "Parent A", "Parent B"),
  "Parent ID/SSN" = c("111-22-3333", "111-22-3333", "444-55-6666"),
  "Parent DOB" = c("1990-05-15", "1990-05-15", "1985-10-20"),
  "Child Name" = c("Child A1", "Child A2", "Child B1"),
  "Child SSN" = c("999-00-0001", "999-00-0002", "999-00-0003"),
  "Child DOB" = c("2020-01-10", "2022-03-15", "2021-07-20"),
  "Child Age" = c(5, 3, 4),
  "Care Level" = c("PreSchool", "Infant", "PreSchool"),
  "Eligibility Category" = c("Category A", "Category A", "Category B"),
  "Eligibility Begin Date" = c("2024-01-01", "2024-01-01", "2024-06-01"),
  "Eligibility End Date" = c("2025-12-31", "2025-12-31", "2025-12-31"),
  "Placement Start Date" = c("2024-02-01", "2024-03-01", "2024-07-01"),
  "Placement End Date" = c(NA, NA, NA),
  "Provider ID" = c("P001", "P001", "P102"),
  "Facility ID" = c("F001", "F001", "F102"),
  "Facility Name" = c("Test Center A", "Test Center A", "Jones Group Home"),
  "Unit of Care" = c("Weekly", "Weekly", "Daily"),
  check.names = FALSE, stringsAsFactors = FALSE
)
wb5 <- createWorkbook()
addWorksheet(wb5, "Sheet1")
writeData(wb5, "Sheet1", enrolled_df)
saveWorkbook(wb5, file.path(FIXTURE_DIR, "test_enrolled.xlsx"), overwrite = TRUE)
cat("Created test_enrolled.xlsx\n")

# 9. Clients (skip=2)
clients_df <- data.frame(
  County = c("Montgomery", "Mobile"),
  "Parent Name" = c("Parent A", "Parent B"),
  "Parent DOB" = c("1990-05-15", "1985-10-20"),
  "Family Address" = c("10 Oak Street, Montgomery, AL 36104", "20 Pine Avenue, Mobile, AL 36602"),
  "Funding Type" = c("Type I", "Type II"),
  "Case ID" = c("C001", "C002"),
  "Child Name" = c("Child A1", "Child B1"),
  "Child DOB" = c("2020-01-10", "2021-07-20"),
  "Child Age" = c(5, 4),
  "Provider ID" = c("P001", "P102"),
  "Provider Name" = c("Test Center A", "Jones Group Home"),
  "Provider Address" = c("123 Main Street, Montgomery, AL 36101", "222 Maple DR, Mobile, AL 36602"),
  "Placement Date" = c("2024-02-01", "2024-07-01"),
  "Copay Weekly" = c(25.50, 0),
  check.names = FALSE, stringsAsFactors = FALSE
)
wb6 <- createWorkbook()
addWorksheet(wb6, "Sheet1")
writeData(wb6, "Sheet1", "CLIENTS REPORT", startRow = 1)
writeData(wb6, "Sheet1", "Date: 2025-07-15", startRow = 2)
writeData(wb6, "Sheet1", clients_df, startRow = 3)
saveWorkbook(wb6, file.path(FIXTURE_DIR, "test_clients.xlsx"), overwrite = TRUE)
cat("Created test_clients.xlsx\n")

cat("\nAll fixtures created successfully!\n")
