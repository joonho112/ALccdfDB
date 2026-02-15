#' @title Melissa Module: Merge
#' @description Merge Melissa.com geocoding coordinates into program and subsidy
#'   household data. Programs are matched by facility_address; households by
#'   family_address. Addresses are normalised (uppercased, punctuation removed,
#'   whitespace collapsed) before matching to handle formatting differences
#'   between DHR and Melissa outputs.
#' @name melissa-merge
NULL

#' Merge Melissa coordinates into program data
#'
#' Joins Melissa geocoding results (latitude, longitude, census tract, etc.)
#' to a program-module S3 object. Matching uses \code{facility_address} in the
#' program data against \code{address} in the Melissa data, with both sides
#' normalised for robust matching.
#'
#' @param program_obj A program-module S3 object (any stage from clean onward)
#' @param melissa_path Path to the Melissa program geocoding file (RDS, Excel,
#'   or CSV). Expected columns: address, LAT, LNG, CT, CENSUSBLOC, FIPS,
#'   COUNTYNAME, PLACENAME, PLACECODE, RESULTCODE, STATUSCODE, MD_City,
#'   MD_PostalCode, etc.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return The input program object augmented with geocoding columns
#'   (latitude, longitude, census_tract, fips_code, etc.)
#'
#' @export
melissa_merge_programs <- function(program_obj, melissa_path, verbose = TRUE) {
  if (verbose) {
    .msg_header("Merging Melissa Coordinates into Programs", level = 3)
  }

  # --- Load Melissa data ----
  melissa_df <- .read_melissa_file(melissa_path)
  if (verbose) .msg_step("Loaded Melissa data: {nrow(melissa_df)} records")

  # --- Extract program data ----
  program_df <- if (is.data.frame(program_obj)) {
    tibble::as_tibble(program_obj)
  } else {
    alccdf_data(program_obj)
  }
  n_program <- nrow(program_df)

  # --- Prepare Melissa columns ----
  melissa_geo <- .prepare_melissa_geo(melissa_df)

  # --- Create normalised join keys ----
  melissa_geo$.join_key <- .normalise_address_for_join(melissa_geo$address)
  program_df$.join_key  <- .normalise_address_for_join(program_df$facility_address)

  # Deduplicate Melissa by normalised key (keep first)
  melissa_geo <- melissa_geo[!duplicated(melissa_geo$.join_key), ]

  # --- Join on normalised address ----
  # Drop the address column from melissa_geo to avoid confusion
  melissa_join <- melissa_geo[, setdiff(names(melissa_geo), "address"), drop = FALSE]

  merged <- dplyr::left_join(
    program_df,
    melissa_join,
    by = ".join_key"
  )

  # Remove temporary join key
  merged$.join_key <- NULL

  n_matched <- sum(!is.na(merged$latitude))
  n_unmatched <- n_program - n_matched
  match_pct <- round(n_matched / n_program * 100, 1)

  if (verbose) {
    .msg_step("Matched: {n_matched}/{n_program} ({match_pct}%)")
    if (n_unmatched > 0) {
      .msg_warn("{n_unmatched} programs without geocoding")
    }
  }

  # --- Validate coordinate bounds (Alabama) ----
  if (n_matched > 0) {
    lat_ok <- merged$latitude >= 30.14 & merged$latitude <= 35.01
    lng_ok <- merged$longitude >= -88.48 & merged$longitude <= -84.89
    lat_ok[is.na(lat_ok)] <- TRUE
    lng_ok[is.na(lng_ok)] <- TRUE
    n_out_of_bounds <- sum(!lat_ok | !lng_ok)
    if (n_out_of_bounds > 0 && verbose) {
      .msg_warn("{n_out_of_bounds} coordinates outside Alabama bounding box")
    }
  }

  if (verbose) {
    .msg_success(
      "Merge complete: {n_matched}/{n_program} programs geocoded ({match_pct}%)"
    )
  }

  # --- Return result ----
  if (is.data.frame(program_obj)) {
    return(tibble::as_tibble(merged))
  }

  program_obj$data <- tibble::as_tibble(merged)
  program_obj$meta$n_rows <- nrow(merged)
  program_obj$meta$n_cols <- ncol(merged)
  program_obj <- .log_step(program_obj, glue::glue(
    "Merged Melissa coordinates: {n_matched}/{n_program} matched ({match_pct}%)"
  ))
  program_obj$diagnostics$melissa_match_rate <- match_pct / 100
  program_obj$diagnostics$melissa_n_matched <- n_matched
  program_obj$diagnostics$melissa_n_unmatched <- n_unmatched

  program_obj
}


#' Merge Melissa coordinates into subsidy client household data
#'
#' Joins Melissa geocoding results to subsidy client data. Matching uses
#' \code{family_address} in the subsidy data against \code{address} in the
#' Melissa household geocoding data, with both sides normalised for robust
#' matching.
#'
#' @param subsidy_obj A subsidy-module S3 object (clients type)
#' @param melissa_path Path to the Melissa household geocoding file
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return The input subsidy object augmented with geocoding columns
#'
#' @export
melissa_merge_households <- function(subsidy_obj, melissa_path, verbose = TRUE) {
  if (verbose) {
    .msg_header("Merging Melissa Coordinates into Households", level = 3)
  }

  # --- Load Melissa data ----
  melissa_df <- .read_melissa_file(melissa_path)
  if (verbose) .msg_step("Loaded Melissa data: {nrow(melissa_df)} records")

  # --- Extract subsidy data ----
  subsidy_df <- if (is.data.frame(subsidy_obj)) {
    tibble::as_tibble(subsidy_obj)
  } else {
    alccdf_data(subsidy_obj)
  }
  n_subsidy <- nrow(subsidy_df)

  # --- Prepare Melissa columns (with hh_ prefix to avoid collision) ----
  melissa_geo <- .prepare_melissa_geo(melissa_df, prefix = "hh_")

  # --- Create normalised join keys ----
  melissa_geo$.join_key <- .normalise_address_for_join(melissa_geo$address)

  # Deduplicate Melissa by normalised key
  melissa_geo <- melissa_geo[!duplicated(melissa_geo$.join_key), ]

  # --- Determine join key ----
  join_col <- if ("family_address" %in% names(subsidy_df)) {
    "family_address"
  } else {
    cli::cli_abort("No address column found in subsidy data for geocoding merge")
  }

  subsidy_df$.join_key <- .normalise_address_for_join(subsidy_df[[join_col]])

  # Drop the address column from melissa_geo to avoid confusion
  melissa_join <- melissa_geo[, setdiff(names(melissa_geo), "address"), drop = FALSE]

  merged <- dplyr::left_join(
    subsidy_df,
    melissa_join,
    by = ".join_key"
  )

  # Remove temporary join key
  merged$.join_key <- NULL

  n_matched <- sum(!is.na(merged[[paste0("hh_", "latitude")]]))
  match_pct <- round(n_matched / n_subsidy * 100, 1)

  if (verbose) {
    .msg_step("Matched: {n_matched}/{n_subsidy} ({match_pct}%)")
    .msg_success(
      "Merge complete: {n_matched}/{n_subsidy} households geocoded ({match_pct}%)"
    )
  }

  # --- Return result ----
  if (is.data.frame(subsidy_obj)) {
    return(tibble::as_tibble(merged))
  }

  subsidy_obj$data <- tibble::as_tibble(merged)
  subsidy_obj$meta$n_rows <- nrow(merged)
  subsidy_obj$meta$n_cols <- ncol(merged)
  subsidy_obj <- .log_step(subsidy_obj, glue::glue(
    "Merged Melissa coordinates: {n_matched}/{n_subsidy} matched ({match_pct}%)"
  ))
  subsidy_obj$diagnostics$melissa_match_rate <- match_pct / 100
  subsidy_obj$diagnostics$melissa_n_matched <- n_matched
  subsidy_obj$diagnostics$melissa_n_unmatched <- n_subsidy - n_matched

  subsidy_obj
}


# ---- Internal helpers --------------------------------------------------------

#' Read a Melissa geocoding file (RDS, Excel, or CSV)
#'
#' @param path File path
#' @return A tibble
#' @keywords internal
.read_melissa_file <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("Melissa file not found: {.path {path}}")
  }

  ext <- tolower(tools::file_ext(path))
  df <- switch(ext,
    rds  = readRDS(path),
    xlsx = {
      .check_suggested("readxl", "Excel import")
      readxl::read_excel(path)
    },
    csv  = utils::read.csv(path, stringsAsFactors = FALSE),
    cli::cli_abort("Unsupported file format: {.val {ext}}")
  )

  tibble::as_tibble(df)
}


#' Normalise an address for join matching
#'
#' Creates a canonical form of address strings for robust matching between
#' DHR program data and Melissa geocoding data. Handles common formatting
#' differences: case, punctuation, whitespace, abbreviation variants, and
#' trailing county names appended by Melissa.
#'
#' @param x Character vector of addresses
#' @return Character vector of normalised addresses
#' @keywords internal
.normalise_address_for_join <- function(x) {
  if (is.null(x)) return(x)

  # Uppercase everything

  out <- toupper(as.character(x))

  # Remove trailing county name (Melissa appends ",County" at end)
  # Pattern: ,<county_name> at end of string (after the zip code)
  out <- gsub(",\\s*[A-Z]+\\s*$", "", out)

  # Normalise common abbreviations
  out <- gsub("\\bSUITE\\b", "STE", out)
  out <- gsub("\\bSTREET\\b", "ST", out)
  out <- gsub("\\bAVENUE\\b", "AVE", out)
  out <- gsub("\\bBOULEVARD\\b", "BLVD", out)
  out <- gsub("\\bDRIVE\\b", "DR", out)
  out <- gsub("\\bROAD\\b", "RD", out)
  out <- gsub("\\bLANE\\b", "LN", out)
  out <- gsub("\\bCOURT\\b", "CT", out)
  out <- gsub("\\bCIRCLE\\b", "CIR", out)
  out <- gsub("\\bHIGHWAY\\b", "HWY", out)
  out <- gsub("\\bPLACE\\b", "PL", out)
  out <- gsub("\\bAPARTMENT\\b", "APT", out)
  out <- gsub("\\bBUILDING\\b", "BLDG", out)
  out <- gsub("\\bNORTH\\b", "N", out)
  out <- gsub("\\bSOUTH\\b", "S", out)
  out <- gsub("\\bEAST\\b", "E", out)
  out <- gsub("\\bWEST\\b", "W", out)

  # Remove all punctuation except digits/letters/spaces
  out <- gsub("[^A-Z0-9 ]", " ", out)

  # Collapse whitespace
  out <- gsub("\\s+", " ", out)
  out <- trimws(out)

  out
}


#' Prepare standardised geocoding columns from Melissa output
#'
#' Renames Melissa's raw column names (LAT, LNG, CT, etc.) to clean,
#' snake_case names for the package standard.
#'
#' @param melissa_df Melissa data tibble with raw column names
#' @param prefix Optional prefix for output column names (e.g., "hh_" for
#'   household geocoding to avoid collisions with program geocoding)
#' @return A tibble with address + renamed geocoding columns
#' @keywords internal
.prepare_melissa_geo <- function(melissa_df, prefix = "") {
  # Map Melissa raw names -> clean names
  rename_map <- c(
    "LAT"        = "latitude",
    "LNG"        = "longitude",
    "CT"         = "census_tract",
    "CENSUSBLOC" = "census_block",
    "FIPS"       = "fips_code",
    "COUNTYNAME" = "melissa_county",
    "PLACENAME"  = "melissa_place",
    "PLACECODE"  = "melissa_place_code",
    "MD_City"    = "melissa_city",
    "MD_State"   = "melissa_state",
    "MD_PostalCode" = "melissa_zip",
    "GEOZIP"     = "melissa_geo_zip",
    "RESULTCODE" = "melissa_result_code",
    "STATUSCODE" = "melissa_status_code"
  )

  # Start with address column (join key)
  result <- tibble::tibble(address = melissa_df$address)

  # Add available geocoding columns with optional prefix
  for (raw_name in names(rename_map)) {
    if (raw_name %in% names(melissa_df)) {
      clean_name <- paste0(prefix, rename_map[[raw_name]])
      result[[clean_name]] <- melissa_df[[raw_name]]
    }
  }

  result
}
