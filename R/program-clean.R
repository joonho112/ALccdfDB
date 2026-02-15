#' @title Program Module: Clean
#' @description Clean and standardise raw program data. Handles address
#'   standardisation, date/time parsing, capacity conversion, age-range
#'   extraction, tier recoding, factor level assignment, and derived-variable
#'   computation matching the established cleaning protocol from
#'   \code{all_programs_appended_cleaned}.
#' @name program-clean
NULL

#' Clean raw program data
#'
#' Applies a comprehensive cleaning pipeline to an \code{alccdf_program_raw}
#' object. All cleaning steps are logged for reproducibility. Produces output
#' consistent with the established \code{all_programs_appended_cleaned}
#' protocol, including proper R factor levels for categorical variables and
#' analysis-ready derived variables.
#'
#' @param raw_obj An \code{alccdf_program_raw} object from
#'   \code{\link{program_read}}
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_program_clean} S3 object
#'
#' @details
#' The cleaning pipeline applies the following operations in order:
#' \enumerate{
#'   \item Standardise addresses (facility_address, physical_address)
#'   \item Create facility_address2 (further standardised)
#'   \item Parse expiration_date and compute days_till_expire
#'   \item Parse time fields and create datetime versions
#'   \item Convert capacities to integer + derive capacity variables
#'   \item Parse age ranges with binary age-group indicators
#'   \item Compute operating hours (day/night) + operation type
#'   \item Recode facility_tier as ordered factor (Star 1--5) with quality metrics
#'   \item Standardise county as factor (67 Alabama counties)
#'   \item Set region as factor
#'   \item Assign facility_type factor using raw data values (preserves
#'     Family Home vs Group Home distinction)
#'   \item Convert children_under_12mo to integer
#' }
#'
#' @examples
#' \dontrun{
#' raw   <- program_read("Centers.xlsx", "center", as.Date("2025-09-12"))
#' clean <- program_clean(raw)
#' }
#'
#' @export
program_clean <- function(raw_obj, verbose = TRUE) {
  .assert_class(raw_obj, "alccdf_program_raw", "program_clean")

  df            <- raw_obj$data
  program_type  <- raw_obj$meta$program_type
  snapshot_date <- raw_obj$meta$snapshot_date

  if (verbose) {
    .msg_header("Cleaning Program Data: {program_type}", level = 3)
  }

  n_start <- nrow(df)

  # --- 1. Standardise addresses ----
  if ("facility_address" %in% names(df)) {
    df$facility_address <- .standardize_address(df$facility_address)
    # Create further-standardised version (facility_address2)
    df$facility_address2 <- .standardize_address_v2(df$facility_address)
    if (verbose) .msg_step("Standardised facility_address + facility_address2")
  }
  if ("physical_address" %in% names(df)) {
    df$physical_address <- .standardize_address(df$physical_address)
    if (verbose) .msg_step("Standardised physical_address")
  }

  # --- 2. Parse expiration_date + compute days_till_expire ----
  if ("expiration_date" %in% names(df)) {
    df$expiration_date <- .parse_program_date(df$expiration_date)
    # current_date = snapshot date for consistency
    df$current_date <- snapshot_date
    df$days_till_expire <- as.integer(
      difftime(df$expiration_date, df$current_date, units = "days")
    )
    if (verbose) {
      n_parsed <- sum(!is.na(df$expiration_date))
      .msg_step("Parsed expiration_date: {n_parsed}/{nrow(df)} valid")
      .msg_step("Computed days_till_expire (ref: {snapshot_date})")
    }
  }

  # --- 3. Parse time fields + create datetime versions ----
  time_cols <- c("day_start", "day_end", "night_start", "night_end")
  for (col in time_cols) {
    if (col %in% names(df)) {
      df[[col]] <- .parse_time_string(df[[col]])
    }
  }

  # Create POSIXct datetime versions using snapshot_date as base
  base_date_str <- format(snapshot_date, "%Y-%m-%d")
  if ("day_start" %in% names(df)) {
    df$day_start_datetime <- .time_to_posixct(df$day_start, base_date_str)
  }
  if ("day_end" %in% names(df)) {
    df$day_end_datetime <- .time_to_posixct(df$day_end, base_date_str)
  }
  if ("night_start" %in% names(df)) {
    df$night_start_datetime <- .time_to_posixct(df$night_start, base_date_str)
  }
  if ("night_end" %in% names(df)) {
    df$night_end_datetime <- .time_to_posixct(df$night_end, base_date_str)
  }
  if (verbose) .msg_step("Parsed time fields + created datetime versions")

  # --- 4. Convert capacities to integer + derive capacity variables ----
  cap_cols <- c("day_capacity", "night_capacity")
  for (col in cap_cols) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.integer(df[[col]]))
    }
  }

  # Binary capacity indicators
  if ("day_capacity" %in% names(df)) {
    df$has_day_capacity <- as.integer(!is.na(df$day_capacity) & df$day_capacity > 0)
  }
  if ("night_capacity" %in% names(df)) {
    df$has_night_capacity <- as.integer(!is.na(df$night_capacity) & df$night_capacity > 0)
  }

  # Total capacity and night-to-day ratio
  if (all(c("day_capacity", "night_capacity") %in% names(df))) {
    df$total_capacity <- ifelse(
      is.na(df$day_capacity) & is.na(df$night_capacity),
      NA_integer_,
      as.integer(ifelse(is.na(df$day_capacity), 0L, df$day_capacity) +
                   ifelse(is.na(df$night_capacity), 0L, df$night_capacity))
    )
    df$night_to_day_ratio <- ifelse(
      !is.na(df$day_capacity) & df$day_capacity > 0,
      round(ifelse(is.na(df$night_capacity), 0, df$night_capacity) /
              df$day_capacity, 3),
      NA_real_
    )
  }

  if (verbose) .msg_step("Converted capacity fields to integer + derived capacity variables")

  # --- 5. Parse age ranges with binary indicators ----
  # Age group thresholds match established cleaning protocol:
  #   infants_toddlers: age_min < 3 (serves ages 0-3)
  #   preschool: age_min < 5 AND age_max >= 3 (serves ages 3-5)
  #   school_aged: age_max >= 5 (serves ages 5+)
  if ("day_age_range" %in% names(df)) {
    age_parsed <- .parse_age_range(df$day_age_range)
    df$day_age_min <- age_parsed$age_min
    df$day_age_max <- age_parsed$age_max
    df$day_age_range_num <- ifelse(
      !is.na(age_parsed$age_max) & !is.na(age_parsed$age_min),
      age_parsed$age_max - age_parsed$age_min,
      NA_real_
    )
    # Binary indicators for age groups served (matching user's established protocol)
    df$day_age_infants_toddlers <- ifelse(
      !is.na(age_parsed$age_min), as.integer(age_parsed$age_min < 3), NA_integer_
    )
    df$day_age_preschool <- ifelse(
      !is.na(age_parsed$age_min) & !is.na(age_parsed$age_max),
      as.integer(age_parsed$age_min < 5 & age_parsed$age_max >= 3),
      NA_integer_
    )
    df$day_age_school_aged <- ifelse(
      !is.na(age_parsed$age_max),
      as.integer(age_parsed$age_max >= 5),
      NA_integer_
    )
    # Clean day_age_range text (standardise "N/A" to NA)
    df$day_age_range <- ifelse(
      toupper(stringr::str_squish(df$day_age_range)) %in% c("N/A", "NA", ""),
      NA_character_,
      df$day_age_range
    )
    if (verbose) {
      n_age <- sum(!is.na(df$day_age_min))
      .msg_step("Parsed day age ranges: {n_age}/{nrow(df)} + binary indicators")
    }
  }

  if ("night_age_range" %in% names(df)) {
    night_age <- .parse_age_range(df$night_age_range)
    df$night_age_min <- night_age$age_min
    df$night_age_max <- night_age$age_max
    df$night_age_range_num <- ifelse(
      !is.na(night_age$age_max) & !is.na(night_age$age_min),
      night_age$age_max - night_age$age_min,
      NA_real_
    )
    df$night_age_infants_toddlers <- ifelse(
      !is.na(night_age$age_min), as.integer(night_age$age_min < 3), NA_integer_
    )
    df$night_age_preschool <- ifelse(
      !is.na(night_age$age_min) & !is.na(night_age$age_max),
      as.integer(night_age$age_min < 5 & night_age$age_max >= 3),
      NA_integer_
    )
    df$night_age_school_aged <- ifelse(
      !is.na(night_age$age_max),
      as.integer(night_age$age_max >= 5),
      NA_integer_
    )
    df$night_age_range <- ifelse(
      toupper(stringr::str_squish(df$night_age_range)) %in% c("N/A", "NA", ""),
      NA_character_,
      df$night_age_range
    )
  }

  # --- 6. Compute derived operating hours + operation type ----
  df <- .compute_oper_hours(df)

  # Operation type classification
  if (all(c("has_day_capacity", "has_night_capacity") %in% names(df))) {
    df$operation_type <- dplyr::case_when(
      df$has_day_capacity == 1L & df$has_night_capacity == 1L ~ "Day & Night",
      df$has_day_capacity == 1L & df$has_night_capacity == 0L ~ "Day Only",
      df$has_day_capacity == 0L & df$has_night_capacity == 1L ~ "Night Only",
      TRUE ~ "No Operation"
    )
  }
  if (verbose) .msg_step("Computed operating hours + operation type")

  # --- 7. Recode facility_tier as ordered factor + quality metrics ----
  if ("facility_tier" %in% names(df)) {
    df$facility_tier <- .recode_facility_tier(df$facility_tier)

    # Preserve original tier for reference (before any NA imputation)
    df$facility_tier_original <- df$facility_tier

    # was_originally_rated: TRUE if facility had a tier assigned
    df$was_originally_rated <- !is.na(df$facility_tier)

    # is_high_quality: Star 3 or above (NAs treated as not high quality)
    df$is_high_quality <- !is.na(df$facility_tier) &
      as.integer(df$facility_tier) >= 3L

    # Quality weight schemes (used in E2SFCA and spatial analysis)
    # Linear: equal 0.2 increments per star level
    # Binary: Star 3+ = 1.0, else 0.0
    # Exponential: steeper at high end (0.1, 0.2, 0.4, 0.7, 1.0)
    tier_int <- as.integer(df$facility_tier)
    df$tier_weight_linear <- dplyr::case_when(
      tier_int == 1L ~ 0.2,
      tier_int == 2L ~ 0.4,
      tier_int == 3L ~ 0.6,
      tier_int == 4L ~ 0.8,
      tier_int == 5L ~ 1.0,
      TRUE ~ NA_real_
    )
    df$tier_weight_binary <- dplyr::case_when(
      tier_int >= 3L ~ 1.0,
      tier_int %in% c(1L, 2L) ~ 0.0,
      TRUE ~ NA_real_
    )
    df$tier_weight_exp <- dplyr::case_when(
      tier_int == 1L ~ 0.1,
      tier_int == 2L ~ 0.2,
      tier_int == 3L ~ 0.4,
      tier_int == 4L ~ 0.7,
      tier_int == 5L ~ 1.0,
      TRUE ~ NA_real_
    )

    # Quality-adjusted capacity (primary supply measure for spatial analysis)
    if ("day_capacity" %in% names(df)) {
      dc <- ifelse(is.na(df$day_capacity), 0, df$day_capacity)
      df$capacity_qa_linear <- round(dc * ifelse(is.na(df$tier_weight_linear), 0, df$tier_weight_linear), 2)
      df$capacity_qa_binary <- round(dc * ifelse(is.na(df$tier_weight_binary), 0, df$tier_weight_binary), 2)
      df$capacity_qa_exp    <- round(dc * ifelse(is.na(df$tier_weight_exp), 0, df$tier_weight_exp), 2)
    }

    n_rated <- sum(!is.na(df$facility_tier))
    if (verbose) .msg_step("Recoded facility_tier: {n_rated} rated, {sum(is.na(df$facility_tier))} NA + quality metrics")
  }

  # --- 8. Standardise county as factor ----
  if ("county" %in% names(df)) {
    df$county <- stringr::str_to_title(stringr::str_squish(df$county))
    df$county <- gsub("\\bDe\\b", "De", df$county)  # DeKalb
    county_levels <- sort(unique(na.omit(df$county)))
    df$county <- factor(df$county, levels = county_levels)
    if (verbose) .msg_step("Standardised county: {length(county_levels)} levels (factor)")
  }

  # --- 9. Set region as factor ----
  if ("region" %in% names(df)) {
    df$region <- stringr::str_squish(df$region)
    region_levels <- sort(unique(na.omit(df$region)))
    df$region <- factor(df$region, levels = region_levels)
    if (verbose) .msg_step("Set region: {length(region_levels)} levels (factor)")
  }

  # --- 10. Assign facility_type factor ----
  # The column maps rename raw type to "facility_type_raw". Check both column
  # names so the case_when logic (which distinguishes Family Home vs Group Home)
  # is always reached when the raw data contains type information.
  ft_source_col <- if ("facility_type_raw" %in% names(df)) {
    "facility_type_raw"
  } else if ("facility_type" %in% names(df)) {
    "facility_type"
  } else {
    NULL
  }

  if (!is.null(ft_source_col) && !all(is.na(df[[ft_source_col]]))) {
    # Preserve the original raw value for audit trail
    if (ft_source_col == "facility_type_raw") {
      # Already has facility_type_raw; keep it as-is
    } else {
      # Rename existing facility_type to facility_type_raw before overwriting
      df$facility_type_raw <- df$facility_type
    }

    # Standardise raw values to canonical levels
    ft_raw <- stringr::str_squish(as.character(df[[ft_source_col]]))
    ft_clean <- dplyr::case_when(
      grepl("^Center$", ft_raw, ignore.case = TRUE)                  ~ "Center",
      grepl("^Licensed\\s*Center", ft_raw, ignore.case = TRUE)       ~ "Center",
      grepl("^Family\\s*Home$", ft_raw, ignore.case = TRUE)          ~ "Family Home",
      grepl("^Group\\s*Home$", ft_raw, ignore.case = TRUE)           ~ "Group Home",
      grepl("^Faith", ft_raw, ignore.case = TRUE)                     ~ "Faith-Based",
      grepl("Excepted.*University", ft_raw, ignore.case = TRUE)      ~ "Excepted (University/Other)",
      grepl("Excepted.*School", ft_raw, ignore.case = TRUE)          ~ "Excepted (University/Other)",
      grepl("Excepted.*Out\\s*of\\s*School", ft_raw, ignore.case = TRUE) ~ "Excepted (University/Other)",
      TRUE ~ NA_character_
    )
    df$facility_type <- factor(
      ft_clean,
      levels = c("Center", "Family Home", "Group Home",
                  "Faith-Based", "Excepted (University/Other)")
    )
  } else {
    # Fall back: assign from program_type argument
    type_label <- .program_type_label_v3(program_type)
    df$facility_type_raw <- type_label
    df$facility_type <- factor(
      type_label,
      levels = c("Center", "Family Home", "Group Home",
                  "Faith-Based", "Excepted (University/Other)")
    )
  }
  if (verbose) {
    ft_dist <- table(df$facility_type, useNA = "ifany")
    .msg_step("Set facility_type factor: {paste(names(ft_dist), '(', ft_dist, ')', collapse = ', ')}")
  }

  # --- 11. Capacity flag (business rule validation) ----
  if ("day_capacity" %in% names(df) && "facility_type" %in% names(df)) {
    ft_char <- as.character(df$facility_type)
    dc <- ifelse(is.na(df$day_capacity), 0L, df$day_capacity)
    nc <- if ("night_capacity" %in% names(df)) {
      ifelse(is.na(df$night_capacity), 0L, df$night_capacity)
    } else {
      rep(0L, nrow(df))
    }

    df$capacity_flag <- dplyr::case_when(
      ft_char == "Family Home" & dc > 8   ~ "Review: Family Home > 8",
      ft_char == "Group Home"  & dc > 12  ~ "Review: Group Home > 12",
      ft_char == "Center"      & dc > 200 ~ "Review: Center > 200",
      nc > dc & nc > 0                     ~ "Review: Night > Day",
      TRUE ~ "OK"
    )
    if (verbose) {
      n_flagged <- sum(df$capacity_flag != "OK")
      .msg_step("Capacity flag: {n_flagged} facilities flagged for review")
    }
  }

  # --- 12. Convert children_under_12mo to integer ----
  if ("children_under_12mo" %in% names(df)) {
    df$children_under_12mo <- suppressWarnings(as.integer(df$children_under_12mo))
    if (verbose) .msg_step("Converted children_under_12mo to integer")
  }

  # --- 13. Add snapshot_date column ----
  df$snapshot_date <- snapshot_date

  # --- Build S3 object ----
  obj <- .make_alccdf_obj(
    data          = df,
    class_name    = "alccdf_program_clean",
    module        = "program",
    stage         = "clean",
    snapshot_date = snapshot_date,
    extra_meta    = list(
      program_type   = program_type,
      format_version = raw_obj$meta$format_version,
      source_file    = raw_obj$meta$source_file,
      n_rows_raw     = n_start,
      cleaning_steps = c(
        "address_standardisation",
        "address2_creation",
        "date_parsing",
        "days_till_expire",
        "time_parsing",
        "datetime_creation",
        "capacity_conversion",
        "capacity_derived_vars",
        "age_range_extraction",
        "age_binary_indicators",
        "operating_hours_computation",
        "operation_type_classification",
        "tier_recoding",
        "tier_quality_metrics",
        "county_factor",
        "region_factor",
        "facility_type_factor",
        "capacity_flag",
        "children_under_12mo_conversion"
      )
    )
  )
  obj <- .log_step(obj, glue::glue(
    "Cleaned {program_type} data: {nrow(df)} rows, ",
    "{ncol(df)} cols (from {n_start} raw rows)"
  ))

  if (verbose) {
    .msg_success("Cleaning complete: {nrow(df)} rows x {ncol(df)} cols")
  }

  obj
}


#' Clean all raw program objects from a list
#'
#' Convenience wrapper that applies \code{\link{program_clean}} to every
#' element of a list of \code{alccdf_program_raw} objects.
#'
#' @param raw_list A named list of \code{alccdf_program_raw} objects (as
#'   returned by \code{\link{program_read_all}})
#' @param verbose Logical; print progress messages?
#' @return A named list of \code{alccdf_program_clean} objects
#'
#' @export
program_clean_all <- function(raw_list, verbose = TRUE) {
  purrr::map(raw_list, program_clean, verbose = verbose)
}


# ---- Internal helpers --------------------------------------------------------

#' Parse program date fields
#'
#' Handles character dates that may come from Excel as serial numbers, POSIXct
#' text, or standard date strings.
#'
#' @param x Character vector of date values
#' @return Date vector
#' @keywords internal
.parse_program_date <- function(x) {
  if (all(is.na(x))) return(as.Date(rep(NA, length(x))))

  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct")) return(as.Date(x))

  # Try Excel serial number (numeric strings like "45814")
  numeric_vals <- suppressWarnings(as.numeric(x))
  is_serial <- !is.na(numeric_vals) & numeric_vals > 30000 & numeric_vals < 60000

  result <- as.Date(rep(NA, length(x)))

  if (any(is_serial, na.rm = TRUE)) {
    result[is_serial] <- as.Date(numeric_vals[is_serial], origin = "1899-12-30")
  }

  remaining <- !is_serial & !is.na(x)
  if (any(remaining)) {
    parsed <- suppressWarnings(
      lubridate::parse_date_time(
        x[remaining],
        orders = c("Ymd HMS", "Ymd", "mdY", "mdy", "dmy"),
        quiet = TRUE
      )
    )
    result[remaining] <- as.Date(parsed)
  }

  result
}


#' Create further-standardised address (v2)
#'
#' Applies additional standardisation beyond the basic address cleaning:
#' normalises abbreviations, fixes directionals consistently. Matches the
#' \code{facility_address2} logic from the established cleaning protocol.
#'
#' @param x Character vector of addresses (already through basic standardisation)
#' @return Character vector
#' @keywords internal
.standardize_address_v2 <- function(x) {
  if (is.null(x)) return(x)

  x2 <- x

  # Step 1: Remove trailing county name (e.g., ", Jefferson")
  x2 <- gsub(",\\s*[A-Za-z]+$", "", x2)
  x2 <- stringr::str_squish(x2)

  # Step 2: Convert to lowercase for standardisation
  x2 <- tolower(x2)

  # Step 3: Standardize road types (full to abbreviation)
  x2 <- gsub("\\bstreet\\b", "st", x2)
  x2 <- gsub("\\bavenue\\b", "ave", x2)
  x2 <- gsub("\\broad\\b", "rd", x2)
  x2 <- gsub("\\bdrive\\b", "dr", x2)
  x2 <- gsub("\\blane\\b", "ln", x2)
  x2 <- gsub("\\btrail\\b", "trl", x2)
  x2 <- gsub("\\bhighway\\b", "hwy", x2)
  x2 <- gsub("\\bboulevard\\b", "blvd", x2)
  x2 <- gsub("\\bcircle\\b", "cir", x2)
  x2 <- gsub("\\bcourt\\b", "ct", x2)

  # Step 4: Convert to Title Case
  x2 <- stringr::str_to_title(x2)

  # Step 5: Force directions and road types to uppercase
  x2 <- gsub("\\bNw\\b", "NW", x2)
  x2 <- gsub("\\bNe\\b", "NE", x2)
  x2 <- gsub("\\bSw\\b", "SW", x2)
  x2 <- gsub("\\bSe\\b", "SE", x2)
  x2 <- gsub("\\bN\\b", "N", x2)
  x2 <- gsub("\\bS\\b", "S", x2)
  x2 <- gsub("\\bE\\b", "E", x2)
  x2 <- gsub("\\bW\\b", "W", x2)
  x2 <- gsub("\\bSt\\b", "ST", x2)
  x2 <- gsub("\\bDr\\b", "DR", x2)
  x2 <- gsub("\\bRd\\b", "RD", x2)
  x2 <- gsub("\\bAve\\b", "AVE", x2)
  x2 <- gsub("\\bHwy\\b", "HWY", x2)
  x2 <- gsub("\\bLn\\b", "LN", x2)
  x2 <- gsub("\\bTrl\\b", "TRL", x2)
  x2 <- gsub("\\bBlvd\\b", "BLVD", x2)
  x2 <- gsub("\\bCir\\b", "CIR", x2)
  x2 <- gsub("\\bCt\\b", "CT", x2)
  x2 <- gsub("\\bAl\\b", "AL", x2)

  # Step 6: Standardize Suite
  x2 <- gsub("Suite\\s+(\\d+)", "STE \\1", x2, ignore.case = TRUE)

  # Step 7: Ensure space after commas
  x2 <- gsub(",(?!\\s)", ", ", x2, perl = TRUE)

  x2 <- stringr::str_squish(x2)
  x2
}


#' Convert time string to POSIXct datetime
#'
#' @param time_str Character vector of HH:MM:SS formatted times
#' @param base_date_str Date string (YYYY-MM-DD) to use as the base
#' @return POSIXct vector
#' @keywords internal
.time_to_posixct <- function(time_str, base_date_str) {
  result <- as.POSIXct(rep(NA, length(time_str)))
  valid <- !is.na(time_str) & nchar(time_str) > 0
  if (any(valid)) {
    datetime_str <- paste(base_date_str, time_str[valid])
    result[valid] <- as.POSIXct(datetime_str, format = "%Y-%m-%d %H:%M:%S",
                                 tz = "America/Chicago")
  }
  result
}


#' Compute operating hours from start/end time strings
#'
#' @param df Data frame with time columns
#' @return Data frame with added day_oper_hours and night_oper_hours columns
#' @keywords internal
.compute_oper_hours <- function(df) {
  if (all(c("day_start", "day_end") %in% names(df))) {
    df$day_oper_hours <- .time_diff_hours(df$day_start, df$day_end)
  }
  if (all(c("night_start", "night_end") %in% names(df))) {
    df$night_oper_hours <- .time_diff_hours(df$night_start, df$night_end)
  }
  df
}


#' Calculate time difference in hours
#'
#' @param start_str Character vector of start times (HH:MM:SS)
#' @param end_str Character vector of end times (HH:MM:SS)
#' @return Numeric vector of hours
#' @keywords internal
.time_diff_hours <- function(start_str, end_str) {
  to_hours <- function(time_str) {
    parts <- strsplit(time_str, ":")
    vapply(parts, function(p) {
      if (any(is.na(p)) || length(p) < 2) return(NA_real_)
      h <- as.numeric(p[1])
      m <- as.numeric(p[2])
      s <- if (length(p) >= 3) as.numeric(p[3]) else 0
      h + m / 60 + s / 3600
    }, numeric(1))
  }

  start_h <- to_hours(start_str)
  end_h   <- to_hours(end_str)

  diff <- end_h - start_h
  # Overnight correction (e.g., 22:00 to 06:00 = 8 hours)
  diff <- ifelse(!is.na(diff) & diff < 0, diff + 24, diff)
  round(diff, 2)
}


#' Recode facility tier values
#'
#' Converts facility tier strings to an ordered factor. "None", empty strings,
#' and similar non-star values are mapped to NA.
#'
#' @param tier_raw Character vector of raw tier values
#' @return Ordered factor with levels Star 1 through Star 5
#' @keywords internal
.recode_facility_tier <- function(tier_raw) {
  tier <- stringr::str_squish(as.character(tier_raw))

  tier <- dplyr::case_when(
    is.na(tier) | tier == "" | tolower(tier) == "none" ~ NA_character_,
    grepl("^\\s*1\\s*$", tier) | grepl("star\\s*1", tolower(tier)) ~ "Star 1",
    grepl("^\\s*2\\s*$", tier) | grepl("star\\s*2", tolower(tier)) ~ "Star 2",
    grepl("^\\s*3\\s*$", tier) | grepl("star\\s*3", tolower(tier)) ~ "Star 3",
    grepl("^\\s*4\\s*$", tier) | grepl("star\\s*4", tolower(tier)) ~ "Star 4",
    grepl("^\\s*5\\s*$", tier) | grepl("star\\s*5", tolower(tier)) ~ "Star 5",
    TRUE ~ NA_character_
  )

  factor(tier, levels = c("Star 1", "Star 2", "Star 3", "Star 4", "Star 5"),
         ordered = TRUE)
}


#' Map program type codes to clean labels (v3)
#'
#' @param type Internal type code
#' @return Character label
#' @keywords internal
.program_type_label_v3 <- function(type) {
  switch(type,
    center      = "Center",
    family_home = "Family Home",
    exempt      = "Faith-Based",
    excepted    = "Excepted (University/Other)",
    cli::cli_abort("Unknown program type: {.val {type}}")
  )
}


#' Return the canonical program type factor levels (v3)
#'
#' @return Character vector of factor levels
#' @export
alccdf_facility_type_levels <- function() {
  c("Center", "Family Home", "Group Home",
    "Faith-Based", "Excepted (University/Other)")
}


#' Return the canonical facility tier levels
#'
#' @return Character vector of ordered factor levels
#' @export
alccdf_facility_tier_levels <- function() {
  c("Star 1", "Star 2", "Star 3", "Star 4", "Star 5")
}


#' Print method for clean program objects
#'
#' @param x An \code{alccdf_program_clean} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_program_clean <- function(x, ...) {
  cli::cli_h2("ALccdfDB Program Data (Clean)")
  cli::cli_alert_info("Type: {x$meta$program_type}")
  cli::cli_alert_info("Snapshot: {x$meta$snapshot_date}")
  cli::cli_alert_info("Dimensions: {x$meta$n_rows} rows x {x$meta$n_cols} cols")
  cli::cli_alert_info("Raw rows: {x$meta$n_rows_raw}")

  cols <- names(x$data)
  cli::cli_alert_info("Columns: {paste(cols, collapse = ', ')}")

  if (length(x$meta$processing_log) > 0) {
    cli::cli_h3("Processing Log")
    for (entry in x$meta$processing_log) {
      cli::cli_text(entry)
    }
  }
  invisible(x)
}
