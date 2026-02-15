#' @title Address Standardization Utilities
#' @description Internal functions for address cleaning and snapshot date parsing.
#' @name utils-address
#' @keywords internal
NULL

#' Standardize an address string
#'
#' Applies the address standardization pipeline documented in the existing
#' analysis scripts. Handles common abbreviations, directionals, and formatting.
#'
#' @param addr Character vector of address strings
#' @return Character vector of standardized addresses
#' @keywords internal
.standardize_address <- function(addr) {
  if (length(addr) == 0) return(character(0))

  addr <- stringr::str_squish(addr)

  # Remove trailing county name (pattern: ", CountyName" at end)
  addr <- stringr::str_remove(addr, ",\\s*[A-Za-z]+\\s*$")

  # Lowercase first, then standardize

  addr <- stringr::str_to_lower(addr)

  # Standardize road types
  road_types <- c(
    "\\bstreet\\b" = "st",
    "\\bavenue\\b" = "ave",
    "\\broad\\b" = "rd",
    "\\bboulevard\\b" = "blvd",
    "\\bdrive\\b" = "dr",
    "\\blane\\b" = "ln",
    "\\bcourt\\b" = "ct",
    "\\bcircle\\b" = "cir",
    "\\bplace\\b" = "pl",
    "\\bterrace\\b" = "ter",
    "\\bparkway\\b" = "pkwy",
    "\\bhighway\\b" = "hwy",
    "\\bsuite\\b" = "ste",
    "\\bapartment\\b" = "apt"
  )
  for (pattern in names(road_types)) {
    addr <- stringr::str_replace_all(addr, pattern, road_types[[pattern]])
  }

  # Title case
  addr <- stringr::str_to_title(addr)

  # Uppercase directionals
  directionals <- c("Nw", "Ne", "Sw", "Se", "^N ", "^S ", "^E ", "^W ",
                     " N ", " S ", " E ", " W ")
  dir_upper <- c("NW", "NE", "SW", "SE", "N ", "S ", "E ", "W ",
                  " N ", " S ", " E ", " W ")
  for (i in seq_along(directionals)) {
    addr <- stringr::str_replace_all(addr,
      stringr::fixed(directionals[i]), dir_upper[i])
  }

  # Uppercase road abbreviations
  abbrevs <- c("St", "Dr", "Rd", "Ave", "Hwy", "Blvd", "Ln", "Ct",
               "Cir", "Pl", "Ter", "Pkwy", "Ste", "Apt")
  abbrevs_upper <- toupper(abbrevs)
  for (i in seq_along(abbrevs)) {
    pattern <- paste0("\\b", abbrevs[i], "\\b")
    addr <- stringr::str_replace_all(addr, pattern, abbrevs_upper[i])
  }

  # Uppercase state
  addr <- stringr::str_replace_all(addr, "\\bAl\\b", "AL")

  # Clean up spacing around commas
  addr <- stringr::str_replace_all(addr, "\\s*,\\s*", ", ")

  # Final squish
  stringr::str_squish(addr)
}

#' Parse snapshot date from a filename
#'
#' Extracts snapshot dates from known DHR filename patterns.
#'
#' @param filename Character string filename
#' @return Date object, or NA if no date pattern found
#' @keywords internal
.parse_snapshot_date <- function(filename) {
  # Pattern 1: "06_11_25" or "9.12.25" (MM_DD_YY or M.DD.YY)
  m1 <- stringr::str_extract(filename, "(\\d{1,2})[._](\\d{1,2})[._](\\d{2,4})")
  if (!is.na(m1)) {
    parts <- as.numeric(stringr::str_split(m1, "[._]")[[1]])
    if (parts[3] < 100) parts[3] <- parts[3] + 2000
    return(as.Date(sprintf("%04d-%02d-%02d", parts[3], parts[1], parts[2])))
  }

  # Pattern 2: "20250715161036" (YYYYMMDDHHmmss)
  m2 <- stringr::str_extract(filename, "\\d{14}")
  if (!is.na(m2)) {
    return(as.Date(substr(m2, 1, 8), format = "%Y%m%d"))
  }

  # Pattern 3: "2025-06-04" (ISO date)
  m3 <- stringr::str_extract(filename, "\\d{4}-\\d{2}-\\d{2}")
  if (!is.na(m3)) {
    return(as.Date(m3))
  }

  NA_real_
}

#' Parse time string to hms-compatible format
#'
#' @param time_str Character vector of time strings
#' @return Character vector in HH:MM:SS format
#' @keywords internal
.parse_time_string <- function(time_str) {
  if (all(is.na(time_str))) return(time_str)

  # Handle common formats: "6:00 AM", "06:00", "6:00:00 AM", etc.
  time_str <- stringr::str_squish(time_str)

  # Try lubridate parsing
  parsed <- suppressWarnings(
    lubridate::parse_date_time(time_str, orders = c("HM", "HMS", "I:Mp", "I:M:Sp"))
  )

  format(parsed, "%H:%M:%S")
}

#' Parse age range text
#'
#' Extracts minimum and maximum ages from text like "0.25 - 12 Years"
#'
#' @param age_text Character vector of age range strings
#' @return A tibble with columns age_min and age_max
#' @keywords internal
.parse_age_range <- function(age_text) {
  # Pattern: "0.25 - 12 Years" or "0 - 5" or similar
  mins <- as.numeric(stringr::str_extract(age_text, "^[\\d.]+"))
  maxs <- as.numeric(stringr::str_extract(age_text, "[\\d.]+(?=\\s*(Years|$))"))

  tibble::tibble(age_min = mins, age_max = maxs)
}
