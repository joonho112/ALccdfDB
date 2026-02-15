#' @title Program Module: Export
#' @description Export program data to multiple file formats and generate
#'   summary statistics for program-level data.
#' @name program-export
NULL

#' Export program data to CSV
#'
#' @param obj A program-module S3 object (any stage) or a data frame
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#'
#' @export
program_export_csv <- function(obj, path, verbose = TRUE) {
  data <- .extract_program_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  utils::write.csv(data, path, row.names = FALSE, na = "")
  if (verbose) .msg_success("Exported CSV: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}


#' Export program data to Excel
#'
#' @param obj A program-module S3 object (any stage) or a data frame
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#'
#' @export
program_export_excel <- function(obj, path, verbose = TRUE) {
  .check_suggested("openxlsx", "Excel export")
  data <- .extract_program_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  openxlsx::write.xlsx(data, path, rowNames = FALSE)
  if (verbose) .msg_success("Exported Excel: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}


#' Export program data to Stata (.dta)
#'
#' @param obj A program-module S3 object (any stage) or a data frame
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#'
#' @export
program_export_stata <- function(obj, path, verbose = TRUE) {
  .check_suggested("haven", "Stata export")
  data <- .extract_program_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)

  # Stata doesn't handle factors well; convert to character
  data <- .prepare_for_stata(data)
  haven::write_dta(data, path)
  if (verbose) .msg_success("Exported Stata: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}


#' Export program data to Parquet
#'
#' @param obj A program-module S3 object (any stage) or a data frame
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#'
#' @export
program_export_parquet <- function(obj, path, verbose = TRUE) {
  .check_suggested("arrow", "Parquet export")
  data <- .extract_program_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  arrow::write_parquet(data, path)
  if (verbose) .msg_success("Exported Parquet: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}


#' Export program data to RDS
#'
#' @param obj A program-module S3 object (any stage) or a data frame
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#'
#' @export
program_export_rds <- function(obj, path, verbose = TRUE) {
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  saveRDS(obj, path)
  if (verbose) {
    n <- if (is.data.frame(obj)) nrow(obj) else obj$meta$n_rows
    .msg_success("Exported RDS: {.path {path}} ({n} rows)")
  }
  invisible(path)
}


#' Export program data to all supported formats
#'
#' Convenience function that exports to CSV, Excel, Stata, Parquet, and RDS.
#'
#' @param obj A program-module S3 object (any stage)
#' @param dir Output directory
#' @param basename Base filename (without extension)
#' @param formats Character vector of formats to export. Default exports all:
#'   \code{c("csv", "excel", "stata", "parquet", "rds")}.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible named character vector of file paths
#'
#' @export
program_export_all <- function(obj,
                               dir,
                               basename,
                               formats = c("csv", "excel", "stata",
                                           "parquet", "rds"),
                               verbose = TRUE) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  ext_map <- c(csv = ".csv", excel = ".xlsx", stata = ".dta",
               parquet = ".parquet", rds = ".rds")
  fn_map <- list(
    csv     = program_export_csv,
    excel   = program_export_excel,
    stata   = program_export_stata,
    parquet = program_export_parquet,
    rds     = program_export_rds
  )

  paths <- character()
  for (fmt in formats) {
    path <- file.path(dir, paste0(basename, ext_map[[fmt]]))
    tryCatch({
      fn_map[[fmt]](obj, path, verbose = verbose)
      paths[fmt] <- path
    }, error = function(e) {
      .msg_warn("Failed to export {fmt}: {conditionMessage(e)}")
    })
  }

  invisible(paths)
}


#' Generate program summary statistics
#'
#' Computes summary statistics for a program-module S3 object, including counts
#' by facility type, county, tier, and capacity distributions.
#'
#' @param obj A program-module S3 object (any stage from clean onward)
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return A named list of summary tibbles:
#'   \describe{
#'     \item{overview}{One-row tibble with high-level counts}
#'     \item{by_type}{Counts by facility_type}
#'     \item{by_county}{Counts by county}
#'     \item{by_tier}{Counts by facility_tier}
#'     \item{capacity}{Capacity distribution statistics}
#'   }
#'
#' @examples
#' \dontrun{
#' stats <- program_summary_stats(unified)
#' stats$overview
#' stats$by_county
#' }
#'
#' @export
program_summary_stats <- function(obj, verbose = TRUE) {
  data <- .extract_program_data(obj)

  if (verbose) {
    .msg_header("Program Summary Statistics", level = 3)
  }

  # --- Overview ----
  overview <- tibble::tibble(
    n_facilities = nrow(data),
    n_columns    = ncol(data),
    n_types      = if ("facility_type" %in% names(data)) {
      length(unique(na.omit(data$facility_type)))
    } else NA_integer_,
    n_counties   = if ("county" %in% names(data)) {
      length(unique(na.omit(data$county)))
    } else NA_integer_,
    snapshot_date = if ("snapshot_date" %in% names(data)) {
      as.character(max(data$snapshot_date, na.rm = TRUE))
    } else NA_character_
  )

  # --- By facility type ----
  by_type <- if ("facility_type" %in% names(data)) {
    data %>%
      dplyr::group_by(.data$facility_type) %>%
      dplyr::summarise(
        n        = dplyr::n(),
        pct      = round(dplyr::n() / nrow(data) * 100, 1),
        .groups  = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(.data$n))
  } else {
    tibble::tibble()
  }

  # --- By county ----
  by_county <- if ("county" %in% names(data)) {
    data %>%
      dplyr::group_by(.data$county) %>%
      dplyr::summarise(
        n        = dplyr::n(),
        pct      = round(dplyr::n() / nrow(data) * 100, 1),
        .groups  = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(.data$n))
  } else {
    tibble::tibble()
  }

  # --- By facility tier ----
  by_tier <- if ("facility_tier" %in% names(data)) {
    data %>%
      dplyr::group_by(.data$facility_tier) %>%
      dplyr::summarise(
        n        = dplyr::n(),
        pct      = round(dplyr::n() / nrow(data) * 100, 1),
        .groups  = "drop"
      ) %>%
      dplyr::arrange(.data$facility_tier)
  } else {
    tibble::tibble()
  }

  # --- Capacity stats ----
  cap_stats <- tibble::tibble()
  for (cap_col in c("day_capacity", "night_capacity")) {
    if (cap_col %in% names(data)) {
      vals <- data[[cap_col]]
      vals <- vals[!is.na(vals)]
      if (length(vals) > 0) {
        row <- tibble::tibble(
          variable = cap_col,
          n_valid  = length(vals),
          n_na     = sum(is.na(data[[cap_col]])),
          mean     = round(mean(vals), 1),
          median   = median(vals),
          sd       = round(stats::sd(vals), 1),
          min      = min(vals),
          max      = max(vals),
          q25      = stats::quantile(vals, 0.25),
          q75      = stats::quantile(vals, 0.75)
        )
        cap_stats <- dplyr::bind_rows(cap_stats, row)
      }
    }
  }

  result <- list(
    overview = overview,
    by_type  = by_type,
    by_county = by_county,
    by_tier  = by_tier,
    capacity = cap_stats
  )

  if (verbose) {
    .msg_step("Facilities: {overview$n_facilities}")
    if (nrow(by_type) > 0) {
      .msg_step("Types: {paste(by_type$facility_type, '(', by_type$n, ')', collapse = ', ')}")
    }
    if (nrow(by_county) > 0) {
      .msg_step("Counties represented: {nrow(by_county)}")
    }
    .msg_success("Summary statistics generated")
  }

  result
}


# ---- Internal helpers --------------------------------------------------------

#' Extract data frame from any program module S3 object
#'
#' @param obj A program S3 object or data frame
#' @return A tibble
#' @keywords internal
.extract_program_data <- function(obj) {
  if (is.data.frame(obj)) return(tibble::as_tibble(obj))
  if (!is.null(obj$data)) return(tibble::as_tibble(obj$data))
  cli::cli_abort("Cannot extract data from {.cls {class(obj)[1]}} object.")
}


#' Prepare data frame for Stata export
#'
#' Converts factor and Date columns to types that Stata can handle, and ensures
#' variable names comply with Stata naming rules.
#'
#' @param data A data frame
#' @return A data frame suitable for haven::write_dta
#' @keywords internal
.prepare_for_stata <- function(data) {
  for (col in names(data)) {
    if (is.factor(data[[col]])) {
      data[[col]] <- as.character(data[[col]])
    }
    if (inherits(data[[col]], "POSIXct")) {
      data[[col]] <- as.character(data[[col]])
    }
  }
  # Stata variable names: alphanumeric + underscore only, max 32 chars
  clean_names <- gsub("[^A-Za-z0-9_]", "_", names(data))
  clean_names <- gsub("_+", "_", clean_names)        # collapse runs
  clean_names <- gsub("^_|_$", "", clean_names)       # trim edges
  clean_names <- substr(clean_names, 1, 32)
  # Ensure uniqueness
  clean_names <- make.unique(clean_names, sep = "_")
  names(data) <- clean_names
  data
}
