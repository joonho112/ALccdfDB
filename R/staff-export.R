#' @title Staff Module: Export
#' @description Export staff data to multiple file formats and generate
#'   summary statistics for staff-level data. Supports PII inclusion control.
#' @name staff-export
NULL

#' Export staff data to CSV
#'
#' @param obj A staff-module S3 object (any stage) or a data frame
#' @param path Output file path
#' @param include_pii Logical; if TRUE and the object has a \code{$pii} table,
#'   join PII fields into the export. Default FALSE.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#'
#' @export
staff_export_csv <- function(obj, path, include_pii = FALSE, verbose = TRUE) {
  data <- .extract_staff_export_data(obj, include_pii)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  utils::write.csv(data, path, row.names = FALSE, na = "")
  if (verbose) {
    pii_msg <- if (include_pii) " (with PII)" else " (without PII)"
    .msg_success("Exported CSV{pii_msg}: {.path {path}} ({nrow(data)} rows)")
  }
  invisible(path)
}


#' Export staff data to Excel
#'
#' @param obj A staff-module S3 object (any stage) or a data frame
#' @param path Output file path
#' @param include_pii Logical; if TRUE and the object has a \code{$pii} table,
#'   join PII fields into the export. Default FALSE.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#'
#' @export
staff_export_excel <- function(obj, path, include_pii = FALSE, verbose = TRUE) {
  .check_suggested("openxlsx", "Excel export")
  data <- .extract_staff_export_data(obj, include_pii)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  openxlsx::write.xlsx(data, path, rowNames = FALSE)
  if (verbose) {
    pii_msg <- if (include_pii) " (with PII)" else " (without PII)"
    .msg_success("Exported Excel{pii_msg}: {.path {path}} ({nrow(data)} rows)")
  }
  invisible(path)
}


#' Export staff data to Stata (.dta)
#'
#' @param obj A staff-module S3 object (any stage) or a data frame
#' @param path Output file path
#' @param include_pii Logical; if TRUE and the object has a \code{$pii} table,
#'   join PII fields into the export. Default FALSE.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#'
#' @export
staff_export_stata <- function(obj, path, include_pii = FALSE, verbose = TRUE) {
  .check_suggested("haven", "Stata export")
  data <- .extract_staff_export_data(obj, include_pii)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)

  # Stata doesn't handle factors well; convert to character
  data <- .prepare_for_stata(data)
  haven::write_dta(data, path)
  if (verbose) {
    pii_msg <- if (include_pii) " (with PII)" else " (without PII)"
    .msg_success("Exported Stata{pii_msg}: {.path {path}} ({nrow(data)} rows)")
  }
  invisible(path)
}


#' Export staff data to Parquet
#'
#' @param obj A staff-module S3 object (any stage) or a data frame
#' @param path Output file path
#' @param include_pii Logical; if TRUE and the object has a \code{$pii} table,
#'   join PII fields into the export. Default FALSE.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#'
#' @export
staff_export_parquet <- function(obj, path, include_pii = FALSE, verbose = TRUE) {
  .check_suggested("arrow", "Parquet export")
  data <- .extract_staff_export_data(obj, include_pii)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  arrow::write_parquet(data, path)
  if (verbose) {
    pii_msg <- if (include_pii) " (with PII)" else " (without PII)"
    .msg_success("Exported Parquet{pii_msg}: {.path {path}} ({nrow(data)} rows)")
  }
  invisible(path)
}


#' Export staff data to RDS
#'
#' @param obj A staff-module S3 object (any stage) or a data frame
#' @param path Output file path
#' @param include_pii Logical; if TRUE, the full object (including \code{$pii})
#'   is saved. If FALSE, PII table is stripped before saving. Default FALSE.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#'
#' @export
staff_export_rds <- function(obj, path, include_pii = FALSE, verbose = TRUE) {
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)

  save_obj <- obj
  if (!include_pii && inherits(obj, "alccdf_staff_clean") && !is.null(obj$pii)) {
    save_obj$pii <- tibble::tibble(
      random_staff_id = obj$pii$random_staff_id,
      note = "PII excluded from export"
    )
  }

  saveRDS(save_obj, path)
  if (verbose) {
    n <- if (is.data.frame(obj)) nrow(obj) else obj$meta$n_rows
    pii_msg <- if (include_pii) " (with PII)" else " (without PII)"
    .msg_success("Exported RDS{pii_msg}: {.path {path}} ({n} rows)")
  }
  invisible(path)
}


#' Export staff data to all supported formats
#'
#' Convenience function that exports to CSV, Excel, Stata, Parquet, and RDS.
#'
#' @param obj A staff-module S3 object (any stage)
#' @param dir Output directory
#' @param basename Base filename (without extension)
#' @param formats Character vector of formats to export. Default exports all:
#'   \code{c("csv", "excel", "stata", "parquet", "rds")}.
#' @param include_pii Logical; if TRUE, include PII in exports. Default FALSE.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible named character vector of file paths
#'
#' @export
staff_export_all <- function(obj,
                             dir,
                             basename,
                             formats = c("csv", "excel", "stata",
                                         "parquet", "rds"),
                             include_pii = FALSE,
                             verbose = TRUE) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  if (include_pii && verbose) {
    .msg_warn("PII inclusion enabled: exported files will contain sensitive data")
  }

  ext_map <- c(csv = ".csv", excel = ".xlsx", stata = ".dta",
               parquet = ".parquet", rds = ".rds")
  fn_map <- list(
    csv     = staff_export_csv,
    excel   = staff_export_excel,
    stata   = staff_export_stata,
    parquet = staff_export_parquet,
    rds     = staff_export_rds
  )

  paths <- character()
  for (fmt in formats) {
    path <- file.path(dir, paste0(basename, ext_map[[fmt]]))
    tryCatch({
      fn_map[[fmt]](obj, path, include_pii = include_pii, verbose = verbose)
      paths[fmt] <- path
    }, error = function(e) {
      .msg_warn("Failed to export {fmt}: {conditionMessage(e)}")
    })
  }

  invisible(paths)
}


#' Generate staff summary statistics
#'
#' Computes summary statistics for a staff-module S3 object, including counts
#' by career lattice level, provider type, county, and operating status.
#'
#' @param obj A staff-module S3 object (any stage from clean onward)
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return A named list of summary tibbles:
#'   \describe{
#'     \item{overview}{One-row tibble with high-level counts}
#'     \item{by_career_level}{Counts by career_lattice_level}
#'     \item{by_provider_type}{Counts by provider_type}
#'     \item{by_user_county}{Counts by user_county}
#'     \item{by_facility_county}{Counts by facility_county}
#'     \item{by_position}{Counts by position}
#'     \item{operating_status}{Counts of currently_operating TRUE/FALSE/NA}
#'   }
#'
#' @examples
#' \dontrun{
#' stats <- staff_summary_stats(staff_clean)
#' stats$overview
#' stats$by_career_level
#' }
#'
#' @export
staff_summary_stats <- function(obj, verbose = TRUE) {
  data <- .extract_staff_data(obj)

  if (verbose) {
    .msg_header("Staff Summary Statistics", level = 3)
  }

  # --- Overview ----
  overview <- tibble::tibble(
    n_staff      = nrow(data),
    n_columns    = ncol(data),
    n_career_levels = if ("career_lattice_level" %in% names(data)) {
      length(unique(na.omit(data$career_lattice_level)))
    } else NA_integer_,
    n_provider_types = if ("provider_type" %in% names(data)) {
      length(unique(na.omit(data$provider_type)))
    } else NA_integer_,
    n_user_counties = if ("user_county" %in% names(data)) {
      length(unique(na.omit(data$user_county)))
    } else NA_integer_,
    n_facility_counties = if ("facility_county" %in% names(data)) {
      length(unique(na.omit(data$facility_county)))
    } else NA_integer_,
    snapshot_date = if ("snapshot_date" %in% names(data)) {
      as.character(max(data$snapshot_date, na.rm = TRUE))
    } else NA_character_
  )

  # --- By career level ----
  by_career_level <- if ("career_lattice_level" %in% names(data)) {
    data %>%
      dplyr::group_by(.data$career_lattice_level) %>%
      dplyr::summarise(
        n       = dplyr::n(),
        pct     = round(dplyr::n() / nrow(data) * 100, 1),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(.data$n))
  } else {
    tibble::tibble()
  }

  # --- By provider type ----
  by_provider_type <- if ("provider_type" %in% names(data)) {
    data %>%
      dplyr::group_by(.data$provider_type) %>%
      dplyr::summarise(
        n       = dplyr::n(),
        pct     = round(dplyr::n() / nrow(data) * 100, 1),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(.data$n))
  } else {
    tibble::tibble()
  }

  # --- By user county ----
  by_user_county <- if ("user_county" %in% names(data)) {
    data %>%
      dplyr::group_by(.data$user_county) %>%
      dplyr::summarise(
        n       = dplyr::n(),
        pct     = round(dplyr::n() / nrow(data) * 100, 1),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(.data$n))
  } else {
    tibble::tibble()
  }

  # --- By facility county ----
  by_facility_county <- if ("facility_county" %in% names(data)) {
    data %>%
      dplyr::group_by(.data$facility_county) %>%
      dplyr::summarise(
        n       = dplyr::n(),
        pct     = round(dplyr::n() / nrow(data) * 100, 1),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(.data$n))
  } else {
    tibble::tibble()
  }

  # --- By position ----
  by_position <- if ("position" %in% names(data)) {
    data %>%
      dplyr::group_by(.data$position) %>%
      dplyr::summarise(
        n       = dplyr::n(),
        pct     = round(dplyr::n() / nrow(data) * 100, 1),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(.data$n))
  } else {
    tibble::tibble()
  }

  # --- Operating status ----
  operating_status <- if ("currently_operating" %in% names(data)) {
    tibble::tibble(
      status   = c("Operating", "Not Operating", "Unknown/NA"),
      n        = c(
        sum(data$currently_operating == TRUE, na.rm = TRUE),
        sum(data$currently_operating == FALSE, na.rm = TRUE),
        sum(is.na(data$currently_operating))
      ),
      pct      = round(c(
        sum(data$currently_operating == TRUE, na.rm = TRUE),
        sum(data$currently_operating == FALSE, na.rm = TRUE),
        sum(is.na(data$currently_operating))
      ) / nrow(data) * 100, 1)
    )
  } else {
    tibble::tibble()
  }

  result <- list(
    overview           = overview,
    by_career_level    = by_career_level,
    by_provider_type   = by_provider_type,
    by_user_county     = by_user_county,
    by_facility_county = by_facility_county,
    by_position        = by_position,
    operating_status   = operating_status
  )

  if (verbose) {
    .msg_step("Staff records: {overview$n_staff}")
    if (nrow(by_career_level) > 0) {
      .msg_step(
        "Career levels: {paste(by_career_level$career_lattice_level, '(', by_career_level$n, ')', collapse = ', ')}"
      )
    }
    if (nrow(by_provider_type) > 0) {
      .msg_step(
        "Provider types: {paste(by_provider_type$provider_type, '(', by_provider_type$n, ')', collapse = ', ')}"
      )
    }
    .msg_success("Staff summary statistics generated")
  }

  result
}


# ---- Internal helpers --------------------------------------------------------

#' Extract data frame from any staff module S3 object
#'
#' @param obj A staff S3 object or data frame
#' @return A tibble (main data only, no PII)
#' @keywords internal
.extract_staff_data <- function(obj) {
  if (is.data.frame(obj)) return(tibble::as_tibble(obj))
  if (!is.null(obj$data)) return(tibble::as_tibble(obj$data))
  cli::cli_abort("Cannot extract data from {.cls {class(obj)[1]}} object.")
}


#' Extract data for export, optionally including PII
#'
#' @param obj A staff S3 object or data frame
#' @param include_pii Logical; whether to join PII fields
#' @return A tibble
#' @keywords internal
.extract_staff_export_data <- function(obj, include_pii = FALSE) {
  if (is.data.frame(obj)) return(tibble::as_tibble(obj))

  data <- tibble::as_tibble(obj$data)

  if (include_pii && !is.null(obj$pii) &&
      "random_staff_id" %in% names(data) &&
      "random_staff_id" %in% names(obj$pii)) {
    # Join PII columns (except random_staff_id which is already in data)
    pii_cols <- setdiff(names(obj$pii), "random_staff_id")
    pii_cols <- pii_cols[pii_cols != "note"]  # exclude placeholder note column
    if (length(pii_cols) > 0) {
      data <- dplyr::left_join(
        data,
        obj$pii[, c("random_staff_id", pii_cols), drop = FALSE],
        by = "random_staff_id"
      )
    }
  }

  data
}
