#' @title Subsidy Module: Export
#' @description Export subsidy data to multiple file formats and generate
#'   summary statistics. Includes PII-safe export: by default, PII fields are
#'   excluded from exports.
#' @name subsidy-export
NULL

#' Export subsidy data to CSV
#'
#' @param obj A subsidy-module S3 object (any stage) or a data frame
#' @param path Output file path
#' @param include_pii Logical; if TRUE, merge PII fields into the export.
#'   Default FALSE (PII excluded for safety).
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#'
#' @export
subsidy_export_csv <- function(obj, path, include_pii = FALSE, verbose = TRUE) {
  data <- .extract_subsidy_export_data(obj, include_pii = include_pii)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  utils::write.csv(data, path, row.names = FALSE, na = "")
  if (verbose) {
    pii_label <- if (include_pii) " (with PII)" else " (PII excluded)"
    .msg_success("Exported CSV: {.path {path}} ({nrow(data)} rows){pii_label}")
  }
  invisible(path)
}


#' Export subsidy data to Excel
#'
#' @param obj A subsidy-module S3 object (any stage) or a data frame
#' @param path Output file path
#' @param include_pii Logical; if TRUE, merge PII fields into the export.
#'   Default FALSE.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#'
#' @export
subsidy_export_excel <- function(obj, path, include_pii = FALSE, verbose = TRUE) {
  .check_suggested("openxlsx", "Excel export")
  data <- .extract_subsidy_export_data(obj, include_pii = include_pii)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  openxlsx::write.xlsx(data, path, rowNames = FALSE)
  if (verbose) {
    pii_label <- if (include_pii) " (with PII)" else " (PII excluded)"
    .msg_success("Exported Excel: {.path {path}} ({nrow(data)} rows){pii_label}")
  }
  invisible(path)
}


#' Export subsidy data to Stata (.dta)
#'
#' @param obj A subsidy-module S3 object (any stage) or a data frame
#' @param path Output file path
#' @param include_pii Logical; if TRUE, merge PII fields into the export.
#'   Default FALSE.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#'
#' @export
subsidy_export_stata <- function(obj, path, include_pii = FALSE, verbose = TRUE) {
  .check_suggested("haven", "Stata export")
  data <- .extract_subsidy_export_data(obj, include_pii = include_pii)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)

  # Stata doesn't handle factors well; convert to character
  data <- .prepare_for_stata(data)
  haven::write_dta(data, path)
  if (verbose) {
    pii_label <- if (include_pii) " (with PII)" else " (PII excluded)"
    .msg_success("Exported Stata: {.path {path}} ({nrow(data)} rows){pii_label}")
  }
  invisible(path)
}


#' Export subsidy data to Parquet
#'
#' @param obj A subsidy-module S3 object (any stage) or a data frame
#' @param path Output file path
#' @param include_pii Logical; if TRUE, merge PII fields into the export.
#'   Default FALSE.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#'
#' @export
subsidy_export_parquet <- function(obj, path, include_pii = FALSE, verbose = TRUE) {
  .check_suggested("arrow", "Parquet export")
  data <- .extract_subsidy_export_data(obj, include_pii = include_pii)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  arrow::write_parquet(data, path)
  if (verbose) {
    pii_label <- if (include_pii) " (with PII)" else " (PII excluded)"
    .msg_success("Exported Parquet: {.path {path}} ({nrow(data)} rows){pii_label}")
  }
  invisible(path)
}


#' Export subsidy data to RDS
#'
#' @param obj A subsidy-module S3 object (any stage) or a data frame
#' @param path Output file path
#' @param include_pii Logical; if TRUE, export the full object including PII.
#'   Default FALSE (PII stripped from export).
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#'
#' @export
subsidy_export_rds <- function(obj, path, include_pii = FALSE, verbose = TRUE) {
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)

  if (include_pii) {
    # Save the full object with PII
    saveRDS(obj, path)
  } else {
    # Strip PII before saving
    export_obj <- obj
    if (!is.null(export_obj$pii)) {
      export_obj$pii <- NULL
    }
    saveRDS(export_obj, path)
  }

  if (verbose) {
    n <- if (is.data.frame(obj)) nrow(obj) else obj$meta$n_rows
    pii_label <- if (include_pii) " (with PII)" else " (PII excluded)"
    .msg_success("Exported RDS: {.path {path}} ({n} rows){pii_label}")
  }
  invisible(path)
}


#' Export subsidy data to all supported formats
#'
#' Convenience function that exports to CSV, Excel, Stata, Parquet, and RDS.
#'
#' @param obj A subsidy-module S3 object (any stage)
#' @param dir Output directory
#' @param basename Base filename (without extension)
#' @param formats Character vector of formats to export. Default exports all:
#'   \code{c("csv", "excel", "stata", "parquet", "rds")}.
#' @param include_pii Logical; if TRUE, merge PII fields into exports.
#'   Default FALSE (PII excluded).
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible named character vector of file paths
#'
#' @export
subsidy_export_all <- function(obj,
                               dir,
                               basename,
                               formats = c("csv", "excel", "stata",
                                           "parquet", "rds"),
                               include_pii = FALSE,
                               verbose = TRUE) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  if (verbose && !include_pii) {
    .msg_step("PII will NOT be included in exports (set include_pii = TRUE to include)")
  }

  ext_map <- c(csv = ".csv", excel = ".xlsx", stata = ".dta",
               parquet = ".parquet", rds = ".rds")
  fn_map <- list(
    csv     = subsidy_export_csv,
    excel   = subsidy_export_excel,
    stata   = subsidy_export_stata,
    parquet = subsidy_export_parquet,
    rds     = subsidy_export_rds
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


#' Generate subsidy summary statistics
#'
#' Computes summary statistics for a subsidy-module S3 object, including counts
#' by county, age distributions, copayment distributions, and provider counts.
#'
#' @param obj A subsidy-module S3 object (any stage from clean onward)
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return A named list of summary tibbles:
#'   \describe{
#'     \item{overview}{One-row tibble with high-level counts}
#'     \item{by_county}{Counts by county}
#'     \item{age_distribution}{Age distribution statistics}
#'     \item{copay_distribution}{Copayment distribution (clients only)}
#'     \item{by_eligibility}{Counts by eligibility category or funding type}
#'   }
#'
#' @examples
#' \dontrun{
#' stats <- subsidy_summary_stats(clean_enrolled)
#' stats$overview
#' stats$by_county
#' }
#'
#' @export
subsidy_summary_stats <- function(obj, verbose = TRUE) {
  data <- .extract_subsidy_data(obj)

  if (verbose) {
    .msg_header("Subsidy Summary Statistics", level = 3)
  }

  # --- Overview ----
  overview <- tibble::tibble(
    n_records  = nrow(data),
    n_columns  = ncol(data),
    n_cases    = if ("case_id" %in% names(data)) {
      length(unique(na.omit(data$case_id)))
    } else NA_integer_,
    n_children = if ("random_child_id" %in% names(data)) {
      length(unique(na.omit(data$random_child_id)))
    } else NA_integer_,
    n_counties = if ("county" %in% names(data)) {
      length(unique(na.omit(data$county)))
    } else NA_integer_,
    n_providers = if ("provider_id" %in% names(data)) {
      length(unique(na.omit(data$provider_id)))
    } else NA_integer_,
    snapshot_date = if ("snapshot_date" %in% names(data)) {
      as.character(max(data$snapshot_date, na.rm = TRUE))
    } else NA_character_
  )

  # --- By county ----
  by_county <- if ("county" %in% names(data)) {
    data %>%
      dplyr::group_by(.data$county) %>%
      dplyr::summarise(
        n   = dplyr::n(),
        pct = round(dplyr::n() / nrow(data) * 100, 1),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(.data$n))
  } else {
    tibble::tibble()
  }

  # --- Age distribution ----
  age_dist <- tibble::tibble()
  if ("child_age" %in% names(data)) {
    age_vals <- data$child_age[!is.na(data$child_age)]
    if (length(age_vals) > 0) {
      age_dist <- tibble::tibble(
        variable = "child_age",
        n_valid  = length(age_vals),
        n_na     = sum(is.na(data$child_age)),
        mean     = round(mean(age_vals), 1),
        median   = stats::median(age_vals),
        sd       = round(stats::sd(age_vals), 1),
        min      = min(age_vals),
        max      = max(age_vals),
        q25      = stats::quantile(age_vals, 0.25),
        q75      = stats::quantile(age_vals, 0.75)
      )
    }
  }

  # --- Copay distribution (clients only) ----
  copay_dist <- tibble::tibble()
  if ("copay_weekly" %in% names(data)) {
    copay_vals <- data$copay_weekly[!is.na(data$copay_weekly)]
    if (length(copay_vals) > 0) {
      copay_dist <- tibble::tibble(
        variable = "copay_weekly",
        n_valid  = length(copay_vals),
        n_na     = sum(is.na(data$copay_weekly)),
        mean     = round(mean(copay_vals), 2),
        median   = stats::median(copay_vals),
        sd       = round(stats::sd(copay_vals), 2),
        min      = min(copay_vals),
        max      = max(copay_vals),
        q25      = stats::quantile(copay_vals, 0.25),
        q75      = stats::quantile(copay_vals, 0.75)
      )
    }
  }

  # --- By eligibility category / funding type ----
  elig_col <- if ("eligibility_category" %in% names(data)) {
    "eligibility_category"
  } else if ("funding_type" %in% names(data)) {
    "funding_type"
  } else {
    NULL
  }

  by_eligibility <- if (!is.null(elig_col)) {
    data %>%
      dplyr::group_by(.data[[elig_col]]) %>%
      dplyr::summarise(
        n   = dplyr::n(),
        pct = round(dplyr::n() / nrow(data) * 100, 1),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(.data$n))
  } else {
    tibble::tibble()
  }

  result <- list(
    overview          = overview,
    by_county         = by_county,
    age_distribution  = age_dist,
    copay_distribution = copay_dist,
    by_eligibility    = by_eligibility
  )

  if (verbose) {
    .msg_step("Records: {overview$n_records}")
    .msg_step("Cases: {overview$n_cases}")
    .msg_step("Children: {overview$n_children}")
    if (nrow(by_county) > 0) {
      .msg_step("Counties represented: {nrow(by_county)}")
    }
    .msg_success("Summary statistics generated")
  }

  result
}


# ---- Internal helpers --------------------------------------------------------

#' Extract data frame from any subsidy module S3 object
#'
#' @param obj A subsidy S3 object or data frame
#' @return A tibble
#' @keywords internal
.extract_subsidy_data <- function(obj) {
  if (is.data.frame(obj)) return(tibble::as_tibble(obj))
  if (!is.null(obj$data)) return(tibble::as_tibble(obj$data))
  cli::cli_abort("Cannot extract data from {.cls {class(obj)[1]}} object.")
}


#' Extract export-ready data from subsidy object, optionally merging PII
#'
#' @param obj A subsidy S3 object or data frame
#' @param include_pii Logical; if TRUE, merge PII fields
#' @return A tibble ready for export
#' @keywords internal
.extract_subsidy_export_data <- function(obj, include_pii = FALSE) {
  if (is.data.frame(obj)) return(tibble::as_tibble(obj))

  data <- tibble::as_tibble(obj$data)

  if (include_pii && !is.null(obj$pii) && nrow(obj$pii) > 0) {
    pii <- obj$pii

    # Merge PII on random IDs
    join_cols <- intersect(
      c("random_parent_id", "random_child_id"),
      intersect(names(data), names(pii))
    )

    if (length(join_cols) > 0) {
      # Determine which PII columns to add (avoid duplicates)
      pii_add_cols <- setdiff(names(pii), names(data))
      if (length(pii_add_cols) > 0) {
        pii_subset <- pii[, c(join_cols, pii_add_cols), drop = FALSE]
        data <- dplyr::left_join(data, pii_subset, by = join_cols)
      }
    }
  }

  data
}
