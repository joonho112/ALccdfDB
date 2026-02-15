#' @title Linkage Module: Programs x Enrolled Children
#' @description Link program data to subsidy enrolled children data by
#'   facility_id. Programs form the backbone (all preserved via LEFT JOIN),
#'   with per-program enrollment summaries computed.
#' @name linkage-programs-enrolled
NULL

#' Link programs to subsidy enrolled children
#'
#' Performs a LEFT JOIN of subsidy enrolled children data onto program data by
#' \code{facility_id}, preserving all program rows. Computes per-program
#' aggregates: number of enrolled children, number of cases, and care level
#' distribution.
#'
#' @param program_obj A program-module S3 object (e.g.,
#'   \code{alccdf_program_unified}, \code{alccdf_program_deduped},
#'   \code{alccdf_program_clean}) containing \code{facility_id}
#' @param enrolled_obj A subsidy-module S3 object with \code{subsidy_type =
#'   "enrolled"} (e.g., \code{alccdf_subsidy_clean})
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_linked_programs_enrolled} S3 object with:
#'   \describe{
#'     \item{data}{Program-level tibble with enrollment aggregates appended}
#'     \item{meta}{Metadata including linkage key, match statistics}
#'     \item{diagnostics}{Unmatched programs, unmatched enrolled records}
#'   }
#'
#' @details
#' The linkage pipeline:
#' \enumerate{
#'   \item Extract data from both objects
#'   \item Aggregate enrolled data to the program (facility_id) level
#'   \item LEFT JOIN aggregated enrollment onto programs by facility_id
#'   \item Compute match diagnostics
#'   \item Build S3 object
#' }
#'
#' Programs without any enrolled children receive \code{NA} for enrollment
#' columns. All 2,193 programs (or however many exist) are preserved.
#'
#' @examples
#' \dontrun{
#' linked <- linkage_programs_enrolled(program_obj, enrolled_obj)
#' alccdf_data(linked)
#' linked$diagnostics$unmatched_programs
#' }
#'
#' @export
linkage_programs_enrolled <- function(program_obj, enrolled_obj,
                                      verbose = TRUE) {
  # --- Validate inputs ----
  if (!is.list(program_obj) || is.null(program_obj$data)) {
    cli::cli_abort("Expected a program-module S3 object with a {.field data} element.")
  }
  .assert_class(enrolled_obj, "alccdf_subsidy_clean",
                "linkage_programs_enrolled")

  if (!identical(enrolled_obj$meta$subsidy_type, "enrolled")) {
    cli::cli_abort(c(
      "Expected a subsidy object with {.val enrolled} type.",
      "i" = "Got {.val {enrolled_obj$meta$subsidy_type}} instead."
    ))
  }

  if (verbose) {
    .msg_header("Linkage: Programs x Enrolled Children", level = 2)
  }


  # --- 1. Extract data ----
  program_df  <- alccdf_data(program_obj)
  enrolled_df <- alccdf_data(enrolled_obj)

  if (!"facility_id" %in% names(program_df)) {
    cli::cli_abort("Program data must contain a {.field facility_id} column.")
  }
  if (!"facility_id" %in% names(enrolled_df)) {
    cli::cli_abort("Enrolled data must contain a {.field facility_id} column.")
  }

  n_programs <- nrow(program_df)
  n_enrolled <- nrow(enrolled_df)

  if (verbose) {
    .msg_step("Programs: {n_programs} rows")
    .msg_step("Enrolled children: {n_enrolled} rows")
    .msg_step("Join key: facility_id")
  }

  # --- 2. Aggregate enrolled data per facility_id ----
  if (verbose) .msg_step("Aggregating enrolled data by facility_id")

  enrolled_agg <- enrolled_df %>%
    dplyr::filter(!is.na(.data$facility_id)) %>%
    dplyr::group_by(.data$facility_id) %>%
    dplyr::summarise(
      n_enrolled_children = dplyr::n(),
      n_cases = length(unique(stats::na.omit(.data$case_id))),
      .groups = "drop"
    )

  # Care level distribution (wide format: one column per care_level)
  if ("care_level" %in% names(enrolled_df)) {
    care_dist <- enrolled_df %>%
      dplyr::filter(!is.na(.data$facility_id), !is.na(.data$care_level)) %>%
      dplyr::group_by(.data$facility_id, .data$care_level) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = "care_level",
        values_from = "n",
        values_fill = 0L,
        names_prefix = "care_level_"
      )

    enrolled_agg <- dplyr::left_join(enrolled_agg, care_dist,
                                      by = "facility_id")
  }

  if (verbose) {
    .msg_step("Aggregated to {nrow(enrolled_agg)} unique facilities in enrolled data")
  }

  # --- 3. LEFT JOIN: programs <- enrolled aggregates ----
  linked_df <- dplyr::left_join(program_df, enrolled_agg, by = "facility_id")

  # Fill NA enrollment counts with 0 for matched context
  linked_df <- linked_df %>%
    dplyr::mutate(
      n_enrolled_children = ifelse(is.na(.data$n_enrolled_children),
                                    0L, .data$n_enrolled_children),
      n_cases = ifelse(is.na(.data$n_cases), 0L, .data$n_cases)
    )

  # --- 4. Match diagnostics ----
  n_matched <- sum(linked_df$n_enrolled_children > 0)
  n_unmatched_programs <- n_programs - n_matched
  match_rate <- round(n_matched / n_programs * 100, 1)

  # Enrolled records that did not match any program
  enrolled_facility_ids <- unique(enrolled_df$facility_id[!is.na(enrolled_df$facility_id)])
  program_facility_ids  <- unique(program_df$facility_id[!is.na(program_df$facility_id)])
  unmatched_enrolled_ids <- setdiff(enrolled_facility_ids, program_facility_ids)

  unmatched_enrolled <- enrolled_df %>%
    dplyr::filter(.data$facility_id %in% unmatched_enrolled_ids)

  # Programs that received no enrolled children

  unmatched_programs <- linked_df %>%
    dplyr::filter(.data$n_enrolled_children == 0L) %>%
    dplyr::select(dplyr::any_of(c("facility_id", "facility_name",
                                    "facility_type", "county")))

  if (verbose) {
    .msg_step("Programs matched: {n_matched}/{n_programs} ({match_rate}%)")
    .msg_step("Programs with no enrolled children: {n_unmatched_programs}")
    if (length(unmatched_enrolled_ids) > 0) {
      .msg_warn("{length(unmatched_enrolled_ids)} enrolled facility_ids not found in program data")
    }
  }

  # --- 5. Build S3 object ----
  diagnostics <- list(
    n_programs           = n_programs,
    n_enrolled_records   = n_enrolled,
    n_matched            = n_matched,
    n_unmatched_programs = n_unmatched_programs,
    match_rate           = match_rate,
    unmatched_programs   = unmatched_programs,
    unmatched_enrolled   = unmatched_enrolled
  )

  obj <- .make_alccdf_obj(
    data       = linked_df,
    class_name = "alccdf_linked_programs_enrolled",
    module     = "linkage",
    stage      = "programs_enrolled",
    extra_meta = list(
      join_type  = "left_join",
      join_key   = "facility_id",
      backbone   = "programs",
      n_programs = n_programs,
      n_enrolled = n_enrolled,
      n_matched  = n_matched,
      match_rate = match_rate
    ),
    diagnostics = diagnostics
  )
  obj <- .log_step(obj, glue::glue(
    "Linked programs x enrolled: {n_matched}/{n_programs} programs matched ",
    "({match_rate}%), {n_enrolled} enrolled records"
  ))

  if (verbose) {
    .msg_success(
      "Linkage complete: {nrow(linked_df)} program rows with enrollment data"
    )
  }

  obj
}


#' Print method for linked programs-enrolled objects
#'
#' @param x An \code{alccdf_linked_programs_enrolled} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_linked_programs_enrolled <- function(x, ...) {
  cli::cli_h2("ALccdfDB Linkage: Programs x Enrolled Children")
  cli::cli_alert_info("Join key: {x$meta$join_key}")
  cli::cli_alert_info("Backbone: {x$meta$backbone}")
  cli::cli_alert_info("Dimensions: {x$meta$n_rows} rows x {x$meta$n_cols} cols")
  cli::cli_alert_info("Programs: {x$meta$n_programs}")
  cli::cli_alert_info("Enrolled records: {x$meta$n_enrolled}")
  cli::cli_alert_info("Match rate: {x$meta$n_matched}/{x$meta$n_programs} ({x$meta$match_rate}%)")
  if (length(x$meta$processing_log) > 0) {
    cli::cli_h3("Processing Log")
    for (entry in x$meta$processing_log) {
      cli::cli_text(entry)
    }
  }
  invisible(x)
}


# ---- Export functions --------------------------------------------------------

#' Export linked programs-enrolled data to CSV
#'
#' @param obj An \code{alccdf_linked_programs_enrolled} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_programs_enrolled_export_csv <- function(obj, path, verbose = TRUE) {
  .assert_class(obj, "alccdf_linked_programs_enrolled",
                "linkage_programs_enrolled_export_csv")
  data <- alccdf_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  utils::write.csv(data, path, row.names = FALSE, na = "")
  if (verbose) .msg_success("Exported CSV: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export linked programs-enrolled data to Excel
#'
#' @param obj An \code{alccdf_linked_programs_enrolled} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_programs_enrolled_export_excel <- function(obj, path, verbose = TRUE) {
  .check_suggested("openxlsx", "Excel export")
  .assert_class(obj, "alccdf_linked_programs_enrolled",
                "linkage_programs_enrolled_export_excel")
  data <- alccdf_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  openxlsx::write.xlsx(data, path, rowNames = FALSE)
  if (verbose) .msg_success("Exported Excel: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export linked programs-enrolled data to Stata (.dta)
#'
#' @param obj An \code{alccdf_linked_programs_enrolled} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_programs_enrolled_export_stata <- function(obj, path, verbose = TRUE) {
  .check_suggested("haven", "Stata export")
  .assert_class(obj, "alccdf_linked_programs_enrolled",
                "linkage_programs_enrolled_export_stata")
  data <- alccdf_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  data <- .prepare_for_stata(data)
  haven::write_dta(data, path)
  if (verbose) .msg_success("Exported Stata: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export linked programs-enrolled data to Parquet
#'
#' @param obj An \code{alccdf_linked_programs_enrolled} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_programs_enrolled_export_parquet <- function(obj, path, verbose = TRUE) {
  .check_suggested("arrow", "Parquet export")
  .assert_class(obj, "alccdf_linked_programs_enrolled",
                "linkage_programs_enrolled_export_parquet")
  data <- alccdf_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  arrow::write_parquet(data, path)
  if (verbose) .msg_success("Exported Parquet: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export linked programs-enrolled data to RDS
#'
#' @param obj An \code{alccdf_linked_programs_enrolled} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_programs_enrolled_export_rds <- function(obj, path, verbose = TRUE) {
  .assert_class(obj, "alccdf_linked_programs_enrolled",
                "linkage_programs_enrolled_export_rds")
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  saveRDS(obj, path)
  if (verbose) {
    .msg_success("Exported RDS: {.path {path}} ({obj$meta$n_rows} rows)")
  }
  invisible(path)
}

#' Export linked programs-enrolled data to all supported formats
#'
#' Convenience function that exports to CSV, Excel, Stata, Parquet, and RDS.
#'
#' @param obj An \code{alccdf_linked_programs_enrolled} object
#' @param dir Output directory
#' @param basename Base filename (without extension)
#' @param formats Character vector of formats to export. Default exports all:
#'   \code{c("csv", "excel", "stata", "parquet", "rds")}.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible named character vector of file paths
#'
#' @export
linkage_programs_enrolled_export_all <- function(obj,
                                                  dir,
                                                  basename,
                                                  formats = c("csv", "excel",
                                                              "stata", "parquet",
                                                              "rds"),
                                                  verbose = TRUE) {
  .assert_class(obj, "alccdf_linked_programs_enrolled",
                "linkage_programs_enrolled_export_all")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  ext_map <- c(csv = ".csv", excel = ".xlsx", stata = ".dta",
               parquet = ".parquet", rds = ".rds")
  fn_map <- list(
    csv     = linkage_programs_enrolled_export_csv,
    excel   = linkage_programs_enrolled_export_excel,
    stata   = linkage_programs_enrolled_export_stata,
    parquet = linkage_programs_enrolled_export_parquet,
    rds     = linkage_programs_enrolled_export_rds
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
