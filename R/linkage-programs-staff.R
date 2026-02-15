#' @title Linkage Module: Programs x Staff
#' @description Link program data to Alabama Pathways staff data by facility
#'   name (case-insensitive, trimmed). Programs form the backbone (all
#'   preserved via LEFT JOIN), with per-program staff summaries computed.
#' @name linkage-programs-staff
NULL

#' Link programs to staff data
#'
#' Performs a LEFT JOIN of staff data onto program data by matching
#' \code{facility_name} (case-insensitive, whitespace-trimmed). Computes
#' per-program aggregates: number of staff and career lattice level
#' distribution.
#'
#' @param program_obj A program-module S3 object (e.g.,
#'   \code{alccdf_program_unified}, \code{alccdf_program_deduped},
#'   \code{alccdf_program_clean}) containing \code{facility_name}
#' @param staff_obj A staff-module S3 object (e.g.,
#'   \code{alccdf_staff_clean}) containing \code{facility_name}
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_linked_programs_staff} S3 object with:
#'   \describe{
#'     \item{data}{Program-level tibble with staff aggregates appended}
#'     \item{meta}{Metadata including linkage key, match statistics}
#'     \item{diagnostics}{Unmatched programs, unmatched staff records}
#'   }
#'
#' @details
#' Staff data uses "Facility Name" (mapped to \code{facility_name}) which
#' matches the \code{facility_name} column in program data. Matching is
#' case-insensitive and whitespace-trimmed to handle minor inconsistencies.
#'
#' The linkage pipeline:
#' \enumerate{
#'   \item Extract data from both objects
#'   \item Create normalised facility name keys in both datasets
#'   \item Aggregate staff data to the facility name level
#'   \item LEFT JOIN aggregated staff onto programs by normalised name
#'   \item Compute match diagnostics
#'   \item Build S3 object
#' }
#'
#' @examples
#' \dontrun{
#' linked <- linkage_programs_staff(program_obj, staff_obj)
#' alccdf_data(linked)
#' linked$diagnostics$unmatched_staff
#' }
#'
#' @export
linkage_programs_staff <- function(program_obj, staff_obj,
                                    verbose = TRUE) {
  # --- Validate inputs ----
  if (!is.list(program_obj) || is.null(program_obj$data)) {
    cli::cli_abort("Expected a program-module S3 object with a {.field data} element.")
  }
  if (!is.list(staff_obj) || is.null(staff_obj$data)) {
    cli::cli_abort("Expected a staff-module S3 object with a {.field data} element.")
  }

  if (verbose) {
    .msg_header("Linkage: Programs x Staff", level = 2)
  }

  # --- 1. Extract data ----
  program_df <- alccdf_data(program_obj)
  staff_df   <- alccdf_data(staff_obj)

  if (!"facility_name" %in% names(program_df)) {
    cli::cli_abort("Program data must contain a {.field facility_name} column.")
  }
  if (!"facility_name" %in% names(staff_df)) {
    cli::cli_abort("Staff data must contain a {.field facility_name} column.")
  }

  n_programs <- nrow(program_df)
  n_staff    <- nrow(staff_df)

  if (verbose) {
    .msg_step("Programs: {n_programs} rows")
    .msg_step("Staff: {n_staff} rows")
    .msg_step("Join key: facility_name (case-insensitive, trimmed)")
  }

  # --- 2. Create normalised join key ----
  program_df <- program_df %>%
    dplyr::mutate(
      .join_key = stringr::str_to_lower(stringr::str_squish(
        as.character(.data$facility_name)
      ))
    )

  staff_df <- staff_df %>%
    dplyr::mutate(
      .join_key = stringr::str_to_lower(stringr::str_squish(
        as.character(.data$facility_name)
      ))
    )

  # --- 3. Aggregate staff data per normalised facility name ----
  if (verbose) .msg_step("Aggregating staff data by facility_name")

  staff_agg <- staff_df %>%
    dplyr::filter(!is.na(.data$.join_key), .data$.join_key != "") %>%
    dplyr::group_by(.data$.join_key) %>%
    dplyr::summarise(
      n_staff = dplyr::n(),
      .groups = "drop"
    )

  # Career lattice level distribution (wide format)
  if ("career_lattice_level" %in% names(staff_df)) {
    career_dist <- staff_df %>%
      dplyr::filter(
        !is.na(.data$.join_key), .data$.join_key != "",
        !is.na(.data$career_lattice_level)
      ) %>%
      dplyr::group_by(.data$.join_key, .data$career_lattice_level) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = "career_lattice_level",
        values_from = "n",
        values_fill = 0L,
        names_prefix = "staff_"
      )

    # Clean up column names: replace spaces with underscores
    names(career_dist) <- gsub("\\s+", "_", names(career_dist))

    staff_agg <- dplyr::left_join(staff_agg, career_dist, by = ".join_key")
  }

  if (verbose) {
    .msg_step("Aggregated to {nrow(staff_agg)} unique facility names in staff data")
  }

  # --- 4. LEFT JOIN: programs <- staff aggregates ----
  linked_df <- dplyr::left_join(program_df, staff_agg, by = ".join_key")

  # Fill NA staff count with 0
  linked_df <- linked_df %>%
    dplyr::mutate(
      n_staff = ifelse(is.na(.data$n_staff), 0L, .data$n_staff)
    )

  # Remove the temporary join key
  linked_df <- linked_df %>%
    dplyr::select(-dplyr::any_of(".join_key"))

  # --- 5. Match diagnostics ----
  n_matched <- sum(linked_df$n_staff > 0)
  n_unmatched_programs <- n_programs - n_matched
  match_rate <- round(n_matched / n_programs * 100, 1)

  # Staff records that did not match any program
  staff_keys   <- unique(staff_df$.join_key[!is.na(staff_df$.join_key) &
                                              staff_df$.join_key != ""])
  program_keys <- unique(program_df$.join_key[!is.na(program_df$.join_key) &
                                                program_df$.join_key != ""])
  unmatched_staff_keys <- setdiff(staff_keys, program_keys)

  unmatched_staff <- staff_df %>%
    dplyr::filter(.data$.join_key %in% unmatched_staff_keys) %>%
    dplyr::select(-dplyr::any_of(".join_key"))

  # Programs that received no staff
  unmatched_programs <- linked_df %>%
    dplyr::filter(.data$n_staff == 0L) %>%
    dplyr::select(dplyr::any_of(c("facility_id", "facility_name",
                                    "facility_type", "county")))

  if (verbose) {
    .msg_step("Programs matched: {n_matched}/{n_programs} ({match_rate}%)")
    .msg_step("Programs with no staff records: {n_unmatched_programs}")
    if (length(unmatched_staff_keys) > 0) {
      .msg_warn("{length(unmatched_staff_keys)} staff facility names not found in program data")
    }
  }

  # --- 6. Build S3 object ----
  diagnostics <- list(
    n_programs           = n_programs,
    n_staff_records      = n_staff,
    n_matched            = n_matched,
    n_unmatched_programs = n_unmatched_programs,
    match_rate           = match_rate,
    unmatched_programs   = unmatched_programs,
    unmatched_staff      = unmatched_staff
  )

  obj <- .make_alccdf_obj(
    data       = linked_df,
    class_name = "alccdf_linked_programs_staff",
    module     = "linkage",
    stage      = "programs_staff",
    extra_meta = list(
      join_type  = "left_join",
      join_key   = "facility_name (case-insensitive, trimmed)",
      backbone   = "programs",
      n_programs = n_programs,
      n_staff    = n_staff,
      n_matched  = n_matched,
      match_rate = match_rate
    ),
    diagnostics = diagnostics
  )
  obj <- .log_step(obj, glue::glue(
    "Linked programs x staff: {n_matched}/{n_programs} programs matched ",
    "({match_rate}%), {n_staff} staff records"
  ))

  if (verbose) {
    .msg_success(
      "Linkage complete: {nrow(linked_df)} program rows with staff data"
    )
  }

  obj
}


#' Print method for linked programs-staff objects
#'
#' @param x An \code{alccdf_linked_programs_staff} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_linked_programs_staff <- function(x, ...) {
  cli::cli_h2("ALccdfDB Linkage: Programs x Staff")
  cli::cli_alert_info("Join key: {x$meta$join_key}")
  cli::cli_alert_info("Backbone: {x$meta$backbone}")
  cli::cli_alert_info("Dimensions: {x$meta$n_rows} rows x {x$meta$n_cols} cols")
  cli::cli_alert_info("Programs: {x$meta$n_programs}")
  cli::cli_alert_info("Staff records: {x$meta$n_staff}")
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

#' Export linked programs-staff data to CSV
#'
#' @param obj An \code{alccdf_linked_programs_staff} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_programs_staff_export_csv <- function(obj, path, verbose = TRUE) {
  .assert_class(obj, "alccdf_linked_programs_staff",
                "linkage_programs_staff_export_csv")
  data <- alccdf_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  utils::write.csv(data, path, row.names = FALSE, na = "")
  if (verbose) .msg_success("Exported CSV: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export linked programs-staff data to Excel
#'
#' @param obj An \code{alccdf_linked_programs_staff} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_programs_staff_export_excel <- function(obj, path, verbose = TRUE) {
  .check_suggested("openxlsx", "Excel export")
  .assert_class(obj, "alccdf_linked_programs_staff",
                "linkage_programs_staff_export_excel")
  data <- alccdf_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  openxlsx::write.xlsx(data, path, rowNames = FALSE)
  if (verbose) .msg_success("Exported Excel: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export linked programs-staff data to Stata (.dta)
#'
#' @param obj An \code{alccdf_linked_programs_staff} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_programs_staff_export_stata <- function(obj, path, verbose = TRUE) {
  .check_suggested("haven", "Stata export")
  .assert_class(obj, "alccdf_linked_programs_staff",
                "linkage_programs_staff_export_stata")
  data <- alccdf_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  data <- .prepare_for_stata(data)
  haven::write_dta(data, path)
  if (verbose) .msg_success("Exported Stata: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export linked programs-staff data to Parquet
#'
#' @param obj An \code{alccdf_linked_programs_staff} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_programs_staff_export_parquet <- function(obj, path, verbose = TRUE) {
  .check_suggested("arrow", "Parquet export")
  .assert_class(obj, "alccdf_linked_programs_staff",
                "linkage_programs_staff_export_parquet")
  data <- alccdf_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  arrow::write_parquet(data, path)
  if (verbose) .msg_success("Exported Parquet: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export linked programs-staff data to RDS
#'
#' @param obj An \code{alccdf_linked_programs_staff} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_programs_staff_export_rds <- function(obj, path, verbose = TRUE) {
  .assert_class(obj, "alccdf_linked_programs_staff",
                "linkage_programs_staff_export_rds")
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  saveRDS(obj, path)
  if (verbose) {
    .msg_success("Exported RDS: {.path {path}} ({obj$meta$n_rows} rows)")
  }
  invisible(path)
}

#' Export linked programs-staff data to all supported formats
#'
#' Convenience function that exports to CSV, Excel, Stata, Parquet, and RDS.
#'
#' @param obj An \code{alccdf_linked_programs_staff} object
#' @param dir Output directory
#' @param basename Base filename (without extension)
#' @param formats Character vector of formats to export. Default exports all:
#'   \code{c("csv", "excel", "stata", "parquet", "rds")}.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible named character vector of file paths
#'
#' @export
linkage_programs_staff_export_all <- function(obj,
                                               dir,
                                               basename,
                                               formats = c("csv", "excel",
                                                           "stata", "parquet",
                                                           "rds"),
                                               verbose = TRUE) {
  .assert_class(obj, "alccdf_linked_programs_staff",
                "linkage_programs_staff_export_all")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  ext_map <- c(csv = ".csv", excel = ".xlsx", stata = ".dta",
               parquet = ".parquet", rds = ".rds")
  fn_map <- list(
    csv     = linkage_programs_staff_export_csv,
    excel   = linkage_programs_staff_export_excel,
    stata   = linkage_programs_staff_export_stata,
    parquet = linkage_programs_staff_export_parquet,
    rds     = linkage_programs_staff_export_rds
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
