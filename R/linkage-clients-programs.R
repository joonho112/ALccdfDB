#' @title Linkage Module: Clients x Programs
#' @description Link subsidy client data to program data by provider_id.
#'   Clients form the backbone (all individual client rows preserved via LEFT
#'   JOIN), with program columns appended for spatial analysis.
#' @name linkage-clients-programs
NULL

#' Link subsidy clients to program data
#'
#' Performs a LEFT JOIN of program data onto subsidy client data by
#' \code{provider_id}, preserving all individual client rows. Each client row
#' receives program-level columns (e.g., \code{facility_type},
#' \code{latitude}, \code{longitude}, \code{county}, \code{facility_tier})
#' for spatial and analytic use.
#'
#' This linkage is designed for spatial analysis comparing household locations
#' (from client \code{family_address}) against program locations (from program
#' \code{latitude}/\code{longitude}).
#'
#' @param clients_obj A subsidy-module S3 object with \code{subsidy_type =
#'   "clients"} (e.g., \code{alccdf_subsidy_clean})
#' @param program_obj A program-module S3 object (e.g.,
#'   \code{alccdf_program_unified}, \code{alccdf_program_deduped},
#'   \code{alccdf_program_clean}) containing \code{provider_id}
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_linked_clients_programs} S3 object with:
#'   \describe{
#'     \item{data}{Client-level tibble with program columns appended}
#'     \item{meta}{Metadata including linkage key, match statistics}
#'     \item{diagnostics}{Unmatched clients, unmatched programs}
#'   }
#'
#' @details
#' The linkage pipeline:
#' \enumerate{
#'   \item Extract data from both objects
#'   \item Select relevant program columns to append (avoiding name collisions)
#'   \item LEFT JOIN program data onto clients by provider_id
#'   \item Compute match diagnostics
#'   \item Build S3 object
#' }
#'
#' Clients without a matching program receive \code{NA} for all program
#' columns. All client rows are preserved.
#'
#' Note: If a \code{provider_id} matches multiple programs, client rows will
#' be duplicated. The diagnostics report any such expansion.
#'
#' @examples
#' \dontrun{
#' linked <- linkage_clients_programs(clients_obj, program_obj)
#' alccdf_data(linked)
#' # Spatial analysis: household vs program coordinates
#' linked$data %>%
#'   dplyr::filter(!is.na(latitude)) %>%
#'   dplyr::select(family_address, provider_name, latitude, longitude)
#' }
#'
#' @export
linkage_clients_programs <- function(clients_obj, program_obj,
                                      verbose = TRUE) {
  # --- Validate inputs ----
  .assert_class(clients_obj, "alccdf_subsidy_clean",
                "linkage_clients_programs")

  if (!identical(clients_obj$meta$subsidy_type, "clients")) {
    cli::cli_abort(c(
      "Expected a subsidy object with {.val clients} type.",
      "i" = "Got {.val {clients_obj$meta$subsidy_type}} instead."
    ))
  }

  if (!is.list(program_obj) || is.null(program_obj$data)) {
    cli::cli_abort("Expected a program-module S3 object with a {.field data} element.")
  }

  if (verbose) {
    .msg_header("Linkage: Clients x Programs", level = 2)
  }

  # --- 1. Extract data ----
  clients_df <- alccdf_data(clients_obj)
  program_df <- alccdf_data(program_obj)

  if (!"provider_id" %in% names(clients_df)) {
    cli::cli_abort("Clients data must contain a {.field provider_id} column.")
  }
  if (!"provider_id" %in% names(program_df)) {
    cli::cli_abort("Program data must contain a {.field provider_id} column.")
  }

  n_clients  <- nrow(clients_df)
  n_programs <- nrow(program_df)

  if (verbose) {
    .msg_step("Clients: {n_clients} rows")
    .msg_step("Programs: {n_programs} rows")
    .msg_step("Join key: provider_id")
  }

  # --- 2. Select program columns to append ----
  # Identify program columns that are useful for spatial/analytic work
  # Avoid duplicating columns that already exist in clients data
  client_cols  <- names(clients_df)
  program_cols <- names(program_df)

  # Core program columns to bring in (excluding provider_id which is the key)
  desired_program_cols <- c(
    "facility_id", "facility_name", "facility_type", "facility_tier",
    "facility_address", "county", "region",
    "latitude", "longitude", "census_tract", "fips_code",
    "day_capacity", "night_capacity",
    "day_age_range", "night_age_range",
    "day_start", "day_end", "night_start", "night_end",
    "expiration_date", "days_till_expire",
    "license_number", "snapshot_date"
  )

  # Keep only columns that exist in program data
  available_program_cols <- intersect(desired_program_cols, program_cols)

  # Resolve name collisions: suffix program columns that conflict
  conflicting_cols <- intersect(available_program_cols, client_cols)
  conflicting_cols <- setdiff(conflicting_cols, "provider_id")  # keep join key

  if (length(conflicting_cols) > 0 && verbose) {
    .msg_step("Resolving {length(conflicting_cols)} column name conflicts with .program suffix")
  }

  # Prepare program subset for joining
  program_join_cols <- c("provider_id", available_program_cols)
  program_join_cols <- unique(program_join_cols)
  program_join_cols <- intersect(program_join_cols, program_cols)

  program_subset <- program_df %>%
    dplyr::select(dplyr::all_of(program_join_cols))

  # Deduplicate program data by provider_id (keep first if duplicates)
  n_before_dedup <- nrow(program_subset)
  program_subset <- program_subset %>%
    dplyr::distinct(.data$provider_id, .keep_all = TRUE)
  n_after_dedup <- nrow(program_subset)

  if (n_before_dedup > n_after_dedup && verbose) {
    .msg_step("Deduplicated programs by provider_id: {n_before_dedup} -> {n_after_dedup}")
  }

  # --- 3. LEFT JOIN: clients <- program columns ----
  linked_df <- dplyr::left_join(
    clients_df,
    program_subset,
    by = "provider_id",
    suffix = c("", ".program")
  )

  n_linked <- nrow(linked_df)

  if (n_linked != n_clients && verbose) {
    .msg_warn("Row count changed from {n_clients} to {n_linked} (possible many-to-one mapping)")
  }

  # --- 4. Match diagnostics ----
  # Determine which identifier column to use for matching assessment
  # Use facility_id if available from joined program columns, else provider_id
  match_indicator <- if ("facility_id" %in% names(linked_df)) {
    !is.na(linked_df$facility_id)
  } else if ("facility_type" %in% names(linked_df)) {
    !is.na(linked_df$facility_type)
  } else {
    # Fallback: check if any program column was populated
    rep(FALSE, nrow(linked_df))
  }

  n_matched   <- sum(match_indicator)
  n_unmatched <- n_linked - n_matched
  match_rate  <- round(n_matched / n_linked * 100, 1)

  # Client provider_ids that did not match any program
  client_provider_ids  <- unique(clients_df$provider_id[!is.na(clients_df$provider_id)])
  program_provider_ids <- unique(program_df$provider_id[!is.na(program_df$provider_id)])
  unmatched_client_ids <- setdiff(client_provider_ids, program_provider_ids)

  unmatched_clients <- clients_df %>%
    dplyr::filter(.data$provider_id %in% unmatched_client_ids) %>%
    dplyr::select(dplyr::any_of(c("provider_id", "provider_name",
                                    "county", "family_address")))

  # Programs that had no clients
  unmatched_program_ids <- setdiff(program_provider_ids, client_provider_ids)

  unmatched_programs <- program_df %>%
    dplyr::filter(.data$provider_id %in% unmatched_program_ids) %>%
    dplyr::select(dplyr::any_of(c("facility_id", "provider_id",
                                    "facility_name", "facility_type",
                                    "county")))

  if (verbose) {
    .msg_step("Client rows matched: {n_matched}/{n_linked} ({match_rate}%)")
    .msg_step("Unmatched client rows: {n_unmatched}")
    if (length(unmatched_client_ids) > 0) {
      .msg_warn("{length(unmatched_client_ids)} client provider_ids not found in program data")
    }
    if (length(unmatched_program_ids) > 0) {
      .msg_step("{length(unmatched_program_ids)} programs had no client records")
    }
  }

  # --- 5. Build S3 object ----
  diagnostics <- list(
    n_clients            = n_clients,
    n_programs           = n_programs,
    n_linked_rows        = n_linked,
    n_matched            = n_matched,
    n_unmatched          = n_unmatched,
    match_rate           = match_rate,
    n_unmatched_client_ids  = length(unmatched_client_ids),
    n_unmatched_program_ids = length(unmatched_program_ids),
    unmatched_clients    = unmatched_clients,
    unmatched_programs   = unmatched_programs
  )

  obj <- .make_alccdf_obj(
    data       = linked_df,
    class_name = "alccdf_linked_clients_programs",
    module     = "linkage",
    stage      = "clients_programs",
    extra_meta = list(
      join_type    = "left_join",
      join_key     = "provider_id",
      backbone     = "clients",
      n_clients    = n_clients,
      n_programs   = n_programs,
      n_linked     = n_linked,
      n_matched    = n_matched,
      match_rate   = match_rate,
      program_cols_appended = setdiff(available_program_cols, "provider_id")
    ),
    diagnostics = diagnostics
  )
  obj <- .log_step(obj, glue::glue(
    "Linked clients x programs: {n_matched}/{n_linked} client rows matched ",
    "({match_rate}%), {length(available_program_cols)} program columns appended"
  ))

  if (verbose) {
    .msg_success(
      "Linkage complete: {n_linked} client rows with {length(available_program_cols)} program columns"
    )
  }

  obj
}


#' Print method for linked clients-programs objects
#'
#' @param x An \code{alccdf_linked_clients_programs} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_linked_clients_programs <- function(x, ...) {
  cli::cli_h2("ALccdfDB Linkage: Clients x Programs")
  cli::cli_alert_info("Join key: {x$meta$join_key}")
  cli::cli_alert_info("Backbone: {x$meta$backbone}")
  cli::cli_alert_info("Dimensions: {x$meta$n_rows} rows x {x$meta$n_cols} cols")
  cli::cli_alert_info("Clients: {x$meta$n_clients}")
  cli::cli_alert_info("Programs: {x$meta$n_programs}")
  cli::cli_alert_info("Match rate: {x$meta$n_matched}/{x$meta$n_linked} ({x$meta$match_rate}%)")
  if (!is.null(x$meta$program_cols_appended)) {
    cli::cli_alert_info(
      "Program columns appended: {paste(x$meta$program_cols_appended, collapse = ', ')}"
    )
  }
  if (length(x$meta$processing_log) > 0) {
    cli::cli_h3("Processing Log")
    for (entry in x$meta$processing_log) {
      cli::cli_text(entry)
    }
  }
  invisible(x)
}


# ---- Export functions --------------------------------------------------------

#' Export linked clients-programs data to CSV
#'
#' @param obj An \code{alccdf_linked_clients_programs} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_clients_programs_export_csv <- function(obj, path, verbose = TRUE) {
  .assert_class(obj, "alccdf_linked_clients_programs",
                "linkage_clients_programs_export_csv")
  data <- alccdf_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  utils::write.csv(data, path, row.names = FALSE, na = "")
  if (verbose) .msg_success("Exported CSV: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export linked clients-programs data to Excel
#'
#' @param obj An \code{alccdf_linked_clients_programs} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_clients_programs_export_excel <- function(obj, path, verbose = TRUE) {
  .check_suggested("openxlsx", "Excel export")
  .assert_class(obj, "alccdf_linked_clients_programs",
                "linkage_clients_programs_export_excel")
  data <- alccdf_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  openxlsx::write.xlsx(data, path, rowNames = FALSE)
  if (verbose) .msg_success("Exported Excel: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export linked clients-programs data to Stata (.dta)
#'
#' @param obj An \code{alccdf_linked_clients_programs} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_clients_programs_export_stata <- function(obj, path, verbose = TRUE) {
  .check_suggested("haven", "Stata export")
  .assert_class(obj, "alccdf_linked_clients_programs",
                "linkage_clients_programs_export_stata")
  data <- alccdf_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  data <- .prepare_for_stata(data)
  haven::write_dta(data, path)
  if (verbose) .msg_success("Exported Stata: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export linked clients-programs data to Parquet
#'
#' @param obj An \code{alccdf_linked_clients_programs} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_clients_programs_export_parquet <- function(obj, path, verbose = TRUE) {
  .check_suggested("arrow", "Parquet export")
  .assert_class(obj, "alccdf_linked_clients_programs",
                "linkage_clients_programs_export_parquet")
  data <- alccdf_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  arrow::write_parquet(data, path)
  if (verbose) .msg_success("Exported Parquet: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export linked clients-programs data to RDS
#'
#' @param obj An \code{alccdf_linked_clients_programs} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_clients_programs_export_rds <- function(obj, path, verbose = TRUE) {
  .assert_class(obj, "alccdf_linked_clients_programs",
                "linkage_clients_programs_export_rds")
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  saveRDS(obj, path)
  if (verbose) {
    .msg_success("Exported RDS: {.path {path}} ({obj$meta$n_rows} rows)")
  }
  invisible(path)
}

#' Export linked clients-programs data to all supported formats
#'
#' Convenience function that exports to CSV, Excel, Stata, Parquet, and RDS.
#'
#' @param obj An \code{alccdf_linked_clients_programs} object
#' @param dir Output directory
#' @param basename Base filename (without extension)
#' @param formats Character vector of formats to export. Default exports all:
#'   \code{c("csv", "excel", "stata", "parquet", "rds")}.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible named character vector of file paths
#'
#' @export
linkage_clients_programs_export_all <- function(obj,
                                                 dir,
                                                 basename,
                                                 formats = c("csv", "excel",
                                                             "stata", "parquet",
                                                             "rds"),
                                                 verbose = TRUE) {
  .assert_class(obj, "alccdf_linked_clients_programs",
                "linkage_clients_programs_export_all")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  ext_map <- c(csv = ".csv", excel = ".xlsx", stata = ".dta",
               parquet = ".parquet", rds = ".rds")
  fn_map <- list(
    csv     = linkage_clients_programs_export_csv,
    excel   = linkage_clients_programs_export_excel,
    stata   = linkage_clients_programs_export_stata,
    parquet = linkage_clients_programs_export_parquet,
    rds     = linkage_clients_programs_export_rds
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
