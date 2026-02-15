#' @title Linkage Module: Programs x Subsidy Clients
#' @description Link program data to subsidy clients (Active Clients With
#'   Addresses) by provider_id. Programs form the backbone (all preserved via
#'   LEFT JOIN), with per-program client summaries computed.
#' @name linkage-programs-clients
NULL

#' Link programs to subsidy clients
#'
#' Performs a LEFT JOIN of subsidy client data onto program data by
#' \code{provider_id}, preserving all program rows. Computes per-program
#' aggregates: number of active clients, average weekly copayment, and number
#' of unique cases.
#'
#' @param program_obj A program-module S3 object (e.g.,
#'   \code{alccdf_program_unified}, \code{alccdf_program_deduped},
#'   \code{alccdf_program_clean}) containing \code{provider_id}
#' @param clients_obj A subsidy-module S3 object with \code{subsidy_type =
#'   "clients"} (e.g., \code{alccdf_subsidy_clean})
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return An \code{alccdf_linked_programs_clients} S3 object with:
#'   \describe{
#'     \item{data}{Program-level tibble with client aggregates appended}
#'     \item{meta}{Metadata including linkage key, match statistics}
#'     \item{diagnostics}{Unmatched programs, unmatched client records}
#'   }
#'
#' @details
#' The linkage pipeline:
#' \enumerate{
#'   \item Extract data from both objects
#'   \item Aggregate client data to the program (provider_id) level
#'   \item LEFT JOIN aggregated clients onto programs by provider_id
#'   \item Compute match diagnostics
#'   \item Build S3 object
#' }
#'
#' Programs without any active clients receive \code{0} for count columns
#' and \code{NA} for average copay.
#'
#' @examples
#' \dontrun{
#' linked <- linkage_programs_clients(program_obj, clients_obj)
#' alccdf_data(linked)
#' linked$diagnostics$unmatched_clients
#' }
#'
#' @export
linkage_programs_clients <- function(program_obj, clients_obj,
                                      verbose = TRUE) {
  # --- Validate inputs ----
  if (!is.list(program_obj) || is.null(program_obj$data)) {
    cli::cli_abort("Expected a program-module S3 object with a {.field data} element.")
  }
  .assert_class(clients_obj, "alccdf_subsidy_clean",
                "linkage_programs_clients")

  if (!identical(clients_obj$meta$subsidy_type, "clients")) {
    cli::cli_abort(c(
      "Expected a subsidy object with {.val clients} type.",
      "i" = "Got {.val {clients_obj$meta$subsidy_type}} instead."
    ))
  }

  if (verbose) {
    .msg_header("Linkage: Programs x Subsidy Clients", level = 2)
  }

  # --- 1. Extract data ----
  program_df <- alccdf_data(program_obj)
  clients_df <- alccdf_data(clients_obj)

  if (!"provider_id" %in% names(program_df)) {
    cli::cli_abort("Program data must contain a {.field provider_id} column.")
  }
  if (!"provider_id" %in% names(clients_df)) {
    cli::cli_abort("Clients data must contain a {.field provider_id} column.")
  }

  n_programs <- nrow(program_df)
  n_clients  <- nrow(clients_df)

  if (verbose) {
    .msg_step("Programs: {n_programs} rows")
    .msg_step("Clients: {n_clients} rows")
    .msg_step("Join key: provider_id")
  }

  # --- 2. Aggregate client data per provider_id ----
  if (verbose) .msg_step("Aggregating client data by provider_id")

  clients_agg <- clients_df %>%
    dplyr::filter(!is.na(.data$provider_id)) %>%
    dplyr::group_by(.data$provider_id) %>%
    dplyr::summarise(
      n_active_clients = dplyr::n(),
      avg_copay_weekly = if ("copay_weekly" %in% names(clients_df)) {
        round(mean(.data$copay_weekly, na.rm = TRUE), 2)
      } else {
        NA_real_
      },
      n_unique_cases = if ("case_id" %in% names(clients_df)) {
        length(unique(stats::na.omit(.data$case_id)))
      } else {
        NA_integer_
      },
      .groups = "drop"
    )

  # Replace NaN from mean() on empty groups with NA
  clients_agg$avg_copay_weekly <- ifelse(
    is.nan(clients_agg$avg_copay_weekly),
    NA_real_,
    clients_agg$avg_copay_weekly
  )

  if (verbose) {
    .msg_step("Aggregated to {nrow(clients_agg)} unique providers in client data")
  }

  # --- 3. LEFT JOIN: programs <- clients aggregates ----
  linked_df <- dplyr::left_join(program_df, clients_agg, by = "provider_id")

  # Fill NA counts with 0
  linked_df <- linked_df %>%
    dplyr::mutate(
      n_active_clients = ifelse(is.na(.data$n_active_clients),
                                 0L, .data$n_active_clients),
      n_unique_cases = ifelse(is.na(.data$n_unique_cases),
                               0L, .data$n_unique_cases)
    )

  # --- 4. Match diagnostics ----
  n_matched <- sum(linked_df$n_active_clients > 0)
  n_unmatched_programs <- n_programs - n_matched
  match_rate <- round(n_matched / n_programs * 100, 1)

  # Client records that did not match any program
  client_provider_ids  <- unique(clients_df$provider_id[!is.na(clients_df$provider_id)])
  program_provider_ids <- unique(program_df$provider_id[!is.na(program_df$provider_id)])
  unmatched_client_ids <- setdiff(client_provider_ids, program_provider_ids)

  unmatched_clients <- clients_df %>%
    dplyr::filter(.data$provider_id %in% unmatched_client_ids)

  # Programs that received no clients
  unmatched_programs <- linked_df %>%
    dplyr::filter(.data$n_active_clients == 0L) %>%
    dplyr::select(dplyr::any_of(c("facility_id", "provider_id",
                                    "facility_name", "facility_type",
                                    "county")))

  if (verbose) {
    .msg_step("Programs matched: {n_matched}/{n_programs} ({match_rate}%)")
    .msg_step("Programs with no active clients: {n_unmatched_programs}")
    if (length(unmatched_client_ids) > 0) {
      .msg_warn("{length(unmatched_client_ids)} client provider_ids not found in program data")
    }
  }

  # --- 5. Build S3 object ----
  diagnostics <- list(
    n_programs           = n_programs,
    n_client_records     = n_clients,
    n_matched            = n_matched,
    n_unmatched_programs = n_unmatched_programs,
    match_rate           = match_rate,
    unmatched_programs   = unmatched_programs,
    unmatched_clients    = unmatched_clients
  )

  obj <- .make_alccdf_obj(
    data       = linked_df,
    class_name = "alccdf_linked_programs_clients",
    module     = "linkage",
    stage      = "programs_clients",
    extra_meta = list(
      join_type  = "left_join",
      join_key   = "provider_id",
      backbone   = "programs",
      n_programs = n_programs,
      n_clients  = n_clients,
      n_matched  = n_matched,
      match_rate = match_rate
    ),
    diagnostics = diagnostics
  )
  obj <- .log_step(obj, glue::glue(
    "Linked programs x clients: {n_matched}/{n_programs} programs matched ",
    "({match_rate}%), {n_clients} client records"
  ))

  if (verbose) {
    .msg_success(
      "Linkage complete: {nrow(linked_df)} program rows with client data"
    )
  }

  obj
}


#' Print method for linked programs-clients objects
#'
#' @param x An \code{alccdf_linked_programs_clients} object
#' @param ... Additional arguments (ignored)
#' @export
print.alccdf_linked_programs_clients <- function(x, ...) {
  cli::cli_h2("ALccdfDB Linkage: Programs x Subsidy Clients")
  cli::cli_alert_info("Join key: {x$meta$join_key}")
  cli::cli_alert_info("Backbone: {x$meta$backbone}")
  cli::cli_alert_info("Dimensions: {x$meta$n_rows} rows x {x$meta$n_cols} cols")
  cli::cli_alert_info("Programs: {x$meta$n_programs}")
  cli::cli_alert_info("Client records: {x$meta$n_clients}")
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

#' Export linked programs-clients data to CSV
#'
#' @param obj An \code{alccdf_linked_programs_clients} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_programs_clients_export_csv <- function(obj, path, verbose = TRUE) {
  .assert_class(obj, "alccdf_linked_programs_clients",
                "linkage_programs_clients_export_csv")
  data <- alccdf_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  utils::write.csv(data, path, row.names = FALSE, na = "")
  if (verbose) .msg_success("Exported CSV: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export linked programs-clients data to Excel
#'
#' @param obj An \code{alccdf_linked_programs_clients} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_programs_clients_export_excel <- function(obj, path, verbose = TRUE) {
  .check_suggested("openxlsx", "Excel export")
  .assert_class(obj, "alccdf_linked_programs_clients",
                "linkage_programs_clients_export_excel")
  data <- alccdf_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  openxlsx::write.xlsx(data, path, rowNames = FALSE)
  if (verbose) .msg_success("Exported Excel: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export linked programs-clients data to Stata (.dta)
#'
#' @param obj An \code{alccdf_linked_programs_clients} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_programs_clients_export_stata <- function(obj, path, verbose = TRUE) {
  .check_suggested("haven", "Stata export")
  .assert_class(obj, "alccdf_linked_programs_clients",
                "linkage_programs_clients_export_stata")
  data <- alccdf_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  data <- .prepare_for_stata(data)
  haven::write_dta(data, path)
  if (verbose) .msg_success("Exported Stata: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export linked programs-clients data to Parquet
#'
#' @param obj An \code{alccdf_linked_programs_clients} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_programs_clients_export_parquet <- function(obj, path, verbose = TRUE) {
  .check_suggested("arrow", "Parquet export")
  .assert_class(obj, "alccdf_linked_programs_clients",
                "linkage_programs_clients_export_parquet")
  data <- alccdf_data(obj)
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  arrow::write_parquet(data, path)
  if (verbose) .msg_success("Exported Parquet: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export linked programs-clients data to RDS
#'
#' @param obj An \code{alccdf_linked_programs_clients} object
#' @param path Output file path
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible file path
#' @export
linkage_programs_clients_export_rds <- function(obj, path, verbose = TRUE) {
  .assert_class(obj, "alccdf_linked_programs_clients",
                "linkage_programs_clients_export_rds")
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  saveRDS(obj, path)
  if (verbose) {
    .msg_success("Exported RDS: {.path {path}} ({obj$meta$n_rows} rows)")
  }
  invisible(path)
}

#' Export linked programs-clients data to all supported formats
#'
#' Convenience function that exports to CSV, Excel, Stata, Parquet, and RDS.
#'
#' @param obj An \code{alccdf_linked_programs_clients} object
#' @param dir Output directory
#' @param basename Base filename (without extension)
#' @param formats Character vector of formats to export. Default exports all:
#'   \code{c("csv", "excel", "stata", "parquet", "rds")}.
#' @param verbose Logical; print progress messages? Default TRUE.
#' @return Invisible named character vector of file paths
#'
#' @export
linkage_programs_clients_export_all <- function(obj,
                                                 dir,
                                                 basename,
                                                 formats = c("csv", "excel",
                                                             "stata", "parquet",
                                                             "rds"),
                                                 verbose = TRUE) {
  .assert_class(obj, "alccdf_linked_programs_clients",
                "linkage_programs_clients_export_all")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  ext_map <- c(csv = ".csv", excel = ".xlsx", stata = ".dta",
               parquet = ".parquet", rds = ".rds")
  fn_map <- list(
    csv     = linkage_programs_clients_export_csv,
    excel   = linkage_programs_clients_export_excel,
    stata   = linkage_programs_clients_export_stata,
    parquet = linkage_programs_clients_export_parquet,
    rds     = linkage_programs_clients_export_rds
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
