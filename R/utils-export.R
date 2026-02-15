#' @title Export Utility Functions
#' @description Shared multi-format export infrastructure used by all modules.
#' @name utils-export
#' @keywords internal
NULL

#' Export data to CSV
#'
#' @param obj An ALccdfDB S3 object or a data frame
#' @param path File path for the output CSV
#' @return Invisible path
#' @export
export_csv <- function(obj, path) {
  data <- if (is.data.frame(obj)) obj else alccdf_data(obj)
  utils::write.csv(data, path, row.names = FALSE)
  .msg_success("Exported CSV: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export data to Excel
#'
#' @param obj An ALccdfDB S3 object or a data frame
#' @param path File path for the output Excel file
#' @return Invisible path
#' @export
export_excel <- function(obj, path) {
  .check_suggested("openxlsx", "Excel export")
  data <- if (is.data.frame(obj)) obj else alccdf_data(obj)
  openxlsx::write.xlsx(data, path, rowNames = FALSE)
  .msg_success("Exported Excel: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export data to Stata (.dta)
#'
#' @param obj An ALccdfDB S3 object or a data frame
#' @param path File path for the output Stata file
#' @return Invisible path
#' @export
export_stata <- function(obj, path) {
  .check_suggested("haven", "Stata export")
  data <- if (is.data.frame(obj)) obj else alccdf_data(obj)
  haven::write_dta(data, path)
  .msg_success("Exported Stata: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export data to Parquet
#'
#' @param obj An ALccdfDB S3 object or a data frame
#' @param path File path for the output Parquet file
#' @return Invisible path
#' @export
export_parquet <- function(obj, path) {
  .check_suggested("arrow", "Parquet export")
  data <- if (is.data.frame(obj)) obj else alccdf_data(obj)
  arrow::write_parquet(data, path)
  .msg_success("Exported Parquet: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export data to RDS
#'
#' @param obj An ALccdfDB S3 object or a data frame
#' @param path File path for the output RDS file
#' @return Invisible path
#' @export
export_rds <- function(obj, path) {
  data <- if (is.data.frame(obj)) obj else alccdf_data(obj)
  saveRDS(data, path)
  .msg_success("Exported RDS: {.path {path}} ({nrow(data)} rows)")
  invisible(path)
}

#' Export data to multiple formats
#'
#' @param obj An ALccdfDB S3 object or a data frame
#' @param dir Output directory
#' @param basename Base filename (without extension)
#' @param formats Character vector of formats: "csv", "excel", "stata", "parquet", "rds"
#' @return Invisible named character vector of paths
#' @export
export_all <- function(obj, dir, basename,
                       formats = c("csv", "excel", "stata", "parquet", "rds")) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  paths <- character()
  ext_map <- c(csv = ".csv", excel = ".xlsx", stata = ".dta",
               parquet = ".parquet", rds = ".rds")
  fn_map <- list(
    csv = export_csv, excel = export_excel, stata = export_stata,
    parquet = export_parquet, rds = export_rds
  )

  for (fmt in formats) {
    path <- file.path(dir, paste0(basename, ext_map[[fmt]]))
    tryCatch({
      fn_map[[fmt]](obj, path)
      paths[fmt] <- path
    }, error = function(e) {
      .msg_warn("Failed to export {fmt}: {conditionMessage(e)}")
    })
  }

  invisible(paths)
}
