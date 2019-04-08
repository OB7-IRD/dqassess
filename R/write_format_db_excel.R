#' @title Generation excel definition format
#' @description Generation of a definition format in a excel file.
#' @name write_format_db_excel
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @param format_db Name of the R's element where is stock the format definition.
#' @param output_file_path_dir Location of export directory for the format definition. By default, the function uses the temporary directory.
#' @param output_file_name Name of the file exported. By default, the function use this syntax: formatdef_\%Y\%m\%d_\%H\%M\%S (check formats of \code{\link[base]{strptime}} function in R).
#' @param output_file_ext Extension of the file exported. You have to choose between xlsx for Excel 2007 OOXML format, or xls for Excel 95 binary format. Be default file are exported in xlsx.
#' @references \url{https://github.com/OB7-IRD/dqassess/blob/master/R/write_format_db_excel.R}
#' @return The function return the path of the exported file.
#' @export
write_format_db_excel <- function(format_db,
                                  output_file_path_dir = tempdir(),
                                  output_file_name = paste("formatdef",
                                                          format(Sys.time(), "%Y%m%d_%H%M%S"),
                                                          sep = "_"),
                                  output_file_ext = "xlsx") {
  if (missing(format_db) && ! output_file_ext %in% c("xlsx", "xls")) {
    stop("Missing format_db\nPlease add it in the function argument before continue")
  }
  # Creation of excel work book ----
  work_book <- xlsx::createWorkbook(type = output_file_ext)
  # Add metadata sheet ----
  suppressWarnings(if (! is.null(format_db[["format_infos"]])) {
    format_infos_sheet <- xlsx::createSheet(wb = work_book,
                                            sheetName = "format_infos")
    xlsx::addDataFrame(x = format_db[["format_infos"]],
                       sheet = format_infos_sheet,
                       row.names = FALSE)
  })
  # Add slots sheet ----
  suppressWarnings(if (! is.null(format_db[["slots"]])) {
    slots_sheet <- xlsx::createSheet(wb = work_book,
                                     sheetName = "slots")
    xlsx::addDataFrame(x = format_db[["slots"]],
                       sheet = slots_sheet,
                       row.names = FALSE)
  })
  # Add slots hierarchy sheet ----
  suppressWarnings(if (! is.null(format_db[["slots_hierarchy"]])) {
    slots_hierarchy_sheet <- xlsx::createSheet(wb = work_book,
                                               sheetName = "slots_hierarchy")
    xlsx::addDataFrame(x = format_db[["slots_hierarchy"]],
                       sheet = slots_hierarchy_sheet,
                       row.names = FALSE)
  })
  # Add numeric types sheet ----
  suppressWarnings(if (! is.null(format_db[["numeric_types"]])) {
    numeric_types_sheet <- xlsx::createSheet(wb = work_book,
                                             sheetName = "numeric_types")
    xlsx::addDataFrame(x = format_db[["numeric_types"]],
                       sheet = numeric_types_sheet,
                       row.names = FALSE)
  })
  # Add logical types sheet ----
  suppressWarnings(if (! is.null(format_db[["logical_types"]])) {
    logical_types_sheet <- xlsx::createSheet(wb = work_book,
                                             sheetName = "logical_types")
    xlsx::addDataFrame(x = format_db[["logical_types"]],
                       sheet = logical_types_sheet,
                       row.names = FALSE)
  })
  # Add text types sheet ----
  suppressWarnings(if (! is.null(format_db[["text_types"]])) {
    text_types_sheet <- xlsx::createSheet(wb = work_book,
                                          sheetName = "text_types")
    xlsx::addDataFrame(x = format_db[["text_types"]],
                       sheet = text_types_sheet,
                       row.names = FALSE)
  })
  # Add date types sheet ----
  suppressWarnings(if (! is.null(format_db[["date_types"]])) {
    date_types_sheet <- xlsx::createSheet(wb = work_book,
                                          sheetName = "date_types")
    xlsx::addDataFrame(x = format_db[["date_types"]],
                       sheet = date_types_sheet,
                       row.names = FALSE)
  })
  # Add codelist types sheet ----
  suppressWarnings(if (! is.null(format_db[["codelist_types"]])) {
    codelist_types_sheet <- xlsx::createSheet(wb = work_book,
                                              sheetName = "codelist_types")
    xlsx::addDataFrame(x = format_db[["codelist_types"]],
                       sheet = codelist_types_sheet,
                       row.names = FALSE)
  })
  # Add all slots sheets ----
  suppressWarnings(if (! identical(format_db[["slots"]]$definition_table,
                                   character(0))) {
    for (slot_name in format_db[["slots"]]$definition_table) {
      current_slot <- xlsx::createSheet(wb = work_book,
                                        sheetName = slot_name)
      xlsx::addDataFrame(x = format_db[[slot_name]],
                         sheet = current_slot,
                         row.names = FALSE)
    }
  })
  # Add all codelists sheets ----
  suppressWarnings(if (! identical(format_db[["codelist_types"]]$enumeration_table,
                                   character(0))) {
    for (codelist_name in format_db[["codelist_types"]]$enumeration_table) {
      current_codelist <- xlsx::createSheet(wb = work_book,
                                            sheetName = codelist_name)
      xlsx::addDataFrame(x = format_db[[codelist_name]],
                         sheet = current_codelist,
                         row.names = FALSE)
    }
  })
  # Write file
  output_file_path <- file.path(output_file_path_dir,
                                paste(output_file_name, ".", output_file_ext, sep = ""),
                                fsep = "\\")
  xlsx::saveWorkbook(wb = work_book,
                     file = output_file_path)
  return(output_file_path)
}
