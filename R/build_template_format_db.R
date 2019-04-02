#' @title Build template definition format
#' @description Build template definition format.
#' @name build_format_db
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @param format_name Name of the format.
#' @param format_version Version of the format (by default 0.1.0). To know more about the definition format take a look the corresponding specification section.
#' @references \url{TO DO}
#' @return A list of 9 elements
#' @section Specifications:
#' \describe{
#' \item{Format version}
#' {The version format is always defined with 3 numbers each separated by a dot: <major>.<minor>.<patch>.
#' The first number (<major>) is related to a major updated. It's when you have added many new features or conceptual changes impacted whose directly to the user interface (typically the new user interface is not compatible with the previous).
#' The second number (<minor>)  is when you add functionality in a backwards-compatible manner.
#' The third number (<patch>) is related to bug resolutions and more preciously when you make backwards-compatible bug fixes.}
#' }
#' @examples
#' # Built a template of the definition format with a version 0.1.0 (default setting)
#' build_format_db <- ("name_of_my_format")
#' # Built a template of the definition format with a version 1.0.1
#' build_format_db <- ("name_of_my_format", format_version = "1.0.1")
#' @export
build_template_format_db <- function(format_name,
                                     format_version = "0.1.0") {
  format_db <- list()
  # Add the metadata table ----
  format_db[["format_infos"]] <- data.frame(format_name = format_name,
                                            format_version = format_version,
                                            stringsAsFactors = FALSE)
  # Add the slots table ----
  format_db[["slots"]] <- data.frame(slot_name = character(),
                                     mandatory = logical(),
                                     definition_table = character(),
                                     stringsAsFactors = FALSE)
  # Add the slots hierarchy table ----
  format_db[["slots_hierarchy"]] <- data.frame(link = character(),
                                               level_1 = character(),
                                               level_2 = character(),
                                               level_3 = character(),
                                               level_4 = character(),
                                               level_5 = character(),
                                               level_6 = character(),
                                               level_7 = character(),
                                               level_8 = character(),
                                               stringsAsFactors = FALSE)
  # Add the "numeric" types definition table ----
  format_db[["numeric_types"]] <- data.frame(type_name = character(),
                                             is_integer = logical(),
                                             min = numeric(),
                                             max = numeric(),
                                             stringsAsFactors = FALSE)
  # add the "logical" types definition table
  format_db[["logical_types"]] <- data.frame(type_name = character(),
                                             stringsAsFactors = FALSE)
  # Add the "text" types definition table ----
  format_db[["text_types"]] <- data.frame(type_name = character(),
                                          stringsAsFactors = FALSE)
  # Add the "date" types definition table ----
  format_db[["date_types"]] <- data.frame(type_name = character(),
                                          time_zone_utc = character(),
                                          format_1 = character(),
                                          format_2 = character(),
                                          format_3 = character(),
                                          format_4 = character(),
                                          stringsAsFactors = FALSE)
  # Add the "codelist" types definition table ----
  format_db[["codelist_types"]] <- data.frame(type_name = "name_codelist_types",
                                              enumeration_table = "codelist_example",
                                              stringsAsFactors = FALSE)
  # Add the "base" slot: ie for the variables in the root of the object ----
  format_db[["slots"]] <- rbind(format_db[["slots"]],
                                data.frame(slot_name = "base",
                                           mandatory = TRUE,
                                           definition_table = "slot_base",
                                           stringsAsFactors = FALSE))
  format_db[["slot_base"]] <- data.frame(column_name = character(),
                                         nullable = logical(),
                                         mandatory = logical(),
                                         pk = logical(),
                                         type_name = character(),
                                         category = character(),
                                         stringsAsFactors = FALSE)
  format_db[["codelist_example"]] <- data.frame(code = character(),
                                                description = character(),
                                                stringsAsFactors = FALSE)
  return(format_db)
}
