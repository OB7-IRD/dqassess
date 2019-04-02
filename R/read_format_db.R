#' @title Read a format file definition
#' @description Read a format file definition from an Excel file
#' @name read_format_db
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @param input_file_path Path of the Excel file.
#' @references \url{TO DO}
#' @return A list of elements from the format file definition.
#' @examples
#' read_format_db <- ("path_of_the_file\\file_name.xlsx")
#' @export
read_format_db <- function(input_file_path) {
  # Check for the argument about the format path ----
  if (missing(input_file_path)) {
    stop("Missing argument input_file_path\nPlease add the correct path to the format definition")
  }
  # List of all sheets in the format document ----
  format_sheets <- readxl::excel_sheets(path = input_file_path)
  # A formater pour avoir la taille parfaite
  format_db <- list()
  # Read the metadata table ----
  if (! "format_infos" %in% format_sheets) {
    stop("Missing \"format_infos\" sheet in the format definition file\nPlease add it in the format definition before continue")
  }
  try(expr = format_db[["format_infos"]] <- as.data.frame(readxl::read_excel(path = input_file_path,
                                                                             sheet = "format_infos")),
      silent = TRUE)
  if (! "format_infos" %in% names(format_db) | is.null(format_db[["format_infos"]])) {
    format_db[["format_infos"]] <- data.frame(format_name = character(),
                                              format_version = character(),
                                              stringsAsFactors = FALSE)
  }
  format_db[["format_infos"]] <- format_db[["format_infos"]][! apply(format_db[["format_infos"]],
                                                                     1,
                                                                     function(x) all(is.na(x))),
                                                             ,
                                                             drop = FALSE]
  names(format_db[["format_infos"]]) <- tolower(names(format_db[["format_infos"]]))
  format_db[["format_infos"]]$format_name <- trimws(tolower(format_db[["format_infos"]]$format_name), which = "both")
  # Read the slots table ----
  if (! "slots" %in% format_sheets) {
    stop("Missing \"slots\" sheet in the format definition file\nPlease add it in the format definition before continue")
  }
  try(expr = format_db[["slots"]] <- as.data.frame(readxl::read_excel(path = input_file_path,
                                                                      sheet = "slots")),
      silent = TRUE)
  if (! "slots" %in% names(format_db) | is.null(format_db[["slots"]])) {
    format_db[["slots"]] <- data.frame(slot_name = character(),
                                       mandatory = logical(),
                                       definition_table = character(),
                                       stringsAsFactors = FALSE)
  }
  format_db[["slots"]] <- format_db[["slots"]][! apply(format_db[["slots"]],
                                                       1,
                                                       function(x) all(is.na(x))),
                                               ,
                                               drop = FALSE]
  names(format_db[["slots"]]) <- tolower(names(format_db[["slots"]]))
  format_db[["slots"]]$slot_name <- trimws(tolower(format_db[["slots"]]$slot_name), which = "both")
  format_db[["slots"]]$mandatory <-  trimws(tolower(format_db[["slots"]]$mandatory), which = "both")
  format_db[["slots"]]$definition_table <- trimws(tolower(format_db[["slots"]]$definition_table), which = "both")
  # Read the slot hierarchy table ----
  if (! "slots_hierarchy" %in% format_sheets) {
    stop("Missing \"slots_hierarchy\" sheet in the format definition file\nPlease add it in the format definition before continue")
  }
  try(expr = format_db[["slots_hierarchy"]] <- as.data.frame(readxl::read_excel(path = input_file_path,
                                                                                sheet = "slots_hierarchy")),
      silent = TRUE)
  if (! "slots_hierarchy" %in% names(format_db) | is.null(format_db[["slots_hierarchy"]])) {
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
  }
  format_db[["slots_hierarchy"]] <- format_db[["slots_hierarchy"]][! apply(format_db[["slots_hierarchy"]],
                                                                           1,
                                                                           function(x) all(is.na(x))),
                                                                   ,
                                                                   drop = FALSE]
  names(format_db[["slots_hierarchy"]]) <- tolower(names(format_db[["slots_hierarchy"]]))
  for (i in 1:dim(format_db[["slots_hierarchy"]])[2]) {
    format_db[["slots_hierarchy"]][[i]] <- trimws(tolower(format_db[["slots_hierarchy"]][[i]]), which = "both")
  }
  # Read the "text" types definition table ----
  if (! "text_types" %in% format_sheets) {
    stop("Missing \"text_types\" sheet in the format definition file\nPlease add it in the format definition before continue")
  }
  try(expr = format_db[["text_types"]] <- as.data.frame(readxl::read_excel(path = input_file_path,
                                                                           sheet = "text_types")),
      silent = TRUE)
  if (! "text_types" %in% names(format_db) | is.null(format_db[["text_types"]])) {
    format_db[["text_types"]] <- data.frame(type_name = character(),
                                            stringsAsFactors = FALSE)
  }
  format_db[["text_types"]] <- format_db[["text_types"]][! apply(format_db[["text_types"]],
                                                                 1,
                                                                 function(x) all(is.na(x))),
                                                         ,
                                                         drop = FALSE]
  names(format_db[["text_types"]]) <- tolower(names(format_db[["text_types"]]))
  format_db[["text_types"]]$type_name <- trimws(tolower(format_db[["text_types"]]$type_name), which = "both")
  # Read the "numeric" types definition table ----
  if (! "numeric_types" %in% format_sheets) {
    stop("Missing \"numeric_types\" sheet in the format definition file\nPlease add it in the format definition before continue")
  }
  try(expr = format_db[["numeric_types"]] <- as.data.frame(readxl::read_excel(path = input_file_path,
                                                                              sheet = "numeric_types")),
      silent = TRUE)
  if (! "numeric_types" %in% names(format_db) | is.null(format_db[["numeric_types"]])) {
    format_db[["numeric_types"]] <- data.frame(type_name = character(),
                                               is_integer = logical(),
                                               min = numeric(),
                                               max = numeric(),
                                               stringsAsFactors = FALSE)
  }
  format_db[["numeric_types"]] <- format_db[["numeric_types"]][! apply(format_db[["numeric_types"]],
                                                                       1,
                                                                       function(x) all(is.na(x))),
                                                               ,
                                                               drop = FALSE]
  names(format_db[["numeric_types"]]) <- tolower(names(format_db[["numeric_types"]]))
  format_db[["numeric_types"]]$type_name <- trimws(tolower(format_db[["numeric_types"]]$type_name), which = "both")
  format_db[["numeric_types"]]$is_integer <- trimws(as.logical(tolower(format_db[["numeric_types"]]$is_integer)), which = "both")
  # Read the "date" types definition table ----
  if (! "date_types" %in% format_sheets) {
    stop("Missing \"date_types\" sheet in the format definition file\nPlease add it in the format definition before continue")
  }
  try(expr = format_db[["date_types"]] <- as.data.frame(readxl::read_excel(path = input_file_path,
                                                                           sheet = "date_types")),
      silent = TRUE)
  if (! "date_types" %in% names(format_db) | is.null(format_db[["date_types"]])) {
    format_db[["date_types"]] <- data.frame(type_name = character(),
                                            stringsAsFactors = FALSE)
  }
  format_db[["date_types"]] <- format_db[["date_types"]][! apply(format_db[["date_types"]],
                                                                 1,
                                                                 function(x) all(is.na(x))),
                                                         ,
                                                         drop = FALSE]
  names(format_db[["date_types"]]) <- tolower(names(format_db[["date_types"]]))
  format_db[["date_types"]]$type_name <- trimws(tolower(format_db[["date_types"]]$type_name), which = "both")
  # Read the "codelist_types" types definition table ----
  if (! "codelist_types" %in% format_sheets) {
    stop("Missing \"codelist_types\" sheet in the format definition file\nPlease add it in the format definition before continue")
  }
  try(expr = format_db[["codelist_types"]] <- as.data.frame(readxl::read_excel(path = input_file_path,
                                                                               sheet = "codelist_types")),
      silent = TRUE)
  if (! "codelist_types" %in% names(format_db) | is.null(format_db[["codelist_types"]])) {
    format_db[["codelist_types"]] <- data.frame(type_name = character(),
                                                enumeration_table = character(),
                                                stringsAsFactors = FALSE)
  }
  format_db[["codelist_types"]] <- format_db[["codelist_types"]][! apply(format_db[["codelist_types"]],
                                                                         1,
                                                                         function(x) all(is.na(x))),
                                                                 ,
                                                                 drop = FALSE]
  names(format_db[["codelist_types"]]) <- tolower(names(format_db[["codelist_types"]]))
  format_db[["codelist_types"]]$type_name <- trimws(tolower(format_db[["codelist_types"]]$type_name), which = "both")
  format_db[["codelist_types"]]$enumeration_table <- trimws(tolower(format_db[["codelist_types"]]$enumeration_table), which = "both")
  # Read the "logical_types" types definition table ----
  if (! "logical_types" %in% format_sheets) {
    stop("Missing \"logical_types\" sheet in the format definition file\nPlease add it in the format definition before continue")
  }
  try(expr = format_db[["logical_types"]] <- as.data.frame(readxl::read_excel(path = input_file_path,
                                                                              sheet = "logical_types")),
      silent = TRUE)
  if (! "logical_types" %in% names(format_db) | is.null(format_db[["logical_types"]])) {
    format_db[["logical_types"]] <- data.frame(type_name = character(),
                                               stringsAsFactors = FALSE)
  }
  format_db[["logical_types"]] <- format_db[["logical_types"]][! apply(format_db[["logical_types"]],
                                                                       1,
                                                                       function(x) all(is.na(x))),
                                                               ,
                                                               drop = FALSE]
  names(format_db[["logical_types"]]) <- tolower(names(format_db[["logical_types"]]))
  format_db[["logical_types"]]$type_name <- trimws(tolower(format_db[["logical_types"]]$type_name), which = "both")
  # Read slots ----
  for (slot in format_db[["slots"]]$definition_table) {
    try(expr = format_db[[slot]] <- as.data.frame(readxl::read_excel(path = input_file_path,
                                                                     sheet = slot)),
        silent = TRUE)
    if (! slot %in% names(format_db) | is.null(format_db[[slot]])) {
      stop("Missing \"", slot, "\" sheet for the definition of the slot in the format definition file\nPlease add it in the format definition before continue")
    }
    format_db[[slot]] <- format_db[[slot]][! apply(format_db[[slot]],
                                                   1,
                                                   function(x) all(is.na(x))),
                                           ,
                                           drop = FALSE]
    names(format_db[[slot]]) <- tolower(names(format_db[[slot]]))
    format_db[[slot]]$column_name <- trimws(tolower(format_db[[slot]]$column_name), which = "both")
    format_db[[slot]]$nullable <- trimws(as.logical(format_db[[slot]]$nullable), which = "both")
    format_db[[slot]]$mandatory <- trimws(as.logical(format_db[[slot]]$mandatory), which = "both")
    format_db[[slot]]$pk <- trimws(as.logical(format_db[[slot]]$pk), which = "both")
    format_db[[slot]]$type_name <- trimws(tolower(format_db[[slot]]$type_name), which = "both")
    format_db[[slot]]$category <- trimws(tolower(format_db[[slot]]$category), which = "both")
  }
  # Read code lists ----
  for (codelist_def in format_db[["codelist_types"]]$enumeration_table) {
    try(expr = format_db[[codelist_def]] <- as.data.frame(readxl::read_excel(path = input_file_path,
                                                                             sheet = codelist_def)),
        silent = TRUE)
    if (! codelist_def %in% names(format_db) | is.null(format_db[[codelist_def]])) {
      stop("Missing sheet \"", codelist_def, "\" code list type in the format definition file\nPlease add it in the format definition before continue")
    }
    format_db[[codelist_def]] <- format_db[[codelist_def]][! apply(format_db[[codelist_def]],
                                                                   1,
                                                                   function(x) all(is.na(x))),
                                                           ,
                                                           drop = FALSE]
    names(format_db[[codelist_def]]) <- tolower(names(format_db[[codelist_def]]))
    format_db[[codelist_def]]$code <- trimws(tolower(format_db[[codelist_def]]$code), which = "both")
    format_db[[codelist_def]]$description <- trimws(tolower(format_db[[codelist_def]]$description), which = "both")
  }
  # Check for consistency ----
  for (slot in format_db[["slots"]]$definition_table) {
    if (slot %in% names(format_db) & nrow(format_db[[slot]]) > 0) {
      for (row_id in 1:nrow(format_db[[slot]])) {
        category <- format_db[[slot]]$category[row_id]
        name_type <- format_db[[slot]]$type_name[row_id]
        # Current column is a codelist type
        if (category == "codelist") {
          curr_codelist <- subset(format_db[["codelist_types"]],
                                  type_name == name_type)
          if (nrow(curr_codelist) == 1) {
            if (curr_codelist$enumeration_table %in% names(format_db)) {
              if (nrow(format_db[[curr_codelist$enumeration_table]]) == 0) {
                message("(INFO) Empty enumeration table (code list) \"", curr_codelist$enumeration_table, "\" in the format definition file.")
              }
            } else {
              stop("Missing sheet \"", curr_codelist$enumeration_table, "\" for the \"", curr_codelist$type_name, "\" code list type in the format definition file\nPlease correct or add it in the format definition before continue")
            }
          } else {
            stop("Missing or duplicated entry for \"", name_type, "\" in the \"codelist_types\" sheet in the format definition file\nPlease correct or add it in the format definition before continue")
          }
        } else {
          # Current column is a numeric type
          if (category == "numeric") {
            if (nrow(subset(format_db[["numeric_types"]],
                            type_name == name_type)) != 1) {
              stop("Missing or duplicated entry for \"", name_type, "\" in the \"numeric_types\" sheet in the format definition file\nPlease correct or add it in the format definition before continue")
            }
          } else {
            # Current column is text type
            if (category == "text") {
              if (nrow(subset(format_db[["text_types"]], type_name == name_type)) != 1) {
                stop("Missing or duplicated entry for \"", name_type, "\" in the \"text_types\" sheet in the format definition file\nPlease correct or add it in the format definition before continue")
              }
            } else {
              # Current column is date type
              if (category == "date") {
                if (nrow(subset(format_db[["date_types"]], type_name == name_type)) != 1) {
                  stop("Missing or duplicated entry for \"", name_type, "\" in the \"date_types\" sheet in the format definition file\nPlease correct or add it in the format definition before continue")
                }
              } else {
                # Current column is logical type
                if (category == "logical") {
                  if (nrow(subset(format_db[["logical_types"]], type_name == name_type)) != 1) {
                    stop("Missing or duplicated entry for \"", name_type, "\" in the \"logical_types\" sheet in the format definition file\nPlease correct or add it in the format definition before continue")
                  }
                } else {
                  stop("Category \"", category, "\" is missing in possible categories of the format definition file\nPlease correct the format definition or update the function before continue")
                }
              }
            }
          }
        }
      }
    } else {
      stop("Missing or empty sheet for the definition of the \"", slot, "\" slot in the format definition file\nPlease correct or add it in the format definition before continue")
    }
  }
  cat()
  return(format_db)
}
