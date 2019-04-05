#' @title Validation process for data
#' @description Validation process for data.
#' @name checking_data
#' @author Mathieu Depetris, \email{mathieu.depetris@@ird.fr}
#' @param obj Path of the file or R's object that contain data.
#' @param format_db Path of the file or R's object that contain the definition format.
#' @param ignore_case_in_codelist Ignored cases (upper or lower) in the codelist. By default yes (TRUE).
#' @param report Selected format for the report. You could choose between files, list or both. By default files selected.
#' @param report_dir Location of export directory for the report. By default, the function uses the temporary directory.
#' @param text_file_sep If the argument obj is a csv file, specify here the field separator of it. By default the separator is ";".
#' @param text_file_dec If the argument obj is a csv file, specify here the string use for decimal points. By default the decimal is ".".
#' @param file_name_slot If the argument obj is a csv file, specify here the name of the slot. By default not provided.
#' @references \url{TO DO}
#' @return The function returns file(s) location or list(s) name(s) of the report element(s).
#' @export
checking_data <- function(obj,
                          format_db,
                          ignore_case_in_codelist = TRUE,
                          report = "files",
                          report_dir = tempdir(),
                          text_file_sep = ";",
                          text_file_dec = ".",
                          file_name_slot) {
  if (missing(obj)) {
    stop("Missing \"obj\"\nPlease add it before continue")
  }

  if (missing(format_db)) {
    stop("Missing \"format_db\"\nPlease add it before continue")
  }

  if (! report %in% c("files", "list", "both")) {
    stop("\"report\" must be \"files\", \"list\" or \"both\"\nPlease correct it before continue")
  }

  # Check for the format definition source type ----
  if (inherits(format_db, "character")) {
    if (! file.exists(format_db)) {
      stop("Wrong format definition file path in \"format_db\" parameter\nPlease correct it before continue")
    }
    format_file_path <- format_db
    format_type <- "file"
    format_file_md5 <- tools::md5sum(format_file_path)
    format_db <- read_format_db(input_file_path = format_file_path)
    cat("Correct import of the definition data format\n")
  } else {
    format_type <- "object"
  }

  # Check for the dataset source type ----
  if (inherits(obj, "character")) {
    # If obj is a file path
    input_file_path <- obj
    if (! file.exists(input_file_path)) {
      stop("Wrong input file path in \"obj\" parameter\nPlease correct it before continue")
    }
    source_type <- "file"
    # Extract the file extension to determine the file type
    file_ext <- tolower(tools::file_ext(input_file_path))
    file_md5 <- tools::md5sum(input_file_path)

    if (file_ext %in% c("xls", "xlsx")) {
      # For an excel file
      xls_sheets <- readxl::excel_sheets(path = input_file_path)
      # Build a list from all the excel sheets
      obj <- list()
      for (current_sheet in xls_sheets) {
        try(expr = obj[[current_sheet]] <- as.data.frame(readxl::read_excel(path = input_file_path,
                                                                            sheet = current_sheet,
                                                                            col_types = "text")),
            silent = TRUE)
        if (current_sheet %in% names(obj)) {
          obj[[current_sheet]] <- obj[[current_sheet]][! apply(obj[[current_sheet]],
                                                               1,
                                                               function(x) all(is.na(x))),
                                                       ,
                                                       drop = FALSE]
        }
      }
      names(obj) <- tolower(names(obj))
    } else {
      if (file_ext %in% c("csv")) {
        # For a csv file
        obj <- read.csv(file = input_file_path,
                        strip.white = TRUE,
                        stringsAsFactors = FALSE,
                        sep = text_file_sep,
                        dec = text_file_dec,
                        colClasses = c("character"))
      } else {
        stop("The file extension is not supported yet by the programm\nPlease correct \"obj\" with a file with one of these extensions: xls, xlsx or csv")
      }
    }
    cat("Correct import of data\n")
  } else {
    # Source object is a R object
    source_type <- "object"
  }

  # Definition prefix name of the report ----
  report_in_files <- ifelse(report %in% c("files", "both"),
                            TRUE,
                            FALSE)
  report_in_list <- ifelse(report %in% c("list", "both"),
                           TRUE,
                           FALSE)

  if (report_in_files == TRUE) {
    if (source_type == "file") {
      report_file_nameprefix <- unlist(strsplit(x = basename(input_file_path),
                                                split = ".",
                                                fixed = TRUE))
      if (length(report_file_nameprefix) > 1) {
        report_file_nameprefix <- report_file_nameprefix[-length(report_file_nameprefix)]
      }
      report_file_nameprefix <- paste0(report_file_nameprefix, collapse = "_")
    } else {
      report_file_nameprefix <- ""
    }
    report_file_nameprefix <- paste(report_file_nameprefix, format(x = Sys.time(),
                                                                   format = "%Y%m%d%H%M%S"),
                                    sep = "_")
  }

  # Creation of the report metadata ----
  report_meta <- data.frame(parameter = c("format_name",
                                         "format_version",
                                         "validate_date",
                                         "dataset_container",
                                         "format_container"),
                           value = c(format_db[["format_infos"]]$format_name,
                                     format_db[["format_infos"]]$format_version,
                                     format(Sys.time(), format = "%Y-%m-%d %H:%M:%S"),
                                     source_type,
                                     format_type),
                           stringsAsFactors = FALSE)
  if (source_type == "file") {
    # If the source is a file, we add extra meta infos
    report_meta <- rbind(report_meta, c("dataset_file_path", input_file_path))
    report_meta <- rbind(report_meta, c("dataset_file_md5", file_md5))
  } else {
    if (format_type == "file") {
      # If the format is a file, we add extra meta infos
      report_meta <- rbind(report_meta, c("format_file_path", format_file_path))
      report_meta <- rbind(report_meta, c("format_file_md5", format_file_md5))
    }
  }

  # Creation of the report objects ----
  report_struct <- data.frame(slot = character(),
                              column = character(),
                              test = character(),
                              result = character(),
                              message = character(),
                              stringsAsFactors = FALSE)
  report_data <- data.frame(slot = character(),
                            column = character(),
                            test = character(),
                            result = character(),
                            message = character(),
                            stringsAsFactors = FALSE)
  report_files <- c()
  report_list <- list()

  for (current_slot_id in seq_len(nrow(format_db[["slots"]]))) {
    current_slot <- format_db[["slots"]][current_slot_id, ]
    def_table <- format_db[[current_slot$definition_table]]

    # Test if the slot exists and get data from ----
    if (source_type == "file" &&
        tools::file_ext(input_file_path) == "csv" &&
        ! missing(file_name_slot)
        && tolower(file_name_slot) == tolower(current_slot$slot_name)) {
      current_obj_slot <- get_slot_from_obj(obj = obj,
                                            slot_name = "base")
    } else {
      current_obj_slot <- get_slot_from_obj(obj = obj,
                                            slot_name = current_slot$slot_name)
    }
    slot_found <- !is.null(current_obj_slot)
    # Slot verification ----
    if (slot_found == TRUE) {
      cat(paste0("Slot ", current_slot$slot_name, " found\nChecking in progress, be patient or take a coffee\n"))
      # Removing empty rows and colums of the slot ----
      if (nrow(current_obj_slot) > 0) {
        # Remove empty rows
        current_obj_slot_char_columns <- unlist(lapply(seq_len(ncol(current_obj_slot)), function(x) class(current_obj_slot[, x]))) == "character"
        current_obj_slot[, current_obj_slot_char_columns] <- apply(current_obj_slot[, current_obj_slot_char_columns],
                                                                   c(1, 2),
                                                                   function(x) {
                                                                     if (! is.na(x)) {
                                                                       if (nchar(x) == 0)
                                                                         return(NA)
                                                                       }
                                                                     return(x)
                                                                     }
                                                                   )
        current_obj_slot <- current_obj_slot[rowSums(is.na(current_obj_slot)) != ncol(current_obj_slot), ]
        # Remove empty colums
        bad_column <- which(is.na(names(current_obj_slot)) |
                              names(current_obj_slot) == "")
        if (length(bad_column) > 0) {
          bad_column <- bad_column[which(colSums(is.na(current_obj_slot[, bad_column, drop = FALSE])) == nrow(current_obj_slot))]
          current_obj_slot <- current_obj_slot[, -c(bad_column), drop = FALSE]
        }
      }
      # Report on slot structure (exists yes/no and number of lines) ----
      if (nrow(current_obj_slot) == 0) {
        report_struct <- rbind(report_struct,
                              data.frame(slot = current_slot$slot_name,
                                         column = NA,
                                         test = "Slot exists ?",
                                         result = "INFO",
                                         message = "Found, but is empty",
                                         stringsAsFactors = FALSE))
      } else {
        report_struct <- rbind(report_struct,
                              data.frame(slot = current_slot$slot_name,
                                         column = NA,
                                         test = "Slot exists ?",
                                         result = "OK",
                                         message = paste0("Found: ",
                                                          nrow(current_obj_slot),
                                                          " rows"),
                                         stringsAsFactors = FALSE))
      }
      # Trim white space on slot columns names ----
      names(current_obj_slot) <- trimws(tolower(names(current_obj_slot)), which = "both")

      # Detailed report on slot
      current_obj_slot_report <- current_obj_slot

      # Test for slots columns ----
      for (current_column_id in seq_len(nrow(def_table))) {
        current_column <- def_table[current_column_id, ]
        current_column_name_formated <- trimws(tolower(current_column$column_name), which = "both")
        if (current_slot$slot_name == "base") {
          # is the base object a data.frame of a S3/S4 R object ?
          if (inherits(current_obj_slot, "data.frame")) {
            # a data frame
            if (current_column_name_formated %in% names(current_obj_slot)) {
              column_found <- TRUE
              current_obj_column <- current_obj_slot[, current_column_name_formated]
            } else {
              column_found <- FALSE
            }
          } else {
            # a S3/S4 object
            # check in the base slot
            if (current_column$column_name %in% slot_names(current_obj_slot)) {
              current_obj_column <- slot(obj, current_column$column_name)
              column_found <- TRUE
            } else {
              column_found <- FALSE
            }
          }
        } else {
          # check in another slot
          if (current_column_name_formated %in% names(current_obj_slot)) {
            column_found <- TRUE
            current_obj_column <- current_obj_slot[, current_column_name_formated]
          } else {
            column_found <- FALSE
          }
        }
        if (column_found == TRUE) {
          current_obj_column <- as.character(current_obj_column)
          if (class(current_obj_column) == "character") {
            # Leading or trailing whitespace
            current_obj_column <- trimws(current_obj_column, which = "both")
          }
          current_obj_column_report <- rep_len(TRUE, length(current_obj_column))

          report_struct <- rbind(report_struct,
                                data.frame(slot = current_slot$slot_name,
                                           column = current_column$column_name,
                                           test = "Column exists ?",
                                           result = "OK",
                                           message = "Found",
                                           stringsAsFactors = FALSE))
          # Check data ----
          type_def <- subset(format_db[[paste0(current_column$category, "_types")]],
                             type_name == current_column$type_name)
          local_checked <- FALSE
          if (current_column$category == "codelist") {
            # Check for codelist type ----
            local_checked <- TRUE
            res <- validate_codelist(data = current_obj_column,
                                     slot_name = current_slot$slot_name,
                                     column_name = current_column$column_name,
                                     ref_codelist = format_db[[type_def$enumeration_table]]$code,
                                     ignore_case_in_codelist = ignore_case_in_codelist)
            report_data <- rbind(report_data, res[["report"]])
            current_obj_column_report <- current_obj_column_report & res[["data_validity"]]
          } else {
            if (current_column$category == "text") {
              # Check for text type ----
              local_checked <- TRUE
              res <- validate_text(data = current_obj_column,
                                   slot_name = current_slot$slot_name,
                                   column_name = current_column$column_name)
              report_data <- rbind(report_data, res[["report"]])
              current_obj_column_report <- current_obj_column_report & res[["data_validity"]]
            } else {
              # Check for date type ----
              if (current_column$category == "date") {
                local_checked <- TRUE
                #Format date verification
                format_date <- as.character()
                for (i in 3:dim(type_def)[2]) {
                  if (type_def[, i] != "" & ! is.na(type_def[, i])) {
                    format_date <- c(format_date, type_def[, i])
                  }
                }
                res <- validate_date(data = current_obj_column,
                                     slot_name = current_slot$slot_name,
                                     column_name = current_column$column_name,
                                     time_zone = "UTC",
                                     format_date = format_date)
                report_data <- rbind(report_data, res[["report"]])
                current_obj_column_report <- current_obj_column_report & res[["data_validity"]]
              } else {
                # Check for numeric type ----
                if (current_column$category == "numeric") {
                  local_checked <- TRUE
                  res <- validate_numeric(data = current_obj_column,
                                          slot_name = current_slot$slot_name,
                                          column_name = current_column$column_name,
                                          is_integer = type_def$is_integer,
                                          range_min = type_def$min,
                                          range_max = type_def$max)
                  report_data <- rbind(report_data, res[["report"]])
                  current_obj_column_report <- current_obj_column_report & res[["data_validity"]]
                } else {
                  # Check for logical type ----
                  if (current_column$category == "logical") {
                    local_checked <- TRUE
                    res <- validate_logical(data = current_obj_column,
                                            slot_name = current_slot$slot_name,
                                            column_name = current_column$column_name)
                    report_data <- rbind(report_data, res[["report"]])
                    current_obj_column_report <- current_obj_column_report & res[["data_validity"]]
                  } else {
                    # Unknown category ----
                    report_data <- rbind(report_data,
                                         data.frame(slot = current_slot$slot_name,
                                                    column = current_column$column_name,
                                                    test = "NO TEST",
                                                    result = "ERROR",
                                                    message = paste0("No test provided for category ",
                                                                     current_column$category),
                                                    stringsAsFactors = FALSE))
                  }
                }
              }
            }
          }
          # Test for NA ----
          if (current_column$nullable == FALSE) {
            # NA not allowed
            res <- validate_null(data = current_obj_column,
                                 slot_name = current_slot$slot_name,
                                 column_name = current_column$column_name)
            report_data <- rbind(report_data, res[["report"]])
            current_obj_column_report <- current_obj_column_report & res[["data_validity"]]
          } else {
            # NA allowed
            current_obj_column_report[is.na(current_obj_column_report)] <- FALSE
          }

          # Append the current_obj_column_report to the current_obj_slot_report
          current_obj_slot_report <- cbind(current_obj_slot_report,
                                           ifelse(current_obj_column_report,
                                                  "VALID",
                                                  "INVALID"))
          names(current_obj_slot_report)[length(names(current_obj_slot_report))] <- paste0(current_column$column_name, "_validity")
        } else {
          # Column not found, it is a mandatory column ?
          if (current_column$mandatory == TRUE) {
            # Yes, data structure invalid
            report_struct <- rbind(report_struct,
                                  data.frame(slot = current_slot$slot_name,
                                             column = current_column$column_name,
                                             test = paste0("Column exists ?"),
                                             result = "ERROR", message = "Not found",
                                             stringsAsFactors = FALSE))
          } else {
            # Not an error, just info
            report_struct <- rbind(report_struct,
                                  data.frame(slot = current_slot$slot_name,
                                             column = current_column$column_name,
                                             test = paste0("Column exists ?"),
                                             result = "INFO",
                                             message = "Not found but not mandatory",
                                             stringsAsFactors = FALSE))
          }
        }
      }

      # Test for primary key (pk) ----
      pk_columns <- subset(def_table,
                           pk == TRUE)$column_name
      # Check for pk validation ----
      if (length(pk_columns) > 0) {
        res <- validate_pk(data = current_obj_slot,
                           slot_name = current_slot$slot_name,
                           pk_columns = trimws(pk_columns, which = "both"))
        report_data <- rbind(report_data, res[["report"]])
        # Append the pk report to the current_obj_slot_report
        current_obj_slot_report <- cbind(current_obj_slot_report,
                                         ifelse(res[["data_validity"]],
                                                "VALID",
                                                "INVALID"))
        names(current_obj_slot_report)[length(names(current_obj_slot_report))] <- "pk_validity"
      }

      # Save the report of the current slot ----
      if (report_in_files) {
        # Save the current_obj_slot_report
        current_obj_slot_report_file_path <- file.path(report_dir,
                                                       paste0(report_file_nameprefix,
                                                              "_slot_",
                                                              current_slot$slot_name,
                                                              ".csv"))
        write.table(x = current_obj_slot_report,
                    file = current_obj_slot_report_file_path,
                    row.names = FALSE,
                    sep = ";")
        report_files <- c(report_files, current_obj_slot_report_file_path)
      }
      if (report_in_list) {
        # Append the current_obj_slot_report to the report list
        report_list[[paste0("slot_", current_slot$slot_name)]] <- current_obj_slot_report
      }
    } else {
      cat(paste0("Slot ", current_slot$slot_name, " not found\n"))
      if (current_slot_id != nrow(format_db[["slots"]])) {
        cat("Process for the next slot available\n")
      }
      # Slot not found ----
      # It's a mandatory slot ?
      if (current_slot$mandatory) {
        # Yes, data structure invalid
        report_struct <- rbind(report_struct,
                              data.frame(slot = current_slot$slot_name,
                                         column = NA,
                                         test = "Slot exists ?",
                                         result = "ERROR",
                                         message = "Not found and mandatory",
                                         stringsAsFactors = FALSE))

      } else {
        # No
        report_struct <- rbind(report_struct,
                              data.frame(slot = current_slot$slot_name,
                                         column = NA,
                                         test = "Slot exists ?",
                                         result = "INFO",
                                         message = "Not found but not mandatory",
                                         stringsAsFactors = FALSE))
      }
    }
  }

  # Test for foreign key ----
  if (nrow(format_db[["slots_hierarchy"]]) != 0) {
    # One or more hierarchy found in the "slots_hierarchy"
    available_slots <- c(t(subset(report_struct,
                                  test == "Slot exists ?" & result == "OK",
                                  select = "slot")))
    available_hierarchy <- lapply(format_db[["slots_hierarchy"]]$link,
                                  function(x) {
                                    y <- strsplit(x, "_")
                                    if (length(y[[1]]) != 2) {
                                      y[[2]] <- "Invalid hierarchy"
                                    } else {
                                      if (length(setdiff(y[[1]], available_slots)) != 0) {
                                        y[[2]] <- paste0("Valid hierarchy but no data associated for slot(s): ",
                                                         paste0(setdiff(y[[1]], available_slots),
                                                                collapse = ", "))
                                      } else {
                                        y[[2]] <- "Valid hierarchy with data associated"
                                        y[[3]] <- c(t(subset(x = format_db[["slots_hierarchy"]], link == x, select = -link)))
                                        y[[3]] <- y[[3]][which(! is.na(y[[3]]))]
                                        y[[4]] <- "Common column(s) found in data"
                                        for (slot_name in y[[1]]) {
                                          z <- c(t(subset(x = report_struct, slot == slot_name & test == "Column exists ?", select = column)))
                                          if (length(setdiff(y[[3]], z)) != 0) {
                                            if (y[[4]] == "All column found in data") {
                                              y[[4]] <- paste0("One or more column in common not found in data for slot(s): ", slot_name)
                                            } else {
                                              y[[4]] <- paste0(y[[4]], ", ", slot_name)
                                            }
                                          }
                                        }
                                        if (y[[4]] != "Common column(s) found in data") {
                                          y[[2]] <- "Valid hierarchy with data associated but one or more column in common are missing"
                                        } else {
                                          y[[2]] <- "Valid hierarchy with data and common column(s) associated"
                                        }
                                      }
                                    }
                                    return(y)
                                  }
                                  )
    names(available_hierarchy) <- format_db[["slots_hierarchy"]]$link
    valid_hierarchy <- list()
    for (i in 1:length(available_hierarchy)) {
      if (available_hierarchy[[i]][[2]] == "Valid hierarchy with data and common column(s) associated") {
        valid_hierarchy <- c(valid_hierarchy, available_hierarchy[i])
      }
    }
    if (length(valid_hierarchy) >= 1) {
      # One or more valid hierarchy with data associated found
      for (i in 1:length(valid_hierarchy)) {
        current_hierarchy <- valid_hierarchy[[i]]
        # Get data from slots
        current_data <- vector(mode = "list", length = length(current_hierarchy[[1]]))
        names(current_data) <- current_hierarchy[[1]]
        for (slot_name in current_hierarchy[[1]]) {
          current_data[[slot_name]] <- get_slot_from_obj(obj = obj,
                                                         slot_name = slot_name)
          colnames(current_data[[slot_name]]) <- trimws(tolower(colnames(current_data[[slot_name]])), which = "both")
          current_data[[slot_name]] <- unique(subset(x = current_data[[slot_name]], select = current_hierarchy[[3]]))
          current_data[[slot_name]]$hierarchy <- apply(current_data[[slot_name]],
                                                       1,
                                                       function(x) {
                                                         paste(x, collapse = " ")
                                                       })
        }
        # Checking
        if (length(setdiff(current_data[[1]]$hierarchy, current_data[[2]]$hierarchy)) != 0) {
          result <- "WARNING"
          current_bad <- setdiff(current_data[[1]]$hierarchy, current_data[[2]]$hierarchy)
          error_percent <- length(current_bad) * 100 / length(current_data[[1]]$hierarchy)
          if (length(current_bad) > 6) {
            current_bad <- c(current_bad[1:2], "...")
          }
          if (round(error_percent, digits = 2) == 0) {
            error_percent_final <- "Less of 1"
          } else {
            error_percent_final <- round(error_percent, digits = 2)
          }
          message <- paste0(error_percent_final,
                            "% of hierarchy id not found in the second slot: [",
                            paste0(current_bad,
                                   collapse = "; "),
                            "]")
        } else {
          result <- "OK"
          message <- "All hierarchy id found in the second slot"
        }
        report_data <- rbind(report_data,
                             data.frame(slot = paste(current_hierarchy[[1]][1],
                                                     current_hierarchy[[1]][2],
                                                     sep = "_"),
                                        column = paste0("[",
                                                        paste0(current_hierarchy[[3]],
                                                               collapse = ", "),
                                                        "]"),
                                        test = "Link between slots consistent ?",
                                        result = result,
                                        message = message,
                                        stringsAsFactors = FALSE))
      }
    }
    if (length(valid_hierarchy) != length(available_hierarchy)) {
      names_invalid_hierarchy <- setdiff(names(available_hierarchy), names(valid_hierarchy))
      invalid_hierarchy <- available_hierarchy
      for (hierarchy_name in names(available_hierarchy)) {
        if (! hierarchy_name %in% names_invalid_hierarchy) {
          invalid_hierarchy[hierarchy_name] <- NULL
        }
      }
      for (hierarchy_name in names(invalid_hierarchy)) {
        report_data <- rbind(report_data,
                             data.frame(slot = hierarchy_name,
                                        column = "NA",
                                        test = "Link between slots consistent ?",
                                        result = "ERROR",
                                        message = invalid_hierarchy[[hierarchy_name]][[2]],
                                        stringsAsFactors = FALSE))
      }
    }
  }

  # Save the report ----
  if (report_in_files == TRUE) {
    #In a file
    report_meta_file_path <- file.path(report_dir,
                                       paste0(report_file_nameprefix,
                                              "_meta.csv"))
    write.table(x = report_meta,
                file = report_meta_file_path,
                row.names = FALSE,
                sep = ";")
    report_struct_file_path <- file.path(report_dir,
                                       paste0(report_file_nameprefix,
                                              "_str.csv"))
    write.table(x = report_struct,
                file = report_struct_file_path,
                row.names = FALSE,
                sep = ";")
    report_data_file_path <- file.path(report_dir,
                                       paste0(report_file_nameprefix,
                                              "_data.csv"))
    write.table(x = report_data,
                file = report_data_file_path,
                row.names = FALSE,
                sep = ";")
    report_files <- c(report_files,
                      report_meta_file_path,
                      report_struct_file_path,
                      report_data_file_path)
  }
  if (report_in_list == TRUE) {
    # In a list
    report_list[["meta"]] <- report_meta
    report_list[["struct"]] <- report_struct
    report_list[["data"]] <- report_data
  }
  if (report_in_files & report_in_list) {
    message(paste0(report_files,
                   collapse = "\n"))
    return(report_list)
  } else {
    if (report_in_files) {
      return(report_files)
    } else {
      if (report_in_list) {
        return(report_list)
      }
    }
  }
}
