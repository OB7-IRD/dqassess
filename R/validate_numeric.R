#' @export
validate_numeric <- function(data,
                             slot_name,
                             column_name,
                             is_integer,
                             range_min = NA,
                             range_max = NA) {
  if (class(data) == "character") {
    data[tolower(data) == "na"]  <- NA
  }
  if (all(is.na(data))) {
    # All data are null ----
    data_validity <- rep_len(TRUE, length(data))
    result <- "INFO"
    message <- "All values are null"
  } else {
    # Test for numeric values ----
    data_validity <- try(unlist(lapply(data,
                                       function(x) {
                                         return(is.na(x) | ! is.na(try(expr = as.numeric(x),
                                                                       silent = TRUE)))
                                       }
                                       )))

    if (all(data_validity, na.rm = TRUE)) {
      # Perfect, all data are numeric values
      result <- "OK"
      message <- "All values are numeric"
    } else {
      result <- "ERROR"
      error_percent <- sum(! data_validity, na.rm = TRUE) / length(data_validity) * 100
      current_bad <- unique(data[! data_validity])
      if (length(current_bad) > 6) {
        current_bad <- c(current_bad[1:6], "...")
      }
      if (round(error_percent, digits = 2) == 0) {
        error_percent_final <- "Less of 1"
      } else {
        error_percent_final <- round(error_percent, digits = 2)
      }
      message <- paste0(error_percent_final,
                        "% of the values are not integer: [",
                        paste0(current_bad,
                               collapse = "; "),
                        "]")
    }
  }
  report <- data.frame(slot = slot_name,
                       column = column_name,
                       test = "is numeric ?",
                       result = result,
                       message = message,
                       stringsAsFactors = FALSE)
  if (result == "OK") {
    data <- as.numeric(data)
    # Test for integer ----
    if (is_integer) {
      data_validity_tmp <- is.na(data) | unlist(lapply(data,
                                                       function(x, tol = .Machine$double.eps ^ 0.5) {
                                                         return(abs(x - round(x)) < tol)
                                                       }))
      data_validity <- data_validity & data_validity_tmp

      if (all(data_validity_tmp, na.rm = TRUE)) {
        # Perfect all data are integer values
        result <- "OK"
        message <- "All values are integer"
      } else {
        result <- "ERROR"
        error_percent <- sum(! data_validity_tmp, na.rm = TRUE) / length(data_validity_tmp) * 100
        current_bad <- unique(data[! data_validity_tmp])
        if (length(current_bad) > 6) {
          current_bad <- c(current_bad[1:6], "...")
        }
        if (round(error_percent, digits = 2) == 0) {
          error_percent_final <- "Less of 1"
        } else {
          error_percent_final <- round(error_percent, digits = 2)
        }
        message <- paste0(error_percent_final,
                          "% of the values are not integer: [",
                          paste0(current_bad,
                                 collapse = "; "),
                          "]")
      }
      report <- rbind(
        report,
        data.frame(slot = slot_name,
                   column = column_name,
                   test = "is integer ?",
                   result = result,
                   message = message,
                   stringsAsFactors = FALSE))
    }
    # Test for min ----
    if (! is.na(range_min)) {
      data_validity_tmp <- is.na(data) | data >= range_min
      data_validity <- data_validity & data_validity_tmp

      test <- paste0("is greater or egal than ", range_min)
      if (all(data_validity_tmp, na.rm = TRUE)) {
        # Perfect, all are gretaer or equal to the min
        result <- "OK"
        message <- paste0("All values are greater or egal than ", range_min)
      } else {
        result <- "ERROR"
        error_percent <- sum(! data_validity_tmp, na.rm = TRUE) / length(data_validity_tmp) * 100
        current_bad <- unique(data[! data_validity_tmp])
        if (length(current_bad) > 6) {
          current_bad <- c(current_bad[1:6], "...")
        }
        if (round(error_percent, digits = 2) == 0) {
          error_percent_final <- "Less of 1"
        } else {
          error_percent_final <- round(error_percent, digits = 2)
        }
        message <- paste0(error_percent_final,
                          "% of the values are lower than ",
                          range_min,
                          ": [",
                          paste0(current_bad,
                                 collapse = "; "),
                          "]")
      }
      report <- rbind(report,
                      data.frame(slot = slot_name,
                                 column = column_name,
                                 test = test,
                                 result = result,
                                 message = message,
                                 stringsAsFactors = FALSE))
    }
    # Test for max ----
    if (! is.na(range_max)) {
      data_validity_tmp <- is.na(data) | data <= range_max
      data_validity <- data_validity & data_validity_tmp

      test <- paste0("is lower or egal than ", range_max)
      if (all(data_validity_tmp, na.rm = TRUE)) {
        # Perfect, all are lower or equal to the max
        result <- "OK"
        message <- paste0("All values are lower or egal than ", range_max)
      } else {
        result <- "ERROR"
        error_percent <- sum(! data_validity_tmp, na.rm = TRUE) / length(data_validity_tmp) * 100
        current_bad <- unique(data[! data_validity_tmp])
        if (length(current_bad) > 6) {
          current_bad <- c(current_bad[1:6], "...")
        }
        if (round(error_percent, digits = 2) == 0) {
          error_percent_final <- "Less of 1"
        } else {
          error_percent_final <- round(error_percent, digits = 2)
        }
        message <- paste0(error_percent_final, "% of the values are greater than ", range_max, ": [", paste0(current_bad, collapse = "; "), "]")
      }
      report <- rbind(
        report,
        data.frame(slot = slot_name,
                   column = column_name,
                   test = test,
                   result = result,
                   message = message,
                   stringsAsFactors = FALSE))
    }
  }
  ret <- list()
  ret[["report"]] <- report
  ret[["data_validity"]] <- data_validity
  return(ret)
}
