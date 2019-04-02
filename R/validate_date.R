validate_date <- function(data,
                          slot_name,
                          column_name,
                          type_format_date,
                          time_zone,
                          format_date) {
  if (all(is.na(data))) {
    # All data are null
    data_validity <- rep_len(TRUE, length(data))
    result <- "INFO"
    message <- "All values are null"
  } else {
    if (time_zone != "UTC") {
      data_validity <- rep_len(FALSE, length(data))
      result <- "ERROR"
      message <- "Values are not in the time zone \"UTC\", validation impossible"
    } else {
      if (sum(match(format_date, "yQq", nomatch = 0)) != 0) {
        if (length(format_date) != 1) {
          # If there are serveral format with the specific case "year Q quarter"
          data_validity <- is.na(data) |
            ! is.na(unlist(lapply(unlist(lapply(data,
                                                function(x) {
                                                  x <- unlist(strsplit(x, split = "[Q?|q?]"))
                                                  x <- unlist(lapply(x,
                                                                     function(x) {
                                                                       x <- gsub(pattern = "^[[:punct:]]*|[[:punct:]]*$",
                                                                                 replacement = "",
                                                                                 x)
                                                                       return(x)
                                                                     }))
                                                  x <- ifelse(x == "", "99", x)
                                                  x <- paste(c(x[1:length(x)]),
                                                             collapse = "-")
                                                })),
                                  function(x) {
                                    lubridate::parse_date_time(x,
                                                               orders = "yq",
                                                               tz = time_zone,
                                                               quiet = TRUE)
                                  }))) |
            ! is.na(unlist(lapply(data,
                                  function(x) {
                                    lubridate::parse_date_time(x,
                                                               orders = format_date[which(format_date != "yQq")],
                                                               tz = time_zone,
                                                               quiet = TRUE)
                                  })))
        } else {
          # Specific test for the format "year Q quarter number"
          data_validity <- is.na(data) |
            ! is.na(unlist(lapply(unlist(lapply(data,
                                                function(x) {
                                                  x <- unlist(strsplit(x, split = "[Q?|q?]"))
                                                  x <- unlist(lapply(x,
                                                                     function(x) {
                                                                       x <- gsub(pattern = "^[[:punct:]]*|[[:punct:]]*$",
                                                                                 replacement = "",
                                                                                 x)
                                                                       return(x)
                                                                     }))
                                                  x <- ifelse(x == "", "99", x)
                                                  x <- paste(c(x[1:length(x)]),
                                                             collapse = "-")
                                                })),
                                  function(x) {
                                    lubridate::parse_date_time(x,
                                                               orders = "yq",
                                                               tz = time_zone,
                                                               quiet = TRUE)
                                  })))
        }
      } else {
        data_validity <- is.na(data) | ! is.na(unlist(lapply(data,
                                                             function(x) {
                                                               lubridate::parse_date_time(x,
                                                                                          orders = format_date,
                                                                                          tz = time_zone,
                                                                                          quiet = TRUE)
                                                             })))
      }
      if (all(data_validity, na.rm = TRUE)) {
        # Perfect, all data are date values
        result <- "OK"
        message <- "All values are date"
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
                          "% of the values are not valid date according to selected format(s): [",
                          paste0(current_bad,
                                 collapse = "; "),
                          "]")
      }
    }
  }
  report <- data.frame(slot = slot_name,
                       column = column_name,
                       test = "is date ?",
                       result = result,
                       message = message,
                       stringsAsFactors = FALSE)
  ret <- list()
  ret[["report"]] <- report
  ret[["data_validity"]] <- data_validity
  return(ret)
}
