validate_logical <- function(data,
                             slot_name,
                             column_name) {
  if (all(is.na(data))) {
    # All data are null ----
    data_validity <- rep_len(TRUE, length(data))
    result <- "INFO"
    message <- "All values are null"
  } else {
    # Test for logical values
    data_validity <- unlist(lapply(data,
                                   function(x) {
                                     return(is.na(x) | ! is.na(as.logical(x)))
                                   }))
    if (all(data_validity, na.rm = TRUE)) {
      # Perfect, all data are numeric values
      result <- "OK"
      message <- "All values are logical"
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
                        "% of the values are not logical: [",
                        paste0(current_bad,
                               collapse = "; "),
                        "]")
    }
  }
  report <- data.frame(slot = slot_name,
                       column = column_name,
                       test = "is logical value ?",
                       result = result,
                       message = message,
                       stringsAsFactors = FALSE)
  ret <- list()
  ret[["report"]] <- report
  ret[["data_validity"]] <- data_validity
  return(ret)
}
