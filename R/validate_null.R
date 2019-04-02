validate_null <- function(data,
                          slot_name,
                          column_name) {
  data_validity <- ! is.na(data)
  if (all(data_validity, na.rm = TRUE)) {
    # Perfect, all data are not null
    result <- "OK"
    message <- "All values are not null"
  } else {
    result <- "ERROR"
    error_percent <- sum(! data_validity, na.rm = TRUE) / length(data_validity) * 100
    if (round(error_percent, digits = 2) == 0) {
      error_percent_final <- "Less of 1"
    } else {
      error_percent_final <- round(error_percent, digits = 2)
    }
    message <- paste0(error_percent_final, "% of the values are null")
  }
  report <- data.frame(slot = slot_name,
                       column = column_name,
                       test = "is null ?",
                       result = result,
                       message = message,
                       stringsAsFactors = FALSE)
  ret <- list()
  ret[["report"]] <- report
  ret[["data_validity"]] <- data_validity
  return(ret)
}
