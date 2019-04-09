#' @export
validate_codelist <- function(data,
                              slot_name,
                              column_name,
                              ref_codelist,
                              ignore_case_in_codelist) {
  if (all(is.na(data))) {
    # All data are null
    data_validity <- rep_len(TRUE, length(data))
    result <- "INFO"
    message <- "All values are null"
  } else {
    if (length(ref_codelist) == 0) {
      data_validity <- rep_len(FALSE, length(data))
      result <- "ERROR"
      message <- "Empty reference codelist"
    } else {
      if (ignore_case_in_codelist) {
        data <- toupper(data)
        ref_codelist <- toupper(ref_codelist)
      }
      data_validity <- is.na(data) | (data %in% ref_codelist)
      if (all(data_validity, na.rm = TRUE)) {
        # perfect: all are character values
        result <- "OK"
        message <- "All values are valid codes"
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
          error_percent_final <- round(error_percent,
                                       digits = 2)
        }
        message <- paste0(error_percent_final,
                          "% of the values are not valid codes: [",
                          paste0(current_bad,
                                 collapse = "; "),
                          "]")
      }
    }
  }
  report <- data.frame(slot = slot_name,
                       column = column_name,
                       test = "is valid code list ?",
                       result = result,
                       message = message,
                       stringsAsFactors = FALSE)
  ret <- list()
  ret[["report"]] <- report
  ret[["data_validity"]] <- data_validity
  return(ret)
}
