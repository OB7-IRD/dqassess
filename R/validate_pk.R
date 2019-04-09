#' @export
validate_pk <- function(data,
                        slot_name,
                        pk_columns) {
  if (length(pk_columns) > 0) {
    # Test if all pk colums are found ----
    if (all(pk_columns %in% names(data))) {
      # ok, all pk columns found
      result <- "OK"
      message <- "All primary key columns found"
    } else {
      result <- "ERROR"
      message <- "Some primary key columns not found"
    }

    report <- data.frame(slot = slot_name,
                         column = paste0("[",
                                         paste0(pk_columns, collapse = ", "),
                                         "]"),
                         test = "Primary key columns exists ?",
                         result = result,
                         message = message,
                         stringsAsFactors = FALSE)

    if (result == "OK") {
      # Test for duplicated pk ----
      pk <- fp_key(tab = data[, pk_columns],
                   col_index = pk_columns)
      dup_pk <- pk[duplicated(pk)]
      data_validity <- ! pk %in% dup_pk
      if (length(dup_pk) == 0) {
        # Perfect, all pk are unique
        result <- "OK"
        message <- "All primary key are unique"
      } else {
        result <- "ERROR"
        error_percent <- sum(! data_validity, na.rm = TRUE) / length(data_validity) * 100
        current_bad <- which(! data_validity)
        if (length(current_bad) > 6) {
          current_bad <- c(current_bad[1:6], "...")
        }
        if (round(error_percent, digits = 2) == 0) {
          error_percent_final <- "Less of 1"
        } else {
          error_percent_final <- round(error_percent, digits = 2)
        }
        message <- paste0(error_percent_final, "% of the rows have duplicated primary key: #[", paste0(current_bad,
                                                                                                       collapse = "; "),
                          "]")
      }
      report <- rbind(report,
                      data.frame(slot = slot_name,
                                 column = paste0("[",
                                                 paste0(pk_columns,
                                                        collapse = ", "),
                                                 "]"),
                                 test = "Primary key all unique ?",
                                 result = result,
                                 message = message,
                                 stringsAsFactors = FALSE))
    } else {
      data_validity <- rep_len(FALSE, nrow(data))
    }

    ret <- list()
    ret[["report"]] <- report
    ret[["data_validity"]] <- data_validity
    return(ret)
  }
}
