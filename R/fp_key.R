#' @export
fp_key <- function(tab,
                   col_index, sep = ":-:") {
  key <- tab[, col_index]
  if (length(col_index) > 1) {
    key <- apply(key,
                 1,
                 paste0,
                 collapse = sep)
  }
  key <- gsub("[[:space:]]", "", key)
  return(key)
}
