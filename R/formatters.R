format_date_for_display <- function(x) {
  if (is.na(x) || is.null(x) || x == "") {
    return("")
  }

  as.character(x)
}
