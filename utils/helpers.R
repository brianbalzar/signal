empty_to_na <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }

  x <- trimws(as.character(x))

  if (identical(x, "")) {
    return(NA_character_)
  }

  x
}

today_string <- function() {
  as.character(Sys.Date())
}
