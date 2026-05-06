# utils/health.R
# Lightweight startup and runtime health checks for Signal.

SIGNAL_REQUIRED_PACKAGES <- c(
  "shiny",
  "DBI",
  "RSQLite",
  "DT",
  "readxl",
  "readr",
  "janitor",
  "yaml",
  "httr2",
  "jsonlite"
)

check_required_packages <- function(packages = SIGNAL_REQUIRED_PACKAGES) {
  missing <- packages[!vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  
  if (length(missing) == 0) {
    return(invisible(TRUE))
  }
  
  stop(
    paste0(
      "Signal is missing required R package(s): ",
      paste(missing, collapse = ", "),
      ". Install them with: install.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

get_app_health_messages <- function(secrets_path = "_secrets.yml") {
  messages <- character(0)
  
  if (!file.exists(secrets_path)) {
    return(c(
      messages,
      "Claude API key is not configured. Draft generation will use the local fallback."
    ))
  }
  
  secrets <- tryCatch(
    yaml::read_yaml(secrets_path),
    error = function(e) NULL
  )
  
  api_key <- ""
  
  if (!is.null(secrets$claude$api_key)) {
    api_key <- secrets$claude$api_key
  }
  
  if (api_key == "") {
    messages <- c(
      messages,
      "Claude API key is blank. Draft generation will use the local fallback."
    )
  }
  
  messages
}
