# R/health.R
# Startup and runtime health checks for Signal.

get_app_health_messages <- function() {
  messages <- character(0)

  # Priority: options (set by signal_server()) > env var > _secrets.yml
  api_key <- getOption("signal.api_key", "")

  if (is.null(api_key) || api_key == "") {
    api_key <- Sys.getenv("ANTHROPIC_API_KEY", unset = "")
  }

  if (is.null(api_key) || api_key == "") {
    secrets <- tryCatch(yaml::read_yaml("_secrets.yml"), error = function(e) list())
    api_key <- secrets$claude$api_key
    if (is.null(api_key)) api_key <- ""
  }

  if (api_key == "") {
    messages <- c(
      messages,
      "Claude API key is not configured. Draft generation will use the local fallback."
    )
  }

  messages
}
