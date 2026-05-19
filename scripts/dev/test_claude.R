# test_claude.R

library(yaml)
library(httr2)
library(jsonlite)

secrets <- yaml::read_yaml("_secrets.yml")

api_key <- secrets$claude$api_key
model <- secrets$claude$model

if (is.null(model) || model == "") {
  model <- "claude-3-5-sonnet-latest"
}

cat("Model:", model, "\n")
cat("API key present:", !is.null(api_key) && nzchar(api_key), "\n")
cat("API key starts with sk-ant:", startsWith(api_key, "sk-ant"), "\n\n")

req <- request("https://api.anthropic.com/v1/messages") |>
  req_headers(
    "x-api-key" = api_key,
    "anthropic-version" = "2023-06-01",
    "content-type" = "application/json"
  ) |>
  req_body_json(list(
    model = model,
    max_tokens = 100,
    messages = list(
      list(
        role = "user",
        content = "Reply with exactly: Claude test successful"
      )
    )
  )) |>
  req_error(is_error = function(resp) FALSE)

resp <- req_perform(req)

cat("HTTP status:", resp_status(resp), "\n\n")
cat("Raw response body:\n")
cat(resp_body_string(resp), "\n")