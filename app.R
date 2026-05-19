# app.R — Standalone Signal runner
#
# During development (from RStudio with signal.Rproj open, or from terminal):
#   devtools::load_all(); shiny::runApp()
#   -- or simply open this file and click Run App --
#
# After installing the package:
#   library(signal); shiny::runApp()

if (file.exists("DESCRIPTION") && requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(quiet = TRUE)
} else {
  library(signal)
}

# Resolve API key: options > env var > _secrets.yml
api_key <- getOption("signal.api_key", "")
if (is.null(api_key) || nchar(api_key) == 0) {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY", unset = "")
}
if (is.null(api_key) || nchar(api_key) == 0) {
  secrets <- tryCatch(yaml::read_yaml("_secrets.yml"), error = function(e) list())
  api_key <- secrets$claude$api_key
  if (is.null(api_key)) api_key <- ""
}

ui <- shiny::fluidPage(
  shiny::div(
    class = "app-header",
    shiny::h1("Signal"),
    shiny::p("Outbound tracking and email generation for facility consulting prospects."),
    shiny::uiOutput("signal_health")
  ),
  signal_ui("signal")
)

server <- function(input, output, session) {
  output$signal_health <- shiny::renderUI({
    msgs <- get_app_health_messages()
    if (length(msgs) == 0) return(NULL)
    shiny::div(class = "app-alert", lapply(msgs, shiny::tags$p))
  })

  signal_server("signal", api_key = api_key)
}

shiny::shinyApp(ui, server)
