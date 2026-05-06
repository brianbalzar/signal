ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  div(
    class = "app-header",
    h1("Signal"),
    p("Outbound tracking and email generation for facility consulting prospects."),
    uiOutput("app_health")
  ),

  tabsetPanel(
    id = "main_tabs",

    tabPanel(
      "Outreach Queue",
      mod_queue_ui("queue")
    ),

    tabPanel(
      "Prospects",
      mod_prospects_ui("prospects")
    )
  )
)
