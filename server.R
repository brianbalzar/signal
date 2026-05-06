server <- function(input, output, session) {
  output$app_health <- renderUI({
    messages <- get_app_health_messages()
    
    if (length(messages) == 0) {
      return(NULL)
    }
    
    div(
      class = "app-alert",
      lapply(messages, tags$p)
    )
  })
  
  mod_queue_server("queue")
  mod_prospects_server("prospects")
}
