# R/mod_conversation_ui.R
# Conversation Queue — prospects who have replied and are in active dialogue.
# Workflow: log calls/meetings, snooze, mark as Customer or Not Interested.

mod_conversation_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(

    # ---- Left: queue table --------------------------------------------------
    shiny::column(
      5,
      shiny::div(
        class = "queue-controls",
        shiny::selectInput(
          ns("conv_scope"),
          label    = NULL,
          choices  = c("Due or Overdue", "All In Conversation"),
          selected = "Due or Overdue",
          width    = "200px"
        )
      ),
      DT::DTOutput(ns("conv_table"))
    ),

    # ---- Right: detail panel ------------------------------------------------
    shiny::column(
      7,
      shiny::uiOutput(ns("conv_detail"))
    )
  )
}
