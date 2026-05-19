# R/mod_customers_ui.R
# Customer Queue — prospects who have moved to CRM but still need check-ins.

mod_customers_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(

    # ---- Left: queue table --------------------------------------------------
    shiny::column(
      5,
      shiny::div(
        class = "queue-controls",
        shiny::selectInput(
          ns("cust_scope"),
          label    = NULL,
          choices  = c("Check-ins Due", "All Customers"),
          selected = "Check-ins Due",
          width    = "200px"
        )
      ),
      DT::DTOutput(ns("cust_table"))
    ),

    # ---- Right: detail panel ------------------------------------------------
    shiny::column(
      7,
      shiny::uiOutput(ns("cust_detail"))
    )
  )
}
