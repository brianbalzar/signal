# R/mod_customers_server.R
# Customer Queue server — log check-in touches, schedule next contact.

mod_customers_server <- function(id, ae_filter = NULL, user_role = "admin") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    refresh  <- shiny::reactiveVal(0)
    selected <- shiny::reactiveVal(NULL)

    # ---- Queue data ---------------------------------------------------------

    queue_data <- shiny::reactive({
      refresh()
      scope <- input$cust_scope %||% "Check-ins Due"

      if (scope == "All Customers") {
        get_all_customers(ae_filter = ae_filter)
      } else {
        get_customer_queue(ae_filter = ae_filter)
      }
    })

    output$cust_table <- DT::renderDT({
      df <- queue_data()

      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(
          data.frame(Message = "No customer check-ins due right now."),
          options  = list(dom = "t", paging = FALSE),
          rownames = FALSE
        ))
      }

      display <- data.frame(
        Name            = df$name,
        Company         = df$company %||% "",
        `Customer Since` = format_date_display(df$customer_since),
        `Next Check-in`  = format_date_display(df$next_touch),
        check.names     = FALSE
      )

      DT::datatable(
        display,
        selection = "single",
        rownames  = FALSE,
        options   = list(
          dom        = "t",
          paging     = FALSE,
          scrollY    = "400px",
          scrollCollapse = TRUE
        )
      )
    })

    # ---- Row selection ------------------------------------------------------

    shiny::observeEvent(input$cust_table_rows_selected, {
      df  <- queue_data()
      idx <- input$cust_table_rows_selected

      if (is.null(idx) || nrow(df) == 0) {
        selected(NULL)
        return()
      }

      selected(get_prospect_by_id(df$id[idx]))
    })

    # ---- Detail panel -------------------------------------------------------

    output$cust_detail <- shiny::renderUI({
      p <- selected()

      if (is.null(p)) {
        return(shiny::div(
          class = "queue-empty-state",
          shiny::p("Select a customer to log a check-in or schedule the next contact.")
        ))
      }

      name <- trimws(paste(p$first_name %||% "", p$last_name %||% ""))

      shiny::tagList(

        shiny::div(
          class = "prospect-card",
          shiny::h4(name),
          shiny::p(shiny::strong(p$company %||% ""), " — ", p$title %||% ""),
          shiny::p(shiny::em("Customer since: "), format_date_display(p$customer_since)),
          if (!is.null(p$customer_notes) && !is.na(p$customer_notes) && nchar(trimws(p$customer_notes)) > 0) {
            shiny::p(shiny::em("Notes: "), p$customer_notes)
          },
          shiny::p(shiny::em("Next check-in: "), format_date_display(p$next_touch))
        ),

        shiny::hr(),

        shiny::h5("Log a Check-in"),
        shiny::fluidRow(
          shiny::column(4,
            shiny::selectInput(ns("touch_type"), "Type",
              choices  = c("Call", "Meeting", "Email", "Manual Note", "Other"),
              selected = "Call"
            )
          ),
          shiny::column(4,
            shiny::selectInput(ns("touch_outcome"), "Outcome",
              choices  = c("Connected", "Meeting Completed", "Voicemail",
                           "No Answer", "Call Back Later", "Manual Note"),
              selected = "Connected"
            )
          )
        ),
        shiny::textAreaInput(ns("touch_notes"), "Notes (optional)", rows = 2),

        shiny::hr(),

        shiny::h5("Schedule Next Check-in"),
        shiny::fluidRow(
          shiny::column(4,
            shiny::numericInput(ns("checkin_days"), "Days from today",
              value = DEFAULT_CUSTOMER_CHECKIN_DAYS, min = 1, max = 730
            )
          ),
          shiny::column(4,
            shiny::dateInput(ns("checkin_date"), "Or pick a date", value = NA)
          )
        ),

        shiny::actionButton(ns("log_checkin_btn"), "Log & Schedule Next", class = "btn-primary"),
        shiny::actionButton(ns("schedule_only_btn"), "Schedule Only", class = "btn-default")
      )
    })

    # ---- Log check-in + schedule next ---------------------------------------

    shiny::observeEvent(input$log_checkin_btn, {
      p <- selected()
      shiny::req(p)

      next_touch <- resolve_customer_next_touch(
        next_touch_date = tryCatch(as.Date(input$checkin_date), error = function(e) NA),
        next_touch_days = suppressWarnings(as.integer(input$checkin_days))
      )

      log_touch(
        prospect_id      = p$id,
        touch_type       = input$touch_type %||% "Call",
        body             = input$touch_notes %||% NULL,
        outcome          = input$touch_outcome %||% "Connected",
        sequence_stage   = p$sequence_stage,
        advance_sequence = FALSE,
        next_touch       = next_touch
      )

      selected(NULL)
      refresh(refresh() + 1)
    })

    # ---- Schedule only (no touch logged) ------------------------------------

    shiny::observeEvent(input$schedule_only_btn, {
      p <- selected()
      shiny::req(p)

      next_touch <- resolve_customer_next_touch(
        next_touch_date = tryCatch(as.Date(input$checkin_date), error = function(e) NA),
        next_touch_days = suppressWarnings(as.integer(input$checkin_days))
      )

      snooze_prospect(p$id, days = as.integer(
        as.Date(next_touch) - Sys.Date()
      ))

      selected(NULL)
      refresh(refresh() + 1)
    })
  })
}


# ---- Helper: all Customer prospects (not just due) --------------------------

get_all_customers <- function(ae_filter = NULL) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ae_sql <- if (!is.null(ae_filter)) "AND coalesce(assigned_to, '') = ?" else ""
  params <- if (!is.null(ae_filter)) list(ae_filter) else list()

  DBI::dbGetQuery(
    con,
    paste0(
      "
      SELECT
        id,
        trim(coalesce(first_name, '') || ' ' || coalesce(last_name, '')) AS name,
        company, title, email, assigned_to,
        status, customer_since, customer_notes, last_touch, next_touch
      FROM prospects
      WHERE status = 'Customer'
      ", ae_sql, "
      ORDER BY customer_since DESC, company ASC, last_name ASC
      "
    ),
    params = params
  )
}
