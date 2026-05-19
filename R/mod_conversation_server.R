# R/mod_conversation_server.R
# Conversation Queue server — prospects in "In Conversation" phase.
# Handles touch logging, snoozing, and transitioning to Customer / Not Interested.

mod_conversation_server <- function(id, ae_filter = NULL, user_role = "admin") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    refresh  <- shiny::reactiveVal(0)
    selected <- shiny::reactiveVal(NULL)

    can_reassign <- user_role %in% c("admin", "manager")

    # ---- Queue data ---------------------------------------------------------

    queue_data <- shiny::reactive({
      refresh()
      scope <- input$conv_scope %||% "Due or Overdue"

      prospects <- if (scope == "All In Conversation") {
        get_all_conversation_prospects(ae_filter = ae_filter)
      } else {
        get_conversation_queue(ae_filter = ae_filter)
      }

      prospects
    })

    output$conv_table <- DT::renderDT({
      df <- queue_data()

      if (is.null(df) || nrow(df) == 0) {
        return(DT::datatable(
          data.frame(Message = "No prospects in the conversation queue right now."),
          options  = list(dom = "t", paging = FALSE),
          rownames = FALSE
        ))
      }

      display <- data.frame(
        Name        = df$name,
        Company     = df$company %||% "",
        `Next Touch` = format_date_display(df$next_touch),
        check.names = FALSE
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

    shiny::observeEvent(input$conv_table_rows_selected, {
      df  <- queue_data()
      idx <- input$conv_table_rows_selected

      if (is.null(idx) || nrow(df) == 0) {
        selected(NULL)
        return()
      }

      selected(get_prospect_by_id(df$id[idx]))
    })

    # ---- Detail panel -------------------------------------------------------

    output$conv_detail <- shiny::renderUI({
      p <- selected()

      if (is.null(p)) {
        return(shiny::div(
          class = "queue-empty-state",
          shiny::p("Select a prospect from the queue to log a touch or take action.")
        ))
      }

      shiny::tagList(

        shiny::div(
          class = "prospect-card",
          shiny::h4(trimws(paste(p$first_name %||% "", p$last_name %||% ""))),
          shiny::p(shiny::strong(p$company %||% ""), " — ", p$title %||% ""),
          shiny::p(shiny::em("Next touch: "), format_date_display(p$next_touch))
        ),

        shiny::hr(),

        # Touch logging
        shiny::h5("Log a Touch"),
        shiny::fluidRow(
          shiny::column(4,
            shiny::selectInput(ns("touch_type"), "Type",
              choices  = c("Call", "Meeting", "Email", "LinkedIn", "Voicemail", "Manual Note", "Other"),
              selected = "Call"
            )
          ),
          shiny::column(4,
            shiny::selectInput(ns("touch_outcome"), "Outcome",
              choices  = c("Connected", "Voicemail", "No Answer", "Call Back Later",
                           "Meeting Scheduled", "Meeting Completed",
                           "Not Interested", "Do Not Contact", "Manual Note"),
              selected = "Connected"
            )
          ),
          shiny::column(4,
            shiny::numericInput(ns("snooze_days"), "Next touch (days)",
              value = DEFAULT_CONVERSATION_NEXT_TOUCH_DAYS, min = 1, max = 365
            )
          )
        ),
        shiny::textAreaInput(ns("touch_notes"), "Notes (optional)", rows = 2),
        shiny::actionButton(ns("log_touch_btn"), "Log Touch", class = "btn-primary"),

        shiny::hr(),

        # Move to Customer
        shiny::h5("Mark as Customer"),
        shiny::fluidRow(
          shiny::column(6,
            shiny::textAreaInput(ns("customer_notes"), "Notes (e.g. deal context)", rows = 2)
          ),
          shiny::column(3,
            shiny::numericInput(ns("checkin_days"), "First check-in (days)",
              value = DEFAULT_CUSTOMER_CHECKIN_DAYS, min = 1, max = 365
            )
          ),
          shiny::column(3,
            shiny::dateInput(ns("checkin_date"), "Or pick a date", value = NA)
          )
        ),
        shiny::actionButton(ns("mark_customer_btn"), "Move to Customer", class = "btn-success"),

        shiny::hr(),

        # Quick actions
        shiny::h5("Other Actions"),
        shiny::actionButton(ns("not_interested_btn"), "Not Interested", class = "btn-warning btn-sm"),
        shiny::actionButton(ns("dnc_btn"), "Do Not Contact", class = "btn-danger btn-sm")
      )
    })

    # ---- Log touch ----------------------------------------------------------

    shiny::observeEvent(input$log_touch_btn, {
      p <- selected()
      shiny::req(p)

      outcome    <- input$touch_outcome %||% "Connected"
      touch_type <- input$touch_type %||% "Call"
      days       <- suppressWarnings(as.integer(input$snooze_days))
      if (is.na(days) || days < 1) days <- DEFAULT_CONVERSATION_NEXT_TOUCH_DAYS

      next_touch <- as.character(Sys.Date() + days)

      log_touch(
        prospect_id    = p$id,
        touch_type     = touch_type,
        body           = input$touch_notes %||% NULL,
        outcome        = outcome,
        sequence_stage = p$sequence_stage,
        advance_sequence = FALSE,
        next_touch     = next_touch
      )

      # Terminal outcomes from conversation phase.
      if (outcome %in% c("Not Interested", "Do Not Contact")) {
        update_prospect_status(p$id, outcome)
      }

      selected(NULL)
      refresh(refresh() + 1)
    })

    # ---- Mark as Customer ---------------------------------------------------

    shiny::observeEvent(input$mark_customer_btn, {
      p <- selected()
      shiny::req(p)

      checkin_date <- tryCatch(as.Date(input$checkin_date), error = function(e) NA)
      checkin_days <- suppressWarnings(as.integer(input$checkin_days))

      mark_as_customer(
        prospect_id     = p$id,
        notes           = input$customer_notes %||% NULL,
        next_touch_days = if (is.na(checkin_days)) DEFAULT_CUSTOMER_CHECKIN_DAYS else checkin_days,
        next_touch_date = if (!is.na(checkin_date)) checkin_date else NULL
      )

      selected(NULL)
      refresh(refresh() + 1)
    })

    # ---- Quick terminal actions ---------------------------------------------

    shiny::observeEvent(input$not_interested_btn, {
      p <- selected()
      shiny::req(p)
      update_prospect_status(p$id, "Not Interested")
      selected(NULL)
      refresh(refresh() + 1)
    })

    shiny::observeEvent(input$dnc_btn, {
      p <- selected()
      shiny::req(p)
      update_prospect_status(p$id, "Do Not Contact")
      selected(NULL)
      refresh(refresh() + 1)
    })
  })
}


# ---- Helper: all In Conversation prospects (not just due) -------------------

get_all_conversation_prospects <- function(ae_filter = NULL) {
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
        company, title, email, source, segment, assigned_to,
        reason_for_outreach, status, sequence_stage, last_touch, next_touch
      FROM prospects
      WHERE status = 'In Conversation'
      ", ae_sql, "
      ORDER BY
        CASE WHEN next_touch IS NULL OR next_touch = '' THEN 1 ELSE 0 END,
        next_touch ASC, company ASC, last_name ASC
      "
    ),
    params = params
  )
}
