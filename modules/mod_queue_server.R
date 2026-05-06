# modules/mod_queue_server.R

mod_queue_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    refresh_counter <- reactiveVal(0)
    history_counter <- reactiveVal(0)
    selected_prospect <- reactiveVal(NULL)
    latest_draft_id <- reactiveVal(NULL)
    latest_research <- reactiveVal(NULL)

    queue_data <- reactive({
      refresh_counter()

      scope <- input$queue_scope

      if (is.null(scope) || scope == "") {
        scope <- "Due or Overdue"
      }

      prospects <- if (scope %in% c("All Active", "Nurture")) {
        get_active_prospects()
      } else {
        get_outreach_queue()
      }

      if (nrow(prospects) == 0) {
        return(prospects)
      }

      next_touch <- suppressWarnings(as.Date(prospects$next_touch))
      today <- Sys.Date()

      if (scope == "Overdue") {
        prospects <- prospects[!is.na(next_touch) & next_touch < today, ]
      } else if (scope == "Due Today") {
        prospects <- prospects[is.na(next_touch) | next_touch == today, ]
      } else if (scope == "Nurture") {
        prospects <- prospects[prospects$status == "Nurture", ]
      } else if (scope != "All Active") {
        prospects <- prospects[prospects$status != "Nurture", ]
      }

      if (!is.null(input$queue_segment_filter) &&
          input$queue_segment_filter != "All") {
        prospects <- prospects[
          !is.na(prospects$segment) &
            prospects$segment == input$queue_segment_filter,
        ]
      }

      if (!is.null(input$queue_source_filter) &&
          input$queue_source_filter != "All") {
        prospects <- prospects[
          !is.na(prospects$source) &
            prospects$source == input$queue_source_filter,
        ]
      }

      prospects
    })

    queue_table_data <- reactive({
      format_queue_table_data(queue_data())
    })

    output$queue_counts <- renderUI({
      refresh_counter()

      prospects <- get_prospects(include_inactive = TRUE)

      if (nrow(prospects) == 0) {
        return(tags$div(
          class = "queue-counts",
          queue_count_ui("Due Today", 0),
          queue_count_ui("Overdue", 0),
          queue_count_ui("Active", 0),
          queue_count_ui("Terminal", 0)
        ))
      }

      active <- prospects[!is_terminal_status(prospects$status), ]
      terminal <- prospects[is_terminal_status(prospects$status), ]
      nurture <- active[active$status == "Nurture", ]
      active_next_touch <- suppressWarnings(as.Date(active$next_touch))
      today <- Sys.Date()

      due_today <- sum(is.na(active_next_touch) | active_next_touch == today)
      overdue <- sum(!is.na(active_next_touch) & active_next_touch < today)

      tags$div(
        class = "queue-counts",
        queue_count_ui("Due Today", due_today),
        queue_count_ui("Overdue", overdue),
        queue_count_ui("Active", nrow(active)),
        queue_count_ui("Nurture", nrow(nurture)),
        queue_count_ui("Terminal", nrow(terminal))
      )
    })

    output$queue_table <- renderDT({
      dblclick_input <- session$ns("queue_table_row_dblclick")

      datatable(
        queue_table_data(),
        rownames = FALSE,
        selection = "single",
        class = "compact stripe hover signal-table",
        options = list(
          pageLength = 8,
          autoWidth = FALSE,
          dom = "tip",
          columnDefs = list(
            list(visible = FALSE, targets = 0)
          )
        ),
        callback = DT::JS(sprintf(
          "
          table.on('dblclick', 'tbody tr', function() {
            var data = table.row(this).data();
            if (data) {
              Shiny.setInputValue('%s', data[0], {priority: 'event'});
            }
          });
          ",
          dblclick_input
        ))
      )
    })

    observeEvent(input$refresh_queue, {
      refresh_counter(refresh_counter() + 1)
    })

    observeEvent(input$queue_table_rows_selected, {
      selected_row <- input$queue_table_rows_selected
      req(selected_row)

      row <- queue_table_data()[selected_row, ]
      select_queue_prospect(row$ID, session, selected_prospect, latest_draft_id, latest_research, history_counter)
    })

    observeEvent(input$queue_table_row_dblclick, {
      prospect <- select_queue_prospect(
        input$queue_table_row_dblclick,
        session,
        selected_prospect,
        latest_draft_id,
        latest_research,
        history_counter
      )

      if (!is.null(prospect)) {
        show_prospect_modal(session$ns, prospect)
      }
    })

    observeEvent(input$open_prospect_modal, {
      prospect <- selected_prospect()

      if (is.null(prospect)) {
        showNotification("Select a prospect first.", type = "warning")
        return()
      }

      show_prospect_modal(session$ns, prospect)
    })

    output$touch_history_table <- renderDT({
      history_counter()
      prospect <- selected_prospect()

      if (is.null(prospect)) {
        return(datatable(
          data.frame(Message = "Select a prospect to view touch history."),
          rownames = FALSE
        ))
      }

      touches <- get_touches_for_prospect(prospect$id)

      if (nrow(touches) == 0) {
        return(datatable(
          data.frame(Message = "No touches logged yet."),
          rownames = FALSE
        ))
      }

      datatable(
        touches[, c("created_at", "touch_type", "outcome", "sequence_stage", "subject")],
        rownames = FALSE,
        options = list(pageLength = 5, autoWidth = TRUE, scrollX = TRUE)
      )
    })

    output$draft_history_table <- renderDT({
      history_counter()
      prospect <- selected_prospect()

      if (is.null(prospect)) {
        return(datatable(
          data.frame(Message = "Select a prospect to view draft history."),
          rownames = FALSE
        ))
      }

      drafts <- get_drafts_for_prospect(prospect$id)

      if (nrow(drafts) == 0) {
        return(datatable(
          data.frame(Message = "No drafts saved yet."),
          rownames = FALSE
        ))
      }

      datatable(
        drafts[, c("created_at", "status", "sequence_stage", "subject")],
        rownames = FALSE,
        options = list(pageLength = 5, autoWidth = TRUE, scrollX = TRUE)
      )
    })

    output$selected_status_badge <- renderUI({
      prospect <- selected_prospect()

      if (is.null(prospect)) {
        return(NULL)
      }

      status_badge_ui(prospect$status)
    })

    output$selected_summary <- renderUI({
      prospect <- selected_prospect()

      if (is.null(prospect)) {
        return(empty_state_ui("Select a prospect from the queue."))
      }

      tagList(
        div(
          class = "prospect-heading",
          h4(format_person_name(prospect)),
          div(class = "muted-text", display_value(prospect$company))
        ),
        div(
          class = "detail-grid",
          detail_item_ui("Title", prospect$title),
          detail_item_ui("Email", prospect$email),
          detail_item_ui("Source", prospect$source),
          detail_item_ui("Segment", prospect$segment),
          detail_item_ui("Stage", format_sequence_stage(prospect$sequence_stage)),
          detail_item_ui("Next Touch", prospect$next_touch)
        ),
        note_block_ui("Reason", prospect$reason_for_outreach),
        note_block_ui("Personalization", prospect$personalization_notes)
      )
    })

    output$research_summary <- renderUI({
      research <- latest_research()
      prospect <- selected_prospect()

      if (is.null(prospect)) {
        return(NULL)
      }

      parsed_research <- parse_research_notes_for_display(
        prospect$research_notes,
        prospect$research_sources
      )

      if (is.null(research) && !isTRUE(parsed_research$has_research)) {
        return(NULL)
      }

      div(
        class = "research-status",
        strong("Research saved."),
        span(" Open the prospect to review the summary, signals, and sources.")
      )
    })

    output$recommended_action <- renderUI({
      prospect <- selected_prospect()

      if (is.null(prospect)) {
        return(empty_state_ui("Select a prospect first."))
      }

      action <- format_next_action(
        status = prospect$status,
        sequence_stage = prospect$sequence_stage,
        next_touch = prospect$next_touch
      )

      div(
        class = "next-step-card",
        div(class = "next-step-label", "Recommended"),
        h4(action),
        div(
          class = "detail-grid compact",
          detail_item_ui("Status", prospect$status),
          detail_item_ui("Stage", format_sequence_stage(prospect$sequence_stage)),
          detail_item_ui("Last Touch", prospect$last_touch),
          detail_item_ui("Next Touch", prospect$next_touch)
        )
      )
    })

    output$open_outlook_link <- renderUI({
      prospect <- selected_prospect()

      if (is.null(prospect)) {
        return(NULL)
      }

      if (is.null(prospect$email) || is.na(prospect$email) || prospect$email == "") {
        return(tags$p(
          class = "helper-text",
          "Add an email address for this prospect to open a prefilled Outlook message."
        ))
      }

      subject <- input$draft_subject %||% ""
      body <- input$draft_body %||% ""

      if (subject == "" && body == "") {
        return(tags$p(
          class = "helper-text",
          "Generate or write a draft to enable the Outlook link."
        ))
      }

      mailto_url <- build_mailto_url(
        to = prospect$email,
        subject = subject,
        body = body
      )

      tags$a(
        href = mailto_url,
        target = "_blank",
        class = "btn btn-primary",
        "Open in Outlook"
      )
    })

    observeEvent(input$generate_draft, {
      prospect <- selected_prospect()
      req(prospect)

      draft <- generate_queue_draft(prospect)

      create_draft(
        prospect_id = prospect$id,
        subject = draft$subject,
        body = draft$body,
        sequence_stage = prospect$sequence_stage
      )

      latest_draft <- get_latest_draft_for_prospect(prospect$id)
      latest_draft_id(latest_draft$id)

      updateTextInput(session, "draft_subject", value = draft$subject)
      updateTextAreaInput(session, "draft_body", value = draft$body)
      history_counter(history_counter() + 1)

      showNotification("Draft generated and saved.", type = "message")
    })

    observeEvent(input$research_prospect, {
      prospect <- selected_prospect()
      req(prospect)

      research <- research_prospect_with_claude_safe(prospect)
      research_notes <- research$formatted_notes %||% research$summary
      research_sources <- paste(research$sources %||% character(0), collapse = "\n")

      update_prospect_research(
        prospect_id = prospect$id,
        research_notes = research_notes,
        research_sources = research_sources
      )

      refreshed <- get_prospect_by_id(prospect$id)
      selected_prospect(refreshed)
      latest_research(research_notes)

      showNotification("Research saved to prospect.", type = "message")
    })

    observeEvent(input$generate_local_draft, {
      prospect <- selected_prospect()
      req(prospect)

      draft <- generate_queue_local_draft(prospect)

      create_draft(
        prospect_id = prospect$id,
        subject = draft$subject,
        body = draft$body,
        sequence_stage = prospect$sequence_stage
      )

      latest_draft <- get_latest_draft_for_prospect(prospect$id)
      latest_draft_id(latest_draft$id)

      updateTextInput(session, "draft_subject", value = draft$subject)
      updateTextAreaInput(session, "draft_body", value = draft$body)
      history_counter(history_counter() + 1)

      showNotification("Local draft generated and saved.", type = "message")
    })

    observeEvent(input$copy_draft, {
      subject <- input$draft_subject %||% ""
      body <- input$draft_body %||% ""

      if (subject == "" && body == "") {
        showNotification("No draft to copy.", type = "warning")
        return()
      }

      draft_text <- paste(
        paste0("Subject: ", subject),
        "",
        body,
        sep = "\n"
      )

      session$sendCustomMessage(
        "copy-draft-to-clipboard",
        list(text = draft_text)
      )

      showNotification("Draft copied to clipboard.", type = "message")
    })

    observeEvent(input$log_sent, {
      prospect <- selected_prospect()
      req(prospect)

      subject <- empty_to_na(input$draft_subject)
      body <- empty_to_na(input$draft_body)

      if (is.null(latest_draft_id()) && (!is.na(subject) || !is.na(body))) {
        create_draft(
          prospect_id = prospect$id,
          subject = subject,
          body = body,
          sequence_stage = prospect$sequence_stage
        )

        latest_draft <- get_latest_draft_for_prospect(prospect$id)
        latest_draft_id(latest_draft$id)
      }

      log_touch(
        prospect_id = prospect$id,
        touch_type = "Email",
        subject = subject,
        body = body,
        outcome = "Sent",
        sequence_stage = prospect$sequence_stage
      )

      if (!is.null(latest_draft_id())) {
        update_draft(
          draft_id = latest_draft_id(),
          subject = subject,
          body = body,
          status = "Sent"
        )
      }

      showNotification("Touch logged. Prospect advanced to next stage.", type = "message")

      selected_prospect(NULL)
      latest_draft_id(NULL)
      history_counter(history_counter() + 1)

      updateTextInput(session, "draft_subject", value = "")
      updateTextAreaInput(session, "draft_body", value = "")

      refresh_counter(refresh_counter() + 1)
    })

    observeEvent(input$snooze, {
      prospect <- selected_prospect()
      req(prospect)

      snooze_prospect(prospect$id, days = DEFAULT_QUEUE_SNOOZE_DAYS)

      showNotification("Prospect snoozed.", type = "message")

      selected_prospect(NULL)
      refresh_counter(refresh_counter() + 1)
    })

    observeEvent(input$mark_replied, {
      prospect <- selected_prospect()
      req(prospect)

      update_prospect_status(
        prospect_id = prospect$id,
        status = "Replied",
        reply_notes = "Marked replied from outreach queue."
      )

      showNotification("Marked as replied. Removed from active queue.", type = "message")

      selected_prospect(NULL)
      refresh_counter(refresh_counter() + 1)
    })

    observeEvent(input$mark_not_interested, {
      prospect <- selected_prospect()
      req(prospect)

      update_prospect_status(
        prospect_id = prospect$id,
        status = "Not Interested",
        reply_notes = "Marked not interested from outreach queue."
      )

      showNotification("Marked not interested. Removed from active queue.", type = "warning")

      selected_prospect(NULL)
      refresh_counter(refresh_counter() + 1)
    })

    observeEvent(input$mark_bounced, {
      prospect <- selected_prospect()
      req(prospect)

      log_touch(
        prospect_id = prospect$id,
        touch_type = "Email",
        subject = empty_to_na(input$draft_subject),
        body = empty_to_na(input$draft_body),
        outcome = "Bounced",
        sequence_stage = prospect$sequence_stage
      )

      showNotification("Marked bounced. Fix the email address before next outreach.", type = "warning")

      selected_prospect(NULL)
      latest_draft_id(NULL)
      history_counter(history_counter() + 1)
      refresh_counter(refresh_counter() + 1)
    })

    observeEvent(input$mark_dnc, {
      prospect <- selected_prospect()
      req(prospect)

      update_prospect_status(
        prospect_id = prospect$id,
        status = "Do Not Contact",
        reply_notes = "Marked do not contact from outreach queue."
      )

      showNotification("Marked do not contact. Removed from active queue.", type = "error")

      selected_prospect(NULL)
      refresh_counter(refresh_counter() + 1)
    })

    observeEvent(input$modal_save_prospect, {
      prospect <- selected_prospect()
      req(prospect)

      update_prospect(
        prospect_id = prospect$id,
        prospect = list(
          first_name = empty_to_na(input$modal_first_name),
          last_name = empty_to_na(input$modal_last_name),
          company = empty_to_na(input$modal_company),
          title = empty_to_na(input$modal_title),
          email = empty_to_na(input$modal_email),
          linkedin_url = empty_to_na(input$modal_linkedin_url),
          website = empty_to_na(input$modal_website),
          city = empty_to_na(input$modal_city),
          state = empty_to_na(input$modal_state),
          source = empty_to_na(input$modal_source),
          segment = empty_to_na(input$modal_segment),
          reason_for_outreach = empty_to_na(input$modal_reason_for_outreach),
          personalization_notes = empty_to_na(input$modal_personalization_notes),
          research_notes = prospect$research_notes,
          research_sources = prospect$research_sources,
          researched_at = prospect$researched_at,
          status = input$modal_status,
          sequence_stage = input$modal_sequence_stage,
          next_touch = as.character(input$modal_next_touch),
          reply_notes = empty_to_na(input$modal_reply_notes)
        )
      )

      refreshed <- get_prospect_by_id(prospect$id)
      selected_prospect(refreshed)
      refresh_counter(refresh_counter() + 1)
      history_counter(history_counter() + 1)
      removeModal()

      showNotification("Prospect saved.", type = "message")
    })

    observeEvent(input$modal_delete_prospect, {
      prospect <- selected_prospect()
      req(prospect)

      delete_prospect(prospect$id)

      selected_prospect(NULL)
      latest_draft_id(NULL)
      latest_research(NULL)
      refresh_counter(refresh_counter() + 1)
      history_counter(history_counter() + 1)

      updateTextInput(session, "draft_subject", value = "")
      updateTextAreaInput(session, "draft_body", value = "")
      removeModal()

      showNotification("Prospect deleted.", type = "warning")
    })
  })
}


# ---- Draft generation wrapper ----------------------------------------------
# Claude + fallback behavior lives in services/claude.R.

generate_queue_draft <- function(prospect) {
  generate_email_safe(prospect)
}

generate_queue_local_draft <- function(prospect) {
  fallback_generate_email_from_claude_service(prospect)
}

build_mailto_url <- function(to, subject = "", body = "") {
  to <- URLencode(to %||% "", reserved = TRUE)
  subject <- URLencode(subject %||% "", reserved = TRUE)
  body <- URLencode(body %||% "", reserved = TRUE)

  paste0(
    "mailto:", to,
    "?subject=", subject,
    "&body=", body
  )
}

queue_count_ui <- function(label, value) {
  tags$div(
    class = "queue-count",
    tags$strong(value),
    tags$span(label)
  )
}

select_queue_prospect <- function(
    prospect_id,
    session,
    selected_prospect,
    latest_draft_id,
    latest_research,
    history_counter
) {
  prospect <- get_prospect_by_id(prospect_id)

  if (is.null(prospect)) {
    return(NULL)
  }

  selected_prospect(prospect)
  latest_draft_id(NULL)
  latest_research(NULL)
  history_counter(history_counter() + 1)

  updateTextInput(session, "draft_subject", value = "")
  updateTextAreaInput(session, "draft_body", value = "")

  prospect
}

show_prospect_modal <- function(ns, prospect) {
  showModal(modalDialog(
    title = tagList(
      div(
        class = "modal-title-row",
        span(format_person_name(prospect)),
        status_badge_ui(prospect$status)
      )
    ),
    size = "l",
    easyClose = TRUE,
    div(
      class = "prospect-modal",
      div(
        class = "detail-grid compact",
        detail_item_ui("Company", prospect$company),
        detail_item_ui("Title", prospect$title),
        detail_item_ui("Email", prospect$email),
        detail_item_ui("Location", format_location_value(prospect$city, prospect$state)),
        detail_item_ui("Stage", format_sequence_stage(prospect$sequence_stage)),
        detail_item_ui("Next Touch", prospect$next_touch)
      ),
      tabsetPanel(
        type = "pills",
        tabPanel(
          "Details",
          fluidRow(
            column(6, textInput(ns("modal_first_name"), "First Name", value = display_value(prospect$first_name, ""))),
            column(6, textInput(ns("modal_last_name"), "Last Name", value = display_value(prospect$last_name, "")))
          ),
          textInput(ns("modal_company"), "Company", value = display_value(prospect$company, "")),
          textInput(ns("modal_title"), "Title", value = display_value(prospect$title, "")),
          textInput(ns("modal_email"), "Email", value = display_value(prospect$email, "")),
          textInput(ns("modal_linkedin_url"), "LinkedIn URL", value = display_value(prospect$linkedin_url, "")),
          textInput(ns("modal_website"), "Website", value = display_value(prospect$website, "")),
          fluidRow(
            column(6, textInput(ns("modal_city"), "City", value = display_value(prospect$city, ""))),
            column(6, textInput(ns("modal_state"), "State", value = display_value(prospect$state, "")))
          ),
          fluidRow(
            column(
              6,
              selectInput(
                ns("modal_source"),
                "Source",
                choices = PROSPECT_SOURCES,
                selected = display_value(prospect$source, "")
              )
            ),
            column(
              6,
              selectInput(
                ns("modal_segment"),
                "Segment",
                choices = PROSPECT_SEGMENTS,
                selected = display_value(prospect$segment, "")
              )
            )
          )
        ),
        tabPanel(
          "Notes",
          textAreaInput(
            ns("modal_reason_for_outreach"),
            "Reason for Outreach",
            rows = 4,
            value = display_value(prospect$reason_for_outreach, "")
          ),
          textAreaInput(
            ns("modal_personalization_notes"),
            "Personalization Notes",
            rows = 5,
            value = display_value(prospect$personalization_notes, "")
          )
        ),
        tabPanel(
          "Research",
          div(
            class = "modal-research-pane",
            research_notes_ui(prospect$research_notes, prospect$research_sources)
          )
        ),
        tabPanel(
          "Workflow",
          fluidRow(
            column(
              6,
              selectInput(
                ns("modal_status"),
                "Status",
                choices = PROSPECT_STATUSES,
                selected = display_value(prospect$status, DEFAULT_PROSPECT_STATUS)
              )
            ),
            column(
              6,
              selectInput(
                ns("modal_sequence_stage"),
                "Sequence Stage",
                choices = setNames(SEQUENCE_STAGES, paste0(
                  SEQUENCE_STAGES,
                  " - ",
                  unname(SEQUENCE_STAGE_LABELS[as.character(SEQUENCE_STAGES)])
                )),
                selected = as.character(normalize_sequence_stage(prospect$sequence_stage))
              )
            )
          ),
          fluidRow(
            column(
              6,
              dateInput(
                ns("modal_next_touch"),
                "Next Touch",
                value = modal_next_touch_value(prospect$next_touch)
              )
            )
          ),
          textAreaInput(
            ns("modal_reply_notes"),
            "Reply / Outcome Notes",
            rows = 4,
            value = display_value(prospect$reply_notes, "")
          )
        )
      )
    ),
    footer = tagList(
      modalButton("Close"),
      actionButton(ns("modal_delete_prospect"), "Delete Prospect", class = "btn-danger"),
      actionButton(ns("modal_save_prospect"), "Save Prospect", class = "btn-primary")
    )
  ))
}

modal_next_touch_value <- function(next_touch) {
  next_touch <- display_value(next_touch, "")

  if (next_touch == "") {
    return(Sys.Date())
  }

  value <- suppressWarnings(as.Date(next_touch))

  if (is.na(value)) {
    return(Sys.Date())
  }

  value
}
