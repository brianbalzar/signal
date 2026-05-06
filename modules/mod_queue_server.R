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
      datatable(
        queue_data(),
        rownames = FALSE,
        selection = "single",
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          scrollX = TRUE
        )
      )
    })
    
    observeEvent(input$refresh_queue, {
      refresh_counter(refresh_counter() + 1)
    })
    
    observeEvent(input$queue_table_rows_selected, {
      selected_row <- input$queue_table_rows_selected
      req(selected_row)
      
      row <- queue_data()[selected_row, ]
      prospect <- get_prospect_by_id(row$id)
      
      selected_prospect(prospect)
      latest_draft_id(NULL)
      latest_research(NULL)
      history_counter(history_counter() + 1)
      
      updateTextInput(session, "draft_subject", value = "")
      updateTextAreaInput(session, "draft_body", value = "")
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
    
    output$selected_summary <- renderText({
      prospect <- selected_prospect()
      
      if (is.null(prospect)) {
        return("Select a prospect from the queue.")
      }
      
      paste(
        paste("Name:", paste(prospect$first_name, prospect$last_name)),
        paste("Company:", prospect$company %||% ""),
        paste("Title:", prospect$title %||% ""),
        paste("Email:", prospect$email %||% ""),
        paste("Source:", prospect$source %||% ""),
        paste("Segment:", prospect$segment %||% ""),
        paste("Status:", prospect$status %||% ""),
        paste("Sequence Stage:", format_sequence_stage(prospect$sequence_stage)),
        paste("Last Touch:", prospect$last_touch %||% ""),
        paste("Next Touch:", prospect$next_touch %||% ""),
        "",
        "Reason for Outreach:",
        prospect$reason_for_outreach %||% "",
        "",
        "Personalization Notes:",
        prospect$personalization_notes %||% "",
        "",
        "Research Notes:",
        prospect$research_notes %||% "",
        "",
        "Research Sources:",
        prospect$research_sources %||% "",
        sep = "\n"
      )
    })
    
    output$research_summary <- renderText({
      research <- latest_research()
      prospect <- selected_prospect()
      
      if (!is.null(research)) {
        return(research)
      }
      
      if (is.null(prospect) ||
          is.null(prospect$research_notes) ||
          is.na(prospect$research_notes) ||
          prospect$research_notes == "") {
        return("")
      }
      
      paste(
        "Stored Research:",
        prospect$research_notes %||% "",
        "",
        "Sources:",
        prospect$research_sources %||% "",
        sep = "\n"
      )
    })
    
    output$recommended_action <- renderText({
      prospect <- selected_prospect()
      
      if (is.null(prospect)) {
        return("Select a prospect first.")
      }
      
      format_next_action(
        status = prospect$status,
        sequence_stage = prospect$sequence_stage,
        next_touch = prospect$next_touch
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
