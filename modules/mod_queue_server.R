# modules/mod_queue_server.R

mod_queue_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    refresh_counter <- reactiveVal(0)
    history_counter <- reactiveVal(0)
    selected_prospect <- reactiveVal(NULL)
    latest_draft_id <- reactiveVal(NULL)
    latest_call_prep_id <- reactiveVal(NULL)
    latest_research <- reactiveVal(NULL)
    generating_draft <- reactiveVal(FALSE)
    generating_local_draft <- reactiveVal(FALSE)
    researching_prospect <- reactiveVal(FALSE)
    prepping_call <- reactiveVal(FALSE)

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

    touch_history_data <- reactive({
      history_counter()
      prospect <- selected_prospect()

      if (is.null(prospect)) {
        return(NULL)
      }

      get_touches_for_prospect(prospect$id)
    })

    draft_history_data <- reactive({
      history_counter()
      prospect <- selected_prospect()

      if (is.null(prospect)) {
        return(NULL)
      }

      get_drafts_for_prospect(prospect$id)
    })

    output$queue_counts <- renderUI({
      refresh_counter()
      scope <- input$queue_scope %||% "Due or Overdue"

      prospects <- get_prospects(include_inactive = TRUE)

      if (nrow(prospects) == 0) {
        return(tags$div(
          class = "queue-counts",
          queue_count_ui("Due Today", 0, session$ns("filter_due_today"), scope == "Due Today"),
          queue_count_ui("Overdue", 0, session$ns("filter_overdue"), scope == "Overdue"),
          queue_count_ui("Active", 0, session$ns("filter_active"), scope == "All Active"),
          queue_count_ui("Nurture", 0, session$ns("filter_nurture"), scope == "Nurture"),
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
        queue_count_ui("Due Today", due_today, session$ns("filter_due_today"), scope == "Due Today"),
        queue_count_ui("Overdue", overdue, session$ns("filter_overdue"), scope == "Overdue"),
        queue_count_ui("Active", nrow(active), session$ns("filter_active"), scope == "All Active"),
        queue_count_ui("Nurture", nrow(nurture), session$ns("filter_nurture"), scope == "Nurture"),
        queue_count_ui("Terminal", nrow(terminal))
      )
    })

    output$today_focus <- renderUI({
      refresh_counter()

      prospects <- get_prospects(include_inactive = TRUE)

      if (nrow(prospects) == 0) {
        return(NULL)
      }

      active <- prospects[!is_terminal_status(prospects$status), ]

      if (nrow(active) == 0) {
        return(NULL)
      }

      next_touch <- suppressWarnings(as.Date(active$next_touch))
      today <- Sys.Date()
      ready_now <- sum(is.na(next_touch) | next_touch <= today)
      overdue <- sum(!is.na(next_touch) & next_touch < today)
      due_today <- sum(!is.na(next_touch) & next_touch == today)
      future_dates <- next_touch[!is.na(next_touch) & next_touch > today]
      next_scheduled <- if (length(future_dates) > 0) {
        as.character(min(future_dates))
      } else {
        "None"
      }

      tags$div(
        class = "today-focus",
        today_focus_item_ui("Ready now", ready_now),
        today_focus_item_ui("Overdue", overdue),
        today_focus_item_ui("Due today", due_today),
        today_focus_item_ui("Next scheduled", next_scheduled)
      )
    })

    observeEvent(input$filter_due_today, {
      updateSelectInput(session, "queue_scope", selected = "Due Today")
    })

    observeEvent(input$filter_overdue, {
      updateSelectInput(session, "queue_scope", selected = "Overdue")
    })

    observeEvent(input$filter_active, {
      updateSelectInput(session, "queue_scope", selected = "All Active")
    })

    observeEvent(input$filter_nurture, {
      updateSelectInput(session, "queue_scope", selected = "Nurture")
    })

    output$queue_table_ui <- renderUI({
      if (nrow(queue_table_data()) > 0) {
        return(DTOutput(session$ns("queue_table")))
      }

      queue_empty_state_ui(
        scope = input$queue_scope %||% "Due or Overdue",
        segment = input$queue_segment_filter %||% "All",
        source = input$queue_source_filter %||% "All"
      )
    })

    output$queue_table <- renderDT({
      req(nrow(queue_table_data()) > 0)
      dblclick_input <- session$ns("queue_table_row_dblclick")
      click_input <- session$ns("queue_table_row_click")

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
          table.on('click', 'tbody tr', function() {
            var data = table.row(this).data();
            if (data) {
              Shiny.setInputValue('%s', data[0], {priority: 'event'});
            }
          });

          table.on('dblclick', 'tbody tr', function() {
            var data = table.row(this).data();
            if (data) {
              Shiny.setInputValue('%s', data[0], {priority: 'event'});
            }
          });
          ",
          click_input,
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

      table_data <- queue_table_data()

      if (selected_row > nrow(table_data)) {
        return()
      }

      row <- table_data[selected_row, ]
      select_queue_prospect(
        row$ID,
        session,
        selected_prospect,
        latest_draft_id,
        latest_call_prep_id,
        latest_research,
        history_counter
      )
    })

    observeEvent(input$queue_table_row_click, {
      select_queue_prospect(
        input$queue_table_row_click,
        session,
        selected_prospect,
        latest_draft_id,
        latest_call_prep_id,
        latest_research,
        history_counter
      )
    })

    observeEvent(input$queue_table_row_dblclick, {
      prospect <- select_queue_prospect(
        input$queue_table_row_dblclick,
        session,
        selected_prospect,
        latest_draft_id,
        latest_call_prep_id,
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

    output$prospect_action_buttons <- renderUI({
      prospect <- selected_prospect()
      has_selection <- !is.null(prospect)
      research_busy <- isTRUE(researching_prospect())
      has_saved_research <- has_selection && has_company_research(prospect)

      research_buttons <- if (!has_selection || research_busy) {
        tagList(queue_action_button(
          session$ns,
          "research_prospect",
          if (research_busy) "Researching..." else "Research Organization",
          enabled = has_selection && !research_busy
        ))
      } else if (has_saved_research) {
        tagList(
          queue_action_button(
            session$ns,
            "use_cached_research",
            "Use Saved Research",
            class = "btn-primary",
            enabled = TRUE
          ),
          queue_action_button(
            session$ns,
            "refresh_research",
            "Refresh Research",
            enabled = TRUE
          )
        )
      } else {
        tagList(queue_action_button(
          session$ns,
          "research_prospect",
          "Research Organization",
          class = "btn-primary",
          enabled = TRUE
        ))
      }

      div(
        class = "button-row",
        queue_action_button(session$ns, "open_prospect_modal", "Open Prospect", enabled = has_selection),
        research_buttons
      )
    })

    output$draft_action_buttons <- renderUI({
      has_selection <- !is.null(selected_prospect())
      draft_busy <- isTRUE(generating_draft())
      local_busy <- isTRUE(generating_local_draft())

      div(
        class = "button-row",
        queue_action_button(
          session$ns,
          "generate_draft",
          if (draft_busy) "Generating..." else "Generate Draft",
          class = "btn-primary",
          enabled = has_selection && !draft_busy && !local_busy
        ),
        queue_action_button(
          session$ns,
          "generate_local_draft",
          if (local_busy) "Creating..." else "Create Local Draft",
          enabled = has_selection && !draft_busy && !local_busy
        ),
        queue_action_button(
          session$ns,
          "log_sent",
          "Log Email as Sent",
          enabled = has_selection && !draft_busy && !local_busy
        ),
        queue_action_button(
          session$ns,
          "snooze",
          paste0("Snooze ", DEFAULT_QUEUE_SNOOZE_DAYS, " Days"),
          enabled = has_selection
        )
      )
    })

    output$call_action_buttons <- renderUI({
      has_selection <- !is.null(selected_prospect())
      call_busy <- isTRUE(prepping_call())

      div(
        class = "button-row",
        queue_action_button(
          session$ns,
          "prep_call",
          if (call_busy) "Prepping..." else "Prep Call",
          class = "btn-primary",
          enabled = has_selection && !call_busy
        ),
        queue_action_button(
          session$ns,
          "quick_log_call",
          "Log Call",
          enabled = has_selection && !call_busy
        )
      )
    })

    output$call_log_buttons <- renderUI({
      has_selection <- !is.null(selected_prospect())
      call_busy <- isTRUE(prepping_call())

      div(
        class = "button-row",
        queue_action_button(
          session$ns,
          "copy_call_prep",
          "Copy Call Prep",
          enabled = has_selection && !call_busy
        ),
        queue_action_button(
          session$ns,
          "log_call",
          "Log Call",
          class = "btn-primary",
          enabled = has_selection && !call_busy
        )
      )
    })

    output$outcome_action_buttons <- renderUI({
      has_selection <- !is.null(selected_prospect())

      div(
        class = "button-row",
        queue_action_button(session$ns, "mark_replied", "Mark Replied", class = "btn-success", enabled = has_selection),
        queue_action_button(session$ns, "mark_not_interested", "Not Interested", class = "btn-warning", enabled = has_selection),
        queue_action_button(session$ns, "mark_bounced", "Mark Bounced", class = "btn-warning", enabled = has_selection),
        queue_action_button(session$ns, "mark_dnc", "Do Not Contact", class = "btn-danger", enabled = has_selection)
      )
    })

    output$touch_history_ui <- renderUI({
      touches <- touch_history_data()

      if (is.null(touches)) {
        return(empty_state_ui("Select a prospect to view touch history."))
      }

      if (nrow(touches) == 0) {
        return(empty_state_ui("No touches logged yet."))
      }

      DTOutput(session$ns("touch_history_table"))
    })

    output$touch_history_table <- renderDT({
      touches <- touch_history_data()
      req(!is.null(touches), nrow(touches) > 0)

      datatable(
        touches[, c("created_at", "touch_type", "outcome", "sequence_stage", "subject")],
        rownames = FALSE,
        class = "compact stripe hover signal-table",
        options = list(pageLength = 5, autoWidth = FALSE, dom = "tip")
      )
    })

    output$draft_history_ui <- renderUI({
      drafts <- draft_history_data()

      if (is.null(drafts)) {
        return(empty_state_ui("Select a prospect to view draft history."))
      }

      if (nrow(drafts) == 0) {
        return(empty_state_ui("No drafts saved yet."))
      }

      DTOutput(session$ns("draft_history_table"))
    })

    output$draft_history_table <- renderDT({
      drafts <- draft_history_data()
      req(!is.null(drafts), nrow(drafts) > 0)

      datatable(
        drafts[, c("created_at", "status", "sequence_stage", "subject")],
        rownames = FALSE,
        class = "compact stripe hover signal-table",
        options = list(pageLength = 5, autoWidth = FALSE, dom = "tip")
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
        strong("Organization research saved."),
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

      if (isTRUE(generating_draft())) {
        return()
      }

      generating_draft(TRUE)
      on.exit(generating_draft(FALSE), add = TRUE)

      draft <- withProgress(
        message = "Generating email draft...",
        value = 0.4,
        generate_queue_draft(prospect)
      )

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

    apply_cached_research_to_selection <- function(prospect) {
      cached <- get_company_research(prospect$company)

      if (is.null(cached)) {
        return(FALSE)
      }

      affected <- update_company_research(
        company = prospect$company,
        research_notes = cached$research_notes,
        research_sources = cached$research_sources,
        researched_at = cached$researched_at %||% Sys.time()
      )

      refreshed <- get_prospect_by_id(prospect$id)
      selected_prospect(refreshed)
      latest_research(cached$research_notes)
      refresh_counter(refresh_counter() + 1)

      showNotification(
        paste(
          "Saved organization research applied to",
          pluralize_count(affected, "prospect.", "prospects.")
        ),
        type = "message"
      )

      TRUE
    }

    run_research_for_selection <- function(force_refresh = FALSE) {
      prospect <- selected_prospect()
      req(prospect)

      if (isTRUE(researching_prospect())) {
        return()
      }

      if (!isTRUE(force_refresh) && apply_cached_research_to_selection(prospect)) {
        return()
      }

      researching_prospect(TRUE)
      on.exit(researching_prospect(FALSE), add = TRUE)
      on.exit(
        session$sendCustomMessage(
          "research-progress-state",
          list(active = FALSE)
        ),
        add = TRUE
      )

      session$sendCustomMessage(
        "research-progress-state",
        list(active = TRUE, stage = "Preparing fast research...")
      )

      research <- withProgress(
        message = "Researching organization...",
        value = 0,
        {
          incProgress(0.15, detail = "Preparing prospect context")
          session$sendCustomMessage(
            "research-progress-state",
            list(active = TRUE, stage = "Searching public signals...")
          )

          result <- research_prospect_with_claude_safe(prospect)

          incProgress(0.65, detail = "Saving to matching prospects")
          session$sendCustomMessage(
            "research-progress-state",
            list(active = TRUE, stage = "Saving organization research...")
          )

          result
        }
      )
      research_notes <- research$formatted_notes %||% research$summary
      research_sources <- collapse_research_sources(research$sources)
      researched_at <- Sys.time()
      affected <- update_company_research(
        company = prospect$company,
        research_notes = research_notes,
        research_sources = research_sources,
        researched_at = researched_at
      )

      if (affected == 0) {
        update_prospect_research(
          prospect_id = prospect$id,
          research_notes = research_notes,
          research_sources = research_sources,
          researched_at = researched_at
        )
        affected <- 1
      }

      refreshed <- get_prospect_by_id(prospect$id)
      selected_prospect(refreshed)
      latest_research(research_notes)
      refresh_counter(refresh_counter() + 1)

      showNotification(
        paste(
          "Research saved to",
          pluralize_count(affected, "prospect from this organization.", "prospects from this organization.")
        ),
        type = "message"
      )
    }

    observeEvent(input$research_prospect, {
      run_research_for_selection(force_refresh = FALSE)
    })

    observeEvent(input$refresh_research, {
      run_research_for_selection(force_refresh = TRUE)
    })

    observeEvent(input$use_cached_research, {
      prospect <- selected_prospect()
      req(prospect)

      if (!apply_cached_research_to_selection(prospect)) {
        showNotification("No saved organization research found.", type = "warning")
      }
    })

    observeEvent(input$generate_local_draft, {
      prospect <- selected_prospect()
      req(prospect)

      if (isTRUE(generating_local_draft())) {
        return()
      }

      generating_local_draft(TRUE)
      on.exit(generating_local_draft(FALSE), add = TRUE)

      draft <- withProgress(
        message = "Creating local draft...",
        value = 0.4,
        generate_queue_local_draft(prospect)
      )

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

    observeEvent(input$prep_call, {
      prospect <- selected_prospect()
      req(prospect)

      if (isTRUE(prepping_call())) {
        return()
      }

      prepping_call(TRUE)
      on.exit(prepping_call(FALSE), add = TRUE)

      call_prep <- withProgress(
        message = "Preparing call talking points...",
        value = 0,
        {
          incProgress(0.35, detail = "Generating talking points")
          prep <- generate_queue_call_prep(prospect)

          incProgress(0.45, detail = "Saving call prep")
          create_draft(
            prospect_id = prospect$id,
            subject = prep$subject,
            body = prep$body,
            sequence_stage = prospect$sequence_stage
          )

          prep
        }
      )

      latest_call_prep <- get_latest_draft_for_prospect(prospect$id)

      if (!is.null(latest_call_prep)) {
        latest_call_prep_id(latest_call_prep$id)
      }

      updateTextAreaInput(session, "call_prep_body", value = call_prep$body)
      history_counter(history_counter() + 1)

      showNotification("Call prep generated and saved.", type = "message")
    })

    observeEvent(input$copy_call_prep, {
      call_prep <- input$call_prep_body %||% ""

      if (call_prep == "") {
        showNotification("No call prep to copy.", type = "warning")
        return()
      }

      session$sendCustomMessage(
        "copy-draft-to-clipboard",
        list(text = call_prep)
      )

      showNotification("Call prep copied to clipboard.", type = "message")
    })

    log_call_for_selected_prospect <- function() {
      prospect <- selected_prospect()
      req(prospect)

      outcome <- input$call_outcome %||% DEFAULT_CALL_OUTCOME
      call_prep <- empty_to_na(input$call_prep_body)
      call_notes <- empty_to_na(input$call_notes)
      next_touch <- input$call_next_touch

      if (is.null(next_touch) || length(next_touch) == 0 || is.na(next_touch)) {
        next_touch <- Sys.Date() + DEFAULT_CALL_BACK_DAYS
      }

      next_touch <- as.character(next_touch)
      call_subject <- paste("Call:", outcome)
      call_body <- build_call_touch_body(call_prep, call_notes)

      if (is.null(latest_call_prep_id()) && !is.na(call_prep)) {
        create_draft(
          prospect_id = prospect$id,
          subject = call_prep_subject(prospect),
          body = call_prep,
          sequence_stage = prospect$sequence_stage
        )

        latest_call_prep <- get_latest_draft_for_prospect(prospect$id)

        if (!is.null(latest_call_prep)) {
          latest_call_prep_id(latest_call_prep$id)
        }
      } else if (!is.null(latest_call_prep_id()) && !is.na(call_prep)) {
        update_draft(
          draft_id = latest_call_prep_id(),
          subject = call_prep_subject(prospect),
          body = call_prep,
          status = DEFAULT_DRAFT_STATUS
        )
      }

      log_touch(
        prospect_id = prospect$id,
        touch_type = if (outcome == "Voicemail") "Voicemail" else "Call",
        subject = call_subject,
        body = call_body,
        outcome = outcome,
        sequence_stage = prospect$sequence_stage,
        advance_sequence = FALSE,
        next_touch = next_touch
      )

      refreshed <- get_prospect_by_id(prospect$id)
      history_counter(history_counter() + 1)
      refresh_counter(refresh_counter() + 1)

      if (is.null(refreshed) || is_terminal_status(refreshed$status)) {
        selected_prospect(NULL)
        latest_call_prep_id(NULL)
        updateTextAreaInput(session, "call_prep_body", value = "")
      } else {
        selected_prospect(refreshed)
      }

      updateTextAreaInput(session, "call_notes", value = "")
      updateDateInput(session, "call_next_touch", value = Sys.Date() + DEFAULT_CALL_BACK_DAYS)

      showNotification("Call logged without advancing the email sequence.", type = "message")
    }

    observeEvent(input$log_call, {
      log_call_for_selected_prospect()
    })

    observeEvent(input$quick_log_call, {
      log_call_for_selected_prospect()
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
      latest_call_prep_id(NULL)
      history_counter(history_counter() + 1)

      updateTextInput(session, "draft_subject", value = "")
      updateTextAreaInput(session, "draft_body", value = "")
      updateTextAreaInput(session, "call_prep_body", value = "")
      updateSelectInput(session, "call_outcome", selected = DEFAULT_CALL_OUTCOME)
      updateDateInput(session, "call_next_touch", value = Sys.Date() + DEFAULT_CALL_BACK_DAYS)
      updateTextAreaInput(session, "call_notes", value = "")

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
      latest_call_prep_id(NULL)
      latest_research(NULL)
      refresh_counter(refresh_counter() + 1)
      history_counter(history_counter() + 1)

      updateTextInput(session, "draft_subject", value = "")
      updateTextAreaInput(session, "draft_body", value = "")
      updateTextAreaInput(session, "call_prep_body", value = "")
      updateSelectInput(session, "call_outcome", selected = DEFAULT_CALL_OUTCOME)
      updateDateInput(session, "call_next_touch", value = Sys.Date() + DEFAULT_CALL_BACK_DAYS)
      updateTextAreaInput(session, "call_notes", value = "")
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

generate_queue_call_prep <- function(prospect) {
  generate_call_prep_safe(prospect)
}

call_prep_subject <- function(prospect) {
  paste("Call prep:", display_value(prospect$company, format_person_name(prospect)))
}

build_call_touch_body <- function(call_prep, call_notes) {
  parts <- character(0)

  if (!is.na(call_prep)) {
    parts <- c(parts, "Call Prep:", call_prep)
  }

  if (!is.na(call_notes)) {
    parts <- c(parts, "Call Notes:", call_notes)
  }

  if (length(parts) == 0) {
    return(NA_character_)
  }

  paste(parts, collapse = "\n\n")
}

has_company_research <- function(prospect) {
  if (is.null(prospect)) {
    return(FALSE)
  }

  !is.null(get_company_research(prospect$company))
}

collapse_research_sources <- function(sources) {
  if (is.null(sources) || length(sources) == 0) {
    return("")
  }

  sources <- unlist(sources, use.names = FALSE)
  sources <- trimws(as.character(sources))
  sources <- sources[!is.na(sources) & sources != ""]

  paste(sources, collapse = "\n")
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

queue_count_ui <- function(label, value, input_id = NULL, active = FALSE) {
  class <- paste(
    "queue-count",
    if (!is.null(input_id)) "queue-count-button" else "",
    if (isTRUE(active)) "active" else ""
  )

  content <- tagList(
    tags$strong(value),
    tags$span(label)
  )

  if (is.null(input_id)) {
    return(tags$div(class = class, content))
  }

  actionLink(
    inputId = input_id,
    label = content,
    class = class
  )
}

today_focus_item_ui <- function(label, value) {
  tags$div(
    class = "today-focus-item",
    tags$span(label),
    tags$strong(value)
  )
}

queue_action_button <- function(ns, input_id, label, class = NULL, enabled = TRUE) {
  args <- list(
    inputId = ns(input_id),
    label = label
  )

  if (!is.null(class)) {
    args$class <- class
  }

  if (!isTRUE(enabled)) {
    args$disabled <- "disabled"
  }

  do.call(actionButton, args)
}

queue_empty_state_ui <- function(scope, segment = "All", source = "All") {
  prospects <- get_prospects(include_inactive = TRUE)

  if (nrow(prospects) == 0) {
    return(empty_state_ui("No prospects have been added yet."))
  }

  active <- prospects[!is_terminal_status(prospects$status), ]

  if (!is.null(segment) && segment != "All") {
    active <- active[!is.na(active$segment) & active$segment == segment, ]
  }

  if (!is.null(source) && source != "All") {
    active <- active[!is.na(active$source) & active$source == source, ]
  }

  if (nrow(active) == 0) {
    return(empty_state_ui("No active prospects match these filters."))
  }

  next_touch <- suppressWarnings(as.Date(active$next_touch))
  today <- Sys.Date()
  due_or_overdue <- active[is.na(next_touch) | next_touch <= today, ]
  future <- active[!is.na(next_touch) & next_touch > today, ]

  if (scope == "Due or Overdue" && nrow(due_or_overdue) == 0 && nrow(future) > 0) {
    return(empty_state_ui(tagList(
      tags$strong("No due prospects."),
      tags$span(paste(
        pluralize_count(nrow(future), "active prospect is", "active prospects are"),
        "scheduled for later. Click Active above or change Queue View to All Active."
      ))
    )))
  }

  if (scope == "Due Today") {
    return(empty_state_ui("No prospects are due today."))
  }

  if (scope == "Overdue") {
    return(empty_state_ui("No prospects are overdue."))
  }

  if (scope == "Nurture") {
    return(empty_state_ui("No nurture prospects match these filters."))
  }

  empty_state_ui("No prospects match the current queue filters.")
}

pluralize_count <- function(value, singular, plural) {
  if (value == 1) {
    return(paste(value, singular))
  }

  paste(value, plural)
}

select_queue_prospect <- function(
    prospect_id,
    session,
    selected_prospect,
    latest_draft_id,
    latest_call_prep_id,
    latest_research,
    history_counter
) {
  prospect <- get_prospect_by_id(prospect_id)

  if (is.null(prospect)) {
    return(NULL)
  }

  selected_prospect(prospect)
  latest_draft_id(NULL)
  latest_call_prep_id(NULL)
  latest_research(NULL)
  history_counter(history_counter() + 1)

  updateTextInput(session, "draft_subject", value = "")
  updateTextAreaInput(session, "draft_body", value = "")
  updateTextAreaInput(session, "call_prep_body", value = "")
  updateSelectInput(session, "call_outcome", selected = DEFAULT_CALL_OUTCOME)
  updateDateInput(session, "call_next_touch", value = Sys.Date() + DEFAULT_CALL_BACK_DAYS)
  updateTextAreaInput(session, "call_notes", value = "")

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
            column(
              6,
              div(
                class = "modal-form-section",
                h4("Contact"),
                fluidRow(
                  column(6, textInput(ns("modal_first_name"), "First Name", value = display_value(prospect$first_name, ""))),
                  column(6, textInput(ns("modal_last_name"), "Last Name", value = display_value(prospect$last_name, "")))
                ),
                textInput(ns("modal_email"), "Email", value = display_value(prospect$email, "")),
                textInput(ns("modal_linkedin_url"), "LinkedIn URL", value = display_value(prospect$linkedin_url, ""))
              )
            ),
            column(
              6,
              div(
                class = "modal-form-section",
                h4("Company"),
                textInput(ns("modal_company"), "Company", value = display_value(prospect$company, "")),
                textInput(ns("modal_title"), "Title", value = display_value(prospect$title, "")),
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
