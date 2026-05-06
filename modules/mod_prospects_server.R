# modules/mod_prospects_server.R

mod_prospects_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    refresh_counter <- reactiveVal(0)
    selected_prospect <- reactiveVal(NULL)
    import_preview_data <- reactiveVal(data.frame())
    latest_import_summary <- reactiveVal(NULL)
    latest_export_dir <- reactiveVal(NULL)

    # ---- Main prospects table ----------------------------------------------

    prospects_data <- reactive({
      refresh_counter()

      prospects <- get_prospects(include_inactive = TRUE)

      if (nrow(prospects) == 0) {
        return(prospects)
      }

      if (!is.null(input$status_filter) && input$status_filter != "All") {
        prospects <- prospects[prospects$status == input$status_filter, ]
      }

      if (!is.null(input$segment_filter) && input$segment_filter != "All" && input$segment_filter != "") {
        prospects <- prospects[prospects$segment == input$segment_filter, ]
      }

      prospects
    })

    prospects_table_data <- reactive({
      format_prospects_table_data(prospects_data())
    })

    output$prospects_table <- renderDT({
      datatable(
        prospects_table_data(),
        rownames = FALSE,
        selection = "single",
        class = "compact stripe hover signal-table",
        options = list(
          pageLength = 9,
          autoWidth = FALSE,
          dom = "tip",
          columnDefs = list(
            list(visible = FALSE, targets = 0)
          )
        )
      )
    })

    observeEvent(input$refresh_prospects, {
      refresh_counter(refresh_counter() + 1)
    })

    observeEvent(input$export_data, {
      export_dir <- export_signal_data()
      latest_export_dir(export_dir)

      showNotification("Data export complete.", type = "message")
    })

    output$export_summary <- renderUI({
      export_dir <- latest_export_dir()

      if (is.null(export_dir)) {
        return(NULL)
      }

      tags$p(
        class = "helper-text",
        paste("Latest export:", export_dir)
      )
    })


    # ---- Import workflow ----------------------------------------------------

    observeEvent(input$preview_import, {
      req(input$prospect_file)

      preview <- build_import_preview(
        file_path = input$prospect_file$datapath,
        file_name = input$prospect_file$name,
        default_source = input$default_source,
        default_segment = input$default_segment
      )

      import_preview_data(preview)
      latest_import_summary(NULL)

      showNotification(
        paste("Preview loaded:", nrow(preview), "rows."),
        type = "message"
      )
    })

    output$import_summary <- renderUI({
      preview <- import_preview_data()
      latest_summary <- latest_import_summary()

      if (!is.null(latest_summary)) {
        return(tags$div(
          class = "helper-text",
          strong("Last import: "),
          latest_summary
        ))
      }

      if (nrow(preview) == 0) {
        return(NULL)
      }

      counts <- table(factor(
        preview$import_status,
        levels = c("Ready", "Duplicate", "Invalid")
      ))

      tags$div(
        class = "helper-text",
        strong("Preview summary: "),
        paste0(
          counts[["Ready"]], " ready, ",
          counts[["Duplicate"]], " duplicate, ",
          counts[["Invalid"]], " invalid"
        )
      )
    })

    output$import_preview_table <- renderDT({
      preview <- import_preview_data()
      preview_table <- format_import_preview_table_data(preview)

      if (nrow(preview) == 0) {
        return(datatable(
          preview_table,
          rownames = FALSE
        ))
      }

      datatable(
        preview_table,
        rownames = FALSE,
        class = "compact stripe hover signal-table",
        options = list(
          pageLength = 8,
          autoWidth = FALSE,
          dom = "tip"
        )
      ) |>
        DT::formatStyle(
          "Import Status",
          target = "row",
          backgroundColor = DT::styleEqual(
            c("Ready", "Duplicate", "Invalid"),
            c("#eefaf0", "#fff7e6", "#fdecec")
          )
        )
    })

    observeEvent(input$confirm_import, {
      preview <- import_preview_data()

      if (nrow(preview) == 0) {
        showNotification("Nothing to import. Preview a file first.", type = "error")
        return()
      }

      valid <- preview[preview$import_status %in% c("Ready", "Duplicate"), ]
      duplicate_count <- sum(preview$import_status == "Duplicate", na.rm = TRUE)
      invalid_count <- sum(preview$import_status == "Invalid", na.rm = TRUE)
      skipped_duplicate_count <- 0
      imported_duplicate_count <- 0

      importable <- valid

      if (isTRUE(input$skip_duplicates)) {
        importable <- importable[!importable$is_duplicate, ]
        skipped_duplicate_count <- duplicate_count
      } else {
        imported_duplicate_count <- sum(importable$is_duplicate, na.rm = TRUE)
      }

      if (nrow(importable) == 0) {
        latest_import_summary(paste0(
          "0 added, ",
          skipped_duplicate_count, " duplicates skipped, ",
          imported_duplicate_count, " duplicates imported, ",
          invalid_count, " invalid."
        ))
        showNotification(latest_import_summary(), type = "warning")
        return()
      }

      for (i in seq_len(nrow(importable))) {
        row <- importable[i, ]

        create_prospect(list(
          first_name = empty_to_na(row$first_name),
          last_name = empty_to_na(row$last_name),
          company = empty_to_na(row$company),
          title = empty_to_na(row$title),
          email = empty_to_na(row$email),
          linkedin_url = empty_to_na(row$linkedin_url),
          website = empty_to_na(row$website),
          city = empty_to_na(row$city),
          state = empty_to_na(row$state),
          source = empty_to_na(row$source),
          segment = empty_to_na(row$segment),
          reason_for_outreach = empty_to_na(row$reason_for_outreach),
          personalization_notes = empty_to_na(row$personalization_notes),
          status = DEFAULT_PROSPECT_STATUS,
          sequence_stage = DEFAULT_SEQUENCE_STAGE,
          next_touch = as.character(Sys.Date()),
          reply_notes = NA_character_
        ))
      }

      latest_import_summary(
        paste0(
          nrow(importable), " added, ",
          skipped_duplicate_count, " duplicates skipped, ",
          imported_duplicate_count, " duplicates imported, ",
          invalid_count, " invalid."
        )
      )

      showNotification(latest_import_summary(), type = "message")

      import_preview_data(data.frame())
      refresh_counter(refresh_counter() + 1)
    })


    # ---- Manual add workflow ------------------------------------------------

    observeEvent(input$add_prospect, {
      req(input$first_name, input$last_name, input$company)

      manual_candidate <- data.frame(
        first_name = input$first_name,
        last_name = input$last_name,
        company = input$company,
        email = input$email,
        linkedin_url = input$linkedin_url,
        stringsAsFactors = FALSE
      )

      duplicate_check <- flag_duplicate_prospects(manual_candidate)

      if (nrow(duplicate_check) > 0 &&
          isTRUE(duplicate_check$is_duplicate[1]) &&
          !isTRUE(input$allow_manual_duplicate)) {
        showNotification(
          paste("Possible duplicate found:", duplicate_check$duplicate_reason[1]),
          type = "warning",
          duration = 8
        )
        return()
      }

      create_prospect(list(
        first_name = empty_to_na(input$first_name),
        last_name = empty_to_na(input$last_name),
        company = empty_to_na(input$company),
        title = empty_to_na(input$title),
        email = empty_to_na(input$email),
        linkedin_url = empty_to_na(input$linkedin_url),
        website = empty_to_na(input$website),
        city = empty_to_na(input$city),
        state = empty_to_na(input$state),
        source = empty_to_na(input$source),
        segment = empty_to_na(input$segment),
        reason_for_outreach = empty_to_na(input$reason_for_outreach),
        personalization_notes = empty_to_na(input$personalization_notes),
        status = input$status,
        sequence_stage = DEFAULT_SEQUENCE_STAGE,
        next_touch = as.character(input$next_touch),
        reply_notes = NA_character_
      ))

      if (nrow(duplicate_check) > 0 && isTRUE(duplicate_check$is_duplicate[1])) {
        showNotification("Prospect added despite duplicate warning.", type = "warning")
      } else {
        showNotification("Prospect added.", type = "message")
      }

      clear_manual_prospect_form(session)

      refresh_counter(refresh_counter() + 1)
    })


    # ---- Selected prospect workflow ----------------------------------------

    observeEvent(input$prospects_table_rows_selected, {
      selected_row <- input$prospects_table_rows_selected
      req(selected_row)

      row <- prospects_table_data()[selected_row, ]
      prospect <- get_prospect_by_id(row$ID)

      selected_prospect(prospect)

      updateTextInput(session, "selected_first_name", value = prospect$first_name %||% "")
      updateTextInput(session, "selected_last_name", value = prospect$last_name %||% "")
      updateTextInput(session, "selected_company", value = prospect$company %||% "")
      updateTextInput(session, "selected_title", value = prospect$title %||% "")
      updateTextInput(session, "selected_email", value = prospect$email %||% "")
      updateTextInput(session, "selected_linkedin_url", value = prospect$linkedin_url %||% "")
      updateTextInput(session, "selected_website", value = prospect$website %||% "")
      updateTextInput(session, "selected_city", value = prospect$city %||% "")
      updateTextInput(session, "selected_state", value = prospect$state %||% "")
      updateSelectInput(session, "selected_source", selected = prospect$source %||% "")
      updateSelectInput(session, "selected_segment", selected = prospect$segment %||% "")
      updateTextAreaInput(
        session,
        "selected_reason_for_outreach",
        value = prospect$reason_for_outreach %||% ""
      )
      updateTextAreaInput(
        session,
        "selected_personalization_notes",
        value = prospect$personalization_notes %||% ""
      )
      updateSelectInput(session, "selected_status", selected = prospect$status)
      updateSelectInput(
        session,
        "selected_sequence_stage",
        selected = as.character(prospect$sequence_stage)
      )

      next_touch_value <- if (
        is.null(prospect$next_touch) ||
        is.na(prospect$next_touch) ||
        prospect$next_touch == ""
      ) {
        Sys.Date()
      } else {
        as.Date(prospect$next_touch)
      }

      updateDateInput(session, "selected_next_touch", value = next_touch_value)
      updateTextAreaInput(session, "reply_notes", value = prospect$reply_notes %||% "")
    })

    output$selected_status_badge <- renderUI({
      prospect <- selected_prospect()

      if (is.null(prospect)) {
        return(NULL)
      }

      status_badge_ui(prospect$status)
    })

    output$selected_prospect_summary <- renderUI({
      prospect <- selected_prospect()

      if (is.null(prospect)) {
        return(empty_state_ui("Select a prospect from the table."))
      }

      location <- trimws(paste(
        display_value(prospect$city, ""),
        display_value(prospect$state, "")
      ))

      parsed_research <- parse_research_notes_for_display(
        prospect$research_notes,
        prospect$research_sources
      )

      research_ui <- if (isTRUE(parsed_research$has_research)) {
        tags$details(
          class = "details-panel",
          tags$summary("Research"),
          research_notes_ui(prospect$research_notes, prospect$research_sources)
        )
      } else {
        NULL
      }

      tagList(
        div(
          class = "prospect-heading",
          h4(format_person_name(prospect)),
          div(
            class = "muted-text",
            paste(
              display_value(prospect$title, "No title"),
              display_value(prospect$company, "No company"),
              sep = " at "
            )
          )
        ),
        div(
          class = "detail-grid",
          detail_item_ui("Email", prospect$email),
          detail_item_ui("LinkedIn", prospect$linkedin_url),
          detail_item_ui("Website", prospect$website),
          detail_item_ui("Location", location),
          detail_item_ui("Source", prospect$source),
          detail_item_ui("Segment", prospect$segment),
          detail_item_ui("Stage", format_sequence_stage(prospect$sequence_stage)),
          detail_item_ui("Last Touch", prospect$last_touch),
          detail_item_ui("Next Touch", prospect$next_touch)
        ),
        note_block_ui("Reason", prospect$reason_for_outreach),
        note_block_ui("Personalization", prospect$personalization_notes),
        research_ui,
        note_block_ui("Reply / Outcome", prospect$reply_notes)
      )
    })

    observeEvent(input$update_status, {
      prospect <- selected_prospect()
      req(prospect)

      update_prospect(
        prospect_id = prospect$id,
        prospect = list(
          first_name = empty_to_na(input$selected_first_name),
          last_name = empty_to_na(input$selected_last_name),
          company = empty_to_na(input$selected_company),
          title = empty_to_na(input$selected_title),
          email = empty_to_na(input$selected_email),
          linkedin_url = empty_to_na(input$selected_linkedin_url),
          website = empty_to_na(input$selected_website),
          city = empty_to_na(input$selected_city),
          state = empty_to_na(input$selected_state),
          source = empty_to_na(input$selected_source),
          segment = empty_to_na(input$selected_segment),
          reason_for_outreach = empty_to_na(input$selected_reason_for_outreach),
          personalization_notes = empty_to_na(input$selected_personalization_notes),
          research_notes = prospect$research_notes,
          research_sources = prospect$research_sources,
          researched_at = prospect$researched_at,
          status = input$selected_status,
          sequence_stage = input$selected_sequence_stage,
          next_touch = as.character(input$selected_next_touch),
          reply_notes = empty_to_na(input$reply_notes)
        )
      )

      showNotification("Prospect updated.", type = "message")

      selected_prospect(get_prospect_by_id(prospect$id))
      refresh_counter(refresh_counter() + 1)
    })

    observeEvent(input$delete_prospect, {
      prospect <- selected_prospect()
      req(prospect)

      delete_prospect(prospect$id)

      showNotification("Prospect deleted.", type = "warning")

      selected_prospect(NULL)
      updateTextAreaInput(session, "reply_notes", value = "")
      refresh_counter(refresh_counter() + 1)
    })
  })
}


# ---- Import helpers ---------------------------------------------------------

build_import_preview <- function(file_path, file_name, default_source = "", default_segment = "") {
  raw <- read_prospect_upload(file_path, file_name)

  cleaned <- normalize_import_columns(raw)

  cleaned <- apply_import_defaults(
    cleaned,
    default_source = default_source,
    default_segment = default_segment
  )

  cleaned <- validate_import_rows(cleaned)
  cleaned <- flag_duplicate_prospects(cleaned)

  cleaned
}


read_prospect_upload <- function(file_path, file_name) {
  extension <- tolower(tools::file_ext(file_name))

  if (extension %in% c("xlsx", "xls")) {
    return(as.data.frame(readxl::read_excel(file_path), stringsAsFactors = FALSE))
  }

  if (extension == "csv") {
    return(as.data.frame(readr::read_csv(file_path, show_col_types = FALSE), stringsAsFactors = FALSE))
  }

  stop("Unsupported file type. Please upload .xlsx, .xls, or .csv.", call. = FALSE)
}


normalize_import_columns <- function(df) {
  df <- janitor::clean_names(df)

  expected_cols <- c(
    "first_name",
    "last_name",
    "company",
    "title",
    "email",
    "linkedin_url",
    "website",
    "city",
    "state",
    "source",
    "segment",
    "reason_for_outreach",
    "personalization_notes"
  )

  # Common aliases from exports / hand-built lists
  alias_map <- list(
    first_name = c("first", "firstname", "contact_first_name"),
    last_name = c("last", "lastname", "contact_last_name"),
    company = c("account", "account_name", "organization", "company_name"),
    title = c("job_title", "position"),
    email = c("email_address", "work_email"),
    linkedin_url = c("linkedin", "linkedin_profile", "profile_url"),
    website = c("company_website", "url"),
    city = c("company_city"),
    state = c("company_state", "st"),
    source = c("lead_source"),
    segment = c("facility_type", "industry", "market"),
    reason_for_outreach = c("reason", "outreach_reason", "why_reach_out"),
    personalization_notes = c("notes", "personalization", "personalization_note")
  )

  for (canonical in names(alias_map)) {
    if (!canonical %in% names(df)) {
      alias_found <- alias_map[[canonical]][alias_map[[canonical]] %in% names(df)]

      if (length(alias_found) > 0) {
        names(df)[names(df) == alias_found[1]] <- canonical
      }
    }
  }

  for (col in expected_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA_character_
    }
  }

  df <- df[, expected_cols, drop = FALSE]

  for (col in expected_cols) {
    df[[col]] <- normalize_text_field(df[[col]])
  }

  df
}


apply_import_defaults <- function(df, default_source = "", default_segment = "") {
  if (!is.null(default_source) && default_source != "") {
    df$source[is.na(df$source) | df$source == ""] <- default_source
  }

  if (!is.null(default_segment) && default_segment != "") {
    df$segment[is.na(df$segment) | df$segment == ""] <- default_segment
  }

  df$status <- DEFAULT_PROSPECT_STATUS
  df$sequence_stage <- DEFAULT_SEQUENCE_STAGE
  df$next_touch <- as.character(Sys.Date())

  df
}


validate_import_rows <- function(df) {
  has_email <- !is.na(df$email) & df$email != ""
  has_name_company <- !is.na(df$first_name) & df$first_name != "" &
    !is.na(df$last_name) & df$last_name != "" &
    !is.na(df$company) & df$company != ""

  df$is_valid <- has_email | has_name_company

  df$validation_issue <- ifelse(
    df$is_valid,
    "",
    "Missing email or first_name + last_name + company"
  )

  df$import_status <- ifelse(df$is_valid, "Ready", "Invalid")

  df
}


flag_duplicate_prospects <- function(df) {
  existing <- get_prospects(include_inactive = TRUE)

  df$is_duplicate <- FALSE
  df$duplicate_reason <- ""

  if (nrow(df) == 0 || nrow(existing) == 0) {
    return(df)
  }

  existing$email_norm <- normalize_match_value(existing$email)
  existing$linkedin_norm <- normalize_match_value(existing$linkedin_url)
  existing$name_company_norm <- build_name_company_key(
    existing$first_name,
    existing$last_name,
    existing$company
  )

  df$email_norm <- normalize_match_value(df$email)
  df$linkedin_norm <- normalize_match_value(df$linkedin_url)
  df$name_company_norm <- build_name_company_key(
    df$first_name,
    df$last_name,
    df$company
  )

  for (i in seq_len(nrow(df))) {
    duplicate_reasons <- c()

    if (!is.na(df$email_norm[i]) && df$email_norm[i] != "" &&
        df$email_norm[i] %in% existing$email_norm) {
      duplicate_reasons <- c(duplicate_reasons, "email match")
    }

    if (!is.na(df$linkedin_norm[i]) && df$linkedin_norm[i] != "" &&
        df$linkedin_norm[i] %in% existing$linkedin_norm) {
      duplicate_reasons <- c(duplicate_reasons, "LinkedIn URL match")
    }

    if (!is.na(df$name_company_norm[i]) && df$name_company_norm[i] != "" &&
        df$name_company_norm[i] %in% existing$name_company_norm) {
      duplicate_reasons <- c(duplicate_reasons, "name + company match")
    }

    if (length(duplicate_reasons) > 0) {
      df$is_duplicate[i] <- TRUE
      df$duplicate_reason[i] <- paste(duplicate_reasons, collapse = "; ")

      if (df$import_status[i] == "Ready") {
        df$import_status[i] <- "Duplicate"
      }
    }
  }

  helper_cols <- c("email_norm", "linkedin_norm", "name_company_norm")
  df[, setdiff(names(df), helper_cols), drop = FALSE]
}


normalize_text_field <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}


normalize_match_value <- function(x) {
  x <- normalize_text_field(x)
  tolower(x)
}


build_name_company_key <- function(first_name, last_name, company) {
  first_name <- normalize_match_value(first_name)
  last_name <- normalize_match_value(last_name)
  company <- normalize_match_value(company)

  key <- paste(first_name, last_name, company, sep = "|")

  key[grepl("^\\|\\|?$", key)] <- ""
  key
}


clear_manual_prospect_form <- function(session) {
  updateTextInput(session, "first_name", value = "")
  updateTextInput(session, "last_name", value = "")
  updateTextInput(session, "company", value = "")
  updateTextInput(session, "title", value = "")
  updateTextInput(session, "email", value = "")
  updateTextInput(session, "linkedin_url", value = "")
  updateTextInput(session, "website", value = "")
  updateTextInput(session, "city", value = "")
  updateTextInput(session, "state", value = "")
  updateSelectInput(session, "source", selected = "Manual")
  updateSelectInput(session, "segment", selected = "")
  updateTextAreaInput(session, "reason_for_outreach", value = "")
  updateTextAreaInput(session, "personalization_notes", value = "")
  updateSelectInput(session, "status", selected = DEFAULT_PROSPECT_STATUS)
  updateDateInput(session, "next_touch", value = Sys.Date())
  updateCheckboxInput(session, "allow_manual_duplicate", value = FALSE)
}
