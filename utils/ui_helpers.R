first_scalar <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }

  value <- x[1]

  if (is.null(value) || is.na(value)) {
    return(NA_character_)
  }

  as.character(value)
}

display_value <- function(x, fallback = "Not set") {
  value <- first_scalar(x)

  if (is.na(value) || trimws(value) == "") {
    return(fallback)
  }

  value
}

format_person_name <- function(prospect) {
  name <- trimws(paste(
    display_value(prospect$first_name, ""),
    display_value(prospect$last_name, "")
  ))

  if (name == "" && !is.null(prospect$name)) {
    name <- display_value(prospect$name, "")
  }

  if (name == "") {
    return(display_value(prospect$company, "Unnamed prospect"))
  }

  name
}

status_badge_ui <- function(status) {
  status <- display_value(status)
  status_class <- gsub("[^a-z0-9]+", "-", tolower(status))
  status_class <- gsub("(^-|-$)", "", status_class)

  tags$span(
    class = paste("status-badge", paste0("status-", status_class)),
    status
  )
}

detail_item_ui <- function(label, value) {
  tags$div(
    class = "detail-item",
    tags$span(class = "detail-label", label),
    tags$span(class = "detail-value", display_value(value))
  )
}

note_block_ui <- function(label, value) {
  value <- display_value(value, "")

  if (value == "") {
    return(NULL)
  }

  tags$div(
    class = "note-block",
    tags$div(class = "note-block-title", label),
    tags$pre(class = "note-block-body", value)
  )
}

empty_state_ui <- function(message) {
  tags$div(
    class = "empty-state",
    message
  )
}

format_location_value <- function(city, state) {
  values <- c(display_value(city, ""), display_value(state, ""))
  values <- values[values != ""]

  if (length(values) == 0) {
    return("Not set")
  }

  paste(values, collapse = ", ")
}

format_queue_table_data <- function(prospects) {
  if (nrow(prospects) == 0) {
    return(data.frame(
      ID = integer(0),
      Prospect = character(0),
      Company = character(0),
      Status = character(0),
      Stage = character(0),
      `Next Touch` = character(0),
      Segment = character(0),
      check.names = FALSE
    ))
  }

  data.frame(
    ID = prospects$id,
    Prospect = vapply(seq_len(nrow(prospects)), function(i) {
      format_person_name(prospects[i, ])
    }, character(1)),
    Company = vapply(prospects$company, display_value, character(1)),
    Status = vapply(prospects$status, display_value, character(1)),
    Stage = vapply(prospects$sequence_stage, format_sequence_stage, character(1)),
    `Next Touch` = vapply(prospects$next_touch, display_value, character(1)),
    Segment = vapply(prospects$segment, display_value, character(1)),
    check.names = FALSE
  )
}

format_prospects_table_data <- function(prospects) {
  if (nrow(prospects) == 0) {
    return(data.frame(
      ID = integer(0),
      Prospect = character(0),
      Company = character(0),
      Title = character(0),
      Email = character(0),
      Status = character(0),
      Stage = character(0),
      `Next Touch` = character(0),
      Segment = character(0),
      Source = character(0),
      check.names = FALSE
    ))
  }

  data.frame(
    ID = prospects$id,
    Prospect = vapply(seq_len(nrow(prospects)), function(i) {
      format_person_name(prospects[i, ])
    }, character(1)),
    Company = vapply(prospects$company, display_value, character(1)),
    Title = vapply(prospects$title, display_value, character(1)),
    Email = vapply(prospects$email, display_value, character(1)),
    Status = vapply(prospects$status, display_value, character(1)),
    Stage = vapply(prospects$sequence_stage, format_sequence_stage, character(1)),
    `Next Touch` = vapply(prospects$next_touch, display_value, character(1)),
    Segment = vapply(prospects$segment, display_value, character(1)),
    Source = vapply(prospects$source, display_value, character(1)),
    check.names = FALSE
  )
}

format_import_preview_table_data <- function(preview) {
  if (nrow(preview) == 0) {
    return(data.frame(Message = "Upload a file and click Preview Import."))
  }

  data.frame(
    `Import Status` = vapply(preview$import_status, display_value, character(1)),
    Prospect = trimws(paste(preview$first_name, preview$last_name)),
    Company = vapply(preview$company, display_value, character(1)),
    Email = vapply(preview$email, display_value, character(1)),
    Source = vapply(preview$source, display_value, character(1)),
    Segment = vapply(preview$segment, display_value, character(1)),
    `Duplicate Reason` = vapply(preview$duplicate_reason, display_value, character(1)),
    `Validation Issue` = vapply(preview$validation_issue, display_value, character(1)),
    check.names = FALSE
  )
}

research_notes_ui <- function(research_notes, research_sources = NULL) {
  research <- parse_research_notes_for_display(research_notes, research_sources)

  if (!research$has_research) {
    return(empty_state_ui("No research saved for this prospect yet."))
  }

  tagList(
    note_block_ui("Summary", research$summary),
    list_block_ui("Signals", research$signals),
    note_block_ui("Reason for Outreach", research$reason),
    note_block_ui("Personalization Notes", research$personalization),
    list_block_ui("Sources", research$sources)
  )
}

list_block_ui <- function(label, values) {
  values <- normalize_research_values(values)

  if (length(values) == 0) {
    return(NULL)
  }

  tags$div(
    class = "note-block",
    tags$div(class = "note-block-title", label),
    tags$ul(
      class = "research-list",
      lapply(values, tags$li)
    )
  )
}

parse_research_notes_for_display <- function(research_notes, research_sources = NULL) {
  notes <- display_value(research_notes, "")
  sources <- normalize_research_values(research_sources)

  if (notes == "" && length(sources) == 0) {
    return(list(
      has_research = FALSE,
      summary = "",
      signals = character(0),
      reason = "",
      personalization = "",
      sources = character(0)
    ))
  }

  parsed <- parse_research_json(notes)

  if (!is.null(parsed)) {
    parsed_sources <- normalize_research_values(parsed$sources)

    return(list(
      has_research = TRUE,
      summary = display_value(parsed$summary, ""),
      signals = normalize_research_values(parsed$signals),
      reason = display_value(parsed$suggested_reason_for_outreach, ""),
      personalization = display_value(parsed$suggested_personalization_notes, ""),
      sources = unique(c(parsed_sources, sources))
    ))
  }

  sections <- parse_research_sections(notes)

  if (length(sections) > 0) {
    return(list(
      has_research = TRUE,
      summary = display_value(sections$summary, ""),
      signals = normalize_research_values(sections$signals),
      reason = display_value(sections$reason, ""),
      personalization = display_value(sections$personalization, ""),
      sources = unique(c(normalize_research_values(sections$sources), sources))
    ))
  }

  list(
    has_research = TRUE,
    summary = notes,
    signals = character(0),
    reason = "",
    personalization = "",
    sources = sources
  )
}

parse_research_json <- function(notes) {
  if (notes == "") {
    return(NULL)
  }

  candidate <- trimws(notes)
  candidate <- sub("^```json\\s*", "", candidate)
  candidate <- sub("^```\\s*", "", candidate)
  candidate <- sub("\\s*```$", "", candidate)

  if (!grepl("^\\{", candidate)) {
    start <- regexpr("\\{", candidate)
    ends <- gregexpr("\\}", candidate)[[1]]

    if (start[1] > 0 && length(ends) > 0 && max(ends) > start[1]) {
      candidate <- substr(candidate, start[1], max(ends))
    }
  }

  tryCatch(
    jsonlite::fromJSON(candidate),
    error = function(e) NULL
  )
}

parse_research_sections <- function(notes) {
  lines <- strsplit(notes, "\n", fixed = TRUE)[[1]]
  known <- c(
    "research summary" = "summary",
    "summary" = "summary",
    "signals" = "signals",
    "suggested reason for outreach" = "reason",
    "reason for outreach" = "reason",
    "suggested personalization notes" = "personalization",
    "personalization notes" = "personalization",
    "sources" = "sources"
  )

  sections <- list()
  current <- NULL

  for (line in lines) {
    clean <- trimws(gsub(":$", "", line))
    key <- known[[tolower(clean)]]

    if (!is.null(key)) {
      current <- key
      if (is.null(sections[[current]])) {
        sections[[current]] <- character(0)
      }
      next
    }

    if (!is.null(current)) {
      sections[[current]] <- c(sections[[current]], line)
    }
  }

  lapply(sections, function(value) {
    trimws(paste(value, collapse = "\n"))
  })
}

normalize_research_values <- function(values) {
  if (is.null(values) || length(values) == 0) {
    return(character(0))
  }

  values <- unlist(values, use.names = FALSE)

  if (length(values) == 0) {
    return(character(0))
  }

  values <- as.character(values)
  values <- unlist(strsplit(values, "\n", fixed = TRUE), use.names = FALSE)
  values <- trimws(gsub("^-\\s*", "", values))
  values <- values[!is.na(values) & values != "" & values != "No sources returned."]
  values <- values[values != "No specific public signals found."]

  unique(values)
}
