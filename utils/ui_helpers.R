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
