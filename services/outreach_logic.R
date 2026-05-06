# services/outreach_logic.R
# Cadence and sequence logic for Signal
#
# Signal only manages the pre-reply outbound loop.
# Once a prospect replies, opts out, or is marked do-not-contact,
# they should leave the active outreach queue.

# ---- Status helpers ---------------------------------------------------------

is_terminal_status <- function(status) {
  status %in% TERMINAL_PROSPECT_STATUSES
}

is_active_status <- function(status) {
  status %in% ACTIVE_PROSPECT_STATUSES
}

normalize_status <- function(status) {
  if (is.null(status) || length(status) == 0 || is.na(status) || status == "") {
    return(DEFAULT_PROSPECT_STATUS)
  }
  
  if (!status %in% PROSPECT_STATUSES) {
    return(DEFAULT_PROSPECT_STATUS)
  }
  
  status
}


# ---- Sequence stage helpers -------------------------------------------------

normalize_sequence_stage <- function(sequence_stage) {
  stage <- suppressWarnings(as.integer(sequence_stage))
  
  if (is.na(stage)) {
    return(DEFAULT_SEQUENCE_STAGE)
  }
  
  if (!stage %in% SEQUENCE_STAGES) {
    return(DEFAULT_SEQUENCE_STAGE)
  }
  
  stage
}

get_sequence_stage_label <- function(sequence_stage) {
  stage <- normalize_sequence_stage(sequence_stage)
  unname(SEQUENCE_STAGE_LABELS[as.character(stage)])
}

get_status_for_sequence_stage <- function(sequence_stage) {
  stage <- normalize_sequence_stage(sequence_stage)
  unname(SEQUENCE_STAGE_STATUSES[as.character(stage)])
}

get_recommended_action_for_stage <- function(sequence_stage) {
  stage <- normalize_sequence_stage(sequence_stage)
  unname(SEQUENCE_RECOMMENDED_ACTIONS[as.character(stage)])
}

get_recommended_action <- function(status, sequence_stage) {
  status <- normalize_status(status)
  
  if (is_terminal_status(status)) {
    return("No outbound action")
  }
  
  if (status == "Bounced") {
    return("Fix email address before next outreach")
  }
  
  get_recommended_action_for_stage(sequence_stage)
}


# ---- Next-step logic ---------------------------------------------------------

get_next_sequence_stage <- function(sequence_stage) {
  stage <- normalize_sequence_stage(sequence_stage)
  
  next_stage <- stage + 1
  
  if (next_stage > max(SEQUENCE_STAGES)) {
    return(max(SEQUENCE_STAGES))
  }
  
  next_stage
}

get_next_status <- function(sequence_stage) {
  next_stage <- get_next_sequence_stage(sequence_stage)
  get_status_for_sequence_stage(next_stage)
}

get_next_touch_days <- function(sequence_stage) {
  stage <- normalize_sequence_stage(sequence_stage)
  
  days <- SEQUENCE_NEXT_TOUCH_DAYS[as.character(stage)]
  
  if (is.na(days)) {
    return(DEFAULT_QUEUE_SNOOZE_DAYS)
  }
  
  as.integer(days)
}

get_next_touch_date <- function(sequence_stage, from_date = Sys.Date()) {
  stage <- normalize_sequence_stage(sequence_stage)
  days <- get_next_touch_days(stage)
  
  as.Date(from_date) + days
}

calculate_next_touch_date <- function(sequence_stage, from_date = Sys.Date()) {
  # Backward-compatible wrapper used by earlier scaffold code.
  get_next_touch_date(sequence_stage, from_date)
}


# ---- Queue eligibility ------------------------------------------------------

is_due_for_touch <- function(next_touch) {
  if (is.null(next_touch) || length(next_touch) == 0) {
    return(TRUE)
  }
  
  next_touch <- next_touch[1]
  
  if (is.na(next_touch)) {
    return(TRUE)
  }
  
  next_touch_text <- trimws(as.character(next_touch))
  
  if (next_touch_text == "") {
    return(TRUE)
  }
  
  parsed_date <- suppressWarnings(as.Date(next_touch_text))
  
  if (is.na(parsed_date)) {
    return(TRUE)
  }
  
  parsed_date <= Sys.Date()
}

next_touch_due <- function(next_touch_values) {
  # Vectorized helper for queue filtering in Shiny modules.
  
  parsed_dates <- suppressWarnings(as.Date(next_touch_values))
  
  is.na(parsed_dates) | parsed_dates <= Sys.Date()
}

should_show_in_queue <- function(status, next_touch) {
  status <- normalize_status(status)
  
  if (is_terminal_status(status)) {
    return(FALSE)
  }
  
  is_due_for_touch(next_touch)
}


# ---- Touch outcome logic ----------------------------------------------------

status_from_touch_outcome <- function(outcome, current_status = NULL) {
  if (is.null(outcome) || length(outcome) == 0 || is.na(outcome) || outcome == "") {
    return(current_status %||% DEFAULT_PROSPECT_STATUS)
  }
  
  if (outcome == "Replied") {
    return("Replied")
  }
  
  if (outcome == "Not Interested") {
    return("Not Interested")
  }
  
  if (outcome == "Do Not Contact") {
    return("Do Not Contact")
  }
  
  current_status %||% DEFAULT_PROSPECT_STATUS
}

is_terminal_outcome <- function(outcome) {
  outcome %in% c("Replied", "Not Interested", "Do Not Contact")
}


# ---- Display helpers --------------------------------------------------------

format_sequence_stage <- function(sequence_stage) {
  stage <- normalize_sequence_stage(sequence_stage)
  paste0(stage, " - ", get_sequence_stage_label(stage))
}

format_next_action <- function(status, sequence_stage, next_touch) {
  status <- normalize_status(status)
  
  if (is_terminal_status(status)) {
    return("Complete")
  }
  
  if (!is_due_for_touch(next_touch)) {
    return(paste("Scheduled for", as.character(as.Date(next_touch))))
  }
  
  get_recommended_action(status, sequence_stage)
}


# ---- Small infix helper -----------------------------------------------------
# Keeps this file self-contained in case rlang is not loaded.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x) || x == "") {
    y
  } else {
    x
  }
}
