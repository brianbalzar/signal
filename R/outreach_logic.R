# R/outreach_logic.R
# Cadence, sequence, and lifecycle logic for Signal.
# Pure R — no DB calls, no Shiny, no side effects.

# ---- Status phase helpers ---------------------------------------------------

is_terminal_status <- function(status) {
  status %in% TERMINAL_PROSPECT_STATUSES
}

is_active_status <- function(status) {
  status %in% ACTIVE_PROSPECT_STATUSES
}

is_outreach_status <- function(status) {
  status %in% OUTREACH_PROSPECT_STATUSES
}

is_conversation_status <- function(status) {
  status %in% CONVERSATION_PROSPECT_STATUSES
}

is_customer_status <- function(status) {
  status %in% CUSTOMER_PROSPECT_STATUSES
}

normalize_status <- function(status) {
  if (is.null(status) || length(status) == 0 || is.na(status) || status == "") {
    return(DEFAULT_PROSPECT_STATUS)
  }

  # Migrate legacy "Replied" status to "In Conversation".
  if (status == "Replied") {
    return("In Conversation")
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

  if (is_terminal_status(status)) return("No further action")
  if (is_conversation_status(status)) return("Schedule or log a call or meeting")
  if (is_customer_status(status)) return("Check in with customer")
  if (status == "Bounced") return("Fix email address before next outreach")

  get_recommended_action_for_stage(sequence_stage)
}


# ---- Next-step logic --------------------------------------------------------

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
  get_next_touch_date(sequence_stage, from_date)
}


# ---- Queue eligibility ------------------------------------------------------

is_due_for_touch <- function(next_touch) {
  if (is.null(next_touch) || length(next_touch) == 0) return(TRUE)

  next_touch <- next_touch[1]
  if (is.na(next_touch)) return(TRUE)

  next_touch_text <- trimws(as.character(next_touch))
  if (next_touch_text == "") return(TRUE)

  parsed_date <- suppressWarnings(as.Date(next_touch_text))
  if (is.na(parsed_date)) return(TRUE)

  parsed_date <= Sys.Date()
}

next_touch_due <- function(next_touch_values) {
  parsed_dates <- suppressWarnings(as.Date(next_touch_values))
  is.na(parsed_dates) | parsed_dates <= Sys.Date()
}

# Each phase has its own eligibility check so queue queries stay independent.
should_show_in_outreach_queue <- function(status, next_touch) {
  is_outreach_status(normalize_status(status)) && is_due_for_touch(next_touch)
}

should_show_in_conversation_queue <- function(status, next_touch) {
  is_conversation_status(normalize_status(status)) && is_due_for_touch(next_touch)
}

should_show_in_customer_queue <- function(status, next_touch) {
  is_customer_status(normalize_status(status)) && is_due_for_touch(next_touch)
}

# Legacy name kept for any modules not yet updated.
should_show_in_queue <- function(status, next_touch) {
  should_show_in_outreach_queue(status, next_touch)
}


# ---- Touch outcome logic ----------------------------------------------------

# "Replied" as a TOUCH OUTCOME moves the prospect into the Conversation phase.
# "Not Interested" / "Do Not Contact" are terminal in all phases.
status_from_touch_outcome <- function(outcome, current_status = NULL) {
  if (is.null(outcome) || length(outcome) == 0 || is.na(outcome) || outcome == "") {
    return(current_status %||% DEFAULT_PROSPECT_STATUS)
  }

  if (outcome == "Replied")         return("In Conversation")
  if (outcome == "Not Interested")  return("Not Interested")
  if (outcome == "Do Not Contact")  return("Do Not Contact")

  current_status %||% DEFAULT_PROSPECT_STATUS
}

# Outcomes that exit the current queue phase (terminal OR phase-transition).
is_terminal_outcome <- function(outcome) {
  outcome %in% c("Replied", "Not Interested", "Do Not Contact")
}


# ---- Customer transition helpers --------------------------------------------

resolve_customer_next_touch <- function(
    next_touch_date = NULL,
    next_touch_days = NULL,
    from_date       = Sys.Date()
) {
  if (!is.null(next_touch_date) && !is.na(next_touch_date) && as.character(next_touch_date) != "") {
    return(as.character(as.Date(next_touch_date)))
  }

  days <- suppressWarnings(as.integer(next_touch_days))
  if (is.na(days) || days < 1) days <- DEFAULT_CUSTOMER_CHECKIN_DAYS

  as.character(as.Date(from_date) + days)
}


# ---- Display helpers --------------------------------------------------------

format_sequence_stage <- function(sequence_stage) {
  stage <- normalize_sequence_stage(sequence_stage)
  paste0(stage, " - ", get_sequence_stage_label(stage))
}

format_next_action <- function(status, sequence_stage, next_touch) {
  status <- normalize_status(status)

  if (is_terminal_status(status)) return("Complete")

  if (!is_due_for_touch(next_touch)) {
    return(paste("Scheduled for", as.character(as.Date(next_touch))))
  }

  get_recommended_action(status, sequence_stage)
}


# ---- Small infix helper -----------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x) || x == "") y else x
}
