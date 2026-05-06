# utils/constants.R
# Shared constants for Signal
#
# Signal is a lightweight outbound workbench.
# These constants define the allowed statuses, prospect segments,
# lead sources, touch types, and sequence labels used across the app.

# ---- Prospect statuses ------------------------------------------------------
# Terminal statuses should be excluded from the active outreach queue:
# - Replied
# - Not Interested
# - Do Not Contact

PROSPECT_STATUSES <- c(
  "Not Started",
  "Ready to Email",
  "Bounced",
  "Intro Sent",
  "Follow-Up 1 Sent",
  "Follow-Up 2 Sent",
  "Breakup Sent",
  "Nurture",
  "Replied",
  "Not Interested",
  "Do Not Contact"
)

ACTIVE_PROSPECT_STATUSES <- c(
  "Not Started",
  "Ready to Email",
  "Bounced",
  "Intro Sent",
  "Follow-Up 1 Sent",
  "Follow-Up 2 Sent",
  "Breakup Sent",
  "Nurture"
)

TERMINAL_PROSPECT_STATUSES <- c(
  "Replied",
  "Not Interested",
  "Do Not Contact"
)

DEFAULT_PROSPECT_STATUS <- "Not Started"


# ---- Prospect sources -------------------------------------------------------
# Where the prospect came from.
# Keep this list broad enough to support manual entry without overcomplicating it.

PROSPECT_SOURCES <- c(
  "",
  "Convex Atlas",
  "LinkedIn",
  "Referral",
  "Google",
  "Existing Network",
  "Website",
  "Conference/Event",
  "Manual",
  "Other"
)


# ---- Prospect segments ------------------------------------------------------
# Segment should be approximate.
# It is mainly used to help the email generator frame the outreach.

PROSPECT_SEGMENTS <- c(
  "",
  "Hospital",
  "Medical Office",
  "School",
  "University",
  "Commercial Office",
  "Industrial Facility",
  "Property Management",
  "Multifamily",
  "Retail",
  "Government",
  "Religious / Nonprofit",
  "Unknown",
  "Other"
)


# ---- Outreach sequence ------------------------------------------------------
# Sequence stage represents the next action to take.
#
# 0 = intro email has not been sent yet
# 1 = intro sent; follow-up 1 is next
# 2 = follow-up 1 sent; follow-up 2 is next
# 3 = follow-up 2 sent; breakup email is next
# 4 = breakup sent; nurture is next
# 5 = nurture / no immediate action

SEQUENCE_STAGES <- c(0, 1, 2, 3, 4, 5)

SEQUENCE_STAGE_LABELS <- c(
  "0" = "Intro Email",
  "1" = "Follow-Up 1",
  "2" = "Follow-Up 2",
  "3" = "Breakup Email",
  "4" = "Nurture",
  "5" = "Nurture"
)

SEQUENCE_STAGE_STATUSES <- c(
  "0" = "Ready to Email",
  "1" = "Intro Sent",
  "2" = "Follow-Up 1 Sent",
  "3" = "Follow-Up 2 Sent",
  "4" = "Breakup Sent",
  "5" = "Nurture"
)

SEQUENCE_RECOMMENDED_ACTIONS <- c(
  "0" = "Generate/send intro email",
  "1" = "Send follow-up 1",
  "2" = "Send follow-up 2",
  "3" = "Send breakup email",
  "4" = "Move to nurture",
  "5" = "Re-engage only if there is a useful reason"
)

# Days until the next touch AFTER a touch is logged at each stage.
#
# Example:
# If stage 0 intro email is sent today, next touch is 3 days from today.

SEQUENCE_NEXT_TOUCH_DAYS <- c(
  "0" = 3,
  "1" = 5,
  "2" = 7,
  "3" = 30,
  "4" = 30,
  "5" = 30
)

DEFAULT_SEQUENCE_STAGE <- 0


# ---- Touches ----------------------------------------------------------------

TOUCH_TYPES <- c(
  "Email",
  "LinkedIn",
  "Call",
  "Voicemail",
  "Manual Note",
  "Other"
)

TOUCH_OUTCOMES <- c(
  "Sent",
  "Connected",
  "Voicemail",
  "No Reply",
  "No Answer",
  "Call Back Later",
  "Replied",
  "Bounced",
  "Not Interested",
  "Do Not Contact",
  "Snoozed",
  "Manual Note"
)

DEFAULT_TOUCH_TYPE <- "Email"
DEFAULT_TOUCH_OUTCOME <- "Sent"


# ---- Calls ------------------------------------------------------------------

CALL_OUTCOMES <- c(
  "Connected",
  "Voicemail",
  "No Answer",
  "Call Back Later",
  "Not Interested",
  "Do Not Contact"
)

DEFAULT_CALL_OUTCOME <- "Connected"
DEFAULT_CALL_BACK_DAYS <- 2


# ---- Drafts -----------------------------------------------------------------

DRAFT_STATUSES <- c(
  "Draft",
  "Approved",
  "Sent",
  "Rejected"
)

DEFAULT_DRAFT_STATUS <- "Draft"


# ---- Email generation -------------------------------------------------------
# These are guidance values for the LLM prompt layer.

DEFAULT_EMAIL_WORD_LIMIT <- 120

EMAIL_TONES <- c(
  "Plainspoken",
  "Consultative",
  "Direct",
  "Warm",
  "Very Short"
)

DEFAULT_EMAIL_TONE <- "Plainspoken"


# ---- Research ---------------------------------------------------------------

DEFAULT_RESEARCH_WEB_SEARCH_USES <- 2
DEFAULT_RESEARCH_MAX_TOKENS <- 1000


# ---- UI defaults ------------------------------------------------------------

DEFAULT_QUEUE_SNOOZE_DAYS <- 7
DEFAULT_NURTURE_SNOOZE_DAYS <- 30

DATE_DISPLAY_FORMAT <- "%Y-%m-%d"
DATETIME_DISPLAY_FORMAT <- "%Y-%m-%d %H:%M"


# ---- Backward compatibility aliases ----------------------------------------
# The first scaffold used CONTACT_* names. Keep aliases temporarily so older
# modules do not break while we refactor them to prospect language.

CONTACT_STATUSES <- PROSPECT_STATUSES
FACILITY_TYPES <- PROSPECT_SEGMENTS
