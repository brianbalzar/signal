# R/constants.R
# Shared constants for Signal.

# ---- Prospect statuses ------------------------------------------------------
#
# Lifecycle phases:
#   Prospecting: Not Started → Nurture  (email sequence, auto-cadence)
#   Conversation: In Conversation        (replied; now in active dialogue)
#   Customer:     Customer               (moved to CRM; still loggable)
#   Terminal:     Not Interested / Do Not Contact (truly done)
#
# "Replied" no longer exists as a status. Migration 004 converts old rows
# to "In Conversation". "Replied" remains a valid TOUCH OUTCOME.

PROSPECT_STATUSES <- c(
  "Not Started",
  "Ready to Email",
  "Bounced",
  "Intro Sent",
  "Follow-Up 1 Sent",
  "Follow-Up 2 Sent",
  "Breakup Sent",
  "Nurture",
  "In Conversation",
  "Customer",
  "Not Interested",
  "Do Not Contact"
)

OUTREACH_PROSPECT_STATUSES <- c(
  "Not Started",
  "Ready to Email",
  "Bounced",
  "Intro Sent",
  "Follow-Up 1 Sent",
  "Follow-Up 2 Sent",
  "Breakup Sent",
  "Nurture"
)

CONVERSATION_PROSPECT_STATUSES <- c("In Conversation")

CUSTOMER_PROSPECT_STATUSES <- c("Customer")

ACTIVE_PROSPECT_STATUSES <- c(
  OUTREACH_PROSPECT_STATUSES,
  CONVERSATION_PROSPECT_STATUSES,
  CUSTOMER_PROSPECT_STATUSES
)

TERMINAL_PROSPECT_STATUSES <- c(
  "Not Interested",
  "Do Not Contact"
)

DEFAULT_PROSPECT_STATUS <- "Not Started"


# ---- Prospect sources -------------------------------------------------------

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
#
# 0 = intro email not yet sent
# 1 = intro sent; follow-up 1 is next
# 2 = follow-up 1 sent; follow-up 2 is next
# 3 = follow-up 2 sent; breakup is next
# 4 = breakup sent; nurture is next
# 5 = nurture

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

# Days until next touch after a touch is logged at each stage.
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
  "Meeting",
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
  "Meeting Scheduled",
  "Meeting Completed",
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


# ---- Cadence defaults -------------------------------------------------------

DEFAULT_QUEUE_SNOOZE_DAYS        <- 7
DEFAULT_NURTURE_SNOOZE_DAYS      <- 30
DEFAULT_CONVERSATION_NEXT_TOUCH_DAYS <- 7
DEFAULT_CUSTOMER_CHECKIN_DAYS    <- 30


# ---- Display ----------------------------------------------------------------

DATE_DISPLAY_FORMAT     <- "%Y-%m-%d"
DATETIME_DISPLAY_FORMAT <- "%Y-%m-%d %H:%M"


# ---- Backward compatibility aliases ----------------------------------------

CONTACT_STATUSES <- PROSPECT_STATUSES
FACILITY_TYPES   <- PROSPECT_SEGMENTS
