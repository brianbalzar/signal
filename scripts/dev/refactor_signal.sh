echo "Refactoring Signal into separate ui/server files..."

mkdir -p modules services data utils prompts www

# Remove old combined module files if they exist
rm -f modules/mod_prospects.R
rm -f modules/mod_queue.R

touch ui.R
touch server.R
touch modules/mod_prospects_ui.R
touch modules/mod_prospects_server.R
touch modules/mod_queue_ui.R
touch modules/mod_queue_server.R

cat > app.R << 'APP'
library(shiny)

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
APP

cat > global.R << 'GLOBAL'
library(shiny)
library(DBI)
library(RSQLite)
library(DT)

source("utils/constants.R")
source("utils/helpers.R")
source("utils/formatters.R")

source("services/db.R")
source("services/outreach_logic.R")
source("services/claude.R")
source("services/email.R")

source("modules/mod_prospects_ui.R")
source("modules/mod_prospects_server.R")
source("modules/mod_queue_ui.R")
source("modules/mod_queue_server.R")

init_db()
GLOBAL

cat > ui.R << 'UI'
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  div(
    class = "app-header",
    h1("Signal"),
    p("Outbound tracking and email generation for facility consulting prospects.")
  ),

  tabsetPanel(
    id = "main_tabs",

    tabPanel(
      "Outreach Queue",
      mod_queue_ui("queue")
    ),

    tabPanel(
      "Prospects",
      mod_prospects_ui("prospects")
    )
  )
)
UI

cat > server.R << 'SERVER'
server <- function(input, output, session) {
  mod_queue_server("queue")
  mod_prospects_server("prospects")
}
SERVER

cat > utils/constants.R << 'CONSTANTS'
CONTACT_STATUSES <- c(
  "Not Started",
  "Researching",
  "Ready to Email",
  "Draft Generated",
  "Email Sent",
  "Follow-Up Needed",
  "Replied",
  "Meeting Booked",
  "Nurture",
  "Not Interested",
  "Do Not Contact"
)

FACILITY_TYPES <- c(
  "",
  "Hospital",
  "Medical Office",
  "School",
  "University",
  "Commercial Office",
  "Industrial",
  "Multifamily",
  "Retail",
  "Other"
)

CONTROL_SYSTEMS <- c(
  "",
  "JCI Metasys",
  "Siemens",
  "Trane Tracer",
  "Honeywell",
  "Schneider Electric",
  "Distech",
  "Automated Logic",
  "Unknown",
  "Other"
)
CONSTANTS

cat > utils/helpers.R << 'HELPERS'
empty_to_na <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }

  x <- trimws(as.character(x))

  if (identical(x, "")) {
    return(NA_character_)
  }

  x
}

today_string <- function() {
  as.character(Sys.Date())
}
HELPERS

cat > utils/formatters.R << 'FORMATTERS'
format_date_for_display <- function(x) {
  if (is.na(x) || is.null(x) || x == "") {
    return("")
  }

  as.character(x)
}
FORMATTERS

cat > services/db.R << 'DB'
get_db <- function() {
  dbConnect(SQLite(), "data/signal.sqlite")
}

init_db <- function() {
  con <- get_db()

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS contacts (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      first_name TEXT,
      last_name TEXT,
      company TEXT,
      title TEXT,
      email TEXT,
      phone TEXT,
      linkedin_url TEXT,
      facility_type TEXT,
      controls_system TEXT,
      city TEXT,
      state TEXT,
      pain_signals TEXT,
      notes TEXT,
      status TEXT DEFAULT 'Not Started',
      sequence_stage INTEGER DEFAULT 0,
      last_touch TEXT,
      next_touch TEXT,
      created_at TEXT,
      updated_at TEXT
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS touches (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      contact_id INTEGER,
      touch_type TEXT,
      direction TEXT,
      subject TEXT,
      body TEXT,
      outcome TEXT,
      sequence_stage INTEGER,
      created_at TEXT,
      FOREIGN KEY(contact_id) REFERENCES contacts(id)
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS email_drafts (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      contact_id INTEGER,
      sequence_stage INTEGER,
      subject TEXT,
      body TEXT,
      status TEXT DEFAULT 'Draft',
      created_at TEXT,
      updated_at TEXT,
      FOREIGN KEY(contact_id) REFERENCES contacts(id)
    )
  ")

  dbDisconnect(con)
}

create_contact <- function(contact) {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  now <- as.character(Sys.time())

  dbExecute(
    con,
    "
    INSERT INTO contacts (
      first_name,
      last_name,
      company,
      title,
      email,
      phone,
      linkedin_url,
      facility_type,
      controls_system,
      city,
      state,
      pain_signals,
      notes,
      status,
      sequence_stage,
      last_touch,
      next_touch,
      created_at,
      updated_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ",
    params = list(
      contact$first_name,
      contact$last_name,
      contact$company,
      contact$title,
      contact$email,
      contact$phone,
      contact$linkedin_url,
      contact$facility_type,
      contact$controls_system,
      contact$city,
      contact$state,
      contact$pain_signals,
      contact$notes,
      contact$status,
      0,
      NA_character_,
      contact$next_touch,
      now,
      now
    )
  )
}

get_contacts <- function() {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  dbGetQuery(
    con,
    "
    SELECT
      id,
      first_name || ' ' || last_name AS name,
      company,
      title,
      email,
      facility_type,
      controls_system,
      city,
      state,
      status,
      sequence_stage,
      last_touch,
      next_touch,
      notes
    FROM contacts
    ORDER BY
      CASE WHEN next_touch IS NULL OR next_touch = '' THEN 1 ELSE 0 END,
      next_touch ASC,
      company ASC,
      last_name ASC
    "
  )
}

get_contact_by_id <- function(contact_id) {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  result <- dbGetQuery(
    con,
    "SELECT * FROM contacts WHERE id = ?",
    params = list(contact_id)
  )

  if (nrow(result) == 0) {
    return(NULL)
  }

  result[1, ]
}

update_contact_status <- function(contact_id, status) {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  dbExecute(
    con,
    "
    UPDATE contacts
    SET status = ?, updated_at = ?
    WHERE id = ?
    ",
    params = list(status, as.character(Sys.time()), contact_id)
  )
}

log_touch <- function(contact_id, touch_type, direction, subject, body, outcome, sequence_stage) {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  now <- as.character(Sys.time())
  next_touch <- calculate_next_touch_date(sequence_stage)

  dbExecute(
    con,
    "
    INSERT INTO touches (
      contact_id,
      touch_type,
      direction,
      subject,
      body,
      outcome,
      sequence_stage,
      created_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    ",
    params = list(
      contact_id,
      touch_type,
      direction,
      subject,
      body,
      outcome,
      sequence_stage,
      now
    )
  )

  dbExecute(
    con,
    "
    UPDATE contacts
    SET
      last_touch = ?,
      next_touch = ?,
      sequence_stage = ?,
      updated_at = ?
    WHERE id = ?
    ",
    params = list(
      as.character(Sys.Date()),
      as.character(next_touch),
      sequence_stage + 1,
      now,
      contact_id
    )
  )
}
DB

cat > services/outreach_logic.R << 'OUTREACH'
calculate_next_touch_date <- function(sequence_stage) {
  stage <- suppressWarnings(as.integer(sequence_stage))

  if (is.na(stage)) {
    stage <- 0
  }

  days_until_next_touch <- switch(
    as.character(stage),
    "0" = 0,
    "1" = 2,
    "2" = 2,
    "3" = 3,
    "4" = 3,
    "5" = 5,
    "6" = 15,
    30
  )

  Sys.Date() + days_until_next_touch
}

get_recommended_action <- function(status, sequence_stage) {
  stage <- suppressWarnings(as.integer(sequence_stage))

  if (is.na(stage)) {
    stage <- 0
  }

  if (status %in% c("Replied", "Meeting Booked", "Not Interested", "Do Not Contact")) {
    return("No outbound action")
  }

  switch(
    as.character(stage),
    "0" = "Generate intro email",
    "1" = "Send LinkedIn view/connect",
    "2" = "Send follow-up email with insight",
    "3" = "Call or leave voicemail",
    "4" = "Send proof/case study email",
    "5" = "Send breakup email",
    "6" = "Move to nurture",
    "Nurture with new trigger"
  )
}
OUTREACH

cat > services/claude.R << 'CLAUDE'
generate_email <- function(contact) {
  # Placeholder. We will wire this to Claude after the tracker workflow is stable.

  first_name <- contact$first_name
  company <- contact$company
  facility_type <- contact$facility_type
  controls_system <- contact$controls_system
  pain_signals <- contact$pain_signals

  if (is.na(first_name) || first_name == "") {
    first_name <- "there"
  }

  if (is.na(company) || company == "") {
    company <- "your facility"
  }

  reason <- paste("I noticed", company)

  if (!is.na(facility_type) && facility_type != "") {
    reason <- paste(reason, "is a", tolower(facility_type))
  }

  if (!is.na(controls_system) && controls_system != "") {
    reason <- paste(reason, "and may be using", controls_system)
  }

  likely_pain <- "hidden HVAC and controls issues that do not always show up as alarms"

  if (!is.na(pain_signals) && pain_signals != "") {
    likely_pain <- pain_signals
  }

  subject <- paste("Quick question on", company)

  body <- paste0(
    "Hi ", first_name, ",\n\n",
    reason, ", so I wanted to reach out.\n\n",
    "A lot of facility teams we talk with are technically running fine, but still have energy waste or comfort issues from ",
    likely_pain, ".\n\n",
    "We help identify those issues using analytics, then prioritize practical fixes around HVAC performance, controls, and operations.\n\n",
    "Would it be worth a quick conversation to compare what we typically look for against what you are seeing?\n\n",
    "Best,\n",
    "Brian"
  )

  list(subject = subject, body = body)
}
CLAUDE

cat > services/email.R << 'EMAIL'
send_email <- function(to, subject, body) {
  # Placeholder. We will add SMTP later.
  message("Email sending not implemented yet.")
  invisible(FALSE)
}
EMAIL

cat > modules/mod_prospects_ui.R << 'PROSPECTS_UI'
mod_prospects_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 4,

        div(
          class = "panel-card",
          h3("Add Prospect"),

          textInput(ns("first_name"), "First Name"),
          textInput(ns("last_name"), "Last Name"),
          textInput(ns("company"), "Company"),
          textInput(ns("title"), "Title"),
          textInput(ns("email"), "Email"),
          textInput(ns("phone"), "Phone"),
          textInput(ns("linkedin_url"), "LinkedIn URL"),

          selectInput(ns("facility_type"), "Facility Type", choices = FACILITY_TYPES),
          selectInput(ns("controls_system"), "Controls System", choices = CONTROL_SYSTEMS),

          fluidRow(
            column(6, textInput(ns("city"), "City")),
            column(6, textInput(ns("state"), "State"))
          ),

          textAreaInput(
            ns("pain_signals"),
            "Pain Signals",
            rows = 3,
            placeholder = "Example: aging controls, high utility bills, comfort complaints, upcoming capital project"
          ),

          textAreaInput(
            ns("notes"),
            "Notes",
            rows = 4,
            placeholder = "Relevant context from Convex Atlas, website, LinkedIn, or discovery notes"
          ),

          selectInput(ns("status"), "Status", choices = CONTACT_STATUSES, selected = "Not Started"),

          dateInput(ns("next_touch"), "Next Touch", value = Sys.Date()),

          actionButton(ns("add_contact"), "Add Prospect", class = "btn-primary")
        )
      ),

      column(
        width = 8,

        div(
          class = "panel-card",
          h3("Prospects"),
          DTOutput(ns("contacts_table"))
        )
      )
    )
  )
}
PROSPECTS_UI

cat > modules/mod_prospects_server.R << 'PROSPECTS_SERVER'
mod_prospects_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    refresh_counter <- reactiveVal(0)

    contacts_data <- reactive({
      refresh_counter()
      get_contacts()
    })

    output$contacts_table <- renderDT({
      datatable(
        contacts_data(),
        rownames = FALSE,
        selection = "single",
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          scrollX = TRUE
        )
      )
    })

    observeEvent(input$add_contact, {
      req(input$first_name, input$last_name, input$company)

      create_contact(list(
        first_name = empty_to_na(input$first_name),
        last_name = empty_to_na(input$last_name),
        company = empty_to_na(input$company),
        title = empty_to_na(input$title),
        email = empty_to_na(input$email),
        phone = empty_to_na(input$phone),
        linkedin_url = empty_to_na(input$linkedin_url),
        facility_type = empty_to_na(input$facility_type),
        controls_system = empty_to_na(input$controls_system),
        city = empty_to_na(input$city),
        state = empty_to_na(input$state),
        pain_signals = empty_to_na(input$pain_signals),
        notes = empty_to_na(input$notes),
        status = input$status,
        next_touch = as.character(input$next_touch)
      ))

      showNotification("Prospect added.", type = "message")

      updateTextInput(session, "first_name", value = "")
      updateTextInput(session, "last_name", value = "")
      updateTextInput(session, "company", value = "")
      updateTextInput(session, "title", value = "")
      updateTextInput(session, "email", value = "")
      updateTextInput(session, "phone", value = "")
      updateTextInput(session, "linkedin_url", value = "")
      updateSelectInput(session, "facility_type", selected = "")
      updateSelectInput(session, "controls_system", selected = "")
      updateTextInput(session, "city", value = "")
      updateTextInput(session, "state", value = "")
      updateTextAreaInput(session, "pain_signals", value = "")
      updateTextAreaInput(session, "notes", value = "")
      updateSelectInput(session, "status", selected = "Not Started")
      updateDateInput(session, "next_touch", value = Sys.Date())

      refresh_counter(refresh_counter() + 1)
    })
  })
}
PROSPECTS_SERVER

cat > modules/mod_queue_ui.R << 'QUEUE_UI'
mod_queue_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 12,
        div(
          class = "panel-card",
          h3("Today’s Outreach Queue"),
          p("Contacts with next touch dates due today or earlier."),

          actionButton(ns("refresh_queue"), "Refresh Queue"),

          br(),
          br(),

          DTOutput(ns("queue_table"))
        )
      )
    ),

    fluidRow(
      column(
        width = 6,
        div(
          class = "panel-card",
          h3("Recommended Action"),
          verbatimTextOutput(ns("recommended_action")),

          actionButton(ns("generate_draft"), "Generate Draft", class = "btn-primary"),
          actionButton(ns("mark_touch"), "Log Touch")
        )
      ),

      column(
        width = 6,
        div(
          class = "panel-card",
          h3("Draft Preview"),
          textInput(ns("draft_subject"), "Subject"),
          textAreaInput(ns("draft_body"), "Body", rows = 12)
        )
      )
    )
  )
}
QUEUE_UI

cat > modules/mod_queue_server.R << 'QUEUE_SERVER'
mod_queue_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    refresh_counter <- reactiveVal(0)
    selected_contact <- reactiveVal(NULL)

    queue_data <- reactive({
      refresh_counter()

      contacts <- get_contacts()

      if (nrow(contacts) == 0) {
        return(contacts)
      }

      contacts[next_touch_due(contacts$next_touch), ]
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
      contact <- get_contact_by_id(row$id)

      selected_contact(contact)
    })

    output$recommended_action <- renderText({
      contact <- selected_contact()

      if (is.null(contact)) {
        return("Select a contact from the queue.")
      }

      get_recommended_action(contact$status, contact$sequence_stage)
    })

    observeEvent(input$generate_draft, {
      contact <- selected_contact()
      req(contact)

      draft <- generate_email(contact)

      updateTextInput(session, "draft_subject", value = draft$subject)
      updateTextAreaInput(session, "draft_body", value = draft$body)

      showNotification("Draft generated.", type = "message")
    })

    observeEvent(input$mark_touch, {
      contact <- selected_contact()
      req(contact)

      log_touch(
        contact_id = contact$id,
        touch_type = "Email",
        direction = "Outbound",
        subject = input$draft_subject,
        body = input$draft_body,
        outcome = "Sent",
        sequence_stage = contact$sequence_stage
      )

      showNotification("Touch logged and next touch updated.", type = "message")

      selected_contact(NULL)
      updateTextInput(session, "draft_subject", value = "")
      updateTextAreaInput(session, "draft_body", value = "")

      refresh_counter(refresh_counter() + 1)
    })
  })
}

next_touch_due <- function(next_touch_values) {
  parsed_dates <- as.Date(next_touch_values)

  is.na(parsed_dates) | parsed_dates <= Sys.Date()
}
QUEUE_SERVER

cat > www/styles.css << 'CSS'
body {
  background-color: #f7f7f8;
}

.app-header {
  margin-bottom: 20px;
  padding: 20px 0 10px 0;
}

.app-header h1 {
  margin-bottom: 4px;
  font-weight: 700;
}

.app-header p {
  color: #555;
  font-size: 16px;
}

.panel-card {
  background: white;
  border: 1px solid #e5e5e5;
  border-radius: 12px;
  padding: 18px;
  margin-bottom: 20px;
  box-shadow: 0 1px 4px rgba(0,0,0,0.04);
}

.btn-primary {
  margin-top: 8px;
}
CSS

cat > prompts/intro_email.txt << 'PROMPT'
You are writing a concise, personalized cold outbound email for a facility consulting company.

The company helps building owners and facility teams improve HVAC performance, controls optimization, BAS/BMS issues, energy efficiency, and operational reliability using analytics.

Write a first-touch intro email.

Rules:
- Maximum 120 words.
- Plainspoken and consultative.
- Do not sound like marketing copy.
- Do not overclaim.
- Mention one specific reason this account may be relevant.
- Mention one likely operational pain.
- Ask for a low-pressure conversation.
- Include a subject line.
- Do not use emojis.
- Do not use fake case study numbers unless provided.
PROMPT

cat > README.md << 'README'
# Signal

Signal is a local R Shiny app for tracking outbound prospects, managing outreach cadence, and generating personalized email drafts.

## Run

```r
shiny::runApp()
```

##Current Features
-Add prospects
-Store contacts in SQLite
-View prospects
-View today's outreach queue
-Generate placeholder email drafts
-Log outbound touches
-Advance next-touch dates
##Folder Structure
-app.R - entry point
-global.R - libraries and source files
-ui.R - top-level UI
-server.R - top-level server
-modules/ - Shiny modules
-services/ - database, outreach logic, Claude, and email logic
-data/ - SQLite database
-utils/ - helper functions and constants
-prompts/ - reusable LLM prompts
-www/ - CSS and static assets
##README

echo "Done."
echo "Run the app with:"
echo "R -e "shiny::runApp()""
EOF
