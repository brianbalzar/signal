#!/bin/bash

echo "🚀 Setting up Signal Shiny App structure..."

# Create directories
mkdir -p modules
mkdir -p services
mkdir -p data
mkdir -p utils
mkdir -p prompts
mkdir -p www

# Create main app files
touch app.R
touch global.R
touch README.md

# Create module files
touch modules/mod_prospects.R
touch modules/mod_queue.R
touch modules/mod_contact_detail.R
touch modules/mod_email.R
touch modules/mod_touches.R

# Create service files
touch services/db.R
touch services/outreach_logic.R
touch services/claude.R
touch services/email.R

# Create utils files
touch utils/helpers.R
touch utils/formatters.R
touch utils/constants.R

# Create prompt file
touch prompts/intro_email.txt

# Create web assets
touch www/styles.css
touch www/logo.png

# Add starter content to key files

# app.R
cat > app.R << 'EOF'
library(shiny)

source("global.R")

source("modules/mod_prospects.R")
source("modules/mod_queue.R")

ui <- fluidPage(
  titlePanel("Signal"),
  tabsetPanel(
    tabPanel("Queue", mod_queue_ui("queue")),
    tabPanel("Prospects", mod_prospects_ui("prospects"))
  )
)

server <- function(input, output, session) {
  mod_queue_server("queue")
  mod_prospects_server("prospects")
}

shinyApp(ui, server)
EOF

# global.R
cat > global.R << 'EOF'
library(shiny)
library(DBI)
library(RSQLite)

source("services/db.R")

init_db()
EOF

# db.R
cat > services/db.R << 'EOF'
library(DBI)
library(RSQLite)

get_db <- function() {
  dbConnect(SQLite(), "data/signal.sqlite")
}

init_db <- function() {
  con <- get_db()

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS contacts (
      id INTEGER PRIMARY KEY,
      name TEXT,
      company TEXT,
      email TEXT,
      facility_type TEXT,
      notes TEXT,
      status TEXT,
      last_touch TEXT,
      next_touch TEXT
    )
  ")

  dbDisconnect(con)
}
EOF

# mod_prospects.R
cat > modules/mod_prospects.R << 'EOF'
mod_prospects_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Prospects"),
    p("Prospect management UI coming soon...")
  )
}

mod_prospects_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}
EOF

# mod_queue.R
cat > modules/mod_queue.R << 'EOF'
mod_queue_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Outreach Queue"),
    p("Queue UI coming soon...")
  )
}

mod_queue_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}
EOF

# outreach_logic.R
cat > services/outreach_logic.R << 'EOF'
# Placeholder for outreach cadence logic

get_next_touch <- function(last_touch, stage) {
  # TODO: implement logic
  return(Sys.Date() + 2)
}
EOF

# claude.R
cat > services/claude.R << 'EOF'
# Placeholder for Claude API integration

generate_email <- function(contact) {
  # TODO: call Claude API
  return(list(
    subject = "Test Subject",
    body = "Test email body"
  ))
}
EOF

# email.R
cat > services/email.R << 'EOF'
# Placeholder for email sending logic

send_email <- function(to, subject, body) {
  # TODO: implement SMTP sending
  print(paste("Sending email to", to))
}
EOF

# intro_email.txt
cat > prompts/intro_email.txt << 'EOF'
Write a concise, personalized cold outbound email.

Rules:
- Max 120 words
- Plainspoken
- No hype
- Mention one specific reason for outreach
- Ask for a low-pressure conversation
EOF

# README
cat > README.md << 'EOF'
# Signal

Outbound tracking + email generation tool built in R Shiny.

## Structure
- modules/: UI modules
- services/: business logic
- data/: SQLite database
- prompts/: LLM prompts
EOF

echo "✅ Signal app structure created!"
echo "👉 Next step: run 'R -e \"shiny::runApp()\"' or open app.R in RStudio."