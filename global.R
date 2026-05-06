source("utils/health.R")

check_required_packages()

library(shiny)
library(DBI)
library(RSQLite)
library(DT)

source("utils/constants.R")
source("utils/helpers.R")
source("utils/formatters.R")

source("services/outreach_logic.R")
source("services/db.R")
source("services/claude.R")
source("services/email.R")

source("modules/mod_prospects_ui.R")
source("modules/mod_prospects_server.R")
source("modules/mod_queue_ui.R")
source("modules/mod_queue_server.R")

init_db()
