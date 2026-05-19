# R/exports.R
# Public API for the signal package.
#
# Embed Signal as a tab in any host Shiny app:
#
#   tabPanel("Signal", signal_ui(ns("signal")))
#   signal_data <- signal_server(
#     "signal",
#     db_path   = config$signal_db,
#     api_key   = config$anthropic_key,
#     user_id   = current_user$id,
#     user_role = current_user$role   # "ae", "manager", or "admin"
#   )
#
# Or run standalone via app.R.

#' Signal module UI
#'
#' Returns the full Signal tab UI: inner tabsetPanel with Outreach Queue,
#' Conversation Queue, Customer Queue, and Prospects tabs.
#'
#' @param id Module namespace ID.
#' @export
signal_ui <- function(id) {
  ns <- shiny::NS(id)

  www_path <- system.file("www", package = "signal", mustWork = FALSE)
  if (www_path == "" || !dir.exists(www_path)) {
    for (candidate in c("inst/www", "www")) {
      if (dir.exists(candidate)) {
        www_path <- normalizePath(candidate)
        break
      }
    }
  }
  if (nchar(www_path) > 0 && dir.exists(www_path)) {
    shiny::addResourcePath("signal-www", www_path)
  }

  shiny::tagList(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "signal-www/styles.css")
    ),
    shiny::tabsetPanel(
      id = ns("signal_tabs"),
      shiny::tabPanel("Outreach Queue",    mod_queue_ui(ns("queue"))),
      shiny::tabPanel("Conversation",      mod_conversation_ui(ns("conversation"))),
      shiny::tabPanel("Customers",         mod_customers_ui(ns("customers"))),
      shiny::tabPanel("Prospects",         mod_prospects_ui(ns("prospects")))
    )
  )
}

#' Signal module server
#'
#' Initialises the database, applies role-based filtering, runs all sub-modules,
#' and returns a store of reactives for the host app to consume.
#'
#' @param id Module namespace ID.
#' @param db_path Path to the SQLite database file. Falls back to
#'   \code{SIGNAL_DB_PATH} env var, then a user-level AppData location.
#' @param api_key Anthropic API key. Falls back to \code{ANTHROPIC_API_KEY}
#'   env var, then \code{_secrets.yml}.
#' @param user_id Current user identifier (e.g. \code{"john.doe"}). AEs are
#'   filtered to their own prospects; admins/managers see all.
#' @param user_role One of \code{"ae"}, \code{"manager"}, \code{"admin"}.
#'   \code{"ae"} restricts data and hides the assignment UI.
#' @param claude_model Claude model name. Default: \code{claude-sonnet-4-6}.
#' @param web_search_type Claude web search tool type.
#' @return A list of reactives for use in the host app:
#'   \code{$connected_calls()}, \code{$meetings()},
#'   \code{$new_customers()}, \code{$pipeline_by_ae()}.
#' @export
signal_server <- function(
    id,
    db_path         = NULL,
    api_key         = NULL,
    user_id         = NULL,
    user_role       = NULL,
    claude_model    = NULL,
    web_search_type = NULL
) {
  if (!is.null(db_path)         && nchar(db_path) > 0)         options(signal.db_path = db_path)
  if (!is.null(api_key)         && nchar(api_key) > 0)         options(signal.api_key = api_key)
  if (!is.null(user_id)         && nchar(user_id) > 0)         options(signal.user_id = user_id)
  if (!is.null(user_role)       && nchar(user_role) > 0)       options(signal.user_role = user_role)
  if (!is.null(claude_model)    && nchar(claude_model) > 0)    options(signal.claude_model = claude_model)
  if (!is.null(web_search_type) && nchar(web_search_type) > 0) options(signal.web_search_type = web_search_type)

  init_db()

  # Compute the AE filter once: AEs see only their own data.
  ae_filter <- if (identical(tolower(user_role %||% ""), "ae")) user_id else NULL

  shiny::moduleServer(id, function(input, output, session) {
    mod_queue_server("queue",             ae_filter = ae_filter, user_role = user_role %||% "admin")
    mod_conversation_server("conversation", ae_filter = ae_filter, user_role = user_role %||% "admin")
    mod_customers_server("customers",     ae_filter = ae_filter, user_role = user_role %||% "admin")
    mod_prospects_server("prospects")

    # ---- Store: reactives for the host app ----------------------------------

    all_touches <- shiny::reactive({
      get_touches_for_all_prospects(ae_filter = ae_filter)
    })

    connected_calls <- shiny::reactive({
      t <- all_touches()
      if (is.null(t) || nrow(t) == 0) return(data.frame())
      t[!is.na(t$touch_type) & t$touch_type == "Call" &
          !is.na(t$outcome)   & t$outcome    == "Connected", ]
    })

    meetings <- shiny::reactive({
      t <- all_touches()
      if (is.null(t) || nrow(t) == 0) return(data.frame())
      t[!is.na(t$touch_type) & t$touch_type == "Meeting", ]
    })

    new_customers <- shiny::reactive({
      get_prospects_by_status("Customer", ae_filter = ae_filter)
    })

    pipeline_by_ae <- shiny::reactive({
      get_pipeline_summary_by_ae(ae_filter = ae_filter)
    })

    list(
      connected_calls = connected_calls,
      meetings        = meetings,
      new_customers   = new_customers,
      pipeline_by_ae  = pipeline_by_ae
    )
  })
}
