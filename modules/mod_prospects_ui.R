# modules/mod_prospects_ui.R

mod_prospects_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 12,

        div(
          class = "panel-card",
          div(
            class = "panel-title-row",
            div(
              h3("Prospect Intake"),
              p(class = "muted-text", "Atlas exports, duplicate review, and import.")
            ),
            actionButton(
              ns("confirm_import"),
              "Import Prospects",
              class = "btn-success"
            )
          ),

          fluidRow(
            column(
              width = 5,

              fileInput(
                ns("prospect_file"),
                "Upload Excel or CSV File",
                accept = c(
                  ".xlsx",
                  ".xls",
                  ".csv"
                )
              )
            ),

            column(
              width = 3,

              selectInput(
                ns("default_source"),
                "Default Source",
                choices = PROSPECT_SOURCES,
                selected = "Convex Atlas"
              )
            ),

            column(
              width = 3,

              selectInput(
                ns("default_segment"),
                "Default Segment",
                choices = PROSPECT_SEGMENTS,
                selected = ""
              )
            )
          ),

          checkboxInput(
            ns("skip_duplicates"),
            "Skip likely duplicates",
            value = TRUE
          ),

          actionButton(
            ns("preview_import"),
            "Preview Import",
            class = "btn-primary"
          ),

          uiOutput(ns("import_summary")),

          tags$details(
            class = "details-panel",
            tags$summary("Expected Columns"),
            tags$ul(
              tags$li("first_name"),
              tags$li("last_name"),
              tags$li("company"),
              tags$li("title"),
              tags$li("email"),
              tags$li("linkedin_url"),
              tags$li("website"),
              tags$li("city"),
              tags$li("state"),
              tags$li("source"),
              tags$li("segment"),
              tags$li("reason_for_outreach"),
              tags$li("personalization_notes")
            ),
            p(
              class = "helper-text",
              "Minimum: email or first_name + last_name + company."
            )
          )
        )
      )
    ),

    fluidRow(
      column(
        width = 12,

        div(
          class = "panel-card",
          div(
            class = "panel-title-row",
            h3("Import Preview"),
            span(class = "panel-kicker", "Ready, duplicate, invalid")
          ),

          uiOutput(ns("import_preview_ui"))
        )
      )
    ),

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
          textInput(ns("linkedin_url"), "LinkedIn URL"),
          textInput(ns("website"), "Website"),

          fluidRow(
            column(6, textInput(ns("city"), "City")),
            column(6, textInput(ns("state"), "State"))
          ),

          selectInput(
            ns("source"),
            "Source",
            choices = PROSPECT_SOURCES,
            selected = "Manual"
          ),

          selectInput(
            ns("segment"),
            "Segment",
            choices = PROSPECT_SEGMENTS
          ),

          textAreaInput(
            ns("reason_for_outreach"),
            "Reason for Outreach",
            rows = 3,
            placeholder = "Example: Hospital facility leader in Oncor territory"
          ),

          textAreaInput(
            ns("personalization_notes"),
            "Personalization Notes",
            rows = 4,
            placeholder = "Example: Website mentions a large Dallas campus. Use a soft facilities-performance angle."
          ),

          selectInput(
            ns("status"),
            "Status",
            choices = PROSPECT_STATUSES,
            selected = DEFAULT_PROSPECT_STATUS
          ),

          dateInput(ns("next_touch"), "Next Touch", value = Sys.Date()),

          checkboxInput(
            ns("allow_manual_duplicate"),
            "Allow likely duplicate",
            value = FALSE
          ),

          actionButton(
            ns("add_prospect"),
            "Add Prospect",
            class = "btn-primary"
          )
        )
      ),

      column(
        width = 8,

        div(
          class = "panel-card",
          div(
            class = "panel-title-row",
            div(
              h3("Prospects"),
              p(class = "muted-text", "Active, inactive, and completed records.")
            ),
            actionButton(ns("refresh_prospects"), "Refresh")
          ),

          div(
            class = "filter-grid",
            selectInput(
              ns("status_filter"),
              "Status",
              choices = c("All", PROSPECT_STATUSES),
              selected = "All"
            ),
            selectInput(
              ns("segment_filter"),
              "Segment",
              choices = c("All", PROSPECT_SEGMENTS),
              selected = "All"
            )
          ),

          actionButton(
            ns("export_data"),
            "Export Data"
          ),

          uiOutput(ns("export_summary")),

          br(),

          DTOutput(ns("prospects_table"))
        ),

        div(
          class = "panel-card selected-panel",
          div(
            class = "panel-title-row",
            h3("Selected Prospect"),
            uiOutput(ns("selected_status_badge"))
          ),

          uiOutput(ns("selected_prospect_summary")),

          tabsetPanel(
            type = "pills",
            tabPanel(
              "Details",
              fluidRow(
                column(6, textInput(ns("selected_first_name"), "First Name")),
                column(6, textInput(ns("selected_last_name"), "Last Name"))
              ),

              textInput(ns("selected_company"), "Company"),
              textInput(ns("selected_title"), "Title"),
              textInput(ns("selected_email"), "Email"),
              textInput(ns("selected_linkedin_url"), "LinkedIn URL"),
              textInput(ns("selected_website"), "Website"),

              fluidRow(
                column(6, textInput(ns("selected_city"), "City")),
                column(6, textInput(ns("selected_state"), "State"))
              ),

              fluidRow(
                column(
                  width = 6,
                  selectInput(
                    ns("selected_source"),
                    "Source",
                    choices = PROSPECT_SOURCES
                  )
                ),

                column(
                  width = 6,
                  selectInput(
                    ns("selected_segment"),
                    "Segment",
                    choices = PROSPECT_SEGMENTS
                  )
                )
              )
            ),

            tabPanel(
              "Notes",
              textAreaInput(
                ns("selected_reason_for_outreach"),
                "Reason for Outreach",
                rows = 4
              ),

              textAreaInput(
                ns("selected_personalization_notes"),
                "Personalization Notes",
                rows = 5
              )
            ),

            tabPanel(
              "Workflow",
              fluidRow(
                column(
                  width = 6,
                  selectInput(
                    ns("selected_status"),
                    "Status",
                    choices = PROSPECT_STATUSES,
                    selected = DEFAULT_PROSPECT_STATUS
                  )
                ),

                column(
                  width = 6,
                  selectInput(
                    ns("selected_sequence_stage"),
                    "Sequence Stage",
                    choices = setNames(SEQUENCE_STAGES, paste0(
                      SEQUENCE_STAGES,
                      " - ",
                      unname(SEQUENCE_STAGE_LABELS[as.character(SEQUENCE_STAGES)])
                    )),
                    selected = DEFAULT_SEQUENCE_STAGE
                  )
                )
              ),

              fluidRow(
                column(
                  width = 6,
                  dateInput(
                    ns("selected_next_touch"),
                    "Next Touch",
                    value = Sys.Date()
                  )
                )
              ),

              textAreaInput(
                ns("reply_notes"),
                "Reply / Outcome Notes",
                rows = 4,
                placeholder = "Example: Replied and asked for availability next week."
              )
            )
          ),

          div(
            class = "button-row form-footer",
            actionButton(
              ns("update_status"),
              "Save Prospect",
              class = "btn-primary"
            ),

            actionButton(
              ns("delete_prospect"),
              "Delete Prospect",
              class = "btn-danger"
            )
          )
        )
      )
    )
  )
}
