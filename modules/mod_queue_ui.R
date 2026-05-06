# modules/mod_queue_ui.R

mod_queue_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('copy-draft-to-clipboard', function(message) {
        var text = message.text || '';

        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(text);
          return;
        }

        var textArea = document.createElement('textarea');
        textArea.value = text;
        textArea.style.position = 'fixed';
        textArea.style.left = '-9999px';
        document.body.appendChild(textArea);
        textArea.focus();
        textArea.select();
        document.execCommand('copy');
        document.body.removeChild(textArea);
      });
    ")),

    div(
      class = "panel-card workbench-toolbar",
      div(
        class = "panel-title-row",
        div(
          h3("Outreach Queue"),
          p(class = "muted-text", "Due work, active prospects, and nurture follow-up.")
        ),
        actionButton(ns("refresh_queue"), "Refresh")
      ),

      uiOutput(ns("queue_counts")),

      div(
        class = "filter-grid",
        selectInput(
          ns("queue_scope"),
          "Queue View",
          choices = c(
            "Due or Overdue",
            "Overdue",
            "Due Today",
            "Nurture",
            "All Active"
          ),
          selected = "Due or Overdue"
        ),

        selectInput(
          ns("queue_segment_filter"),
          "Segment",
          choices = c("All", setdiff(PROSPECT_SEGMENTS, "")),
          selected = "All"
        ),

        selectInput(
          ns("queue_source_filter"),
          "Source",
          choices = c("All", setdiff(PROSPECT_SOURCES, "")),
          selected = "All"
        )
      )
    ),

    fluidRow(
      column(
        width = 7,

        div(
          class = "panel-card",
          div(
            class = "panel-title-row",
            h3("Queue"),
            span(class = "panel-kicker", "Select one row")
          ),
          DTOutput(ns("queue_table"))
        )
      ),

      column(
        width = 5,

        div(
          class = "panel-card selected-panel",
          div(
            class = "panel-title-row",
            h3("Prospect"),
            uiOutput(ns("selected_status_badge"))
          ),
          uiOutput(ns("selected_summary")),

          div(
            class = "button-row",
            actionButton(
              ns("research_prospect"),
              "Research Prospect"
            )
          ),

          uiOutput(ns("research_summary"))
        )
      )
    ),

    fluidRow(
      column(
        width = 5,

        div(
          class = "panel-card",
          h3("Next Step"),

          uiOutput(ns("recommended_action")),

          div(
            class = "action-group",
            h4("Draft"),
            actionButton(
              ns("generate_draft"),
              "Generate Draft",
              class = "btn-primary"
            ),

            actionButton(
              ns("generate_local_draft"),
              "Create Local Draft"
            ),

            actionButton(
              ns("log_sent"),
              "Log Email as Sent"
            ),

            actionButton(
              ns("snooze"),
              paste0("Snooze ", DEFAULT_QUEUE_SNOOZE_DAYS, " Days")
            )
          ),

          div(
            class = "action-group",
            h4("Outcome"),
            actionButton(
              ns("mark_replied"),
              "Mark Replied",
              class = "btn-success"
            ),

            actionButton(
              ns("mark_not_interested"),
              "Not Interested",
              class = "btn-warning"
            ),

            actionButton(
              ns("mark_bounced"),
              "Mark Bounced",
              class = "btn-warning"
            ),

            actionButton(
              ns("mark_dnc"),
              "Do Not Contact",
              class = "btn-danger"
            )
          )
        ),

        div(
          class = "panel-card",
          h3("History"),
          tabsetPanel(
            type = "pills",
            tabPanel("Touches", DTOutput(ns("touch_history_table"))),
            tabPanel("Drafts", DTOutput(ns("draft_history_table")))
          )
        )
      ),

      column(
        width = 7,

        div(
          class = "panel-card",
          h3("Draft"),
          textInput(ns("draft_subject"), "Subject"),

          textAreaInput(
            ns("draft_body"),
            "Body",
            rows = 16,
            placeholder = "Generated email draft will appear here."
          ),

          actionButton(
            ns("copy_draft"),
            "Copy Draft"
          ),

          uiOutput(ns("open_outlook_link")),

          br(),

          p(
            class = "helper-text",
            "Sending integration comes later. For now, this app tracks the workflow and saves the draft/touch history."
          )
        )
      )
    )
  )
}
