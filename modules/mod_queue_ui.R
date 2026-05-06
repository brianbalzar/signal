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
    
    fluidRow(
      column(
        width = 12,
        
        div(
          class = "panel-card",
          h3("Today's Outreach Queue"),
          p(
            "Prospects due today or overdue. ",
            "Prospects marked Replied, Not Interested, or Do Not Contact are excluded."
          ),
          
          uiOutput(ns("queue_counts")),
          
          fluidRow(
            column(
              width = 3,
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
              )
            ),
            
            column(
              width = 3,
              selectInput(
                ns("queue_segment_filter"),
                "Segment",
                choices = c("All", setdiff(PROSPECT_SEGMENTS, "")),
                selected = "All"
              )
            ),
            
            column(
              width = 3,
              selectInput(
                ns("queue_source_filter"),
                "Source",
                choices = c("All", setdiff(PROSPECT_SOURCES, "")),
                selected = "All"
              )
            )
          ),
          
          actionButton(ns("refresh_queue"), "Refresh Queue"),
          
          br(),
          br(),
          
          DTOutput(ns("queue_table"))
        )
      )
    ),
    
    fluidRow(
      column(
        width = 5,
        
        div(
          class = "panel-card",
          h3("Selected Prospect"),
          p("Select a prospect from the queue to view context and generate a draft."),
          verbatimTextOutput(ns("selected_summary")),
          
          actionButton(
            ns("research_prospect"),
            "Research Prospect"
          ),
          
          verbatimTextOutput(ns("research_summary")),
          
          h4("Touch History"),
          DTOutput(ns("touch_history_table")),
          
          h4("Draft History"),
          DTOutput(ns("draft_history_table"))
        ),
        
        div(
          class = "panel-card",
          h3("Recommended Next Step"),
          
          verbatimTextOutput(ns("recommended_action")),
          
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
          ),
          
          br(),
          br(),
          
          h4("End Workflow"),
          
          p("Use these when the pre-reply outreach loop is complete."),
          
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
      
      column(
        width = 7,
        
        div(
          class = "panel-card",
          h3("Draft"),
          
          p("Generate a draft, review/edit it here, then copy it into Gmail or your email client."),
          
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
