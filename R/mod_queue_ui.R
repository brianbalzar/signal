# modules/mod_queue_ui.R

mod_queue_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$script(HTML(sprintf("
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

      function setResearchProgress(active, stage) {
        var el = document.getElementById('%s');
        if (!el) {
          return;
        }

        var stageEl = el.querySelector('[data-research-stage]');
        var timerEl = el.querySelector('[data-research-timer]');

        if (stageEl && stage) {
          stageEl.textContent = stage;
        }

        if (active) {
          el.classList.add('active');
          var started = Date.now();

          if (window.signalResearchTimer) {
            clearInterval(window.signalResearchTimer);
          }

          if (timerEl) {
            timerEl.textContent = '0s';
          }

          window.signalResearchTimer = setInterval(function() {
            if (timerEl) {
              timerEl.textContent = Math.round((Date.now() - started) / 1000) + 's';
            }
          }, 1000);
        } else {
          el.classList.remove('active');

          if (window.signalResearchTimer) {
            clearInterval(window.signalResearchTimer);
            window.signalResearchTimer = null;
          }
        }
      }

      Shiny.addCustomMessageHandler('research-progress-state', function(message) {
        setResearchProgress(!!message.active, message.stage || 'Researching public signals...');
      });

      document.addEventListener('click', function(event) {
        var target = event.target;

        if (!target) {
          return;
        }

        if (target.id === '%s' || target.id === '%s') {
          setResearchProgress(true, 'Researching public signals...');
        }
      });
    ", ns("research_progress"), ns("research_prospect"), ns("refresh_research")))),

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
      uiOutput(ns("today_focus")),

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
            span(class = "panel-kicker", "Double-click for details")
          ),
          uiOutput(ns("queue_table_ui"))
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

          uiOutput(ns("prospect_action_buttons")),
          div(
            id = ns("research_progress"),
            class = "research-progress",
            div(class = "research-spinner"),
            div(
              class = "research-progress-copy",
              strong("Research in progress"),
              span(`data-research-stage` = "", "Researching public signals..."),
              tags$small(`data-research-timer` = "", "0s")
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
            h4("Email"),
            uiOutput(ns("draft_action_buttons"))
          ),

          div(
            class = "action-group",
            h4("Call"),
            uiOutput(ns("call_action_buttons"))
          ),

          div(
            class = "action-group",
            h4("Outcome"),
            uiOutput(ns("outcome_action_buttons"))
          )
        ),

        div(
          class = "panel-card",
          h3("History"),
          tabsetPanel(
            type = "pills",
            tabPanel("Touches", uiOutput(ns("touch_history_ui"))),
            tabPanel("Drafts", uiOutput(ns("draft_history_ui")))
          )
        )
      ),

      column(
        width = 7,

        div(
          class = "panel-card prep-card",
          h3("Outreach Prep"),

          tabsetPanel(
            type = "pills",
            tabPanel(
              "Email",
              div(
                class = "draft-card",
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
            ),

            tabPanel(
              "Call",
              div(
                class = "call-card",
                uiOutput(ns("call_contact_header")),
                textAreaInput(
                  ns("call_prep_body"),
                  "Call Prep",
                  rows = 15,
                  placeholder = "Generated call talking points will appear here."
                ),

                fluidRow(
                  column(
                    width = 6,
                    selectInput(
                      ns("call_outcome"),
                      "Call Outcome",
                      choices = CALL_OUTCOMES,
                      selected = DEFAULT_CALL_OUTCOME
                    )
                  ),
                  column(
                    width = 6,
                    dateInput(
                      ns("call_next_touch"),
                      "Next Touch",
                      value = Sys.Date() + DEFAULT_CALL_BACK_DAYS
                    )
                  )
                ),

                textAreaInput(
                  ns("call_notes"),
                  "Call Notes",
                  rows = 5,
                  placeholder = "What happened on the call?"
                ),

                uiOutput(ns("call_log_buttons"))
              )
            )
          )
        )
      )
    )
  )
}
