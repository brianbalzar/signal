source("../../utils/constants.R")
source("../../utils/helpers.R")
source("../../services/outreach_logic.R")
source("../../utils/ui_helpers.R")

test_that("research display parser handles raw JSON", {
  raw <- '{
    "summary": "Found a capital project.",
    "signals": ["bond project", "HVAC reference"],
    "suggested_reason_for_outreach": "capital project may create facility performance needs",
    "suggested_personalization_notes": "Mention the project softly.",
    "sources": ["https://example.com"]
  }'
  
  parsed <- parse_research_notes_for_display(raw)
  
  expect_true(parsed$has_research)
  expect_equal(parsed$summary, "Found a capital project.")
  expect_equal(length(parsed$signals), 2)
  expect_equal(parsed$sources, "https://example.com")
})

test_that("queue table formatter returns readable columns", {
  prospects <- data.frame(
    id = 1,
    name = "Ada Lovelace",
    company = "Analytical Engines Inc.",
    status = "Ready to Email",
    sequence_stage = 0,
    next_touch = "2026-05-06",
    segment = "Commercial Office",
    stringsAsFactors = FALSE
  )
  
  table_data <- format_queue_table_data(prospects)
  
  expect_true(all(c("Prospect", "Company", "Next Touch") %in% names(table_data)))
  expect_false("first_name" %in% names(table_data))
  expect_equal(table_data$Prospect, "Ada Lovelace")
  expect_equal(table_data$Stage, "0 - Intro Email")
})
