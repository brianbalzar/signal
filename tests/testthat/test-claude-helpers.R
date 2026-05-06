source("../../utils/constants.R")
source("../../utils/helpers.R")
source("../../services/outreach_logic.R")
source("../../services/claude.R")

test_that("fallback draft does not include internal diagnostic notes", {
  prospect <- data.frame(
    first_name = "Ada",
    last_name = "Lovelace",
    company = "Analytical Engines Inc.",
    title = "Facilities Director",
    email = "ada@example.com",
    source = "Manual",
    segment = "Commercial Office",
    reason_for_outreach = "large campus with likely controls complexity",
    personalization_notes = "Use a soft approach.",
    research_notes = "Research Summary: Public capital plan found.",
    sequence_stage = 0,
    status = "Ready to Email",
    stringsAsFactors = FALSE
  )
  
  draft <- fallback_generate_email_from_claude_service(
    prospect,
    error_message = "Missing API key"
  )
  
  expect_false(grepl("Internal note", draft$body, fixed = TRUE))
  expect_false(grepl("Internal personalization note", draft$body, fixed = TRUE))
  expect_false(grepl("Internal research note", draft$body, fixed = TRUE))
  expect_true(grepl("Best,\\s*Brian$", draft$body, perl = TRUE))
})

test_that("research response parser handles valid JSON", {
  raw <- '{
    "summary": "Found a capital project.",
    "signals": ["bond project", "HVAC reference"],
    "suggested_reason_for_outreach": "capital project may create facility performance needs",
    "suggested_personalization_notes": "Mention the project softly.",
    "sources": ["https://example.com"]
  }'
  
  parsed <- parse_claude_research_response(raw)
  
  expect_equal(parsed$summary, "Found a capital project.")
  expect_equal(length(parsed$signals), 2)
  expect_equal(parsed$sources, "https://example.com")
})
