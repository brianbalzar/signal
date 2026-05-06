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
    "signals": ["bond project", "HVAC reference", "board agenda"],
    "suggested_reason_for_outreach": "capital project may create facility performance needs",
    "suggested_personalization_notes": "Mention the project softly.",
    "sources": ["https://example.com", "https://example.com/agenda"]
  }'

  parsed <- parse_claude_research_response(raw)
  formatted <- format_research_notes(parsed)

  expect_equal(parsed$summary, "Found a capital project.")
  expect_equal(length(parsed$signals), 3)
  expect_equal(length(parsed$sources), 2)
  expect_true(grepl("board agenda", formatted, fixed = TRUE))
  expect_true(grepl("https://example.com/agenda", formatted, fixed = TRUE))
})

test_that("call prep response parser handles valid JSON", {
  raw <- '{
    "objective": "Start a practical facilities conversation.",
    "opener": "I noticed your campus work and had a quick question.",
    "talking_points": ["ask about controls", "mention analytics softly"],
    "discovery_questions": ["What is hardest to manage right now?"],
    "voicemail": "I had a quick facilities-performance question.",
    "follow_up_angle": "Send a short note after the call attempt."
  }'

  parsed <- parse_claude_call_prep_response(raw)
  formatted <- format_call_prep_notes(parsed)

  expect_equal(parsed$objective, "Start a practical facilities conversation.")
  expect_equal(length(parsed$talking_points), 2)
  expect_true(grepl("Discovery Questions:", formatted, fixed = TRUE))
  expect_true(grepl("Voicemail:", formatted, fixed = TRUE))
})

test_that("fallback call prep includes seller talking sections", {
  prospect <- data.frame(
    first_name = "Ada",
    last_name = "Lovelace",
    company = "Analytical Engines Inc.",
    reason_for_outreach = "large campus with likely controls complexity",
    research_notes = "",
    stringsAsFactors = FALSE
  )

  prep <- fallback_generate_call_prep_from_claude_service(
    prospect,
    error_message = "Missing API key"
  )

  expect_true(grepl("^Call prep:", prep$subject))
  expect_true(grepl("Talking Points:", prep$body, fixed = TRUE))
  expect_true(grepl("Discovery Questions:", prep$body, fixed = TRUE))
  expect_true(grepl("Follow-Up Angle:", prep$body, fixed = TRUE))
})
