library(DBI)
suppressWarnings(library(RSQLite))

source("../../utils/constants.R")
source("../../utils/helpers.R")
source("../../services/outreach_logic.R")
source("../../services/db.R")

with_test_db <- function(code) {
  old_path <- getOption("signal.db_path", NULL)
  db_path <- tempfile(fileext = ".sqlite")

  options(signal.db_path = db_path)

  on.exit({
    if (is.null(old_path)) {
      options(signal.db_path = NULL)
    } else {
      options(signal.db_path = old_path)
    }

    unlink(db_path, force = TRUE)
  }, add = TRUE)

  init_db()

  eval.parent(substitute(code))
}

create_test_prospect <- function(next_touch = Sys.Date(), sequence_stage = 0) {
  create_prospect(list(
    first_name = "Test",
    last_name = "Prospect",
    company = "Test Co",
    title = "Facilities Director",
    email = "test@example.com",
    source = "Manual",
    segment = "Commercial Office",
    reason_for_outreach = "Testing workflow",
    personalization_notes = "Test notes",
    status = "Ready to Email",
    sequence_stage = sequence_stage,
    next_touch = as.character(next_touch)
  ))

  get_prospects(include_inactive = TRUE)$id[1]
}

test_that("database initialization records a baseline migration", {
  with_test_db({
    con <- get_db()
    on.exit(dbDisconnect(con), add = TRUE)

    expect_true("schema_migrations" %in% dbListTables(con))

    migrations <- dbGetQuery(con, "SELECT id FROM schema_migrations")

    expect_true("001_initial_schema" %in% migrations$id)
    expect_true("002_add_research_fields" %in% migrations$id)

    prospect_columns <- dbGetQuery(con, "PRAGMA table_info(prospects)")

    expect_true("research_notes" %in% prospect_columns$name)
    expect_true("research_sources" %in% prospect_columns$name)
    expect_true("researched_at" %in% prospect_columns$name)
  })
})

test_that("snoozing a prospect moves the next touch date", {
  with_test_db({
    prospect_id <- create_test_prospect(next_touch = Sys.Date())

    snooze_prospect(prospect_id, days = 4)

    prospect <- get_prospect_by_id(prospect_id)

    expect_equal(as.Date(prospect$next_touch), Sys.Date() + 4)
  })
})

test_that("logging a sent touch advances status and sequence stage", {
  with_test_db({
    prospect_id <- create_test_prospect(sequence_stage = 0)

    log_touch(
      prospect_id = prospect_id,
      touch_type = "Email",
      subject = "Hello",
      body = "Body",
      outcome = "Sent",
      sequence_stage = 0
    )

    prospect <- get_prospect_by_id(prospect_id)
    touches <- get_touches_for_prospect(prospect_id)

    expect_equal(nrow(touches), 1)
    expect_equal(prospect$status, "Intro Sent")
    expect_equal(prospect$sequence_stage, 1)
    expect_equal(as.Date(prospect$next_touch), Sys.Date() + 3)
  })
})

test_that("terminal touch outcomes remove prospects from the active queue", {
  with_test_db({
    prospect_id <- create_test_prospect(sequence_stage = 1)

    log_touch(
      prospect_id = prospect_id,
      touch_type = "Email",
      outcome = "Replied",
      sequence_stage = 1
    )

    prospect <- get_prospect_by_id(prospect_id)
    queue <- get_outreach_queue()

    expect_equal(prospect$status, "Replied")
    expect_true(is.na(prospect$next_touch))
    expect_false(prospect_id %in% queue$id)
  })
})

test_that("bounced touch outcomes keep prospects active for correction", {
  with_test_db({
    prospect_id <- create_test_prospect(sequence_stage = 1)

    log_touch(
      prospect_id = prospect_id,
      touch_type = "Email",
      outcome = "Bounced",
      sequence_stage = 1
    )

    prospect <- get_prospect_by_id(prospect_id)
    queue <- get_outreach_queue()

    expect_equal(prospect$status, "Bounced")
    expect_true(is.na(prospect$next_touch))
    expect_true(prospect_id %in% queue$id)
  })
})

test_that("logging a call can keep the email sequence in place", {
  with_test_db({
    prospect_id <- create_test_prospect(sequence_stage = 1)

    log_touch(
      prospect_id = prospect_id,
      touch_type = "Call",
      subject = "Call: Connected",
      body = "Call notes",
      outcome = "Connected",
      sequence_stage = 1,
      advance_sequence = FALSE,
      next_touch = as.character(Sys.Date() + 2)
    )

    prospect <- get_prospect_by_id(prospect_id)
    touches <- get_touches_for_prospect(prospect_id)

    expect_equal(nrow(touches), 1)
    expect_equal(prospect$status, "Ready to Email")
    expect_equal(prospect$sequence_stage, 1)
    expect_equal(as.Date(prospect$next_touch), Sys.Date() + 2)
    expect_equal(touches$touch_type[1], "Call")
    expect_equal(touches$outcome[1], "Connected")
  })
})

test_that("export creates csv snapshots and a sqlite copy", {
  with_test_db({
    create_test_prospect()

    export_root <- tempfile("signal_exports_")
    export_dir <- export_signal_data(export_root = export_root)

    expect_true(dir.exists(export_dir))
    expect_true(file.exists(file.path(export_dir, "prospects.csv")))
    expect_true(file.exists(file.path(export_dir, "touches.csv")))
    expect_true(file.exists(file.path(export_dir, "drafts.csv")))
    expect_true(file.exists(file.path(export_dir, "schema_migrations.csv")))
    expect_true(file.exists(file.path(export_dir, basename(get_db_path()))))

    unlink(export_root, recursive = TRUE, force = TRUE)
  })
})

test_that("prospect research can be stored without changing outreach notes", {
  with_test_db({
    prospect_id <- create_test_prospect()

    update_prospect_research(
      prospect_id = prospect_id,
      research_notes = "Public bond project found.",
      research_sources = "https://example.com/source"
    )

    prospect <- get_prospect_by_id(prospect_id)

    expect_equal(prospect$reason_for_outreach, "Testing workflow")
    expect_equal(prospect$personalization_notes, "Test notes")
    expect_equal(prospect$research_notes, "Public bond project found.")
    expect_equal(prospect$research_sources, "https://example.com/source")
    expect_false(is.na(prospect$researched_at))
  })
})

test_that("organization research can be shared across matching company prospects", {
  with_test_db({
    create_test_prospect()
    create_prospect(list(
      first_name = "Second",
      last_name = "Prospect",
      company = " test co ",
      title = "Operations Manager",
      email = "second@example.com",
      source = "Manual",
      segment = "Commercial Office",
      status = "Ready to Email",
      sequence_stage = 0,
      next_touch = as.character(Sys.Date())
    ))

    affected <- update_company_research(
      company = "Test Co",
      research_notes = "Shared organization research",
      research_sources = "https://example.com/source",
      researched_at = "2026-05-06 09:00:00"
    )

    prospects <- get_prospects(include_inactive = TRUE)
    company_matches <- tolower(trimws(prospects$company)) == "test co"
    cached <- get_company_research("TEST CO")

    expect_equal(affected, 2)
    expect_true(all(prospects$research_notes[company_matches] == "Shared organization research"))
    expect_equal(cached$research_notes, "Shared organization research")
    expect_equal(cached$research_sources, "https://example.com/source")
  })
})
