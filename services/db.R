# services/db.R
# Database layer for Signal
#
# Signal is intentionally NOT a full CRM.
# It is a lightweight outbound workbench for getting a first reply.
#
# Workflow:
#   Prospect added -> email/follow-up sequence -> Replied / Not Interested / Do Not Contact
#   Once a prospect replies, they leave the active outreach queue.
#
# This file should only handle database work.
# Cadence/status logic belongs in services/outreach_logic.R.

# ---- Connection -------------------------------------------------------------

get_db_path <- function() {
  db_path <- getOption("signal.db_path", NULL)

  if (is.null(db_path) || db_path == "") {
    db_path <- Sys.getenv("SIGNAL_DB_PATH", unset = "")
  }

  if (is.null(db_path) || db_path == "") {
    db_path <- default_signal_db_path()
    seed_local_db_from_project_copy(db_path)
  }

  db_path
}

default_signal_db_path <- function() {
  candidates <- c(
    file.path(Sys.getenv("LOCALAPPDATA", unset = ""), "Signal"),
    file.path(tempdir(), "Signal")
  )

  candidates <- candidates[candidates != "" & !is.na(candidates)]

  for (candidate in candidates) {
    if (directory_is_writable(candidate)) {
      return(file.path(candidate, "signal.db"))
    }
  }

  file.path(tempdir(), "Signal", "signal.db")
}

seed_local_db_from_project_copy <- function(db_path) {
  if (file.exists(db_path)) {
    return(invisible(FALSE))
  }

  seed_candidates <- c(
    "data/signal.local.sqlite",
    "data/signal.sqlite"
  )

  seed_candidates <- seed_candidates[file.exists(seed_candidates)]

  if (length(seed_candidates) == 0) {
    return(invisible(FALSE))
  }

  ensure_db_directory(db_path)

  for (seed_path in seed_candidates) {
    file.copy(seed_path, db_path, overwrite = TRUE)

    if (sqlite_file_is_readable(db_path)) {
      return(invisible(TRUE))
    }

    unlink(db_path, force = TRUE)
  }

  invisible(FALSE)
}

directory_is_writable <- function(path) {
  if (!dir.exists(path)) {
    ok <- suppressWarnings(dir.create(path, recursive = TRUE, showWarnings = FALSE))

    if (!isTRUE(ok) && !dir.exists(path)) {
      return(FALSE)
    }
  }

  test_file <- tempfile(tmpdir = path)

  ok <- tryCatch(
    {
      file.create(test_file)
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )

  unlink(test_file, force = TRUE)
  isTRUE(ok)
}

sqlite_file_is_readable <- function(path) {
  tryCatch(
    {
      con <- dbConnect(SQLite(), path, synchronous = NULL)
      on.exit(dbDisconnect(con), add = TRUE)
      dbListTables(con)
      TRUE
    },
    error = function(e) FALSE
  )
}

ensure_db_directory <- function(db_path) {
  db_dir <- dirname(db_path)

  if (db_dir != "." && !dir.exists(db_dir)) {
    dir.create(db_dir, recursive = TRUE)
  }

  invisible(TRUE)
}

get_db <- function() {
  dbConnect(SQLite(), get_db_path(), synchronous = NULL)
}


# ---- Initialization ---------------------------------------------------------

init_db <- function() {
  ensure_db_directory(get_db_path())

  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS prospects (
      id INTEGER PRIMARY KEY AUTOINCREMENT,

      first_name TEXT,
      last_name TEXT,
      company TEXT,
      title TEXT,
      email TEXT,
      linkedin_url TEXT,
      website TEXT,

      city TEXT,
      state TEXT,

      source TEXT,
      segment TEXT,

      reason_for_outreach TEXT,
      personalization_notes TEXT,
      research_notes TEXT,
      research_sources TEXT,
      researched_at TEXT,

      status TEXT DEFAULT 'Not Started',
      sequence_stage INTEGER DEFAULT 0,

      last_touch TEXT,
      next_touch TEXT,

      reply_notes TEXT,

      created_at TEXT,
      updated_at TEXT
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS touches (
      id INTEGER PRIMARY KEY AUTOINCREMENT,

      prospect_id INTEGER,

      touch_type TEXT,
      subject TEXT,
      body TEXT,
      outcome TEXT,

      sequence_stage INTEGER,

      created_at TEXT,

      FOREIGN KEY(prospect_id) REFERENCES prospects(id)
    )
  ")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS drafts (
      id INTEGER PRIMARY KEY AUTOINCREMENT,

      prospect_id INTEGER,

      sequence_stage INTEGER,
      subject TEXT,
      body TEXT,

      status TEXT DEFAULT 'Draft',

      created_at TEXT,
      updated_at TEXT,

      FOREIGN KEY(prospect_id) REFERENCES prospects(id)
    )
  ")

  create_indexes(con)
  apply_schema_migrations(con)
}


apply_schema_migrations <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS schema_migrations (
      id TEXT PRIMARY KEY,
      applied_at TEXT
    )
  ")

  record_schema_migration(con, "001_initial_schema")
  run_schema_migration(con, "002_add_research_fields", function() {
    add_column_if_missing(con, "prospects", "research_notes", "TEXT")
    add_column_if_missing(con, "prospects", "research_sources", "TEXT")
    add_column_if_missing(con, "prospects", "researched_at", "TEXT")
  })

  invisible(TRUE)
}


schema_migration_applied <- function(con, migration_id) {
  result <- dbGetQuery(
    con,
    "
    SELECT id
    FROM schema_migrations
    WHERE id = ?
    ",
    params = list(migration_id)
  )

  nrow(result) > 0
}


run_schema_migration <- function(con, migration_id, migrate) {
  if (schema_migration_applied(con, migration_id)) {
    return(invisible(FALSE))
  }

  migrate()
  record_schema_migration(con, migration_id)

  invisible(TRUE)
}


record_schema_migration <- function(con, migration_id) {
  dbExecute(
    con,
    "
    INSERT OR IGNORE INTO schema_migrations (
      id,
      applied_at
    ) VALUES (?, ?)
    ",
    params = list(
      migration_id,
      as.character(Sys.time())
    )
  )

  invisible(TRUE)
}


add_column_if_missing <- function(con, table_name, column_name, column_type) {
  columns <- dbGetQuery(con, paste0("PRAGMA table_info(", table_name, ")"))

  if (column_name %in% columns$name) {
    return(invisible(FALSE))
  }

  dbExecute(
    con,
    paste(
      "ALTER TABLE",
      table_name,
      "ADD COLUMN",
      column_name,
      column_type
    )
  )

  invisible(TRUE)
}


create_indexes <- function(con) {
  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_prospects_status
    ON prospects(status)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_prospects_next_touch
    ON prospects(next_touch)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_prospects_sequence_stage
    ON prospects(sequence_stage)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_touches_prospect_id
    ON touches(prospect_id)
  ")

  dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_drafts_prospect_id
    ON drafts(prospect_id)
  ")
}


# ---- Prospect helpers -------------------------------------------------------

create_prospect <- function(prospect) {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  now <- as.character(Sys.time())

  status <- normalize_status(prospect$status %||% DEFAULT_PROSPECT_STATUS)
  sequence_stage <- normalize_sequence_stage(prospect$sequence_stage %||% DEFAULT_SEQUENCE_STAGE)

  next_touch <- prospect$next_touch
  if (is.null(next_touch) || is.na(next_touch) || next_touch == "") {
    next_touch <- as.character(Sys.Date())
  } else {
    next_touch <- as.character(next_touch)
  }

  dbExecute(
    con,
    "
    INSERT INTO prospects (
      first_name,
      last_name,
      company,
      title,
      email,
      linkedin_url,
      website,
      city,
      state,
      source,
      segment,
      reason_for_outreach,
      personalization_notes,
      research_notes,
      research_sources,
      researched_at,
      status,
      sequence_stage,
      last_touch,
      next_touch,
      reply_notes,
      created_at,
      updated_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ",
    params = list(
      prospect$first_name %||% NA_character_,
      prospect$last_name %||% NA_character_,
      prospect$company %||% NA_character_,
      prospect$title %||% NA_character_,
      prospect$email %||% NA_character_,
      prospect$linkedin_url %||% NA_character_,
      prospect$website %||% NA_character_,
      prospect$city %||% NA_character_,
      prospect$state %||% NA_character_,
      prospect$source %||% NA_character_,
      prospect$segment %||% NA_character_,
      prospect$reason_for_outreach %||% NA_character_,
      prospect$personalization_notes %||% NA_character_,
      prospect$research_notes %||% NA_character_,
      prospect$research_sources %||% NA_character_,
      prospect$researched_at %||% NA_character_,
      status,
      sequence_stage,
      NA_character_,
      next_touch,
      prospect$reply_notes %||% NA_character_,
      now,
      now
    )
  )

  invisible(TRUE)
}


get_prospects <- function(include_inactive = TRUE) {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  where_clause <- ""

  if (!include_inactive) {
    where_clause <- "
      WHERE status NOT IN ('Replied', 'Not Interested', 'Do Not Contact')
    "
  }

  dbGetQuery(
    con,
    paste0(
      "
      SELECT
        id,
        trim(coalesce(first_name, '') || ' ' || coalesce(last_name, '')) AS name,
        first_name,
        last_name,
        company,
        title,
        email,
        linkedin_url,
        website,
        city,
        state,
        source,
        segment,
        reason_for_outreach,
        personalization_notes,
        research_notes,
        research_sources,
        researched_at,
        status,
        sequence_stage,
        last_touch,
        next_touch,
        reply_notes,
        created_at,
        updated_at
      FROM prospects
      ",
      where_clause,
      "
      ORDER BY
        CASE
          WHEN status IN ('Replied', 'Not Interested', 'Do Not Contact') THEN 1
          ELSE 0
        END,
        CASE
          WHEN next_touch IS NULL OR next_touch = '' THEN 1
          ELSE 0
        END,
        next_touch ASC,
        company ASC,
        last_name ASC
      "
    )
  )
}


get_active_prospects <- function() {
  get_prospects(include_inactive = FALSE)
}


get_prospect_by_id <- function(prospect_id) {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  result <- dbGetQuery(
    con,
    "
    SELECT *
    FROM prospects
    WHERE id = ?
    ",
    params = list(prospect_id)
  )

  if (nrow(result) == 0) {
    return(NULL)
  }

  result[1, ]
}


update_prospect <- function(prospect_id, prospect) {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  status <- normalize_status(prospect$status %||% DEFAULT_PROSPECT_STATUS)
  sequence_stage <- normalize_sequence_stage(prospect$sequence_stage %||% DEFAULT_SEQUENCE_STAGE)

  next_touch <- prospect$next_touch
  if (is.null(next_touch) || is.na(next_touch) || next_touch == "") {
    next_touch <- NA_character_
  } else {
    next_touch <- as.character(next_touch)
  }

  if (is_terminal_status(status)) {
    next_touch <- NA_character_
  }

  dbExecute(
    con,
    "
    UPDATE prospects
    SET
      first_name = ?,
      last_name = ?,
      company = ?,
      title = ?,
      email = ?,
      linkedin_url = ?,
      website = ?,
      city = ?,
      state = ?,
      source = ?,
      segment = ?,
      reason_for_outreach = ?,
      personalization_notes = ?,
      research_notes = ?,
      research_sources = ?,
      researched_at = ?,
      status = ?,
      sequence_stage = ?,
      next_touch = ?,
      reply_notes = ?,
      updated_at = ?
    WHERE id = ?
    ",
    params = list(
      prospect$first_name %||% NA_character_,
      prospect$last_name %||% NA_character_,
      prospect$company %||% NA_character_,
      prospect$title %||% NA_character_,
      prospect$email %||% NA_character_,
      prospect$linkedin_url %||% NA_character_,
      prospect$website %||% NA_character_,
      prospect$city %||% NA_character_,
      prospect$state %||% NA_character_,
      prospect$source %||% NA_character_,
      prospect$segment %||% NA_character_,
      prospect$reason_for_outreach %||% NA_character_,
      prospect$personalization_notes %||% NA_character_,
      prospect$research_notes %||% NA_character_,
      prospect$research_sources %||% NA_character_,
      prospect$researched_at %||% NA_character_,
      status,
      sequence_stage,
      next_touch,
      prospect$reply_notes %||% NA_character_,
      as.character(Sys.time()),
      prospect_id
    )
  )

  invisible(TRUE)
}


update_prospect_status <- function(prospect_id, status, reply_notes = NULL) {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  now <- as.character(Sys.time())
  status <- normalize_status(status)

  if (is_terminal_status(status)) {
    dbExecute(
      con,
      "
      UPDATE prospects
      SET
        status = ?,
        reply_notes = ?,
        next_touch = NULL,
        updated_at = ?
      WHERE id = ?
      ",
      params = list(
        status,
        reply_notes %||% NA_character_,
        now,
        prospect_id
      )
    )
  } else {
    dbExecute(
      con,
      "
      UPDATE prospects
      SET
        status = ?,
        reply_notes = ?,
        updated_at = ?
      WHERE id = ?
      ",
      params = list(
        status,
        reply_notes %||% NA_character_,
        now,
        prospect_id
      )
    )
  }

  invisible(TRUE)
}


delete_prospect <- function(prospect_id) {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  dbExecute(con, "DELETE FROM drafts WHERE prospect_id = ?", params = list(prospect_id))
  dbExecute(con, "DELETE FROM touches WHERE prospect_id = ?", params = list(prospect_id))
  dbExecute(con, "DELETE FROM prospects WHERE id = ?", params = list(prospect_id))

  invisible(TRUE)
}


# ---- Outreach queue ---------------------------------------------------------

get_outreach_queue <- function() {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  today <- as.character(Sys.Date())

  dbGetQuery(
    con,
    "
    SELECT
      id,
      trim(coalesce(first_name, '') || ' ' || coalesce(last_name, '')) AS name,
      company,
      title,
      email,
      source,
      segment,
      reason_for_outreach,
      status,
      sequence_stage,
      last_touch,
      next_touch
    FROM prospects
    WHERE
      status NOT IN ('Replied', 'Not Interested', 'Do Not Contact')
      AND (
        next_touch IS NULL
        OR next_touch = ''
        OR next_touch <= ?
      )
    ORDER BY
      CASE
        WHEN next_touch IS NULL OR next_touch = '' THEN 1
        ELSE 0
      END,
      next_touch ASC,
      company ASC,
      last_name ASC
    ",
    params = list(today)
  )
}


snooze_prospect <- function(prospect_id, days = DEFAULT_QUEUE_SNOOZE_DAYS) {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  days <- suppressWarnings(as.integer(days))
  if (is.na(days) || days < 1) {
    days <- DEFAULT_QUEUE_SNOOZE_DAYS
  }

  next_touch <- as.character(Sys.Date() + days)

  dbExecute(
    con,
    "
    UPDATE prospects
    SET
      next_touch = ?,
      updated_at = ?
    WHERE id = ?
    ",
    params = list(
      next_touch,
      as.character(Sys.time()),
      prospect_id
    )
  )

  invisible(TRUE)
}


# ---- Touch helpers ----------------------------------------------------------

log_touch <- function(
    prospect_id,
    touch_type = DEFAULT_TOUCH_TYPE,
    subject = NULL,
    body = NULL,
    outcome = DEFAULT_TOUCH_OUTCOME,
    sequence_stage = NULL,
    advance_sequence = TRUE,
    next_touch = NULL
) {
  prospect <- get_prospect_by_id(prospect_id)

  if (is.null(prospect)) {
    stop("Prospect not found.", call. = FALSE)
  }

  if (is.null(sequence_stage) || is.na(sequence_stage)) {
    sequence_stage <- prospect$sequence_stage
  }

  sequence_stage <- normalize_sequence_stage(sequence_stage)
  outcome <- outcome %||% DEFAULT_TOUCH_OUTCOME
  touch_type <- touch_type %||% DEFAULT_TOUCH_TYPE

  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  now <- as.character(Sys.time())

  dbExecute(
    con,
    "
    INSERT INTO touches (
      prospect_id,
      touch_type,
      subject,
      body,
      outcome,
      sequence_stage,
      created_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?)
    ",
    params = list(
      prospect_id,
      touch_type,
      subject %||% NA_character_,
      body %||% NA_character_,
      outcome,
      sequence_stage,
      now
    )
  )

  if (is_terminal_outcome(outcome)) {
    next_status <- status_from_touch_outcome(outcome, current_status = prospect$status)

    dbExecute(
      con,
      "
      UPDATE prospects
      SET
        status = ?,
        last_touch = ?,
        next_touch = NULL,
        updated_at = ?
      WHERE id = ?
      ",
      params = list(
        next_status,
        as.character(Sys.Date()),
        now,
        prospect_id
      )
    )

    return(invisible(TRUE))
  }

  if (outcome == "Bounced") {
    dbExecute(
      con,
      "
      UPDATE prospects
      SET
        status = 'Bounced',
        last_touch = ?,
        next_touch = NULL,
        updated_at = ?
      WHERE id = ?
      ",
      params = list(
        as.character(Sys.Date()),
        now,
        prospect_id
      )
    )

    return(invisible(TRUE))
  }

  if (!isTRUE(advance_sequence)) {
    next_touch_value <- normalize_next_touch_value(next_touch, prospect$next_touch)

    dbExecute(
      con,
      "
      UPDATE prospects
      SET
        last_touch = ?,
        next_touch = ?,
        updated_at = ?
      WHERE id = ?
      ",
      params = list(
        as.character(Sys.Date()),
        next_touch_value,
        now,
        prospect_id
      )
    )

    return(invisible(TRUE))
  }

  next_status <- get_next_status(sequence_stage)
  next_stage <- get_next_sequence_stage(sequence_stage)
  next_touch <- get_next_touch_date(sequence_stage)

  dbExecute(
    con,
    "
    UPDATE prospects
    SET
      status = ?,
      sequence_stage = ?,
      last_touch = ?,
      next_touch = ?,
      updated_at = ?
    WHERE id = ?
    ",
    params = list(
      next_status,
      next_stage,
      as.character(Sys.Date()),
      as.character(next_touch),
      now,
      prospect_id
    )
  )

  invisible(TRUE)
}


get_touches_for_prospect <- function(prospect_id) {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  dbGetQuery(
    con,
    "
    SELECT
      id,
      prospect_id,
      touch_type,
      subject,
      body,
      outcome,
      sequence_stage,
      created_at
    FROM touches
    WHERE prospect_id = ?
    ORDER BY created_at DESC
    ",
    params = list(prospect_id)
  )
}


# ---- Draft helpers ----------------------------------------------------------

create_draft <- function(prospect_id, subject, body, sequence_stage = NULL) {
  prospect <- get_prospect_by_id(prospect_id)

  if (is.null(prospect)) {
    stop("Prospect not found.", call. = FALSE)
  }

  if (is.null(sequence_stage) || is.na(sequence_stage)) {
    sequence_stage <- prospect$sequence_stage
  }

  sequence_stage <- normalize_sequence_stage(sequence_stage)

  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  now <- as.character(Sys.time())

  dbExecute(
    con,
    "
    INSERT INTO drafts (
      prospect_id,
      sequence_stage,
      subject,
      body,
      status,
      created_at,
      updated_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?)
    ",
    params = list(
      prospect_id,
      sequence_stage,
      subject %||% NA_character_,
      body %||% NA_character_,
      DEFAULT_DRAFT_STATUS,
      now,
      now
    )
  )

  dbExecute(
    con,
    "
    UPDATE prospects
    SET
      status = CASE
        WHEN status IN ('Not Started', 'Ready to Email') THEN 'Ready to Email'
        ELSE status
      END,
      updated_at = ?
    WHERE id = ?
    ",
    params = list(now, prospect_id)
  )

  invisible(TRUE)
}


get_drafts_for_prospect <- function(prospect_id) {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  dbGetQuery(
    con,
    "
    SELECT
      id,
      prospect_id,
      sequence_stage,
      subject,
      body,
      status,
      created_at,
      updated_at
    FROM drafts
    WHERE prospect_id = ?
    ORDER BY created_at DESC
    ",
    params = list(prospect_id)
  )
}


get_latest_draft_for_prospect <- function(prospect_id) {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  result <- dbGetQuery(
    con,
    "
    SELECT
      id,
      prospect_id,
      sequence_stage,
      subject,
      body,
      status,
      created_at,
      updated_at
    FROM drafts
    WHERE prospect_id = ?
    ORDER BY created_at DESC
    LIMIT 1
    ",
    params = list(prospect_id)
  )

  if (nrow(result) == 0) {
    return(NULL)
  }

  result[1, ]
}


update_draft <- function(draft_id, subject, body, status = DEFAULT_DRAFT_STATUS) {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  if (!status %in% DRAFT_STATUSES) {
    status <- DEFAULT_DRAFT_STATUS
  }

  dbExecute(
    con,
    "
    UPDATE drafts
    SET
      subject = ?,
      body = ?,
      status = ?,
      updated_at = ?
    WHERE id = ?
    ",
    params = list(
      subject %||% NA_character_,
      body %||% NA_character_,
      status,
      as.character(Sys.time()),
      draft_id
    )
  )

  invisible(TRUE)
}


mark_draft_sent <- function(draft_id) {
  update_draft_status(draft_id, status = "Sent")
}


update_draft_status <- function(draft_id, status) {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  if (!status %in% DRAFT_STATUSES) {
    status <- DEFAULT_DRAFT_STATUS
  }

  dbExecute(
    con,
    "
    UPDATE drafts
    SET
      status = ?,
      updated_at = ?
    WHERE id = ?
    ",
    params = list(
      status,
      as.character(Sys.time()),
      draft_id
    )
  )

  invisible(TRUE)
}

normalize_next_touch_value <- function(next_touch, fallback = NULL) {
  value <- if (is.null(next_touch) || length(next_touch) == 0) {
    fallback
  } else {
    next_touch[1]
  }

  if (is.null(value) || length(value) == 0 || is.na(value) || value == "") {
    return(NA_character_)
  }

  as.character(value)
}


update_prospect_research <- function(
    prospect_id,
    research_notes = NULL,
    research_sources = NULL,
    researched_at = Sys.time()
) {
  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  dbExecute(
    con,
    "
    UPDATE prospects
    SET
      research_notes = ?,
      research_sources = ?,
      researched_at = ?,
      updated_at = ?
    WHERE id = ?
    ",
    params = list(
      research_notes %||% NA_character_,
      research_sources %||% NA_character_,
      as.character(researched_at),
      as.character(Sys.time()),
      prospect_id
    )
  )

  invisible(TRUE)
}


# ---- Export helpers ---------------------------------------------------------

export_signal_data <- function(export_root = "data/exports") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  export_dir <- file.path(export_root, paste0("signal_export_", timestamp))

  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

  con <- get_db()
  on.exit(dbDisconnect(con), add = TRUE)

  export_table <- function(table_name) {
    if (!table_name %in% dbListTables(con)) {
      return(invisible(FALSE))
    }

    data <- dbGetQuery(con, paste("SELECT * FROM", table_name))
    write.csv(
      data,
      file = file.path(export_dir, paste0(table_name, ".csv")),
      row.names = FALSE,
      na = ""
    )

    invisible(TRUE)
  }

  export_table("prospects")
  export_table("touches")
  export_table("drafts")
  export_table("schema_migrations")

  db_path <- get_db_path()

  if (file.exists(db_path)) {
    file.copy(
      from = db_path,
      to = file.path(export_dir, basename(db_path)),
      overwrite = TRUE
    )
  }

  normalizePath(export_dir, winslash = "\\", mustWork = FALSE)
}
