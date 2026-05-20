# R/db.R
# Database layer for Signal. All DB work lives here.
# Cadence/status logic belongs in outreach_logic.R.

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
      con <- DBI::dbConnect(RSQLite::SQLite(), path, synchronous = NULL)
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      DBI::dbListTables(con)
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
  DBI::dbConnect(RSQLite::SQLite(), get_db_path(), synchronous = NULL)
}


# ---- Initialization ---------------------------------------------------------

init_db <- function() {
  ensure_db_directory(get_db_path())

  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "
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

      assigned_to TEXT,

      last_touch TEXT,
      next_touch TEXT,

      reply_notes TEXT,

      customer_since TEXT,
      customer_notes TEXT,

      created_at TEXT,
      updated_at TEXT
    )
  ")

  DBI::dbExecute(con, "
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

  DBI::dbExecute(con, "
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
  DBI::dbExecute(con, "
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
  run_schema_migration(con, "003_add_phone", function() {
    add_column_if_missing(con, "prospects", "phone", "TEXT")
  })
  run_schema_migration(con, "004_add_lifecycle_fields", function() {
    add_column_if_missing(con, "prospects", "assigned_to",     "TEXT")
    add_column_if_missing(con, "prospects", "customer_since",  "TEXT")
    add_column_if_missing(con, "prospects", "customer_notes",  "TEXT")
    # Convert legacy "Replied" status to the new "In Conversation" phase.
    DBI::dbExecute(con, "UPDATE prospects SET status = 'In Conversation' WHERE status = 'Replied'")
  })

  invisible(TRUE)
}


schema_migration_applied <- function(con, migration_id) {
  result <- DBI::dbGetQuery(
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
  DBI::dbExecute(
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
  columns <- DBI::dbGetQuery(con, paste0("PRAGMA table_info(", table_name, ")"))

  if (column_name %in% columns$name) {
    return(invisible(FALSE))
  }

  DBI::dbExecute(
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
  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_prospects_status
    ON prospects(status)
  ")

  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_prospects_next_touch
    ON prospects(next_touch)
  ")

  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_prospects_sequence_stage
    ON prospects(sequence_stage)
  ")

  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_touches_prospect_id
    ON touches(prospect_id)
  ")

  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_drafts_prospect_id
    ON drafts(prospect_id)
  ")

  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_prospects_assigned_to
    ON prospects(assigned_to)
  ")
}


# ---- Prospect helpers -------------------------------------------------------

create_prospect <- function(prospect) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  now <- as.character(Sys.time())

  status <- normalize_status(prospect$status %||% DEFAULT_PROSPECT_STATUS)
  sequence_stage <- normalize_sequence_stage(prospect$sequence_stage %||% DEFAULT_SEQUENCE_STAGE)

  next_touch <- prospect$next_touch
  if (is.null(next_touch) || is.na(next_touch) || next_touch == "") {
    next_touch <- as.character(Sys.Date())
  } else {
    next_touch <- as.character(next_touch)
  }

  assigned_to <- prospect$assigned_to %||% getOption("signal.user_id", NA_character_)

  DBI::dbExecute(
    con,
    "
    INSERT INTO prospects (
      first_name,
      last_name,
      company,
      title,
      email,
      phone,
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
      assigned_to,
      last_touch,
      next_touch,
      reply_notes,
      created_at,
      updated_at
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    ",
    params = list(
      prospect$first_name %||% NA_character_,
      prospect$last_name %||% NA_character_,
      prospect$company %||% NA_character_,
      prospect$title %||% NA_character_,
      prospect$email %||% NA_character_,
      prospect$phone %||% NA_character_,
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
      assigned_to,
      NA_character_,
      next_touch,
      prospect$reply_notes %||% NA_character_,
      now,
      now
    )
  )

  invisible(TRUE)
}


get_prospects <- function(include_inactive = TRUE, ae_filter = NULL) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  conditions <- character(0)
  if (!include_inactive) {
    conditions <- c(conditions, "status NOT IN ('Replied', 'Not Interested', 'Do Not Contact')")
  }
  if (!is.null(ae_filter)) {
    conditions <- c(conditions, "coalesce(assigned_to, '') = ?")
  }
  where_clause <- if (length(conditions) > 0) {
    paste("WHERE", paste(conditions, collapse = " AND "))
  } else {
    ""
  }
  params <- if (!is.null(ae_filter)) list(ae_filter) else list()

  DBI::dbGetQuery(
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
    ),
    params = params
  )
}


get_active_prospects <- function(ae_filter = NULL) {
  get_prospects(include_inactive = FALSE, ae_filter = ae_filter)
}


get_prospect_by_id <- function(prospect_id) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  result <- DBI::dbGetQuery(
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
  on.exit(DBI::dbDisconnect(con), add = TRUE)

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

  DBI::dbExecute(
    con,
    "
    UPDATE prospects
    SET
      first_name = ?,
      last_name = ?,
      company = ?,
      title = ?,
      email = ?,
      phone = ?,
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
      prospect$phone %||% NA_character_,
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
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  now <- as.character(Sys.time())
  status <- normalize_status(status)

  if (is_terminal_status(status)) {
    DBI::dbExecute(
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
    DBI::dbExecute(
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
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "DELETE FROM drafts WHERE prospect_id = ?", params = list(prospect_id))
  DBI::dbExecute(con, "DELETE FROM touches WHERE prospect_id = ?", params = list(prospect_id))
  DBI::dbExecute(con, "DELETE FROM prospects WHERE id = ?", params = list(prospect_id))

  invisible(TRUE)
}


# ---- Outreach queue ---------------------------------------------------------

get_outreach_queue <- function(ae_filter = NULL) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  today   <- as.character(Sys.Date())
  ae_sql  <- if (!is.null(ae_filter)) "AND coalesce(assigned_to, '') = ?" else ""
  params  <- if (!is.null(ae_filter)) list(today, ae_filter) else list(today)

  DBI::dbGetQuery(
    con,
    paste0(
      "
      SELECT
        id,
        trim(coalesce(first_name, '') || ' ' || coalesce(last_name, '')) AS name,
        company,
        title,
        email,
        source,
        segment,
        assigned_to,
        reason_for_outreach,
        status,
        sequence_stage,
        last_touch,
        next_touch
      FROM prospects
      WHERE
        status NOT IN ('In Conversation', 'Customer', 'Replied', 'Not Interested', 'Do Not Contact')
        AND (
          next_touch IS NULL
          OR next_touch = ''
          OR next_touch <= ?
        )
        ", ae_sql, "
      ORDER BY
        CASE
          WHEN next_touch IS NULL OR next_touch = '' THEN 1
          ELSE 0
        END,
        next_touch ASC,
        company ASC,
        last_name ASC
      "
    ),
    params = params
  )
}


get_conversation_queue <- function(ae_filter = NULL) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  today  <- as.character(Sys.Date())
  ae_sql <- if (!is.null(ae_filter)) "AND coalesce(assigned_to, '') = ?" else ""
  params <- if (!is.null(ae_filter)) list(today, ae_filter) else list(today)

  DBI::dbGetQuery(
    con,
    paste0(
      "
      SELECT
        id,
        trim(coalesce(first_name, '') || ' ' || coalesce(last_name, '')) AS name,
        company,
        title,
        email,
        source,
        segment,
        assigned_to,
        reason_for_outreach,
        status,
        sequence_stage,
        last_touch,
        next_touch
      FROM prospects
      WHERE
        status = 'In Conversation'
        AND (
          next_touch IS NULL
          OR next_touch = ''
          OR next_touch <= ?
        )
        ", ae_sql, "
      ORDER BY
        CASE
          WHEN next_touch IS NULL OR next_touch = '' THEN 1
          ELSE 0
        END,
        next_touch ASC,
        company ASC,
        last_name ASC
      "
    ),
    params = params
  )
}


get_customer_queue <- function(ae_filter = NULL) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  today  <- as.character(Sys.Date())
  ae_sql <- if (!is.null(ae_filter)) "AND coalesce(assigned_to, '') = ?" else ""
  params <- if (!is.null(ae_filter)) list(today, ae_filter) else list(today)

  DBI::dbGetQuery(
    con,
    paste0(
      "
      SELECT
        id,
        trim(coalesce(first_name, '') || ' ' || coalesce(last_name, '')) AS name,
        company,
        title,
        email,
        source,
        segment,
        assigned_to,
        status,
        customer_since,
        customer_notes,
        last_touch,
        next_touch
      FROM prospects
      WHERE
        status = 'Customer'
        AND (
          next_touch IS NULL
          OR next_touch = ''
          OR next_touch <= ?
        )
        ", ae_sql, "
      ORDER BY
        CASE
          WHEN next_touch IS NULL OR next_touch = '' THEN 1
          ELSE 0
        END,
        next_touch ASC,
        company ASC,
        last_name ASC
      "
    ),
    params = params
  )
}


mark_as_customer <- function(
    prospect_id,
    notes           = NULL,
    next_touch_days = NULL,
    next_touch_date = NULL
) {
  next_touch <- resolve_customer_next_touch(
    next_touch_date = next_touch_date,
    next_touch_days = next_touch_days
  )

  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(
    con,
    "
    UPDATE prospects
    SET
      status         = 'Customer',
      customer_since = ?,
      customer_notes = ?,
      next_touch     = ?,
      updated_at     = ?
    WHERE id = ?
    ",
    params = list(
      as.character(Sys.Date()),
      notes %||% NA_character_,
      next_touch,
      as.character(Sys.time()),
      prospect_id
    )
  )

  invisible(TRUE)
}


snooze_prospect <- function(prospect_id, days = DEFAULT_QUEUE_SNOOZE_DAYS) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  days <- suppressWarnings(as.integer(days))
  if (is.na(days) || days < 1) {
    days <- DEFAULT_QUEUE_SNOOZE_DAYS
  }

  next_touch <- as.character(Sys.Date() + days)

  DBI::dbExecute(
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
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  now <- as.character(Sys.time())

  DBI::dbExecute(
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

    DBI::dbExecute(
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
    DBI::dbExecute(
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

    DBI::dbExecute(
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

  DBI::dbExecute(
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
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbGetQuery(
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
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  now <- as.character(Sys.time())

  DBI::dbExecute(
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

  DBI::dbExecute(
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
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbGetQuery(
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
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  result <- DBI::dbGetQuery(
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
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (!status %in% DRAFT_STATUSES) {
    status <- DEFAULT_DRAFT_STATUS
  }

  DBI::dbExecute(
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
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (!status %in% DRAFT_STATUSES) {
    status <- DEFAULT_DRAFT_STATUS
  }

  DBI::dbExecute(
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
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(
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

update_prospect_from_import <- function(prospect_id, row) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(
    con,
    "
    UPDATE prospects
    SET
      first_name            = COALESCE(NULLIF(?, ''), first_name),
      last_name             = COALESCE(NULLIF(?, ''), last_name),
      company               = COALESCE(NULLIF(?, ''), company),
      title                 = COALESCE(NULLIF(?, ''), title),
      email                 = COALESCE(NULLIF(?, ''), email),
      phone                 = COALESCE(NULLIF(?, ''), phone),
      linkedin_url          = COALESCE(NULLIF(?, ''), linkedin_url),
      website               = COALESCE(NULLIF(?, ''), website),
      city                  = COALESCE(NULLIF(?, ''), city),
      state                 = COALESCE(NULLIF(?, ''), state),
      source                = COALESCE(NULLIF(?, ''), source),
      segment               = COALESCE(NULLIF(?, ''), segment),
      reason_for_outreach   = COALESCE(NULLIF(?, ''), reason_for_outreach),
      personalization_notes = COALESCE(NULLIF(?, ''), personalization_notes),
      updated_at            = ?
    WHERE id = ?
    ",
    params = list(
      row$first_name %||% NA_character_,
      row$last_name %||% NA_character_,
      row$company %||% NA_character_,
      row$title %||% NA_character_,
      row$email %||% NA_character_,
      row$phone %||% NA_character_,
      row$linkedin_url %||% NA_character_,
      row$website %||% NA_character_,
      row$city %||% NA_character_,
      row$state %||% NA_character_,
      row$source %||% NA_character_,
      row$segment %||% NA_character_,
      row$reason_for_outreach %||% NA_character_,
      row$personalization_notes %||% NA_character_,
      as.character(Sys.time()),
      prospect_id
    )
  )

  invisible(TRUE)
}

update_prospect_personalization_notes <- function(prospect_id, personalization_notes) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(
    con,
    "
    UPDATE prospects
    SET
      personalization_notes = ?,
      updated_at = ?
    WHERE id = ?
    ",
    params = list(
      personalization_notes %||% NA_character_,
      as.character(Sys.time()),
      prospect_id
    )
  )

  invisible(TRUE)
}

get_prospects_by_company <- function(company) {
  company <- normalize_company_match_value(company)

  if (is.na(company)) {
    return(data.frame())
  }

  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbGetQuery(
    con,
    "
    SELECT
      id,
      first_name,
      last_name,
      title,
      personalization_notes
    FROM prospects
    WHERE
      lower(trim(coalesce(company, ''))) = lower(trim(?))
      AND coalesce(status, '') <> 'Do Not Contact'
    ORDER BY last_name, first_name
    ",
    params = list(company)
  )
}

get_company_research <- function(company) {
  company <- normalize_company_match_value(company)

  if (is.na(company)) {
    return(NULL)
  }

  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  result <- DBI::dbGetQuery(
    con,
    "
    SELECT
      id,
      company,
      research_notes,
      research_sources,
      researched_at
    FROM prospects
    WHERE
      lower(trim(coalesce(company, ''))) = lower(trim(?))
      AND research_notes IS NOT NULL
      AND trim(research_notes) <> ''
    ORDER BY
      CASE WHEN researched_at IS NULL OR researched_at = '' THEN 1 ELSE 0 END,
      researched_at DESC,
      updated_at DESC
    LIMIT 1
    ",
    params = list(company)
  )

  if (nrow(result) == 0) {
    return(NULL)
  }

  result[1, ]
}


update_company_research <- function(
    company,
    research_notes = NULL,
    research_sources = NULL,
    researched_at = Sys.time()
) {
  company <- normalize_company_match_value(company)

  if (is.na(company)) {
    return(0L)
  }

  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  affected <- DBI::dbExecute(
    con,
    "
    UPDATE prospects
    SET
      research_notes = ?,
      research_sources = ?,
      researched_at = ?,
      updated_at = ?
    WHERE lower(trim(coalesce(company, ''))) = lower(trim(?))
    ",
    params = list(
      research_notes %||% NA_character_,
      research_sources %||% NA_character_,
      as.character(researched_at),
      as.character(Sys.time()),
      company
    )
  )

  as.integer(affected)
}


normalize_company_match_value <- function(company) {
  if (is.null(company) || length(company) == 0) {
    return(NA_character_)
  }

  company <- trimws(as.character(company[1]))

  if (is.na(company) || company == "") {
    return(NA_character_)
  }

  company
}


# ---- Store helpers (consumed by signal_server() reactives) ------------------

get_touches_for_all_prospects <- function(ae_filter = NULL) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ae_sql <- if (!is.null(ae_filter)) {
    "JOIN prospects p ON t.prospect_id = p.id WHERE coalesce(p.assigned_to, '') = ?"
  } else {
    "JOIN prospects p ON t.prospect_id = p.id"
  }
  params <- if (!is.null(ae_filter)) list(ae_filter) else list()

  DBI::dbGetQuery(
    con,
    paste0(
      "
      SELECT
        t.id,
        t.prospect_id,
        p.assigned_to,
        p.company,
        t.touch_type,
        t.outcome,
        t.sequence_stage,
        t.created_at
      FROM touches t
      ", ae_sql, "
      ORDER BY t.created_at DESC
      "
    ),
    params = params
  )
}

get_prospects_by_status <- function(status, ae_filter = NULL) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ae_sql <- if (!is.null(ae_filter)) "AND coalesce(assigned_to, '') = ?" else ""
  params <- if (!is.null(ae_filter)) list(status, ae_filter) else list(status)

  DBI::dbGetQuery(
    con,
    paste0(
      "
      SELECT
        id,
        trim(coalesce(first_name, '') || ' ' || coalesce(last_name, '')) AS name,
        company,
        assigned_to,
        status,
        customer_since,
        customer_notes,
        created_at,
        updated_at
      FROM prospects
      WHERE status = ?
      ", ae_sql, "
      ORDER BY customer_since DESC, company ASC
      "
    ),
    params = params
  )
}

get_pipeline_summary_by_ae <- function(ae_filter = NULL) {
  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ae_sql <- if (!is.null(ae_filter)) "WHERE coalesce(assigned_to, '') = ?" else ""
  params <- if (!is.null(ae_filter)) list(ae_filter) else list()

  DBI::dbGetQuery(
    con,
    paste0(
      "
      SELECT
        coalesce(assigned_to, 'Unassigned') AS ae,
        status,
        count(*) AS n
      FROM prospects
      ", ae_sql, "
      GROUP BY assigned_to, status
      ORDER BY ae, status
      "
    ),
    params = params
  )
}


# ---- Export helpers ---------------------------------------------------------

export_signal_data <- function(export_root = "data/exports") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  export_dir <- file.path(export_root, paste0("signal_export_", timestamp))

  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

  con <- get_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  export_table <- function(table_name) {
    if (!table_name %in% DBI::dbListTables(con)) {
      return(invisible(FALSE))
    }

    data <- DBI::dbGetQuery(con, paste("SELECT * FROM", table_name))
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
