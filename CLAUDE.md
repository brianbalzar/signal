# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What Signal Is

Signal is an R package that provides a lightweight outbound outreach workbench — intentionally NOT a CRM. Every feature should pass this test: *Does this help write a better outbound email, track whether we followed up, or know who needs attention today?*

It can run standalone or be embedded as a tab in any host Shiny app via `signal_ui()` / `signal_server()`.

## Running Standalone

```r
# From RStudio with signal.Rproj open — click Run App, or:
devtools::load_all()
shiny::runApp()
```

**Setup:** Copy `_secrets.example.yml` to `_secrets.yml` and add your Anthropic API key. The SQLite database initialises automatically on first run.

## Package Structure

```
DESCRIPTION         — package metadata and dependencies
NAMESPACE           — exports: signal_ui, signal_server
app.R               — standalone runner (calls pkgload::load_all then signal_ui/server)

R/
  exports.R         — signal_ui(), signal_server()  ← PUBLIC API
  zzz.R             — .onLoad(): registers signal-www/ Shiny resource path
  constants.R       — all enums: statuses, sources, segments, touch types, stages
  outreach_logic.R  — pure sequence/status logic (no DB, no UI, no side effects)
  db.R              — all SQLite CRUD and schema migrations
  claude.R          — Claude API: config, prompt building, HTTP, response parsing
  email.R           — stub for future send integration
  health.R          — runtime health check (API key configured?)
  helpers.R         — shared utilities incl. %||%
  ui_helpers.R      — display formatters and Shiny UI components
  formatters.R      — data formatters
  mod_queue_ui.R / mod_queue_server.R       — main outreach workbench module
  mod_prospects_ui.R / mod_prospects_server.R — prospect intake and editing module

inst/
  prompts/          — customisable Claude prompt templates (intro_email.txt)
  www/              — styles.css (served as signal-www/styles.css)

data/               — SQLite database files (gitignored), import staging, CSV exports
```

## Public API

```r
# In host app UI:
signal_ui("signal")           # or session$ns("signal") inside a module

# In host app server:
signal_server(
  "signal",
  db_path  = "/path/to/signal.sqlite",   # NULL → env var → AppData
  api_key  = "sk-ant-...",               # NULL → ANTHROPIC_API_KEY → _secrets.yml
  claude_model    = "claude-sonnet-4-6", # optional
  web_search_type = "web_search_20250305" # optional
)
```

See [INTEGRATING.md](INTEGRATING.md) for the full step-by-step guide, including the salesActivity wiring and database location options.

## Key Patterns

**Config resolution** — `signal_server()` sets `options(signal.api_key, signal.db_path, ...)`. All services read options first, then env vars, then `_secrets.yml` (standalone backward compat). Never read `_secrets.yml` directly from new code — go through `get_claude_config()` or `get_db_path()`.

**Database access** — always through `R/db.R`. Use `get_db()` for the connection. Schema changes require a new migration in `apply_schema_migrations()`.

**Sequence logic** — `R/outreach_logic.R` is pure R with no side effects. Status/stage transitions, due-date calculation, and queue eligibility live here.

**Claude API** — `R/claude.R` handles everything. Prompts load from `inst/prompts/` via `system.file()`. All Claude calls must be triggered by explicit user actions (buttons), never automatically.

**Constants** — `R/constants.R` is the single source of truth for all lookup values (statuses, sources, segments, etc.).

**CSS / www** — styles live in `inst/www/styles.css`, served as `signal-www/styles.css` after `addResourcePath()` is called in `.onLoad()` / `signal_ui()`.

## Outreach Sequence

| Stage | Label | Delay after touch |
|-------|-------|-------------------|
| 0 | Intro | — |
| 1 | Follow-Up 1 | 3 days |
| 2 | Follow-Up 2 | 5 days |
| 3 | Breakup | 7 days |
| 4 | Nurture | 30 days |
| 5 | Nurture 2 | 30 days |

Terminal statuses (exit the queue permanently): Replied, Not Interested, Do Not Contact.

## Database

SQLite via `R/db.R`. Three tables: `prospects`, `touches`, `drafts`. Migrations tracked in `schema_migrations`. Applied: `001_initial_schema`, `002_add_research_fields`, `003_add_phone`.

## Adding Features

1. New constants → `R/constants.R`
2. New DB operations → `R/db.R`
3. New sequence/status logic → `R/outreach_logic.R`
4. New Claude integration → `R/claude.R`
5. New UI → new `mod_*.R` pair or extend existing module; wire into `R/exports.R`

Empty stubs exist for `mod_contact_detail.R`, `mod_email.R`, `mod_touches.R`.

## Installing for Embedding

```r
# From the host app's console:
devtools::install("../signal")   # adjust path as needed
```
