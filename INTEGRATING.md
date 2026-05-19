# Integrating Signal into a Host Shiny App

Signal is an R package that exposes two functions: `signal_ui()` and `signal_server()`. Drop them into any host Shiny app to add the full outreach workbench as a tab.

---

## 1. Install the package

```r
# Option A — devtools (recommended during development)
devtools::install("../signal")

# Option B — pak
pak::local_install("../signal")
```

---

## 2. Add to DESCRIPTION

```
Imports:
    signal
```

If the host app is not an R package, use `library(signal)` in `global.R`.

---

## 3. Wire up the tab

### UI

```r
shiny::tabPanel("Signal", signal::signal_ui(session$ns("signal")))
```

### Server

```r
signal_data <- signal::signal_server(
  "signal",
  db_path   = config$signal_db,      # path to SQLite file
  api_key   = config$anthropic_key,  # Anthropic API key
  user_id   = current_user$id,       # e.g. "john.doe"
  user_role = current_user$role      # "ae", "manager", or "admin"
)
```

The `id` passed to `signal_server()` must match the one in `signal_ui()`.

---

## 4. Configuration parameters

| Parameter | Required | Description |
|-----------|----------|-------------|
| `db_path` | Recommended | Path to the SQLite file. Signal creates it on first run. Falls back to `SIGNAL_DB_PATH` env var, then a user-level AppData location. |
| `api_key` | Recommended | Anthropic API key. Falls back to `ANTHROPIC_API_KEY` env var, then `_secrets.yml`. Without a key, email generation uses local fallback templates. |
| `user_id` | Recommended | Current user identifier. AEs are filtered to their own prospects automatically. |
| `user_role` | Recommended | `"ae"`, `"manager"`, or `"admin"`. AEs see only their assigned data and cannot reassign prospects. Managers and admins see all data. |
| `claude_model` | Optional | Default: `claude-sonnet-4-6`. |
| `web_search_type` | Optional | Default: `web_search_20250305`. |

### Role behaviour

| Role | Queue filter | Reassign prospects | See all AEs |
|------|----|----|----|
| `ae` | Own prospects only | No | No |
| `manager` | All | Yes | Yes |
| `admin` | All | Yes | Yes |

---

## 5. Using the store (data sharing with salesActivity)

`signal_server()` returns a named list of reactives. Use them anywhere in the host app's server function:

```r
signal_data <- signal::signal_server("signal", ...)

# AE call activity (touch_type == "Call", outcome == "Connected")
signal_data$connected_calls()

# AE meeting activity (touch_type == "Meeting")
signal_data$meetings()

# Prospects marked as Customer, with customer_since timestamp
signal_data$new_customers()

# Prospect counts by AE and status (pipeline funnel)
signal_data$pipeline_by_ae()
```

All store reactives respect the `ae_filter` — an AE user only sees their own data.

---

## 6. Prospect lifecycle in Signal

```
Not Started → [email sequence] → Nurture
                                    ↓  (prospect replies — "Replied" touch outcome)
                              In Conversation  ←→  snooze / schedule calls & meetings
                                    ↓  (Mark as Customer button)
                                 Customer       ←→  scheduled check-ins
                                    ↓
                           [still loggable indefinitely]

From any active phase:
  → Not Interested   (terminal)
  → Do Not Contact   (terminal)
```

The **Conversation Queue** shows prospects due for a call or meeting, with a default next-touch of 7 days (configurable per prospect). The **Customer Queue** shows customers due for a check-in, default 30 days.

---

## 7. Full salesActivity example

In `R/module_sales_activity.R`:

```r
# UI — inside the tabsetPanel
shiny::tabPanel("Signal", signal::signal_ui(session$ns("signal")))

# Server — after auth is resolved
signal_data <- signal::signal_server(
  "signal",
  db_path   = config$signal_db_path,
  api_key   = config$anthropic_api_key,
  user_id   = identity$user_id,
  user_role = identity$role           # from auth_roles.R
)

# Optionally pull signal data into salesActivity's own reporting:
# signal_data$connected_calls()
# signal_data$pipeline_by_ae()
```

---

## 8. Database location quick reference

| Scenario | Recommended `db_path` |
|----------|-----------------------|
| Standalone dev | omit (uses AppData) or `"data/signal.sqlite"` |
| Embedded, shared server | `"//server/share/signal/signal.sqlite"` |
| Embedded, per-user | `file.path(Sys.getenv("LOCALAPPDATA"), "Signal", "signal.sqlite")` |
| Docker / Posit Connect | mount a volume; set `db_path` to the mount path |

Signal uses SQLite. A single shared file on a network path works for a small team. For simultaneous multi-user writes, migrate to PostgreSQL (replace `RSQLite` with `RPostgres` in `DESCRIPTION` and update `get_db()` in `R/db.R`).

---

## 9. Standalone development

```r
# From the signal/ directory:
devtools::load_all()
shiny::runApp()
# Or open app.R in RStudio and click Run App
```

API key resolution order: `options(signal.api_key)` → `ANTHROPIC_API_KEY` env var → `_secrets.yml`. Copy `_secrets.example.yml` → `_secrets.yml` for local dev.
