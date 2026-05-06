# Signal

Signal is a lightweight outbound outreach workbench built in R Shiny.

It is intentionally **not** a CRM.

The purpose of Signal is to help manage the pre-reply outbound workflow: identifying a prospect, storing minimal context, generating a personalized intro or follow-up email, tracking touches, and stopping once the prospect replies.

Once a prospect replies, the workflow in Signal ends. The relationship can then move into normal sales follow-up outside of this app.

---

## Core Purpose

Signal exists to answer four daily questions:

1. Who should I reach out to today?
2. Why am I reaching out to them?
3. What email should I send?
4. When should I follow up if they do not reply?

The app should remain simple, fast, and focused on getting a first response.

---

## What Signal Is

Signal is a local R Shiny app for:

- Tracking outbound prospects
- Storing minimal personalization context
- Managing a simple follow-up cadence
- Generating email drafts using Claude or another LLM
- Logging outbound email touches
- Removing prospects from the active queue once they reply

---

## What Signal Is Not

Signal is not intended to manage:

- Deals
- Opportunities
- Proposals
- Contracts
- Revenue forecasts
- Meeting notes after a reply
- Long-term customer relationships
- Full account/contact hierarchy
- Sales pipeline stages after first response

If a prospect replies, they should be marked as `Replied` and removed from the active outreach queue.

---

## Business Context

The user runs or supports a facility consulting business.

The company helps building owners and facility teams operate their facilities more efficiently using data analytics. The work may include:

- HVAC performance improvement
- Controls optimization
- BAS/BMS issue identification
- Energy efficiency
- Operational reliability
- Utility incentive opportunities
- Mechanical and controls performance analysis

Typical outbound targets may include:

- Hospitals
- Schools
- Universities
- Commercial office buildings
- Industrial facilities
- Property managers
- Facility directors
- Operations leaders
- Engineering managers

Signal should help write plainspoken, consultative outbound emails that get a foot in the door.

---

## Design Philosophy

Every field, feature, and screen should pass this test:

> Does this help write a better outbound email, track whether we followed up, or know who needs attention today?

If the answer is no, do not add it yet.

Avoid building a full CRM.

Keep the app focused on first-response outreach.

---

## Workflow

The intended workflow is:

1. Add a prospect with minimal known information.
2. Add a reason for outreach and any personalization notes.
3. Prospect appears in the outreach queue when they are due.
4. Generate or manually write an email draft.
5. Send the email manually or through a future sending integration.
6. Log the touch.
7. Signal advances the prospect to the next follow-up stage.
8. Repeat until the prospect replies, opts out, or moves to nurture.
9. Once the prospect replies, mark them as `Replied`.
10. Signal stops managing that prospect.

---

## Sequence

Signal uses a simple pre-reply outreach sequence.

| Stage | Status | Timing | Recommended Action |
|---:|---|---:|---|
| 0 | Ready to Email / Not Started | Today | Generate/send intro email |
| 1 | Intro Sent | +3 days | Send follow-up 1 |
| 2 | Follow-Up 1 Sent | +5 days | Send follow-up 2 |
| 3 | Follow-Up 2 Sent | +7 days | Send breakup email |
| 4 | Breakup Sent | +30 days | Move to nurture |
| 5 | Nurture | Later | Re-engage only if useful |
| End | Replied | - | Remove from active workflow |

Terminal statuses should not appear in the active outreach queue:

- `Replied`
- `Not Interested`
- `Do Not Contact`

---

## Recommended Statuses

Current intended statuses:

- `Not Started`
- `Ready to Email`
- `Intro Sent`
- `Follow-Up 1 Sent`
- `Follow-Up 2 Sent`
- `Breakup Sent`
- `Replied`
- `Nurture`
- `Not Interested`
- `Do Not Contact`

---

## Minimal Prospect Data Model

Signal should use a prospect-centered model, not a full account/contact CRM model.

A prospect is usually one person at one organization.

Recommended fields:

```text
id
first_name
last_name
company
title
email
linkedin_url
website
city
state
source
segment
reason_for_outreach
personalization_notes
status
sequence_stage
last_touch
next_touch
reply_notes
created_at
updated_at
```

### Key Fields

#### `source`

Where the prospect came from.

Examples:

- Convex Atlas
- LinkedIn
- Referral
- Google
- Existing Network
- Manual
- Other

#### `segment`

A rough category that helps the email generator choose the right framing.

Examples:

- Hospital
- School
- University
- Commercial Office
- Industrial Facility
- Property Management
- Unknown
- Other

#### `reason_for_outreach`

The most important field for personalization.

This should be a short explanation of why this person/company might be relevant.

Examples:

- Hospital facility leader in Oncor territory
- Large building owner with likely HVAC optimization opportunities
- Appears to manage multiple commercial properties
- Facilities director at a campus with likely energy and controls complexity
- Company recently completed or announced a capital project

#### `personalization_notes`

Loose notes for the email generator.

Examples:

- Website mentions a large Dallas campus.
- Contact is Director of Facilities.
- Possible angle around utility incentives.
- Their team may be dealing with aging controls or HVAC runtime issues.
- Use a soft approach; do not assume specific problems.

#### `reply_notes`

Optional notes to capture why the workflow ended.

Examples:

- Replied and asked for availability next week.
- Not interested right now.
- Asked to follow up next quarter.
- Moving to normal sales process outside Signal.

---

## Database Tables

The app should use SQLite locally.

Recommended tables:

### `prospects`

Stores one outbound prospect.

This is the main table.

### `touches`

Stores outbound touches.

Examples:

- Intro email sent
- Follow-up 1 sent
- Follow-up 2 sent
- Breakup email sent
- Manual note

### `drafts`

Stores generated or manually edited email drafts.

Drafts may later be sent manually, copied into Gmail, or sent via SMTP/API.

---

## Folder Structure

Recommended project structure:

```text
signal/
|
|-- app.R
|-- global.R
|-- ui.R
|-- server.R
|
|-- modules/
|   |-- mod_prospects_ui.R
|   |-- mod_prospects_server.R
|   |-- mod_queue_ui.R
|   |-- mod_queue_server.R
|   |-- mod_contact_detail_ui.R
|   `-- mod_contact_detail_server.R
|
|-- services/
|   |-- db.R
|   |-- outreach_logic.R
|   |-- claude.R
|   `-- email.R
|
|-- data/
|   `-- signal.sqlite
|
|-- utils/
|   |-- constants.R
|   |-- helpers.R
|   `-- formatters.R
|
|-- prompts/
|   `-- intro_email.txt
|
|-- www/
|   |-- styles.css
|   `-- logo.png
|
`-- README.md
```

---

## File Responsibilities

### `app.R`

Thin entry point.

Should load:

- `global.R`
- `ui.R`
- `server.R`

Then call:

```r
shinyApp(ui = ui, server = server)
```

### `global.R`

Loads libraries, utility files, service files, and module files.

Also initializes the local SQLite database.

### `ui.R`

Top-level Shiny UI shell.

Should contain layout/navigation only, not business logic.

### `server.R`

Top-level Shiny server function.

Should call module servers only.

### `modules/`

Contains Shiny UI and server modules.

Modules should be focused and reusable.

Recommended modules:

- Prospects module
- Outreach queue module
- Contact/prospect detail module
- Draft/email module
- Touch history module

### `services/db.R`

Database layer.

Should contain:

- database connection helper
- database initialization
- table creation
- indexes
- CRUD functions
- queue query helpers
- touch logging
- draft storage

UI modules should call service functions rather than writing SQL directly.

### `services/outreach_logic.R`

Cadence and sequence logic.

Should contain:

- next status logic
- next touch date logic
- recommended action logic
- terminal status helpers
- overdue/due logic

### `services/claude.R`

LLM integration.

Should contain:

- prompt construction
- Claude API call
- response parsing
- draft creation helpers

Claude should only be called when the user intentionally generates a draft.

The app should avoid unnecessary LLM calls.

### `services/email.R`

Email sending integration.

This can remain a placeholder at first.

Possible future sending options:

- Manual copy/paste into Gmail
- SMTP
- Gmail API
- Microsoft Graph API

Do not auto-send emails until the review workflow is reliable.

### `prompts/`

Stores reusable LLM prompts.

Prompts should not be hardcoded directly inside UI modules.

---

## Email Generation Philosophy

Generated emails should be:

- Short
- Plainspoken
- Consultative
- Specific enough to feel human
- Low-pressure
- Honest about what is known
- Free of exaggerated claims
- Free of fake metrics or fake case studies

Emails should not sound like marketing copy.

The app should never invent:

- equipment types
- controls systems
- utility spend
- savings percentages
- facility size
- prior relationship
- case study results

Unless the user provided the information, the email should use softer language such as:

- "I noticed..."
- "I was curious..."
- "A lot of teams we talk with..."
- "This may not be relevant, but..."
- "Would it be worth a quick conversation..."

---

## Default Intro Email Prompt

A good default prompt for Claude:

```text
You are writing a concise, personalized cold outbound email for a facility consulting company.

The company helps building owners and facility teams improve HVAC performance, controls optimization, BAS/BMS issues, energy efficiency, and operational reliability using analytics.

Write a first-touch intro email.

Rules:
- Maximum 120 words.
- Plainspoken and consultative.
- Do not sound like marketing copy.
- Do not overclaim.
- Mention one specific reason this account may be relevant.
- Mention one likely operational pain, but do not pretend we know it is happening.
- Ask for a low-pressure conversation.
- Include a subject line.
- Do not use emojis.
- Do not use fake case study numbers unless provided.

Prospect data:
- First name:
- Last name:
- Company:
- Title:
- Website:
- City:
- State:
- Source:
- Segment:
- Reason for outreach:
- Personalization notes:
```

---

## Development Rules for Future Coding Agents

Future coding agents should follow these rules:

1. Do not turn Signal into a full CRM.
2. Keep the data model prospect-centered.
3. Do not add deal/opportunity management.
4. Do not add forecasting, proposal tracking, or post-reply pipeline stages.
5. Preserve the rule that `Replied`, `Not Interested`, and `Do Not Contact` prospects leave the active queue.
6. Keep Claude calls intentional and user-triggered.
7. Store prompts in `prompts/`, not inside Shiny UI code.
8. Keep SQL inside `services/db.R`.
9. Keep cadence logic inside `services/outreach_logic.R`.
10. Keep `app.R`, `ui.R`, and `server.R` thin.
11. Prefer simple, local-first implementation before adding integrations.
12. Do not auto-send emails without explicit user review and action.

---

## Run the App

From the project folder:

```bash
R -e "shiny::runApp()"
```

Or open `app.R` in RStudio and click **Run App**.

---

## Required R Packages

Current expected packages:

```r
install.packages(c(
  "shiny",
  "DBI",
  "RSQLite",
  "DT",
  "readxl",
  "readr",
  "janitor",
  "yaml",
  "httr2",
  "jsonlite",
  "testthat"
))
```

Future integrations may require:

```r
install.packages(c(
  "blastula"
))
```

---

## Current Development Priority

The next development priority should be:

1. Finalize `utils/constants.R`
2. Finalize `services/db.R`
3. Update prospects module to use the `prospects` table
4. Update queue module to use `get_outreach_queue()`
5. Add draft generation and storage
6. Add manual copy/send workflow
7. Add Claude integration only after the local workflow is stable

---

## Guiding Principle

Signal should be boring, focused, and useful.

It should help the user consistently send better first-touch and follow-up emails without losing track of prospects.

The win condition is not a closed deal.

The win condition is a first reply.
