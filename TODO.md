# Signal TODO

This is the working backlog for Signal. Keep the app focused on first-response outbound outreach, not full CRM or pipeline management.

## Stabilize the Current App

- [x] Update README package requirements to match the real app dependencies: `readxl`, `readr`, `janitor`, `yaml`, `httr2`, and `jsonlite`.
- [x] Fix text encoding issues in the UI, such as `Today's Outreach Queue`.
- [x] Decide whether to remove or ignore the old empty `contacts` table in SQLite.
  Decision: leave it untouched for now so we do not silently mutate local data. Remove it later only through an explicit migration/backup step.
- [x] Add a simple startup/app health check so missing packages or missing `_secrets.yml` produce friendly messages.
- [x] Add a lightweight database migration pattern for future schema changes.
- [x] Add automated tests for core outreach sequence logic: status advancement, terminal statuses, cadence dates, and queue eligibility.
- [x] Add automated tests for database-backed workflow actions such as snoozing and touch logging.
- [x] Add automated tests for import cleaning, validation, and duplicate detection.

## Prospect Import

- [x] Improve import preview to clearly show `Ready`, `Duplicate`, and `Invalid` rows.
- [x] Add an import result summary after import: added, skipped duplicates, imported duplicates, and invalid rows.
- [x] Allow manually overriding duplicate detection when adding a one-off prospect.

## Prospect Management

- [x] Add edit capability for full prospect details, not only status/next touch.
- [x] Add touch history display for a selected prospect.
- [x] Add draft history display for a selected prospect.

## Drafts and Outreach

- [x] Save edits made in the draft subject/body before logging as sent, even if the draft was manually written.
- [x] Add a "Copy Draft" button for subject/body.
- [x] Add a "Create Draft Without Claude" option using the local fallback generator.
- [x] Make Claude research available from the UI only as an intentional button action.
- [x] Add fields or storage for research notes/sources if research is going to be used regularly.
- [x] Tighten the prompt file and Claude fallback so their instructions match exactly.

## Queue Behavior

- [x] Add a "mark bounced" workflow and decide whether bounced prospects stay active or leave the queue.
  Decision: `Bounced` stays active and visible so the email address can be corrected before the next outreach.
- [x] Add a nurture workflow decision: hide nurture by default, show in a separate filter, or keep in queue when due.
  Decision: default queue excludes nurture; the queue has a dedicated `Nurture` view.
- [x] Add queue filters: due today, overdue, all active, by segment/source.
- [x] Add visible queue counts: due today, overdue, active total, terminal total.

## Design and Operations

- [x] Move the default working SQLite database outside the OneDrive project folder when possible to avoid reparse-point I/O errors while leaving project copies untouched.
- [x] Improve visual layout and spacing while keeping it utilitarian.
- [x] Decide whether Outlook `mailto:` is enough for now or whether to add Gmail/SMTP/Microsoft Graph later.
  Decision: keep Outlook `mailto:` for now. Deeper Gmail/SMTP/Microsoft Graph integrations can wait until the review workflow is mature.
- [x] Add a local backup/export option for prospects, touches, and drafts.
- [ ] Initialize this folder as a Git repo once we are ready to track changes formally.
