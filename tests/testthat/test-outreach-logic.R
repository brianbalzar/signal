source("../../utils/constants.R")
source("../../services/outreach_logic.R")

test_that("terminal statuses are excluded from active outreach", {
  expect_true(is_terminal_status("Replied"))
  expect_true(is_terminal_status("Not Interested"))
  expect_true(is_terminal_status("Do Not Contact"))
  expect_false(is_terminal_status("Ready to Email"))
  expect_false(is_terminal_status("Bounced"))
})

test_that("sequence stages advance to the next expected status", {
  expect_equal(get_next_sequence_stage(0), 1)
  expect_equal(get_next_status(0), "Intro Sent")
  expect_equal(get_next_sequence_stage(1), 2)
  expect_equal(get_next_status(1), "Follow-Up 1 Sent")
  expect_equal(get_next_sequence_stage(5), 5)
  expect_equal(get_next_status(5), "Nurture")
})

test_that("next touch dates follow the configured cadence", {
  start_date <- as.Date("2026-05-05")
  
  expect_equal(get_next_touch_date(0, start_date), as.Date("2026-05-08"))
  expect_equal(get_next_touch_date(1, start_date), as.Date("2026-05-10"))
  expect_equal(get_next_touch_date(2, start_date), as.Date("2026-05-12"))
  expect_equal(get_next_touch_date(3, start_date), as.Date("2026-06-04"))
})

test_that("queue eligibility respects due dates and terminal statuses", {
  expect_true(should_show_in_queue("Ready to Email", Sys.Date()))
  expect_true(should_show_in_queue("Bounced", NA_character_))
  expect_true(should_show_in_queue("Ready to Email", Sys.Date() - 1))
  expect_false(should_show_in_queue("Ready to Email", Sys.Date() + 1))
  expect_false(should_show_in_queue("Replied", Sys.Date() - 1))
})

test_that("bounced prospects have a fix-address recommendation", {
  expect_equal(
    get_recommended_action("Bounced", 1),
    "Fix email address before next outreach"
  )
})

test_that("touch outcomes map terminal outcomes to terminal statuses", {
  expect_equal(status_from_touch_outcome("Replied", "Intro Sent"), "Replied")
  expect_equal(status_from_touch_outcome("Not Interested", "Intro Sent"), "Not Interested")
  expect_equal(status_from_touch_outcome("Do Not Contact", "Intro Sent"), "Do Not Contact")
  expect_equal(status_from_touch_outcome("Sent", "Intro Sent"), "Intro Sent")
})
