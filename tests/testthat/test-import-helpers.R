source("../../utils/constants.R", local = environment())
source("../../utils/helpers.R", local = environment())
source("../../services/outreach_logic.R", local = environment())
source("../../modules/mod_prospects_server.R", local = environment())

test_that("import column normalization accepts common aliases", {
  raw <- data.frame(
    First = " Ada ",
    Last = " Lovelace ",
    Account = " Analytical Engines Inc. ",
    Email.Address = " ada@example.com ",
    Facility.Type = " University ",
    Notes = " Capital planning angle ",
    stringsAsFactors = FALSE
  )
  
  cleaned <- normalize_import_columns(raw)
  
  expect_equal(cleaned$first_name, "Ada")
  expect_equal(cleaned$last_name, "Lovelace")
  expect_equal(cleaned$company, "Analytical Engines Inc.")
  expect_equal(cleaned$email, "ada@example.com")
  expect_equal(cleaned$segment, "University")
  expect_equal(cleaned$personalization_notes, "Capital planning angle")
})

test_that("import validation accepts email or full name plus company", {
  candidates <- data.frame(
    first_name = c("", "Grace", "Missing"),
    last_name = c("", "Hopper", ""),
    company = c("", "Navy", "Company"),
    email = c("email-only@example.com", "", ""),
    stringsAsFactors = FALSE
  )
  
  validated <- validate_import_rows(candidates)
  
  expect_equal(validated$import_status, c("Ready", "Ready", "Invalid"))
  expect_true(validated$is_valid[1])
  expect_true(validated$is_valid[2])
  expect_false(validated$is_valid[3])
})

test_that("duplicate detection checks email, LinkedIn, and name-company keys", {
  assign("get_prospects", function(include_inactive = TRUE) {
    data.frame(
      first_name = "Brian",
      last_name = "Balzar",
      company = "Upchurch",
      email = "bbalzar@upchurchus.com",
      linkedin_url = "https://linkedin.com/in/brian-balzar",
      stringsAsFactors = FALSE
    )
  }, envir = environment(flag_duplicate_prospects))
  
  candidates <- data.frame(
    first_name = c("Someone", "Other", "Brian", "New"),
    last_name = c("Else", "Person", "Balzar", "Person"),
    company = c("Other Co", "Other Co", "Upchurch", "New Co"),
    email = c("bbalzar@upchurchus.com", "", "", ""),
    linkedin_url = c("", "https://linkedin.com/in/brian-balzar", "", ""),
    import_status = "Ready",
    stringsAsFactors = FALSE
  )
  
  checked <- flag_duplicate_prospects(candidates)
  
  expect_equal(checked$import_status, c("Duplicate", "Duplicate", "Duplicate", "Ready"))
  expect_true(grepl("email match", checked$duplicate_reason[1]))
  expect_true(grepl("LinkedIn URL match", checked$duplicate_reason[2]))
  expect_true(grepl("name \\+ company match", checked$duplicate_reason[3]))
  expect_false(checked$is_duplicate[4])
})
