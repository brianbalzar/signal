# services/claude.R
# Claude integration for Signal
#
# Claude should only be called when the user intentionally takes an action:
# - Generate Draft
# - Research Prospect
# - Prep Call
#
# This file handles:
# - reading local secrets
# - building prospect-centered email prompts
# - calling Anthropic's Messages API
# - optionally using Claude web search for public research
# - parsing subject/body output
# - parsing research output
# - cleaning placeholders/signatures
# - falling back safely if Claude is unavailable
#
# Requires:
# install.packages(c("yaml", "httr2", "jsonlite"))

# ---- Secrets ----------------------------------------------------------------

read_secrets <- function(path = "_secrets.yml") {
  if (!file.exists(path)) {
    stop(
      "Missing _secrets.yml. Create it from _secrets.example.yml and add your Anthropic API key.",
      call. = FALSE
    )
  }

  yaml::read_yaml(path)
}

get_claude_config <- function() {
  secrets <- read_secrets()

  if (is.null(secrets$claude$api_key) || secrets$claude$api_key == "") {
    stop("Missing claude.api_key in _secrets.yml.", call. = FALSE)
  }

  model <- secrets$claude$model

  if (is.null(model) || model == "") {
    model <- "claude-sonnet-4-6"
  }

  web_search_type <- secrets$claude$web_search_type

  if (is.null(web_search_type) || web_search_type == "") {
    web_search_type <- "web_search_20260209"
  }

  list(
    api_key = secrets$claude$api_key,
    model = model,
    web_search_type = web_search_type
  )
}


# ---- Prompt loading ---------------------------------------------------------

load_prompt_template <- function(path = "prompts/intro_email.txt") {
  if (!file.exists(path)) {
    return(default_intro_prompt())
  }

  paste(readLines(path, warn = FALSE), collapse = "\n")
}

default_intro_prompt <- function() {
  paste(
    "You are writing a concise, personalized outbound email for a facility consulting company.",
    "",
    "The company helps building owners and facility teams improve HVAC performance, controls optimization, BAS/BMS issues, energy efficiency, and operational reliability using analytics.",
    "",
    "Write an outbound email for the current sequence stage.",
    "",
    "Rules:",
    "- Maximum 120 words.",
    "- Plainspoken and consultative.",
    "- Do not sound like marketing copy.",
    "- Do not overclaim.",
    "- Mention one specific reason this account may be relevant.",
    "- Mention one likely operational pain, but do not pretend we know it is happening.",
    "- You may mention that analytics-driven HVAC and controls optimization can often uncover 10-30% energy or operational savings opportunities, but do not guarantee savings.",
    "- Use conditional language such as 'can often,' 'may,' 'depending on the facility,' or 'opportunities can fall in the 10-30% range.'",
    "- You may subtly mention that Brian has spent nearly 20 years working around facility performance, HVAC, controls, and energy analytics, but only if it supports a useful observation.",
    "- Do not make the email about Brian.",
    "- Prefer phrasing like: 'I've spent nearly 20 years working around facility performance, HVAC, controls, and energy analytics, and one thing I keep seeing is...'",
    "- Do not describe the prospect as being in 'facility consulting' unless that phrase is explicitly provided in the prospect data.",
    "- For most prospects, refer to their facility role, organization, building type, public project, or operational responsibility instead.",
    "- Do not mention 'Energy Manager as a Service' unless the user explicitly includes that as the offer or angle in the prospect notes or research notes.",
    "- Ask for a low-pressure conversation.",
    "- Prefer the CTA: 'Would it be worth a brief conversation to compare what we typically look for against what you're seeing?'",
    "- Include a subject line.",
    "- Do not use emojis.",
    "- Do not use fake case study numbers unless provided.",
    "- End the email with exactly:",
    "  Best,",
    "  Brian",
    "- Never use placeholders like [Your Name], [Company], [Prospect], [First Name], or anything inside square brackets.",
    "- If a field is missing, write around it naturally instead of inserting a placeholder.",
    sep = "\n"
  )
}


# ---- Prompt construction: email ---------------------------------------------

build_prospect_context <- function(prospect) {
  paste(
    paste0("First name: ", prospect$first_name %||% ""),
    paste0("Last name: ", prospect$last_name %||% ""),
    paste0("Company: ", prospect$company %||% ""),
    paste0("Title: ", prospect$title %||% ""),
    paste0("Email: ", prospect$email %||% ""),
    paste0("LinkedIn URL: ", prospect$linkedin_url %||% ""),
    paste0("Website: ", prospect$website %||% ""),
    paste0("City: ", prospect$city %||% ""),
    paste0("State: ", prospect$state %||% ""),
    paste0("Source: ", prospect$source %||% ""),
    paste0("Segment: ", prospect$segment %||% ""),
    paste0("Reason for outreach: ", prospect$reason_for_outreach %||% ""),
    paste0("Personalization notes: ", prospect$personalization_notes %||% ""),
    paste0("Research notes: ", prospect$research_notes %||% ""),
    paste0("Research sources: ", prospect$research_sources %||% ""),
    paste0("Current sequence stage: ", format_sequence_stage(prospect$sequence_stage)),
    paste0("Recommended action: ", get_recommended_action(prospect$status, prospect$sequence_stage)),
    sep = "\n"
  )
}

build_stage_instructions <- function(sequence_stage) {
  stage <- normalize_sequence_stage(sequence_stage)

  if (stage == 0) {
    return(paste(
      "Email type: First-touch intro email.",
      "Goal: Start a low-pressure conversation.",
      "Do not imply prior conversation.",
      "Use one specific reason for outreach if available.",
      sep = "\n"
    ))
  }

  if (stage == 1) {
    return(paste(
      "Email type: Follow-up 1.",
      "Goal: Politely follow up on the first note and add a useful insight.",
      "Keep it short. Do not guilt the reader.",
      sep = "\n"
    ))
  }

  if (stage == 2) {
    return(paste(
      "Email type: Follow-up 2.",
      "Goal: Add a more specific operational angle around HVAC, controls, scheduling, overrides, energy waste, or operations.",
      "Keep it consultative and low-pressure.",
      sep = "\n"
    ))
  }

  if (stage == 3) {
    return(paste(
      "Email type: Breakup email.",
      "Goal: Politly ask whether to close the loop.",
      "Keep it very short and respectful.",
      sep = "\n"
    ))
  }

  paste(
    "Email type: Nurture email.",
    "Goal: Re-engage only if there is a useful reason.",
    "Keep it short and do not force urgency.",
    sep = "\n"
  )
}

build_claude_prompt <- function(prospect) {
  base_prompt <- load_prompt_template()
  stage_instructions <- build_stage_instructions(prospect$sequence_stage)
  prospect_context <- build_prospect_context(prospect)

  paste(
    base_prompt,
    "",
    "Additional sequence instructions:",
    stage_instructions,
    "",
    "Credibility guidance:",
    "- Brian has spent nearly 20 years working around facility performance, HVAC, controls, and energy analytics.",
    "- You may use this as subtle credibility, but only to support a useful observation.",
    "- Do not make the email about Brian's resume.",
    "- Good example: 'I've spent nearly 20 years working around facility performance, HVAC, controls, and energy analytics, and one thing I keep seeing is that many issues are hard to catch until they show up as comfort complaints, high utility bills, or operator workarounds.'",
    "- Bad example: 'I have 20 years of experience and can help you.'",
    "",
    "Savings language guidance:",
    "- It is acceptable to say we use analytics as the backbone to uncover HVAC, controls, and operational issues.",
    "- It is acceptable to say these opportunities can often fall in the 10-30% range for energy or operational savings, depending on the facility.",
    "- Never phrase savings as guaranteed.",
    "- Do not say 'we will save you 10-30%.'",
    "- Prefer language like: 'opportunities can often fall in the 10-30% range depending on the facility and what is uncovered.'",
    "",
    "Offer guidance:",
    "- Default offer: analytics-driven HVAC, controls, and operational performance review.",
    "- Do not use named service models unless they appear in reason_for_outreach, personalization_notes, or research_notes.",
    "- Do not mention 'Energy Manager as a Service' unless it appears in reason_for_outreach, personalization_notes, or research_notes.",
    "- Do not describe the prospect as being in 'facility consulting' unless that phrase is explicitly provided in the prospect data.",
    "- For most prospects, refer to their facility role, organization, building type, public project, or operational responsibility instead.",
    "",
    "CTA guidance:",
    "- Prefer this CTA or a close variation:",
    "  Would it be worth a brief conversation to compare what we typically look for against what you're seeing?",
    "",
    "Signature rules:",
    "- End with exactly:",
    "  Best,",
    "  Brian",
    "- Do not use 'Thanks, [Your Name]'.",
    "- Do not use any bracketed placeholders.",
    "",
    "Output format:",
    "Return only valid JSON with exactly these keys:",
    "{",
    '  "subject": "string",',
    '  "body": "string"',
    "}",
    "",
    "Do not wrap the JSON in markdown.",
    "",
    "Prospect data:",
    prospect_context,
    sep = "\n"
  )
}


# ---- Prompt construction: research ------------------------------------------

build_research_prompt <- function(prospect) {
  company <- prospect$company %||% ""
  first_name <- prospect$first_name %||% ""
  last_name <- prospect$last_name %||% ""
  title <- prospect$title %||% ""
  city <- prospect$city %||% ""
  state <- prospect$state %||% ""
  website <- prospect$website %||% ""
  segment <- prospect$segment %||% ""

  paste(
    "You are doing quick public research for a facility consulting outbound email.",
    "",
    "Goal:",
    "Find only the most useful public signals that could make an outreach email more relevant.",
    "",
    "Use a fast scan. Prefer 1-2 high-quality searches over a broad search. Look for:",
    "- bond activity or bond-funded projects",
    "- capital projects",
    "- construction, renovation, or expansion",
    "- facility improvement plans",
    "- energy efficiency, sustainability, or infrastructure initiatives",
    "- HVAC, controls, BAS/BMS, or mechanical system references",
    "",
    "Important rules:",
    "- Use only public information found through web search.",
    "- Prefer official/public sources such as company websites, district pages, board agendas, public filings, local news, and government sources.",
    "- Do not invent facts.",
    "- If nothing useful is found, say that.",
    "- Do not claim the prospect has a problem.",
    "- Use soft language such as 'may be relevant,' 'I noticed,' or 'could be worth a conversation.'",
    "- Do not write the email here. Only return research and suggested personalization inputs.",
    "- Do not suggest 'Energy Manager as a Service' unless public information or user-provided notes clearly support that angle.",
    "",
    "Prospect:",
    paste0("- First name: ", first_name),
    paste0("- Last name: ", last_name),
    paste0("- Title: ", title),
    paste0("- Company: ", company),
    paste0("- Website: ", website),
    paste0("- City: ", city),
    paste0("- State: ", state),
    paste0("- Segment: ", segment),
    "",
    "Suggested searches to prioritize:",
    paste0("- ", company, " capital project"),
    paste0("- ", company, " bond HVAC controls facilities"),
    "",
    "Output format:",
    "Return only valid JSON with exactly these keys:",
    "{",
    '  "summary": "short summary of what was found",',
    '  "signals": ["signal 1", "signal 2", "signal 3"],',
    '  "suggested_reason_for_outreach": "short reason for outreach",',
    '  "suggested_personalization_notes": "notes that can help write the email",',
    '  "sources": ["source title or URL 1", "source title or URL 2"]',
    "}",
    "",
    "Do not wrap the JSON in markdown.",
    sep = "\n"
  )
}

build_call_prep_prompt <- function(prospect) {
  prospect_context <- build_prospect_context(prospect)

  paste(
    "You are preparing a seller for a phone call with a facility prospect.",
    "",
    "Goal:",
    "Create concise talking points that help Brian make a useful, low-pressure call.",
    "",
    "Important rules:",
    "- Do not write a long script.",
    "- Do not invent facts.",
    "- Use the prospect data and research notes if available.",
    "- Keep the tone plainspoken, consultative, and practical.",
    "- Focus on facility performance, HVAC, controls, analytics, reliability, comfort, energy, and operations where relevant.",
    "- Do not claim the prospect has a problem.",
    "- Use soft language such as 'may be worth asking,' 'could be relevant,' or 'I noticed.'",
    "- Include a voicemail option.",
    "- Include a simple follow-up email angle if the call does not connect.",
    "",
    "Output format:",
    "Return only valid JSON with exactly these keys:",
    "{",
    '  "objective": "string",',
    '  "opener": "string",',
    '  "talking_points": ["point 1", "point 2", "point 3"],',
    '  "discovery_questions": ["question 1", "question 2", "question 3"],',
    '  "voicemail": "string",',
    '  "follow_up_angle": "string"',
    "}",
    "",
    "Do not wrap the JSON in markdown.",
    "",
    "Prospect data:",
    prospect_context,
    sep = "\n"
  )
}


# ---- Claude API calls --------------------------------------------------------

call_claude <- function(prompt, max_tokens = 700, temperature = 0.4) {
  call_claude_messages(
    prompt = prompt,
    max_tokens = max_tokens,
    temperature = temperature,
    use_web_search = FALSE
  )
}

call_claude_with_web_search <- function(
    prompt,
    max_tokens = DEFAULT_RESEARCH_MAX_TOKENS,
    temperature = 0.2,
    max_uses = DEFAULT_RESEARCH_WEB_SEARCH_USES
) {
  call_claude_messages(
    prompt = prompt,
    max_tokens = max_tokens,
    temperature = temperature,
    use_web_search = TRUE,
    max_uses = max_uses
  )
}

call_claude_messages <- function(
    prompt,
    max_tokens = 700,
    temperature = 0.4,
    use_web_search = FALSE,
    max_uses = 5
) {
  config <- get_claude_config()

  body <- list(
    model = config$model,
    max_tokens = max_tokens,
    temperature = temperature,
    messages = list(
      list(
        role = "user",
        content = prompt
      )
    )
  )

  if (isTRUE(use_web_search)) {
    body$tools <- list(
      list(
        type = config$web_search_type,
        name = "web_search",
        max_uses = max_uses
      )
    )
  }

  response <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      "x-api-key" = config$api_key,
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_perform()

  parsed <- httr2::resp_body_json(response, simplifyVector = FALSE)

  extract_text_from_claude_response(parsed)
}

extract_text_from_claude_response <- function(parsed) {
  if (is.null(parsed$content) || length(parsed$content) == 0) {
    stop("Claude response did not include content.", call. = FALSE)
  }

  text_blocks <- vapply(
    parsed$content,
    function(block) {
      if (!is.null(block$type) && block$type == "text") {
        return(block$text %||% "")
      }

      ""
    },
    character(1)
  )

  text <- paste(text_blocks, collapse = "\n")
  text <- trimws(text)

  if (text == "") {
    stop("Claude response did not include text content.", call. = FALSE)
  }

  text
}


# ---- Response parsing: email ------------------------------------------------

parse_claude_email_response <- function(raw_text) {
  cleaned <- strip_json_code_fence(raw_text)

  parsed <- tryCatch(
    jsonlite::fromJSON(cleaned),
    error = function(e) NULL
  )

  if (is.null(parsed)) {
    return(list(
      subject = "Draft email",
      body = clean_email_signature(clean_email_placeholders(cleaned))
    ))
  }

  subject <- parsed$subject %||% "Draft email"
  body <- parsed$body %||% ""

  list(
    subject = clean_email_placeholders(subject),
    body = clean_email_signature(clean_email_placeholders(body))
  )
}


# ---- Response parsing: research --------------------------------------------

parse_claude_research_response <- function(raw_text) {
  cleaned <- strip_json_code_fence(raw_text)
  json_candidate <- extract_json_object(cleaned)

  parsed <- tryCatch(
    jsonlite::fromJSON(json_candidate),
    error = function(e) NULL
  )

  if (is.null(parsed)) {
    return(list(
      summary = cleaned,
      signals = character(0),
      suggested_reason_for_outreach = "",
      suggested_personalization_notes = cleaned,
      sources = character(0),
      raw = cleaned
    ))
  }

  signals <- parsed$signals
  if (is.null(signals) || length(signals) == 0) {
    signals <- character(0)
  }

  sources <- parsed$sources
  if (is.null(sources) || length(sources) == 0) {
    sources <- character(0)
  }

  list(
    summary = parsed$summary %||% "",
    signals = as.character(signals),
    suggested_reason_for_outreach = parsed$suggested_reason_for_outreach %||% "",
    suggested_personalization_notes = parsed$suggested_personalization_notes %||% "",
    sources = as.character(sources),
    raw = cleaned
  )
}

parse_claude_call_prep_response <- function(raw_text) {
  cleaned <- strip_json_code_fence(raw_text)
  json_candidate <- extract_json_object(cleaned)

  parsed <- tryCatch(
    jsonlite::fromJSON(json_candidate),
    error = function(e) NULL
  )

  if (is.null(parsed)) {
    return(list(
      objective = "Make a useful, low-pressure call.",
      opener = "",
      talking_points = cleaned,
      discovery_questions = character(0),
      voicemail = "",
      follow_up_angle = "",
      raw = cleaned
    ))
  }

  list(
    objective = parsed$objective %||% "",
    opener = parsed$opener %||% "",
    talking_points = normalize_call_prep_values(parsed$talking_points),
    discovery_questions = normalize_call_prep_values(parsed$discovery_questions),
    voicemail = parsed$voicemail %||% "",
    follow_up_angle = parsed$follow_up_angle %||% "",
    raw = cleaned
  )
}

format_research_notes <- function(research_result) {
  signals <- normalize_research_values(research_result$signals)
  sources <- normalize_research_values(research_result$sources)

  signals_text <- if (length(signals) > 0) {
    paste0("- ", signals, collapse = "\n")
  } else {
    "No specific public signals found."
  }

  sources_text <- if (length(sources) > 0) {
    paste0("- ", sources, collapse = "\n")
  } else {
    "No sources returned."
  }

  paste(
    "Research Summary:",
    research_result$summary %||% "",
    "",
    "Signals:",
    signals_text,
    "",
    "Suggested Reason for Outreach:",
    research_result$suggested_reason_for_outreach %||% "",
    "",
    "Suggested Personalization Notes:",
    research_result$suggested_personalization_notes %||% "",
    "",
    "Sources:",
    sources_text,
    sep = "\n"
  )
}

normalize_research_values <- function(values) {
  if (is.null(values) || length(values) == 0) {
    return(character(0))
  }

  values <- unlist(values, use.names = FALSE)
  values <- trimws(as.character(values))
  values[!is.na(values) & values != ""]
}

format_call_prep_notes <- function(call_prep) {
  talking_points <- normalize_call_prep_values(call_prep$talking_points)
  questions <- normalize_call_prep_values(call_prep$discovery_questions)

  talking_points_text <- if (length(talking_points) > 0) {
    paste0("- ", talking_points, collapse = "\n")
  } else {
    "- Ask what is top of mind for facilities performance right now."
  }

  questions_text <- if (length(questions) > 0) {
    paste0("- ", questions, collapse = "\n")
  } else {
    "- Are HVAC, controls, comfort, or utility costs creating any operational pressure?"
  }

  paste(
    "Call Objective:",
    call_prep$objective %||% "",
    "",
    "Opener:",
    call_prep$opener %||% "",
    "",
    "Talking Points:",
    talking_points_text,
    "",
    "Discovery Questions:",
    questions_text,
    "",
    "Voicemail:",
    call_prep$voicemail %||% "",
    "",
    "Follow-Up Angle:",
    call_prep$follow_up_angle %||% "",
    sep = "\n"
  )
}

normalize_call_prep_values <- function(values) {
  if (is.null(values) || length(values) == 0) {
    return(character(0))
  }

  values <- unlist(values, use.names = FALSE)
  values <- trimws(as.character(values))
  values[!is.na(values) & values != ""]
}


# ---- Public draft generation ------------------------------------------------

generate_email <- function(prospect) {
  prompt <- build_claude_prompt(prospect)
  raw_response <- call_claude(prompt)
  parse_claude_email_response(raw_response)
}

generate_email_safe <- function(prospect) {
  tryCatch(
    {
      generate_email(prospect)
    },
    error = function(e) {
      fallback_generate_email_from_claude_service(
        prospect,
        error_message = conditionMessage(e)
      )
    }
  )
}

generate_call_prep <- function(prospect) {
  prompt <- build_call_prep_prompt(prospect)
  raw_response <- call_claude(prompt, max_tokens = 900, temperature = 0.3)
  call_prep <- parse_claude_call_prep_response(raw_response)
  fallback_name <- trimws(paste(prospect$first_name %||% "", prospect$last_name %||% ""))

  if (fallback_name == "") {
    fallback_name <- "Prospect"
  }

  list(
    subject = paste("Call prep:", prospect$company %||% fallback_name),
    body = format_call_prep_notes(call_prep),
    raw = call_prep$raw
  )
}

generate_call_prep_safe <- function(prospect) {
  tryCatch(
    {
      generate_call_prep(prospect)
    },
    error = function(e) {
      fallback_generate_call_prep_from_claude_service(
        prospect,
        error_message = conditionMessage(e)
      )
    }
  )
}


# ---- Public research generation --------------------------------------------

research_prospect_with_claude <- function(prospect) {
  prompt <- build_research_prompt(prospect)
  raw_response <- call_claude_with_web_search(
    prompt,
    max_tokens = DEFAULT_RESEARCH_MAX_TOKENS,
    max_uses = DEFAULT_RESEARCH_WEB_SEARCH_USES
  )
  research_result <- parse_claude_research_response(raw_response)

  research_result$formatted_notes <- format_research_notes(research_result)
  research_result
}

research_prospect_with_claude_safe <- function(prospect) {
  tryCatch(
    {
      research_prospect_with_claude(prospect)
    },
    error = function(e) {
      fallback_research_result(
        prospect,
        error_message = conditionMessage(e)
      )
    }
  )
}

fallback_research_result <- function(prospect, error_message = NULL) {
  company <- prospect$company %||% "this prospect"

  summary <- paste0(
    "Claude research was unavailable for ",
    company,
    ". No public research was added."
  )

  if (!is.null(error_message) && error_message != "") {
    summary <- paste0(summary, " Error: ", error_message)
  }

  result <- list(
    summary = summary,
    signals = character(0),
    suggested_reason_for_outreach = prospect$reason_for_outreach %||% "",
    suggested_personalization_notes = prospect$personalization_notes %||% "",
    sources = character(0),
    raw = summary
  )

  result$formatted_notes <- format_research_notes(result)
  result
}


# ---- Cleanup helpers --------------------------------------------------------

strip_json_code_fence <- function(x) {
  cleaned <- trimws(x)

  cleaned <- sub("^```json\\s*", "", cleaned)
  cleaned <- sub("^```\\s*", "", cleaned)
  cleaned <- sub("\\s*```$", "", cleaned)

  trimws(cleaned)
}

extract_json_object <- function(x) {
  x <- trimws(x)

  if (grepl("^\\{", x)) {
    return(x)
  }

  start <- regexpr("\\{", x)
  ends <- gregexpr("\\}", x)[[1]]

  if (start[1] < 1 || length(ends) == 0 || max(ends) <= start[1]) {
    return(x)
  }

  substr(x, start[1], max(ends))
}

clean_email_placeholders <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("")
  }

  x <- as.character(x)

  x <- gsub("\\[Your Name\\]", "Brian", x, ignore.case = TRUE)
  x <- gsub("\\[your name\\]", "Brian", x, ignore.case = TRUE)
  x <- gsub("\\[Name\\]", "Brian", x, ignore.case = TRUE)
  x <- gsub("\\[Company\\]", "", x, ignore.case = TRUE)
  x <- gsub("\\[Prospect\\]", "", x, ignore.case = TRUE)
  x <- gsub("\\[First Name\\]", "", x, ignore.case = TRUE)

  # Remove any remaining bracketed placeholders.
  x <- gsub("\\[[^\\]]+\\]", "", x)

  trimws(x)
}

clean_email_signature <- function(body) {
  if (is.null(body) || length(body) == 0 || is.na(body)) {
    return("")
  }

  body <- trimws(as.character(body))
  body <- clean_email_placeholders(body)

  # Normalize common generic signatures.
  body <- gsub(
    "(?i)\\n\\s*Thanks,?\\s*Brian\\s*$",
    "\n\nBest,\nBrian",
    body,
    perl = TRUE
  )

  body <- gsub(
    "(?i)\\n\\s*Thank you,?\\s*Brian\\s*$",
    "\n\nBest,\nBrian",
    body,
    perl = TRUE
  )

  body <- gsub(
    "(?i)\\n\\s*Thanks,?\\s*$",
    "\n\nBest,\nBrian",
    body,
    perl = TRUE
  )

  body <- gsub(
    "(?i)\\n\\s*Thank you,?\\s*$",
    "\n\nBest,\nBrian",
    body,
    perl = TRUE
  )

  body <- gsub(
    "(?i)\\n\\s*Best regards,?\\s*Brian\\s*$",
    "\n\nBest,\nBrian",
    body,
    perl = TRUE
  )

  body <- gsub(
    "(?i)\\n\\s*Regards,?\\s*Brian\\s*$",
    "\n\nBest,\nBrian",
    body,
    perl = TRUE
  )

  # If the body does not end with Brian, append the approved signature.
  if (!grepl("(?i)\\bBrian\\s*$", body, perl = TRUE)) {
    body <- paste0(body, "\n\nBest,\nBrian")
  }

  # If it ends with Brian but not the exact approved signature, normalize the last signoff.
  body <- gsub(
    "(?is)\\n\\s*(thanks|thank you|best regards|regards|best),?\\s*\\n?\\s*Brian\\s*$",
    "\n\nBest,\nBrian",
    body,
    perl = TRUE
  )

  trimws(body)
}


# ---- Fallback email generation ---------------------------------------------
# This keeps the app usable when:
# - _secrets.yml is missing
# - API key is missing
# - internet/API request fails
# - Claude returns malformed output

fallback_generate_email_from_claude_service <- function(prospect, error_message = NULL) {
  first_name <- prospect$first_name %||% "there"
  company <- prospect$company %||% "your organization"
  reason <- prospect$reason_for_outreach %||% "your facilities work"
  notes <- prospect$personalization_notes %||% ""
  research_notes <- prospect$research_notes %||% ""

  if (reason == "your facilities work" && research_notes != "") {
    reason <- "some public information that may be relevant to your facilities work"
  }

  stage <- normalize_sequence_stage(prospect$sequence_stage)

  if (stage == 0) {
    subject <- paste("Quick question on", company)

    body <- paste0(
      "Hi ", first_name, ",\n\n",
      "I noticed ", company, " and wanted to reach out because ", reason, ".\n\n",
      "I've spent nearly 20 years working around facility performance, HVAC, controls, and energy analytics, and one thing I keep seeing is that many issues are hard to catch until they show up as comfort complaints, high utility bills, or operator workarounds.\n\n",
      "We use analytics as the backbone to uncover those issues earlier. Depending on the facility and what is uncovered, opportunities can often fall in the 10-30% range for energy or operational savings.\n\n",
      "Would it be worth a brief conversation to compare what we typically look for against what you're seeing?\n\n",
      "Best,\n",
      "Brian"
    )
  } else if (stage == 1) {
    subject <- paste("Re:", "Quick question on", company)

    body <- paste0(
      "Hi ", first_name, ",\n\n",
      "Just wanted to follow up on my note below.\n\n",
      "The main reason I reached out is that many facility issues are hard to catch until they show up as comfort complaints, utility spend, or operator workarounds. Analytics can often surface those issues earlier across HVAC performance, controls, and day-to-day operations.\n\n",
      "Would a brief conversation be useful?\n\n",
      "Best,\n",
      "Brian"
    )
  } else if (stage == 2) {
    subject <- "Worth comparing notes?"

    body <- paste0(
      "Hi ", first_name, ",\n\n",
      "One more quick follow-up.\n\n",
      "We usually start by looking for signs of schedule drift, overrides, simultaneous heating/cooling, sensor issues, or controls sequences that no longer match how the building is actually used.\n\n",
      "Would it be worth comparing notes for ", company, "?\n\n",
      "Best,\n",
      "Brian"
    )
  } else {
    subject <- "Should I close the loop?"

    body <- paste0(
      "Hi ", first_name, ",\n\n",
      "I do not want to keep cluttering your inbox.\n\n",
      "Should I close the loop here, or is improving HVAC / controls performance something worth revisiting at ", company, "?\n\n",
      "Best,\n",
      "Brian"
    )
  }

  list(
    subject = clean_email_placeholders(subject),
    body = clean_email_signature(clean_email_placeholders(body))
  )
}

fallback_generate_call_prep_from_claude_service <- function(prospect, error_message = NULL) {
  company <- prospect$company %||% "the organization"
  first_name <- prospect$first_name %||% "there"
  reason <- prospect$reason_for_outreach %||% "their facility work may be relevant"
  research_notes <- prospect$research_notes %||% ""

  public_signal <- if (research_notes != "") {
    "Reference the saved research softly if it feels relevant."
  } else {
    "Use the reason for outreach and ask a practical facilities question."
  }

  if (!is.null(error_message) && error_message != "") {
    public_signal <- paste(public_signal, "Claude call prep was unavailable, so this is a local prep.")
  }

  body <- paste(
    "Call Objective:",
    paste("Start a low-pressure conversation with", company, "about facility performance, HVAC, controls, analytics, or operational reliability."),
    "",
    "Opener:",
    paste0("Hi ", first_name, ", this is Brian Balzar. I noticed ", company, " and wanted to ask a quick facilities-performance question."),
    "",
    "Talking Points:",
    paste0("- Reason for outreach: ", reason),
    "- Many HVAC and controls issues are hard to see until they show up as comfort complaints, utility spend, or operator workarounds.",
    "- Analytics can help identify scheduling drift, overrides, sensor issues, simultaneous heating/cooling, and controls sequences that no longer match building use.",
    paste0("- ", public_signal),
    "",
    "Discovery Questions:",
    "- Are HVAC, controls, comfort, or utility costs creating any operational pressure right now?",
    "- Are there facilities or buildings where performance has become harder to manage?",
    "- Would it be useful to compare what we typically look for against what you are seeing?",
    "",
    "Voicemail:",
    paste0("Hi ", first_name, ", this is Brian Balzar. I had a quick facilities-performance question for you around HVAC, controls, and analytics. I will send a short note as well."),
    "",
    "Follow-Up Angle:",
    "Send a brief email referencing the call attempt and ask whether a short comparison conversation would be useful.",
    sep = "\n"
  )

  list(
    subject = paste("Call prep:", company),
    body = body,
    raw = body
  )
}


# ---- Small infix helper -----------------------------------------------------
# Keeps this file self-contained in case outreach_logic.R has not been sourced.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }

  first_value <- x[1]

  if (is.null(first_value) || is.na(first_value) || first_value == "") {
    return(y)
  }

  x
}
