library(tablespan)
library(testthat)
library(dplyr)

test_tables <- build_tablespan_test_tables()

remove_token <- function(x) {
  x$token <- NULL
  x$url <- "https://sheets.googleapis.com/v4/spreadsheets/spreadsheet_id:batchUpdate"
  return(x)
}

googlesheets4::gs4_deauth()

dry_run <- TRUE

if (dry_run) {
  token <- NULL
} else {
  googlesheets4::gs4_auth()
  token <- googlesheets4::gs4_token()
}

# google_sheet <- googlesheets4::gs4_get("your-test-sheet-id-here")
google_sheet <- fake_gs4_dribble()

run_test <- function(
  tbl,
  google_sheet,
  sheet,
  token,
  dry_run,
  snapshot_variant = sheet,
  ...
) {
  if (!dry_run && !sheet %in% googlesheets4::sheet_names(google_sheet)) {
    googlesheets4::sheet_add(google_sheet, sheet)
  } else if (dry_run) {
    google_sheet <- add_fake_sheet(google_sheet, sheet_name = sheet)
  }

  gs_request <- as_googlesheet_request(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = sheet,
    dry_run = dry_run,
    token = token,
    silent = TRUE,
    ...
  )
  if (!dry_run) {
    googlesheets4::request_make(
      gs_request
    )
  }

  gs_request <- gs_request |>
    remove_token()

  request_text <- paste(capture.output(str(gs_request)), collapse = "\n")
  sanitized_url <- "https://sheets.googleapis.com/v4/spreadsheets/spreadsheet_id:batchUpdate"

  testthat::expect_null(gs_request$token)
  testthat::expect_true(
    grepl(sanitized_url, request_text, fixed = TRUE),
    info = "Request URL was not sanitized to the placeholder spreadsheet id."
  )
  testthat::expect_false(
    grepl("docs\\.google\\.com/spreadsheets/d/", request_text, perl = TRUE),
    info = "Request text contains a direct Google Sheets document URL."
  )
  testthat::expect_false(
    grepl(
      "Authorization|Bearer|access_token|refresh_token|oauth|api_key|client_secret|private_key",
      request_text,
      ignore.case = TRUE,
      perl = TRUE
    ),
    info = "Request text contains credential-like fields."
  )

  testthat::expect_snapshot(
    x = str(gs_request),
    variant = snapshot_variant
  )
}

testthat::test_that("Googlesheets works", {
  for (tbl in names(test_tables$tables)) {
    run_test(
      tbl = test_tables$tables[[tbl]],
      google_sheet = google_sheet,
      sheet = tbl,
      dry_run = dry_run,
      token = token
    )
  }
})

testthat::test_that("Googlesheets works with shift", {
  run_test(
    tbl = test_tables$tables$cars_tsf_hf_cs3_cell_style,
    google_sheet = google_sheet,
    sheet = paste0(
      "cars_tsf_hf_cs3_cell_style",
      "_shifted"
    ),
    dry_run = dry_run,
    token = token,
    start_row = 5,
    start_col = 3
  )
})

testthat::test_that("formula-esaping works", {
  data <- tibble::tibble(
    equal = "=A1",
    plus = "+A1",
    minus = "-A1",
    at = "@A1"
  )
  tbl <- tablespan::tablespan(
    data = data,
    formula = `=A1`:equal ~ `+A1`:plus + `-A1`:minus + `@A1`:at
  )

  run_test(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = "Formula_Escaping",
    dry_run = dry_run,
    token = token,
    start_row = 4,
    start_col = 2
  )
})
