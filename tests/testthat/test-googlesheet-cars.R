library(tablespan)
library(testthat)
library(dplyr)
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
    req <- googlesheets4::request_make(
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

test_that("cars", {
  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(
      N = n(),
      mean_hp = mean(hp),
      sd_hp = sd(hp),
      mean_wt = mean(wt),
      sd_wt = sd(wt)
    )

  tbl <- tablespan(
    data = summarized_table,
    formula = Cylinder:cyl + Engine:vs ~
      N +
      (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
      (`Weight` = Mean:mean_wt + SD:sd_wt),
    title = "Motor Trend Car Road Tests",
    subtitle = "A table created with tablespan",
    footnote = "Data from the infamous mtcars data set."
  )

  run_test(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = "Test1",
    dry_run = dry_run,
    token = token
  )
})

test_that("cars-additional_spanners", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(
      N = n(),
      mean_hp = mean(hp),
      sd_hp = sd(hp),
      mean_wt = mean(wt),
      sd_wt = sd(wt)
    )

  tbl <- tablespan(
    data = summarized_table,
    formula = Cylinder:cyl + Engine:vs ~
      (Results = N +
        (`Horse Power` = (Mean = Mean:mean_hp) +
          (`Standard Deviation` = SD:sd_hp)) +
        (`Weight` = Mean:mean_wt + SD:sd_wt)),
    title = "Motor Trend Car Road Tests",
    subtitle = "A table created with tablespan",
    footnote = "Data from the infamous mtcars data set."
  )

  run_test(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = "Test2",
    dry_run = dry_run,
    token = token
  )
})

test_that("cars-no_row_names", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(
      N = n(),
      mean_hp = mean(hp),
      sd_hp = sd(hp),
      mean_wt = mean(wt),
      sd_wt = sd(wt)
    )

  # no row names
  tbl <- tablespan(
    data = summarized_table,
    formula = 1 ~
      (Results = N +
        (`Horse Power` = (Mean = Mean:mean_hp) +
          (`Standard Deviation` = SD:sd_hp)) +
        (`Weight` = Mean:mean_wt + SD:sd_wt)),
    title = "Motor Trend Car Road Tests",
    subtitle = "A table created with tablespan",
    footnote = "Data from the infamous mtcars data set."
  )

  run_test(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = "Test3",
    dry_run = dry_run,
    token = token
  )
})

test_that("cars-no_titles", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(
      N = n(),
      mean_hp = mean(hp),
      sd_hp = sd(hp),
      mean_wt = mean(wt),
      sd_wt = sd(wt)
    )

  tbl <- tablespan(
    data = summarized_table,
    formula = Cylinder:cyl + Engine:vs ~
      N +
      (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
      (`Weight` = Mean:mean_wt + SD:sd_wt),
    footnote = "Data from the infamous mtcars data set."
  )

  run_test(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = "Test4",
    dry_run = dry_run,
    token = token
  )
})

test_that("cars-no_titles_no_footnotes", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(
      N = n(),
      mean_hp = mean(hp),
      sd_hp = sd(hp),
      mean_wt = mean(wt),
      sd_wt = sd(wt)
    )

  tbl <- tablespan(
    data = summarized_table,
    formula = Cylinder:cyl + Engine:vs ~
      N +
      (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
      (`Weight` = Mean:mean_wt + SD:sd_wt)
  )

  run_test(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = "Test5",
    dry_run = dry_run,
    token = token
  )
})

test_that("cars-duplicated_spanner_names", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  model_1 <- lm(mpg ~ wt + qsec, data = mtcars) |>
    summary() |>
    (\(.x) as.data.frame(.x$coefficients))()
  model_2 <- lm(mpg ~ wt + qsec, data = mtcars) |>
    summary() |>
    (\(.x) as.data.frame(.x$coefficients))()

  model_1$Parameter <- rownames(model_1)
  model_2$Parameter <- rownames(model_2)

  combined_models <- full_join(model_1, model_2, by = "Parameter")

  tbl <- combined_models |>
    dplyr::as_tibble() |>
    tablespan(
      formula = Parameter ~
        (`Model 1` = Estimate:Estimate.x +
          (Significance = `t-value`:`t value.x` + `p-value`:`Pr(>|t|).x`)) +
        (`Model 2` = Estimate:Estimate.y +
          (Significance = `t-value`:`t value.y` + `p-value`:`Pr(>|t|).y`))
    )

  run_test(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = "Test6",
    dry_run = dry_run,
    token = token
  )
})


test_that("cars - googlesheets styling", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(
      N = n(),
      mean_hp = mean(hp),
      sd_hp = sd(hp),
      mean_wt = mean(wt),
      sd_wt = sd(wt)
    )

  tbl <- tablespan(
    data = summarized_table,
    formula = Cylinder:cyl + Engine:vs ~
      N +
      (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
      (`Weight` = Mean:mean_wt + SD:sd_wt),
    title = "Motor Trend Car Road Tests",
    subtitle = "A table created with tablespan",
    footnote = "Data from the infamous mtcars data set."
  )

  run_test(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = "Test7",
    dry_run = dry_run,
    token = token
  )

  # title
  run_test(
    tbl = tbl |>
      style_title(text_color = "#000000", background_color = "#983439"),
    google_sheet = google_sheet,
    sheet = "Test8",
    dry_run = dry_run,
    token = token
  )

  run_test(
    tbl = tbl |>
      style_title(background_color = "#983439", text_color = "#ffffff"),
    google_sheet = google_sheet,
    sheet = "Test9",
    dry_run = dry_run,
    token = token,
    start_row = 4,
    start_col = 2
  )

  run_test(
    tbl = tbl |>
      style_title(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ),
    google_sheet = google_sheet,
    sheet = "Test10",
    dry_run = dry_run,
    token = token,
    start_row = 4,
    start_col = 2
  )

  run_test(
    tbl = tbl |>
      style_title(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      style_subtitle(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ),
    google_sheet = google_sheet,
    sheet = "Test11",
    dry_run = dry_run,
    token = token,
    start_row = 4,
    start_col = 2
  )

  run_test(
    tbl = tbl |>
      style_title(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      style_subtitle(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      style_footnote(bold = TRUE),
    google_sheet = google_sheet,
    sheet = "Test12",
    dry_run = dry_run,
    token = token,
    start_row = 4,
    start_col = 2
  )

  run_test(
    tbl = tbl |>
      style_title(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      style_subtitle(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      style_footnote(bold = TRUE) |>
      style_header(background_color = "#B65455", bold = TRUE),
    google_sheet = google_sheet,
    sheet = "Test13",
    dry_run = dry_run,
    token = token,
    start_row = 4,
    start_col = 2
  )

  run_test(
    tbl = tbl |>
      style_title(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      style_subtitle(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      style_footnote(italic = TRUE) |>
      style_header(background_color = "#B65455", bold = TRUE) |>
      format_column(
        columns = dplyr::where(is.double),
        rows = 2:3,
        fmt = format_number(decimals = 1)
      ) |>
      style_column(
        columns = dplyr::where(is.double),
        rows = 2:3,
        italic = TRUE,
        text_color = "#B54321"
      ),
    google_sheet = google_sheet,
    sheet = "Test14",
    dry_run = dry_run,
    token = token,
    start_row = 4,
    start_col = 2
  )

  color_scale = c(
    "#123456" = min(
      summarized_table |> select(where(is.double)),
      na.rm = TRUE
    ),
    "#B46983" = max(
      summarized_table |> select(where(is.double)),
      na.rm = TRUE
    )
  )

  run_test(
    tbl = tbl |>
      style_title(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      style_subtitle(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      style_footnote(bold = TRUE) |>
      style_header(background_color = "#B65455", bold = TRUE) |>
      format_column(
        columns = dplyr::where(is.double),
        rows = 2:3,
        fmt = format_number(decimals = 1)
      ) |>
      style_column(
        columns = dplyr::where(is.double),
        color_scale = color_scale
      ) |>
      style_column(
        columns = dplyr::where(is.double),
        rows = 2:3,
        italic = TRUE,
        text_color = "#B54321"
      ),
    google_sheet = google_sheet,
    sheet = "Test15",
    dry_run = dry_run,
    token = token,
    start_row = 4,
    start_col = 2
  )

  color_scale = c(
    "#123456" = min(
      summarized_table |> select(where(is.double)),
      na.rm = TRUE
    ),
    "#ffffff" = 50,
    "#B46983" = max(
      summarized_table |> select(where(is.double)),
      na.rm = TRUE
    )
  )

  lower_scale <- scales::col_numeric(
    palette = names(color_scale)[1:2],
    domain = color_scale[1:2]
  )
  upper_scale <- scales::col_numeric(
    palette = names(color_scale)[2:3],
    domain = color_scale[2:3]
  )

  run_test(
    tbl = tbl |>
      style_title(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      style_subtitle(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      style_footnote(bold = TRUE) |>
      style_header(background_color = "#B65455", bold = TRUE) |>
      format_column(
        columns = dplyr::where(is.double),
        rows = 2:3,
        fmt = format_number(decimals = 1)
      ) |>
      style_column(
        columns = dplyr::where(is.double),
        color_scale = color_scale
      ) |>
      style_column(
        columns = dplyr::where(is.double),
        rows = 2:3,
        italic = TRUE,
        text_color = "#B54321"
      ),
    google_sheet = google_sheet,
    sheet = "Test16",
    dry_run = dry_run,
    token = token,
    start_row = 4,
    start_col = 2
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
    sheet = "Test17",
    dry_run = dry_run,
    token = token,
    start_row = 4,
    start_col = 2
  )
})
