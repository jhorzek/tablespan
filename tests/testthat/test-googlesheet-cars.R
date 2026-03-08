library(tablespan)
library(testthat)
library(dplyr)
google_sheet <- fake_gs4_dribble()
remove_token <- function(x) {
  x$token <- NULL
  x$url <- "https://sheets.googleapis.com/v4/spreadsheets/spreadsheet_id:batchUpdate"
  return(x)
}

googlesheets4::gs4_deauth()

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

  gs_request <- as_googlesheet_request(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = "Sheet1",
    dry_run = TRUE,
    token = NULL
  ) |>
    remove_token()

  testthat::expect_snapshot(x = str(gs_request))
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

  gs_request <- as_googlesheet_request(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = "Sheet1",
    dry_run = TRUE,
    token = NULL
  ) |>
    remove_token()

  testthat::expect_snapshot(x = str(gs_request))
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

  gs_request <- as_googlesheet_request(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = "Sheet1",
    dry_run = TRUE,
    token = NULL
  ) |>
    remove_token()

  testthat::expect_snapshot(x = str(gs_request))
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

  gs_request <- as_googlesheet_request(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = "Sheet1",
    dry_run = TRUE,
    token = NULL
  ) |>
    remove_token()

  testthat::expect_snapshot(x = str(gs_request))
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

  gs_request <- as_googlesheet_request(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = "Sheet1",
    dry_run = TRUE,
    token = NULL
  ) |>
    remove_token()

  testthat::expect_snapshot(x = str(gs_request))
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

  gs_request <- as_googlesheet_request(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = "Sheet1",
    dry_run = TRUE,
    token = NULL
  ) |>
    remove_token()

  testthat::expect_snapshot(x = str(gs_request))
})


test_that("cars - gt styling", {
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

  gs_request <- as_googlesheet_request(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = "Sheet1",
    dry_run = TRUE,
    token = NULL
  ) |>
    remove_token()

  testthat::expect_snapshot(x = str(gs_request))

  # title
  testthat::expect_snapshot(
    x = str(
      tbl |>
        style_title(text_color = "#000000", background_color = "#983439") |>
        as_googlesheet_request(
          google_sheet = google_sheet,
          sheet = "Sheet1",
          dry_run = TRUE,
          token = NULL
        ) |>
        remove_token()
    )
  )

  testthat::expect_snapshot(
    x = str(
      tbl |>
        style_title(background_color = "#983439", text_color = "#ffffff") |>
        as_googlesheet_request(
          google_sheet = google_sheet,
          sheet = "Sheet1",
          dry_run = TRUE,
          token = NULL
        ) |>
        remove_token()
    )
  )

  testthat::expect_snapshot(
    x = str(
      tbl |>
        style_title(
          background_color = "#983439",
          text_color = "#ffffff",
          bold = TRUE,
          italic = TRUE
        ) |>
        as_googlesheet_request(
          google_sheet = google_sheet,
          sheet = "Sheet1",
          dry_run = TRUE,
          token = NULL
        ) |>
        remove_token()
    )
  )

  testthat::expect_snapshot(
    x = str(
      tbl |>
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
        as_googlesheet_request(
          google_sheet = google_sheet,
          sheet = "Sheet1",
          dry_run = TRUE,
          token = NULL
        ) |>
        remove_token()
    )
  )

  testthat::expect_snapshot(
    x = str(
      tbl |>
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
        style_footnote(gt_style = gt::cell_text(weight = "lighter")) |>
        as_googlesheet_request(
          google_sheet = google_sheet,
          sheet = "Sheet1",
          dry_run = TRUE,
          token = NULL
        ) |>
        remove_token()
    )
  )

  testthat::expect_snapshot(
    x = str(
      tbl |>
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
        style_footnote(gt_style = gt::cell_text(weight = "lighter")) |>
        style_header(background_color = "#B65455", bold = TRUE) |>
        as_googlesheet_request(
          google_sheet = google_sheet,
          sheet = "Sheet1",
          dry_run = TRUE,
          token = NULL
        ) |>
        remove_token()
    )
  )

  testthat::expect_snapshot(
    x = str(
      tbl |>
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
        ) |>
        as_googlesheet_request(
          google_sheet = google_sheet,
          sheet = "Sheet1",
          dry_run = TRUE,
          token = NULL
        ) |>
        remove_token()
    )
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

  testthat::expect_snapshot(
    x = str(
      tbl |>
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
        style_footnote(gt_style = gt::cell_text(weight = "lighter")) |>
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
        ) |>
        as_googlesheet_request(
          google_sheet = google_sheet,
          sheet = "Sheet1",
          dry_run = TRUE,
          token = NULL
        ) |>
        remove_token()
    )
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

  testthat::expect_snapshot(
    x = str(
      tbl |>
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
        style_footnote(gt_style = gt::cell_text(weight = "lighter")) |>
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
        ) |>
        as_googlesheet_request(
          google_sheet = google_sheet,
          sheet = "Sheet1",
          dry_run = TRUE,
          token = NULL
        ) |>
        remove_token()
    )
  )
})
