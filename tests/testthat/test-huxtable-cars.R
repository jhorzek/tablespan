test_that("cars", {
  library(tablespan)
  library(huxtable)
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

  hux_tbl <- as_huxtable(x = tbl)

  expected <- summarized_table |>
    huxtable::as_huxtable(add_colnames = FALSE, add_rownames = FALSE) |>
    huxtable::insert_row(
      c("Cylinder", "Engine", "N", "Mean", "SD", "Mean", "SD"),
      after = 0
    ) |>
    huxtable::insert_row(
      c("", "", "", "Horse Power", "", "Weight", ""),
      after = 0
    ) |>
    huxtable::merge_cells(row = 1, col = 4:5) |>
    huxtable::merge_cells(row = 1, col = 6:7) |>
    huxtable::set_all_borders(row = 1:2, col = 1:7) |>
    huxtable::set_bottom_border(col = 1:7, row = 7) |>
    huxtable::set_left_border(row = 1:7, col = 1) |>
    huxtable::set_right_border(row = 1:7, col = 7) |>
    tablespan:::hux_add_merged_row(text = "A table created with tablespan") |>
    tablespan:::hux_add_merged_row(
      text = "Motor Trend Car Road Tests",
      border = NULL
    ) |>
    huxtable::add_footnote(text = "Data from the infamous mtcars data set.")

  # Remove vertical borders between empty neighbor cells
  for (row in 3:4) {
    for (col in 1:ncol(expected)) {
      if (col == 1) {
        next
      }
      left_empty <- expected[row, col - 1] %in% c("", NA)
      right_empty <- expected[row, col] %in% c("", NA)
      if (left_empty & right_empty) {
        huxtable::right_border(expected)[row, col - 1] <- 0
        huxtable::left_border(expected)[row, col] <- 0
      }
    }
  }

  # Remove horizontal borders between empty neighbor cells
  for (col in 3:4) {
    for (row in 1:nrow(expected)) {
      if (row == nrow(expected)) {
        next
      }
      top_empty <- expected[row, col] %in% c("", NA)
      bottom_empty <- expected[row + 1, col] %in% c("", NA)
      if (top_empty & bottom_empty) {
        huxtable::bottom_border(expected)[row, col] <- 0
        huxtable::top_border(expected)[row + 1, col] <- 0
      }
    }
  }

  for (co in colnames(summarized_table)) {
    digits <- smart_round(x = summarized_table[[co]])
    expected <- expected |>
      huxtable::set_number_format(
        row = 5:9,
        col = co,
        value = list(
          local({
            d <- digits
            function(x) {
              formatC(
                x = x,
                big.mark = ",",
                decimal.mark = ".",
                digits = d,
                format = "f"
              )
            }
          })
        )
      )
  }

  expected <- expected |>
    huxtable::set_right_border(col = 2)

  testthat::expect_identical(
    huxtable::as_html(hux_tbl),
    huxtable::as_html(expected)
  )
})

test_that("cars-additional_spanners", {
  library(tablespan)
  library(huxtable)
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

  hux_tbl <- as_huxtable(x = tbl)

  expected <- summarized_table |>
    huxtable::as_huxtable(add_colnames = FALSE, add_rownames = FALSE) |>
    huxtable::insert_row(
      c("Cylinder", "Engine", "N", "Mean", "SD", "Mean", "SD"),
      after = 0
    ) |>
    huxtable::insert_row(
      c("", "", "", "Mean", "Standard Deviation", "Weight", ""),
      after = 0
    ) |>
    huxtable::insert_row(
      c("", "", "", "Horse Power", "", "", ""),
      after = 0
    ) |>
    huxtable::insert_row(
      c("", "", "Results", "", "", "", ""),
      after = 0
    ) |>
    huxtable::merge_cells(row = 1, col = 3:7) |>
    huxtable::merge_cells(row = 2, col = 4:5) |>
    huxtable::merge_cells(row = 3, col = 6:7) |>
    huxtable::set_all_borders(row = 1:4, col = 1:7) |>
    huxtable::set_bottom_border(col = 1:7, row = 9) |>
    huxtable::set_left_border(row = 1:9, col = 1) |>
    huxtable::set_right_border(row = 1:9, col = 7) |>
    tablespan:::hux_add_merged_row(text = "A table created with tablespan") |>
    tablespan:::hux_add_merged_row(
      text = "Motor Trend Car Road Tests",
      border = NULL
    ) |>
    huxtable::add_footnote(text = "Data from the infamous mtcars data set.")

  # Remove vertical borders between empty neighbor cells
  for (row in 3:5) {
    for (col in 1:ncol(expected)) {
      if (col == 1) {
        next
      }
      left_empty <- expected[row, col - 1] %in% c("", NA)
      right_empty <- expected[row, col] %in% c("", NA)
      if (left_empty & right_empty) {
        huxtable::right_border(expected)[row, col - 1] <- 0
        huxtable::left_border(expected)[row, col] <- 0
      }
    }
  }

  # Remove horizontal borders between empty neighbor cells
  for (col in 1:ncol(expected)) {
    for (row in 1:nrow(expected)) {
      if (row == nrow(expected)) {
        next
      }
      top_empty <- expected[row, col] %in% c("", NA)
      bottom_empty <- expected[row + 1, col] %in% c("", NA)
      if (top_empty & bottom_empty) {
        huxtable::bottom_border(expected)[row, col] <- 0
        huxtable::top_border(expected)[row + 1, col] <- 0
      }
    }
  }

  for (co in colnames(summarized_table)) {
    digits <- smart_round(x = summarized_table[[co]])
    expected <- expected |>
      huxtable::set_number_format(
        row = 7:11,
        col = co,
        value = list(
          local({
            d <- digits
            function(x) {
              formatC(
                x = x,
                big.mark = ",",
                decimal.mark = ".",
                digits = d,
                format = "f"
              )
            }
          })
        )
      )
  }

  expected <- expected |>
    huxtable::set_right_border(col = 2)

  testthat::expect_identical(
    huxtable::as_html(hux_tbl),
    huxtable::as_html(expected)
  )
})

test_that("cars-no_row_names", {
  library(tablespan)
  library(huxtable)
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

  hux_tbl <- as_huxtable(x = tbl)

  summarized_table_reduced <- summarized_table |>
    ungroup() |>
    select(all_of(c("N", "mean_hp", "sd_hp", "mean_wt", "sd_wt")))

  expected <- summarized_table_reduced |>
    huxtable::as_huxtable(add_colnames = FALSE, add_rownames = FALSE) |>
    huxtable::insert_row(
      c("N", "Mean", "SD", "Mean", "SD"),
      after = 0
    ) |>
    huxtable::insert_row(
      c("", "Mean", "Standard Deviation", "Weight", ""),
      after = 0
    ) |>
    huxtable::insert_row(
      c("", "Horse Power", "", "", ""),
      after = 0
    ) |>
    huxtable::insert_row(
      c("Results", "", "", "", ""),
      after = 0
    ) |>
    huxtable::merge_cells(row = 1, col = 1:5) |>
    huxtable::merge_cells(row = 2, col = 2:3) |>
    huxtable::merge_cells(row = 3, col = 4:5) |>
    huxtable::set_all_borders(row = 1:4, col = 1:5) |>
    huxtable::set_bottom_border(col = 1:5, row = 9) |>
    huxtable::set_left_border(row = 1:9, col = 1) |>
    huxtable::set_right_border(row = 1:9, col = 5) |>
    tablespan:::hux_add_merged_row(text = "A table created with tablespan") |>
    tablespan:::hux_add_merged_row(
      text = "Motor Trend Car Road Tests",
      border = NULL
    ) |>
    huxtable::add_footnote(text = "Data from the infamous mtcars data set.")

  # Remove vertical borders between empty neighbor cells
  for (row in 3:5) {
    for (col in 1:ncol(expected)) {
      if (col == 1) {
        next
      }
      left_empty <- expected[row, col - 1] %in% c("", NA)
      right_empty <- expected[row, col] %in% c("", NA)
      if (left_empty & right_empty) {
        huxtable::right_border(expected)[row, col - 1] <- 0
        huxtable::left_border(expected)[row, col] <- 0
      }
    }
  }

  # Remove horizontal borders between empty neighbor cells
  for (col in 1:ncol(expected)) {
    for (row in 1:nrow(expected)) {
      if (row == nrow(expected)) {
        next
      }
      top_empty <- expected[row, col] %in% c("", NA)
      bottom_empty <- expected[row + 1, col] %in% c("", NA)
      if (top_empty & bottom_empty) {
        huxtable::bottom_border(expected)[row, col] <- 0
        huxtable::top_border(expected)[row + 1, col] <- 0
      }
    }
  }

  for (co in colnames(summarized_table_reduced)) {
    digits <- smart_round(x = summarized_table_reduced[[co]])
    expected <- expected |>
      huxtable::set_number_format(
        row = 7:11,
        col = co,
        value = list(
          local({
            d <- digits
            function(x) {
              formatC(
                x = x,
                big.mark = ",",
                decimal.mark = ".",
                digits = d,
                format = "f"
              )
            }
          })
        )
      )
  }

  testthat::expect_identical(
    huxtable::as_html(hux_tbl),
    huxtable::as_html(expected)
  )
})

test_that("cars-no_titles", {
  library(tablespan)
  library(huxtable)
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

  hux_tbl <- as_huxtable(x = tbl)

  expected <- summarized_table |>
    huxtable::as_huxtable(add_colnames = FALSE, add_rownames = FALSE) |>
    huxtable::insert_row(
      c("Cylinder", "Engine", "N", "Mean", "SD", "Mean", "SD"),
      after = 0
    ) |>
    huxtable::insert_row(
      c("", "", "", "Horse Power", "", "Weight", ""),
      after = 0
    ) |>
    huxtable::merge_cells(row = 1, col = 4:5) |>
    huxtable::merge_cells(row = 1, col = 6:7) |>
    huxtable::set_all_borders(row = 1:2, col = 1:7) |>
    huxtable::set_bottom_border(col = 1:7, row = 7) |>
    huxtable::set_left_border(row = 1:7, col = 1) |>
    huxtable::set_right_border(row = 1:7, col = 7) |>
    huxtable::add_footnote(text = "Data from the infamous mtcars data set.")

  # Remove vertical borders between empty neighbor cells
  for (row in 1:2) {
    for (col in 1:ncol(expected)) {
      if (col == 1) {
        next
      }
      left_empty <- expected[row, col - 1] %in% c("", NA)
      right_empty <- expected[row, col] %in% c("", NA)
      if (left_empty & right_empty) {
        huxtable::right_border(expected)[row, col - 1] <- 0
        huxtable::left_border(expected)[row, col] <- 0
      }
    }
  }

  # Remove horizontal borders between empty neighbor cells
  for (col in 1:ncol(expected)) {
    for (row in 1:nrow(expected)) {
      if (row == nrow(expected)) {
        next
      }
      top_empty <- expected[row, col] %in% c("", NA)
      bottom_empty <- expected[row + 1, col] %in% c("", NA)
      if (top_empty & bottom_empty) {
        huxtable::bottom_border(expected)[row, col] <- 0
        huxtable::top_border(expected)[row + 1, col] <- 0
      }
    }
  }

  for (co in colnames(summarized_table)) {
    digits <- smart_round(x = summarized_table[[co]])
    expected <- expected |>
      huxtable::set_number_format(
        row = 3:7,
        col = co,
        value = list(
          local({
            d <- digits
            function(x) {
              formatC(
                x = x,
                big.mark = ",",
                decimal.mark = ".",
                digits = d,
                format = "f"
              )
            }
          })
        )
      )
  }

  expected <- expected |>
    huxtable::set_right_border(col = 2)

  testthat::expect_identical(
    huxtable::as_html(hux_tbl),
    huxtable::as_html(expected)
  )
})

test_that("cars-no_titles_no_footnotes", {
  library(tablespan)
  library(huxtable)
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

  testthat::expect_no_error(as_huxtable(x = tbl))
})

test_that("cars-duplicated_spanner_names", {
  library(tablespan)
  library(huxtable)
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

  testthat::expect_no_error(as_huxtable(x = tbl))
})


test_that("cars - hux styling", {
  library(tablespan)
  library(huxtable)
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

  hux_tbl_base <- as_huxtable(x = tbl)

  expected_base <- summarized_table |>
    huxtable::as_huxtable(add_colnames = FALSE, add_rownames = FALSE) |>
    huxtable::insert_row(
      c("Cylinder", "Engine", "N", "Mean", "SD", "Mean", "SD"),
      after = 0
    ) |>
    huxtable::insert_row(
      c("", "", "", "Horse Power", "", "Weight", ""),
      after = 0
    ) |>
    huxtable::merge_cells(row = 1, col = 4:5) |>
    huxtable::merge_cells(row = 1, col = 6:7) |>
    huxtable::set_all_borders(row = 1:2, col = 1:7) |>
    huxtable::set_bottom_border(col = 1:7, row = 7) |>
    huxtable::set_left_border(row = 1:7, col = 1) |>
    huxtable::set_right_border(row = 1:7, col = 7) |>
    tablespan:::hux_add_merged_row(text = "A table created with tablespan") |>
    tablespan:::hux_add_merged_row(
      text = "Motor Trend Car Road Tests",
      border = NULL
    ) |>
    huxtable::add_footnote(text = "Data from the infamous mtcars data set.")

  # Remove vertical borders between empty neighbor cells
  for (row in 3:4) {
    for (col in 1:ncol(expected_base)) {
      if (col == 1) {
        next
      }
      left_empty <- expected_base[row, col - 1] %in% c("", NA)
      right_empty <- expected_base[row, col] %in% c("", NA)
      if (left_empty & right_empty) {
        huxtable::right_border(expected_base)[row, col - 1] <- 0
        huxtable::left_border(expected_base)[row, col] <- 0
      }
    }
  }

  # Remove horizontal borders between empty neighbor cells
  for (col in 3:4) {
    for (row in 1:nrow(expected_base)) {
      if (row == nrow(expected_base)) {
        next
      }
      top_empty <- expected_base[row, col] %in% c("", NA)
      bottom_empty <- expected_base[row + 1, col] %in% c("", NA)
      if (top_empty & bottom_empty) {
        huxtable::bottom_border(expected_base)[row, col] <- 0
        huxtable::top_border(expected_base)[row + 1, col] <- 0
      }
    }
  }

  for (co in colnames(summarized_table)) {
    digits <- smart_round(x = summarized_table[[co]])
    expected_base <- expected_base |>
      huxtable::set_number_format(
        row = 5:9,
        col = co,
        value = list(
          local({
            d <- digits
            function(x) {
              formatC(
                x = x,
                big.mark = ",",
                decimal.mark = ".",
                digits = d,
                format = "f"
              )
            }
          })
        )
      )
  }

  expected_base <- expected_base |>
    huxtable::set_right_border(col = 2)

  testthat::expect_identical(
    huxtable::to_html(hux_tbl_base),
    huxtable::to_html(expected_base)
  )

  # title
  testthat::expect_identical(
    tbl |>
      style_title(text_color = "#000000", background_color = "#983439") |>
      as_huxtable() |>
      huxtable::to_html(),
    expected_base |>
      huxtable::set_background_color(row = 1, value = "#983439") |>
      huxtable::set_text_color(row = 1, value = "#000000") |>
      huxtable::to_html()
  )

  testthat::expect_identical(
    tbl |>
      style_title(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      as_huxtable() |>
      huxtable::to_html(),
    expected_base |>
      huxtable::set_background_color(row = 1, value = "#983439") |>
      huxtable::set_text_color(row = 1, value = "#ffffff") |>
      huxtable::set_bold(row = 1) |>
      huxtable::set_italic(row = 1) |>
      huxtable::to_html()
  )

  testthat::expect_identical(
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
      as_huxtable() |>
      huxtable::to_html(),
    expected_base |>
      huxtable::set_background_color(row = 1:2, value = "#983439") |>
      huxtable::set_text_color(row = 1:2, value = "#ffffff") |>
      huxtable::set_bold(row = 1:2) |>
      huxtable::set_italic(row = 1:2) |>
      huxtable::to_html()
  )

  testthat::expect_identical(
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
      style_header(background_color = "#B65455", bold = TRUE) |>
      as_huxtable() |>
      huxtable::to_html(),
    expected_base |>
      huxtable::set_background_color(row = 1:2, value = "#983439") |>
      huxtable::set_text_color(row = 1:2, value = "#ffffff") |>
      huxtable::set_bold(row = 1:2) |>
      huxtable::set_italic(row = 1:2) |>
      huxtable::set_background_color(row = 3:4, value = "#B65455") |>
      huxtable::set_bold(row = 3:4) |>
      huxtable::to_html()
  )

  testthat::expect_identical(
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
      style_column(
        columns = dplyr::where(is.double),
        rows = 2:3,
        italic = TRUE,
        text_color = "#B54321"
      ) |>
      as_huxtable() |>
      huxtable::to_html(),
    expected_base |>
      huxtable::set_background_color(row = 1:2, value = "#983439") |>
      huxtable::set_text_color(row = 1:2, value = "#ffffff") |>
      huxtable::set_bold(row = 1:2) |>
      huxtable::set_italic(row = 1:2) |>
      huxtable::set_background_color(row = 3:4, value = "#B65455") |>
      huxtable::set_bold(row = 3:4) |>
      huxtable::set_text_color(
        row = 6:7,
        col = c(1:2, 4:7),
        value = "#B54321"
      ) |>
      huxtable::set_italic(
        row = 6:7,
        col = c(1:2, 4:7)
      ) |>
      huxtable::to_html()
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

  testthat::expect_no_error(
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
      style_header(background_color = "#B65455", bold = TRUE) |>
      style_column(
        columns = dplyr::where(is.double),
        rows = 2:3,
        italic = TRUE,
        text_color = "#B54321"
      ) |>
      style_column(
        columns = dplyr::where(is.double),
        color_scale = color_scale
      ) |>
      as_huxtable()
  )
})
