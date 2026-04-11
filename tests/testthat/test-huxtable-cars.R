library(testthat)
library(dplyr)
library(tablespan)
library(huxtable)

test_tables <- build_tablespan_test_tables()

test_that("cars", {
  summarized_table <- test_tables$data$summarized_table
  hux_tbl <- as_huxtable(x = test_tables$tables$cars)

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
    tablespan:::add_merged_row_hux(text = test_tables$metadata$subtitle) |>
    tablespan:::add_merged_row_hux(
      text = test_tables$metadata$title,
      border = NULL
    ) |>
    huxtable::add_footnote(text = test_tables$metadata$footnote)

  # Remove vertical borders between empty neighbor cells
  for (row in 3:4) {
    for (col in 1:ncol(expected)) {
      if (col == 1) {
        next
      }
      left_empty <- expected[row, col - 1] %in% c("", NA)
      right_empty <- expected[row, col] %in% c("", NA)
      if (isTRUE(left_empty) && isTRUE(right_empty)) {
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
      if (isTRUE(top_empty) && isTRUE(bottom_empty)) {
        huxtable::bottom_border(expected)[row, col] <- 0
        huxtable::top_border(expected)[row + 1, col] <- 0
      }
    }
  }

  for (co in colnames(summarized_table)) {
    digits <- tablespan:::smart_round(x = summarized_table[[co]])
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

  compare_html_tables(
    huxtable::as_html(hux_tbl),
    huxtable::as_html(expected)
  )
})

test_that("cars-additional_spanners", {
  summarized_table <- test_tables$data$summarized_table
  hux_tbl <- as_huxtable(x = test_tables$tables$cars_additional_spanners)

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
    tablespan:::add_merged_row_hux(text = test_tables$metadata$subtitle) |>
    tablespan:::add_merged_row_hux(
      text = test_tables$metadata$title,
      border = NULL
    ) |>
    huxtable::add_footnote(text = test_tables$metadata$footnote)

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
    digits <- tablespan:::smart_round(x = summarized_table[[co]])
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

  compare_html_tables(
    huxtable::as_html(hux_tbl),
    huxtable::as_html(expected)
  )
})

test_that("cars-no_row_names", {
  summarized_table <- test_tables$data$summarized_table
  hux_tbl <- as_huxtable(x = test_tables$tables$cars_no_row_names)

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
    tablespan:::add_merged_row_hux(text = test_tables$metadata$subtitle) |>
    tablespan:::add_merged_row_hux(
      text = test_tables$metadata$title,
      border = NULL
    ) |>
    huxtable::add_footnote(text = test_tables$metadata$footnote)

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
    digits <- tablespan:::smart_round(x = summarized_table_reduced[[co]])
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

  compare_html_tables(
    huxtable::as_html(hux_tbl),
    huxtable::as_html(expected)
  )
})

test_that("cars-no_titles", {
  summarized_table <- test_tables$data$summarized_table
  hux_tbl <- as_huxtable(x = test_tables$tables$cars_no_titles)

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
    huxtable::add_footnote(text = test_tables$metadata$footnote)

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
    digits <- tablespan:::smart_round(x = summarized_table[[co]])
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

  compare_html_tables(
    huxtable::as_html(hux_tbl),
    huxtable::as_html(expected)
  )
})

test_that("cars-no_titles_no_footnotes", {
  summarized_table <- test_tables$data$summarized_table
  hux_tbl <- as_huxtable(x = test_tables$tables$cars_no_titles_no_footnotes)

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
    huxtable::set_right_border(row = 1:7, col = 7)

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
    digits <- tablespan:::smart_round(x = summarized_table[[co]])
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

  compare_html_tables(
    huxtable::as_html(hux_tbl),
    huxtable::as_html(expected)
  )
})

test_that("cars-duplicated_spanner_names", {
  hux_tbl <- as_huxtable(
    x = test_tables$tables$cars_duplicated_spanner_names
  )

  expected <- test_tables$data$combined_models |>
    select(all_of(c(
      "Parameter",
      "Estimate.x",
      "t value.x",
      "Pr(>|t|).x",
      "Estimate.y",
      "t value.y",
      "Pr(>|t|).y"
    ))) |>
    huxtable::as_huxtable(add_colnames = FALSE, add_rownames = FALSE) |>
    huxtable::insert_row(
      c(
        "Parameter",
        "Estimate",
        "t-value",
        "p-value",
        "Estimate",
        "t-value",
        "p-value"
      ),
      after = 0
    ) |>
    huxtable::insert_row(
      c("", "", "Significance", "", "", "Significance", ""),
      after = 0
    ) |>
    huxtable::insert_row(
      c("", "Model 1", "", "", "Model 2", "", ""),
      after = 0
    ) |>
    huxtable::merge_cells(row = 1, col = 2:4) |>
    huxtable::merge_cells(row = 1, col = 5:7) |>
    huxtable::merge_cells(row = 2, col = 3:4) |>
    huxtable::merge_cells(row = 2, col = 6:7) |>
    huxtable::set_all_borders(row = 1:3, col = 1:7) |>
    huxtable::set_bottom_border(col = 1:7, row = 6) |>
    huxtable::set_left_border(row = 1:6, col = 1) |>
    huxtable::set_right_border(row = 1:6, col = 7)

  # Remove vertical borders between empty neighbor cells
  for (row in 1:3) {
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

  for (co in c(
    "Parameter",
    "Estimate.x",
    "t value.x",
    "Pr(>|t|).x",
    "Estimate.y",
    "t value.y",
    "Pr(>|t|).y"
  )) {
    if (!is.numeric(test_tables$data$combined_models[[co]])) {
      next
    }
    digits <- tablespan:::smart_round(
      x = test_tables$data$combined_models[[co]]
    )
    expected <- expected |>
      huxtable::set_number_format(
        row = 3:6,
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
    huxtable::set_right_border(col = 1)

  compare_html_tables(
    huxtable::as_html(hux_tbl),
    huxtable::as_html(expected)
  )
})


test_that("cars - hux styling", {
  summarized_table <- test_tables$data$summarized_table
  hux_tbl_base <- as_huxtable(x = test_tables$tables$cars)

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
    tablespan:::add_merged_row_hux(text = test_tables$metadata$subtitle) |>
    tablespan:::add_merged_row_hux(
      text = test_tables$metadata$title,
      border = NULL
    ) |>
    huxtable::add_footnote(text = test_tables$metadata$footnote)

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
    digits <- tablespan:::smart_round(x = summarized_table[[co]])
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

  compare_html_tables(
    huxtable::as_html(hux_tbl_base),
    huxtable::as_html(expected_base)
  )

  compare_html_tables(
    as_huxtable(test_tables$tables$cars_title_style_black_red) |>
      huxtable::as_html(),
    expected_base |>
      huxtable::set_background_color(row = 1, value = "#983439") |>
      huxtable::set_text_color(row = 1, value = "#000000") |>
      huxtable::as_html()
  )

  compare_html_tables(
    as_huxtable(test_tables$tables$cars_title_style_white_red) |>
      huxtable::as_html(),
    expected_base |>
      huxtable::set_background_color(row = 1, value = "#983439") |>
      huxtable::set_text_color(row = 1, value = "#ffffff") |>
      huxtable::as_html()
  )

  compare_html_tables(
    as_huxtable(test_tables$tables$cars_title_subtitle_footnote_header_style) |>
      huxtable::to_html(),
    expected_base |>
      huxtable::set_background_color(row = 1:2, value = "#983439") |>
      huxtable::set_text_color(row = 1:2, value = "#ffffff") |>
      huxtable::set_bold(row = 1:2) |>
      huxtable::set_italic(row = 1:2) |>
      huxtable::set_background_color(row = 3:4, value = "#B65455") |>
      huxtable::set_bold(row = 3:4) |>
      huxtable::set_font_size(row = 10, value = 8) |>
      huxtable::to_html()
  )

  compare_html_tables(
    as_huxtable(
      test_tables$tables$cars_tsf_hf_cell_style
    ) |>
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
      huxtable::set_number_format(
        row = 6:7,
        col = c(1:2, 4:7),
        value = list(function(x) {
          formatC(
            x = x,
            big.mark = ",",
            decimal.mark = ".",
            digits = 1,
            format = "f"
          )
        })
      ) |>
      huxtable::set_italic(
        row = 6:7,
        col = c(1:2, 4:7)
      ) |>
      huxtable::set_italic(
        row = 10
      ) |>
      huxtable::to_html()
  )

  testthat::expect_no_error(as_huxtable(
    test_tables$tables$cars_tsf_hf_cs2_cell_style
  ))
  testthat::expect_no_error(as_huxtable(
    test_tables$tables$cars_tsf_hf_cs3_cell_style
  ))
})

build_expected_hux_cars_base <- function() {
  summarized_table <- test_tables$data$summarized_table

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
    tablespan:::add_merged_row_hux(text = test_tables$metadata$subtitle) |>
    tablespan:::add_merged_row_hux(
      text = test_tables$metadata$title,
      border = NULL
    ) |>
    huxtable::add_footnote(text = test_tables$metadata$footnote)

  # Remove vertical borders between empty neighbor cells
  for (row in 3:4) {
    for (col in 1:ncol(expected)) {
      if (col == 1) {
        next
      }
      left_empty <- expected[row, col - 1] %in% c("", NA)
      right_empty <- expected[row, col] %in% c("", NA)
      if (isTRUE(left_empty) && isTRUE(right_empty)) {
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
      if (isTRUE(top_empty) && isTRUE(bottom_empty)) {
        huxtable::bottom_border(expected)[row, col] <- 0
        huxtable::top_border(expected)[row + 1, col] <- 0
      }
    }
  }

  for (co in colnames(summarized_table)) {
    digits <- tablespan:::smart_round(x = summarized_table[[co]])
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

  expected |>
    huxtable::set_right_border(col = 2)
}

build_expected_hux_date_text_base <- function() {
  mixed_data <- test_tables$data$mixed_date_text_data

  expected <- mixed_data |>
    huxtable::as_huxtable(add_colnames = FALSE, add_rownames = FALSE) |>
    huxtable::insert_row(
      c("Group", "Date", "Comment", "Amount"),
      after = 0
    ) |>
    huxtable::set_all_borders(row = 1, col = 1:4) |>
    huxtable::set_bottom_border(col = 1:4, row = 5) |>
    huxtable::set_left_border(row = 1:5, col = 1) |>
    huxtable::set_right_border(row = 1:5, col = 4) |>
    tablespan:::add_merged_row_hux(
      text = "Fixture for format_date and format_text"
    ) |>
    tablespan:::add_merged_row_hux(
      text = "Date/Text Formatting Example",
      border = NULL
    ) |>
    huxtable::add_footnote(text = "Synthetic data for tests")

  digits <- tablespan:::smart_round(x = mixed_data$amount)
  expected |>
    huxtable::set_number_format(
      row = 4:7,
      col = "amount",
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
    ) |>
    huxtable::set_right_border(col = 1)
}

test_that("date_text_base", {
  hux_tbl <- as_huxtable(x = test_tables$tables$date_text_base)
  expected <- build_expected_hux_date_text_base() |>
    huxtable::set_number_format(
      row = 4:7,
      col = "event_date",
      value = list(function(x) {
        format(as.Date(x), "%Y-%m-%d")
      })
    ) |>
    huxtable::set_number_format(
      row = 4:7,
      col = "amount",
      value = "%5.3f"
    )

  compare_html_tables(
    huxtable::as_html(hux_tbl),
    huxtable::as_html(expected)
  )
})

test_that("date_text_formatted", {
  hux_tbl <- as_huxtable(x = test_tables$tables$date_text_formatted)

  expected <- build_expected_hux_date_text_base() |>
    huxtable::set_number_format(
      row = 4:7,
      col = "event_date",
      value = list(function(x) {
        format(as.Date(x), "%d-%m-%Y")
      })
    ) |>
    huxtable::set_number_format(
      row = 4:7,
      col = "amount",
      value = "%5.1f"
    )

  compare_html_tables(
    huxtable::as_html(hux_tbl),
    huxtable::as_html(expected)
  )
})

test_that("cars_header_cells_styled", {
  hux_tbl <- as_huxtable(x = test_tables$tables$cars_header_cells_styled)
  expected <- build_expected_hux_cars_base()

  compare_html_tables(
    huxtable::as_html(hux_tbl),
    huxtable::as_html(expected)
  )
})

test_that("cars_hline_styled", {
  hux_tbl <- as_huxtable(x = test_tables$tables$cars_hline_styled)
  expected <- build_expected_hux_cars_base()

  compare_html_tables(
    huxtable::as_html(hux_tbl),
    huxtable::as_html(expected)
  )
})

test_that("cars_vline_styled", {
  hux_tbl <- as_huxtable(x = test_tables$tables$cars_vline_styled)
  expected <- build_expected_hux_cars_base()

  compare_html_tables(
    huxtable::as_html(hux_tbl),
    huxtable::as_html(expected)
  )
})

test_that("cars_header_cells_hline_vline_styled", {
  hux_tbl <- as_huxtable(
    x = test_tables$tables$cars_header_cells_hline_vline_styled
  )
  expected <- build_expected_hux_cars_base()

  compare_html_tables(
    huxtable::as_html(hux_tbl),
    huxtable::as_html(expected)
  )
})
