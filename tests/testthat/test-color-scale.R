# Tests for preprocess_color_scale function
library(testthat)
library(dplyr)
library(tablespan)

setup_test_data <- function() {
  data("mtcars")

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
    title = "Motor Trend Car Road Tests"
  )

  return(tbl)
}

test_that("preprocess_color_scale handles NULL input", {
  tbl <- setup_test_data()

  expect_null(preprocess_color_scale(tbl, NULL, "mean_hp", NULL))
})

test_that("preprocess_color_scale validates input structure", {
  tbl <- setup_test_data()

  # Test invalid length
  expect_error(
    preprocess_color_scale(tbl, c("#EE2F43"), "mean_hp", NULL),
    "color_scale must be of length 2 or 3."
  )

  # Test unnamed vector
  expect_error(
    preprocess_color_scale(tbl, c("#EE2F43", "#37E65A"), "mean_hp", NULL),
    "color_scale must be a named vector"
  )
})

test_that("preprocess_color_scale returns unchanged scale when no NAs present", {
  tbl <- setup_test_data()

  # Test 2-color scale
  input_scale <- c("#EE2F43" = -1, "#37E65A" = 1)
  expect_equal(
    preprocess_color_scale(tbl, input_scale, "mean_hp", NULL),
    input_scale
  )

  # Test 3-color scale
  input_scale <- c("#EE2F43" = -1, "#FFFFFF" = 0, "#37E65A" = 1)
  expect_equal(
    preprocess_color_scale(tbl, input_scale, "mean_hp", NULL),
    input_scale
  )
})

test_that("preprocess_color_scale fills NA values correctly", {
  tbl <- setup_test_data()

  # Test 2-color scale with NAs
  processed <- preprocess_color_scale(
    tbl,
    c("#EE2F43" = NA, "#37E65A" = NA),
    "mean_hp",
    NULL
  )
  expect_equal(length(processed), 2)
  expect_true(!any(is.na(processed)))
  expect_equal(
    processed,
    c(
      "#EE2F43" = min(tbl$table_data$col_data$mean_hp),
      "#37E65A" = max(tbl$table_data$col_data$mean_hp)
    )
  )

  # Test 3-color scale with NAs
  processed <- preprocess_color_scale(
    tbl,
    c("#EE2F43" = NA, "#FFFFFF" = NA, "#37E65A" = NA),
    "mean_hp",
    NULL
  )
  expect_equal(length(processed), 3)
  expect_true(!any(is.na(processed)))
  expect_equal(
    processed,
    c(
      "#EE2F43" = min(tbl$table_data$col_data$mean_hp),
      "#FFFFFF" = mean(tbl$table_data$col_data$mean_hp),
      "#37E65A" = max(tbl$table_data$col_data$mean_hp)
    )
  )

  # Test partial NAs in 3-color scale
  processed <- preprocess_color_scale(
    tbl,
    c("#EE2F43" = -1, "#FFFFFF" = NA, "#37E65A" = NA),
    "mean_hp",
    NULL
  )
  expect_equal(processed[1], c("#EE2F43" = -1))
  expect_true(!any(is.na(processed[2:3])))
  expect_equal(
    processed,
    c(
      "#EE2F43" = -1,
      "#FFFFFF" = mean(tbl$table_data$col_data$mean_hp),
      "#37E65A" = max(tbl$table_data$col_data$mean_hp)
    )
  )
})

test_that("preprocess_color_scale works with specific rows", {
  tbl <- setup_test_data()

  # Test with specific rows
  processed <- preprocess_color_scale(
    tbl,
    c("#EE2F43" = NA, "#37E65A" = NA),
    "mean_hp",
    rows = 1:2
  )
  expect_equal(length(processed), 2)
  expect_true(!any(is.na(processed)))
  expect_equal(
    processed,
    c(
      "#EE2F43" = min(tbl$table_data$col_data$mean_hp[1:2]),
      "#37E65A" = max(tbl$table_data$col_data$mean_hp[1:2])
    )
  )
})

test_that("preprocess_color_scale validates increasing values", {
  tbl <- setup_test_data()

  # Test non-increasing values
  expect_error(
    preprocess_color_scale(
      tbl,
      c("#EE2F43" = 1, "#37E65A" = 0),
      "mean_hp",
      NULL
    ),
    "The values of the color_scale must be increasing"
  )

  # Test equal values
  expect_error(
    preprocess_color_scale(
      tbl,
      c("#EE2F43" = 0, "#37E65A" = 0),
      "mean_hp",
      NULL
    ),
    "The values of the color_scale must be increasing"
  )
})

test_that("preprocess_color_scale works with different columns", {
  tbl <- setup_test_data()

  # Test with mean_wt column
  processed <- preprocess_color_scale(
    tbl,
    c("#EE2F43" = NA, "#37E65A" = NA),
    "mean_wt",
    NULL
  )
  expect_equal(length(processed), 2)
  expect_true(!any(is.na(processed)))

  # Test with multiple columns
  processed <- preprocess_color_scale(
    tbl,
    c("#EE2F43" = NA, "#37E65A" = NA),
    c("mean_hp", "mean_wt"),
    NULL
  )
  expect_equal(length(processed), 2)
  expect_true(!any(is.na(processed)))
  expect_equal(
    processed,
    c(
      "#EE2F43" = min(c(
        tbl$table_data$col_data$mean_hp,
        tbl$table_data$col_data$mean_wt
      )),
      "#37E65A" = max(c(
        tbl$table_data$col_data$mean_hp,
        tbl$table_data$col_data$mean_wt
      ))
    )
  )
})
