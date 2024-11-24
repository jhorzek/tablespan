test_that("cars", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  compare_tables <- function(tbl_1, tbl_2){
    tbl_1_html <- tbl_1 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    tbl_2_html <- tbl_2 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    return(tbl_1_html == tbl_2_html)
  }

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(N = n(),
              mean_hp = mean(hp),
              sd_hp = sd(hp),
              mean_wt = mean(wt),
              sd_wt = sd(wt))

  tbl <- tablespan(data = summarized_table,
                   formula = Cylinder:cyl + Engine:vs ~
                     N +
                     (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
                     (`Weight` = Mean:mean_wt + SD:sd_wt),
                   title = "Motor Trend Car Road Tests",
                   subtitle = "A table created with tablespan",
                   footnote = "Data from the infamous mtcars data set.")

  gt_tbl <- as_gt(tbl = tbl)

  expected <- summarized_table |>
    gt::gt(groupname_col = NULL) |>
    gt::tab_header(title = "Motor Trend Car Road Tests",
                   subtitle = "A table created with tablespan") |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_footnote(footnote = "Data from the infamous mtcars data set.") |>
    gt::tab_spanner(label = "Horse Power",
                    id = "__BASE_LEVEL__Horse Power",
                    columns = dplyr::all_of(c("mean_hp", "sd_hp"))) |>
    gt::tab_spanner(label = "Weight",
                    id = "__BASE_LEVEL__Weight",
                    columns = dplyr::all_of(c("mean_wt", "sd_wt"))) |>
    gt::tab_style(style = gt::cell_borders(sides = c("right"),
                                           weight = gt::px(1),
                                           color = "gray"),
                  locations = gt::cells_body(columns = all_of(c("vs")))) |>
    gt::cols_label(cyl = "Cylinder",
                   vs = "Engine",
                   mean_hp = "Mean",
                   sd_hp = "SD",
                   mean_wt = "Mean",
                   sd_wt = "SD") |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "")

  testthat::expect_true(compare_tables(tbl_1 = gt_tbl, tbl_2 = expected))
})

test_that("cars - no autostyle", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  compare_tables <- function(tbl_1, tbl_2){
    tbl_1_html <- tbl_1 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    tbl_2_html <- tbl_2 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    return(tbl_1_html == tbl_2_html)
  }

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(N = n(),
              mean_hp = mean(hp),
              sd_hp = sd(hp),
              mean_wt = mean(wt),
              sd_wt = sd(wt))

  tbl <- tablespan(data = summarized_table,
                   formula = Cylinder:cyl + Engine:vs ~
                     N +
                     (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
                     (`Weight` = Mean:mean_wt + SD:sd_wt),
                   title = "Motor Trend Car Road Tests",
                   subtitle = "A table created with tablespan",
                   footnote = "Data from the infamous mtcars data set.")

  gt_tbl <- as_gt(tbl = tbl, auto_format = FALSE)

  expected <- summarized_table |>
    gt::gt(groupname_col = NULL) |>
    gt::tab_header(title = "Motor Trend Car Road Tests",
                   subtitle = "A table created with tablespan") |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_footnote(footnote = "Data from the infamous mtcars data set.") |>
    gt::tab_spanner(label = "Horse Power",
                    id = "__BASE_LEVEL__Horse Power",
                    columns = dplyr::all_of(c("mean_hp", "sd_hp"))) |>
    gt::tab_spanner(label = "Weight",
                    id = "__BASE_LEVEL__Weight",
                    columns = dplyr::all_of(c("mean_wt", "sd_wt"))) |>
    gt::tab_style(style = gt::cell_borders(sides = c("right"),
                                           weight = gt::px(1),
                                           color = "gray"),
                  locations = gt::cells_body(columns = all_of(c("vs")))) |>
    gt::cols_label(cyl = "Cylinder",
                   vs = "Engine",
                   mean_hp = "Mean",
                   sd_hp = "SD",
                   mean_wt = "Mean",
                   sd_wt = "SD")

  testthat::expect_true(compare_tables(tbl_1 = gt_tbl, tbl_2 = expected))

})

test_that("cars-additional_spanners", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  compare_tables <- function(tbl_1, tbl_2){
    tbl_1_html <- tbl_1 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    tbl_2_html <- tbl_2 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    return(tbl_1_html == tbl_2_html)
  }

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(N = n(),
              mean_hp = mean(hp),
              sd_hp = sd(hp),
              mean_wt = mean(wt),
              sd_wt = sd(wt))

  tbl <- tablespan(data = summarized_table,
                   formula = Cylinder:cyl + Engine:vs ~
                     (Results = N +
                        (`Horse Power` = (Mean = Mean:mean_hp) + (`Standard Deviation` = SD:sd_hp)) +
                        (`Weight` = Mean:mean_wt + SD:sd_wt)),
                   title = "Motor Trend Car Road Tests",
                   subtitle = "A table created with tablespan",
                   footnote = "Data from the infamous mtcars data set.")

  gt_tbl <- as_gt(tbl = tbl)

  expected <- summarized_table |>
    gt::gt(groupname_col = NULL) |>
    gt::tab_header(title = "Motor Trend Car Road Tests",
                   subtitle = "A table created with tablespan") |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_footnote(footnote = "Data from the infamous mtcars data set.") |>
    gt::tab_spanner(label = "Mean",
                    id = "__BASE_LEVEL__Results_Horse Power_Mean",
                    columns = dplyr::all_of(c("mean_hp"))) |>
    gt::tab_spanner(label = "Standard Deviation",
                    id = "__BASE_LEVEL__Results_Horse Power_Standard Deviation",
                    columns = dplyr::all_of(c("sd_hp"))) |>
    gt::tab_spanner(label = "Horse Power",
                    id = "__BASE_LEVEL__Results_Horse Power",
                    spanners = c("__BASE_LEVEL__Results_Horse Power_Mean",
                                 "__BASE_LEVEL__Results_Horse Power_Standard Deviation")) |>
    gt::tab_spanner(label = "Weight",
                    id = "__BASE_LEVEL__Results_Weight",
                    columns = dplyr::all_of(c("mean_wt", "sd_wt"))) |>
    gt::tab_spanner(label = "Results",
                    id = "__BASE_LEVEL__Results",
                    columns = dplyr::all_of("N"),
                    spanners = c("__BASE_LEVEL__Results_Horse Power",
                                 "__BASE_LEVEL__Results_Weight")) |>
    gt::tab_style(style = gt::cell_borders(sides = c("right"),
                                           weight = gt::px(1),
                                           color = "gray"),
                  locations = gt::cells_body(columns = all_of(c("vs")))) |>
    gt::cols_label(cyl = "Cylinder",
                   vs = "Engine",
                   mean_hp = "Mean",
                   sd_hp = "SD",
                   mean_wt = "Mean",
                   sd_wt = "SD") |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "")

  testthat::expect_true(compare_tables(tbl_1 = gt_tbl, tbl_2 = expected))

})

test_that("cars-no_row_names", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  compare_tables <- function(tbl_1, tbl_2){
    tbl_1_html <- tbl_1 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    tbl_2_html <- tbl_2 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    return(tbl_1_html == tbl_2_html)
  }

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(N = n(),
              mean_hp = mean(hp),
              sd_hp = sd(hp),
              mean_wt = mean(wt),
              sd_wt = sd(wt))

  # no row names
  tbl <- tablespan(data = summarized_table,
                   formula = 1 ~
                     (Results = N +
                        (`Horse Power` = (Mean = Mean:mean_hp) + (`Standard Deviation` = SD:sd_hp)) +
                        (`Weight` = Mean:mean_wt + SD:sd_wt)),
                   title = "Motor Trend Car Road Tests",
                   subtitle = "A table created with tablespan",
                   footnote = "Data from the infamous mtcars data set.")

  gt_tbl <- as_gt(tbl = tbl)

  expected <- summarized_table |>
    ungroup() |>
    select(-dplyr::all_of(c("cyl", "vs"))) |>
    gt::gt(groupname_col = NULL) |>
    gt::tab_header(title = "Motor Trend Car Road Tests",
                   subtitle = "A table created with tablespan") |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_footnote(footnote = "Data from the infamous mtcars data set.") |>
    gt::tab_spanner(label = "Mean",
                    id = "__BASE_LEVEL__Results_Horse Power_Mean",
                    columns = dplyr::all_of(c("mean_hp"))) |>
    gt::tab_spanner(label = "Standard Deviation",
                    id = "__BASE_LEVEL__Results_Horse Power_Standard Deviation",
                    columns = dplyr::all_of(c("sd_hp"))) |>
    gt::tab_spanner(label = "Horse Power",
                    id = "__BASE_LEVEL__Results_Horse Power",
                    spanners = c("__BASE_LEVEL__Results_Horse Power_Mean",
                                 "__BASE_LEVEL__Results_Horse Power_Standard Deviation")) |>
    gt::tab_spanner(label = "Weight",
                    id = "__BASE_LEVEL__Results_Weight",
                    columns = dplyr::all_of(c("mean_wt", "sd_wt"))) |>
    gt::tab_spanner(label = "Results",
                    id = "__BASE_LEVEL__Results",
                    columns = dplyr::all_of("N"),
                    spanners = c("__BASE_LEVEL__Results_Horse Power",
                                 "__BASE_LEVEL__Results_Weight")) |>
    gt::cols_label(mean_hp = "Mean",
                   sd_hp = "SD",
                   mean_wt = "Mean",
                   sd_wt = "SD") |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "")

  testthat::expect_true(compare_tables(tbl_1 = gt_tbl, tbl_2 = expected))

})

test_that("cars-no_titles", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  compare_tables <- function(tbl_1, tbl_2){
    tbl_1_html <- tbl_1 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    tbl_2_html <- tbl_2 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    return(tbl_1_html == tbl_2_html)
  }

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(N = n(),
              mean_hp = mean(hp),
              sd_hp = sd(hp),
              mean_wt = mean(wt),
              sd_wt = sd(wt))

  tbl <- tablespan(data = summarized_table,
                   formula = Cylinder:cyl + Engine:vs ~
                     N +
                     (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
                     (`Weight` = Mean:mean_wt + SD:sd_wt),
                   footnote = "Data from the infamous mtcars data set.")

  gt_tbl <- as_gt(tbl = tbl)

  expected <- summarized_table |>
    gt::gt(groupname_col = NULL) |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_footnote(footnote = "Data from the infamous mtcars data set.") |>
    gt::tab_spanner(label = "Horse Power",
                    id = "__BASE_LEVEL__Horse Power",
                    columns = dplyr::all_of(c("mean_hp", "sd_hp"))) |>
    gt::tab_spanner(label = "Weight",
                    id = "__BASE_LEVEL__Weight",
                    columns = dplyr::all_of(c("mean_wt", "sd_wt"))) |>
    gt::tab_style(style = gt::cell_borders(sides = c("right"),
                                           weight = gt::px(1),
                                           color = "gray"),
                  locations = gt::cells_body(columns = all_of(c("vs")))) |>
    gt::cols_label(cyl = "Cylinder",
                   vs = "Engine",
                   mean_hp = "Mean",
                   sd_hp = "SD",
                   mean_wt = "Mean",
                   sd_wt = "SD") |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "")

  testthat::expect_true(compare_tables(tbl_1 = gt_tbl, tbl_2 = expected))
})

test_that("cars-no_titles_no_footnotes", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  compare_tables <- function(tbl_1, tbl_2){
    tbl_1_html <- tbl_1 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    tbl_2_html <- tbl_2 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    return(tbl_1_html == tbl_2_html)
  }

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(N = n(),
              mean_hp = mean(hp),
              sd_hp = sd(hp),
              mean_wt = mean(wt),
              sd_wt = sd(wt))

  tbl <- tablespan(data = summarized_table,
                   formula = Cylinder:cyl + Engine:vs ~
                     N +
                     (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
                     (`Weight` = Mean:mean_wt + SD:sd_wt))

  gt_tbl <- as_gt(tbl = tbl)

  expected <- summarized_table |>
    gt::gt(groupname_col = NULL) |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_spanner(label = "Horse Power",
                    id = "__BASE_LEVEL__Horse Power",
                    columns = dplyr::all_of(c("mean_hp", "sd_hp"))) |>
    gt::tab_spanner(label = "Weight",
                    id = "__BASE_LEVEL__Weight",
                    columns = dplyr::all_of(c("mean_wt", "sd_wt"))) |>
    gt::tab_style(style = gt::cell_borders(sides = c("right"),
                                           weight = gt::px(1),
                                           color = "gray"),
                  locations = gt::cells_body(columns = all_of(c("vs")))) |>
    gt::cols_label(cyl = "Cylinder",
                   vs = "Engine",
                   mean_hp = "Mean",
                   sd_hp = "SD",
                   mean_wt = "Mean",
                   sd_wt = "SD") |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "")

  testthat::expect_true(compare_tables(tbl_1 = gt_tbl, tbl_2 = expected))

})

test_that("cars-duplicated_spanner_names", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  compare_tables <- function(tbl_1, tbl_2){
    tbl_1_html <- tbl_1 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    tbl_2_html <- tbl_2 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    return(tbl_1_html == tbl_2_html)
  }

  model_1 <- lm(mpg ~ wt + qsec, data = mtcars) |>
    summary() |>
    (\(.x) as.data.frame(.x$coefficients))()
  model_2 <- lm(mpg ~ wt + qsec, data = mtcars) |>
    summary() |>
    (\(.x) as.data.frame(.x$coefficients))()


  model_1$Parameter <- rownames(model_1)
  model_2$Parameter <- rownames(model_2)

  combined_models <- full_join(model_1,
                               model_2,
                               by = "Parameter")

  tbl <- combined_models |>
    tablespan(formula = Parameter ~
                (`Model 1` = Estimate:Estimate.x + (Significance = `t-value`:`t value.x` + `p-value`:`Pr(>|t|).x`)) +
                (`Model 2` = Estimate:Estimate.y + (Significance = `t-value`:`t value.y` + `p-value`:`Pr(>|t|).y`)))

  gt_tbl <- as_gt(tbl)

  expected <- combined_models |>
    select(all_of(c("Parameter",
                    "Estimate.x", "t value.x", "Pr(>|t|).x",
                    "Estimate.y", "t value.y", "Pr(>|t|).y"))) |>
    gt::gt(groupname_col = NULL) |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_spanner(label = "Significance",
                    id = "__BASE_LEVEL__Model 1_Significance",
                    columns = dplyr::all_of(c("t value.x", "Pr(>|t|).x"))) |>
    gt::tab_spanner(label = "Significance",
                    id = "__BASE_LEVEL__Model 2_Significance",
                    columns = dplyr::all_of(c("t value.y", "Pr(>|t|).y"))) |>
    gt::tab_spanner(label = "Model 1",
                    id = "__BASE_LEVEL__Model 1",
                    columns = dplyr::all_of(c("Estimate.x")),
                    spanners = "__BASE_LEVEL__Model 1_Significance") |>
    gt::tab_spanner(label = "Model 2",
                    id = "__BASE_LEVEL__Model 2",
                    columns = dplyr::all_of(c("Estimate.y")),
                    spanners = "__BASE_LEVEL__Model 2_Significance") |>
    gt::tab_style(style = gt::cell_borders(sides = c("right"),
                                           weight = gt::px(1),
                                           color = "gray"),
                  locations = gt::cells_body(columns = all_of(c("Parameter")))) |>
    gt::cols_label(Estimate.x = "Estimate",
                   `t value.x` = "t-value",
                   `Pr(>|t|).x` = "p-value",
                   Estimate.y = "Estimate",
                   `t value.y` = "t-value",
                   `Pr(>|t|).y` = "p-value",) |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "")

  testthat::expect_true(compare_tables(tbl_1 = gt_tbl, tbl_2 = expected))

})
