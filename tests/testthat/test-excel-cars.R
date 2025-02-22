test_that("cars", {
  library(tablespan)
  library(testthat)
  library(dplyr)
  library(openxlsx)

  comp_file_dir <- paste0(testthat::test_path(), "/xlsx_files/")

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

  wb <- as_excel(tbl = tbl)

  # Compare just the data
  xlsx_compare <- openxlsx::read.xlsx(xlsxFile = paste0(comp_file_dir, "cars.xlsx"))
  testthat::expect_true(all.equal(openxlsx::read.xlsx(wb), xlsx_compare))

  # to compare workbooks, we have to write and reload the xlsx file
  tmp_dir <- tempdir()
  openxlsx::saveWorkbook(wb,
                         file = paste0(tmp_dir, "/cars_test.xlsx"),
                         overwrite = TRUE)

  wb <- openxlsx::loadWorkbook(file = paste0(tmp_dir, "/cars_test.xlsx"))
  wb_compare <- openxlsx::loadWorkbook(file = paste0(comp_file_dir, "cars.xlsx"))
  testthat::expect_true(all.equal(wb$worksheets,  wb_compare$worksheets))

  # Check that the data was written correctly:
  locs <- tablespan:::get_locations(tbl, start_row = 1, start_col = 1)
  testthat::expect_true(
    max(abs(openxlsx::read.xlsx(wb,
                                rows = locs$row$start_row_data:locs$row$end_row_data,
                                cols = locs$col$start_col_header_lhs:locs$col$end_col_header_rhs,
                                colNames = FALSE) - summarized_table),
        na.rm = TRUE) < 1e-5)
  testthat::expect_true(
    all(is.na(openxlsx::read.xlsx(wb,
                                  rows = locs$row$start_row_data:locs$row$end_row_data,
                                  cols = locs$col$start_col_header_lhs:locs$col$end_col_header_rhs,
                                  colNames = FALSE)) == is.na(summarized_table)))
})

test_that("cars - color", {
  library(tablespan)
  library(testthat)
  library(dplyr)
  library(openxlsx)

  comp_file_dir <- paste0(testthat::test_path(), "/xlsx_files/")

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

  wb <- as_excel(tbl = tbl, styles = style_color(primary_color = "#987349"))

  # Compare just the data
  xlsx_compare <- openxlsx::read.xlsx(xlsxFile = paste0(comp_file_dir, "cars_987349.xlsx"))
  testthat::expect_true(all.equal(openxlsx::read.xlsx(wb), xlsx_compare))

  # to compare workbooks, we have to write and reload the xlsx file
  tmp_dir <- tempdir()
  openxlsx::saveWorkbook(wb,
                         file = paste0(tmp_dir, "/cars_test.xlsx"),
                         overwrite = TRUE)

  wb <- openxlsx::loadWorkbook(file = paste0(tmp_dir, "/cars_test.xlsx"))
  wb_compare <- openxlsx::loadWorkbook(file = paste0(comp_file_dir, "cars.xlsx"))
  testthat::expect_true(all.equal(wb$worksheets,  wb_compare$worksheets))

  # Check that the data was written correctly:
  locs <- tablespan:::get_locations(tbl, start_row = 1, start_col = 1)
  testthat::expect_true(
    max(abs(openxlsx::read.xlsx(wb,
                                rows = locs$row$start_row_data:locs$row$end_row_data,
                                cols = locs$col$start_col_header_lhs:locs$col$end_col_header_rhs,
                                colNames = FALSE) - summarized_table),
        na.rm = TRUE) < 1e-5)
  testthat::expect_true(
    all(is.na(openxlsx::read.xlsx(wb,
                                  rows = locs$row$start_row_data:locs$row$end_row_data,
                                  cols = locs$col$start_col_header_lhs:locs$col$end_col_header_rhs,
                                  colNames = FALSE)) == is.na(summarized_table)))

})

test_that("cars - color - 2", {
  library(tablespan)
  library(testthat)
  library(dplyr)
  library(openxlsx)

  comp_file_dir <- paste0(testthat::test_path(), "/xlsx_files/")

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

  wb <- as_excel(tbl = tbl, styles = style_color(primary_color = "#893485"))

  # Compare just the data
  xlsx_compare <- openxlsx::read.xlsx(xlsxFile = paste0(comp_file_dir, "cars_893485.xlsx"))
  testthat::expect_true(all.equal(openxlsx::read.xlsx(wb), xlsx_compare))

  # to compare workbooks, we have to write and reload the xlsx file
  tmp_dir <- tempdir()
  openxlsx::saveWorkbook(wb,
                         file = paste0(tmp_dir, "/cars_test.xlsx"),
                         overwrite = TRUE)

  wb <- openxlsx::loadWorkbook(file = paste0(tmp_dir, "/cars_test.xlsx"))
  wb_compare <- openxlsx::loadWorkbook(file = paste0(comp_file_dir, "cars.xlsx"))
  testthat::expect_true(all.equal(wb$worksheets,  wb_compare$worksheets))

  # Check that the data was written correctly:
  locs <- tablespan:::get_locations(tbl, start_row = 1, start_col = 1)
  testthat::expect_true(
    max(abs(openxlsx::read.xlsx(wb,
                                rows = locs$row$start_row_data:locs$row$end_row_data,
                                cols = locs$col$start_col_header_lhs:locs$col$end_col_header_rhs,
                                colNames = FALSE) - summarized_table),
        na.rm = TRUE) < 1e-5)
  testthat::expect_true(
    all(is.na(openxlsx::read.xlsx(wb,
                                  rows = locs$row$start_row_data:locs$row$end_row_data,
                                  cols = locs$col$start_col_header_lhs:locs$col$end_col_header_rhs,
                                  colNames = FALSE)) == is.na(summarized_table)))

})

test_that("cars-offset", {
  library(tablespan)
  library(testthat)
  library(dplyr)
  library(openxlsx)

  comp_file_dir <- paste0(testthat::test_path(), "/xlsx_files/")

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

  wb <- as_excel(tbl = tbl, start_row = 3, start_col = 5)

  # Compare just the data
  xlsx_compare <- openxlsx::read.xlsx(xlsxFile = paste0(comp_file_dir, "cars_offset.xlsx"))
  testthat::expect_true(all.equal(openxlsx::read.xlsx(wb), xlsx_compare))

  # to compare workbooks, we have to write and reload the xlsx file
  tmp_dir <- tempdir()
  openxlsx::saveWorkbook(wb,
                         file = paste0(tmp_dir, "/cars_test.xlsx"),
                         overwrite = TRUE)

  wb <- openxlsx::loadWorkbook(file = paste0(tmp_dir, "/cars_test.xlsx"))
  wb_compare <- openxlsx::loadWorkbook(file = paste0(comp_file_dir, "cars_offset.xlsx"))
  testthat::expect_true(all.equal(wb$worksheets,  wb_compare$worksheets))

  # Check that the data was written correctly:
  locs <- tablespan:::get_locations(tbl, start_row = 3, start_col = 5)
  testthat::expect_true(
    max(abs(openxlsx::read.xlsx(wb,
                                rows = locs$row$start_row_data:locs$row$end_row_data,
                                cols = locs$col$start_col_header_lhs:locs$col$end_col_header_rhs,
                                colNames = FALSE) - summarized_table),
        na.rm = TRUE) < 1e-5)
  testthat::expect_true(
    all(is.na(openxlsx::read.xlsx(wb,
                                  rows = locs$row$start_row_data:locs$row$end_row_data,
                                  cols = locs$col$start_col_header_lhs:locs$col$end_col_header_rhs,
                                  colNames = FALSE)) == is.na(summarized_table)))
})

test_that("cars-cell_styles", {
  library(tablespan)
  library(testthat)
  library(dplyr)
  library(openxlsx)

  comp_file_dir <- paste0(testthat::test_path(), "/xlsx_files/")

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

  bold <- openxlsx::createStyle(textDecoration = "bold")
  wb <- as_excel(tbl = tbl,
                 styles = tbl_styles(cell_styles = list(cell_style(rows = 2:3,
                                                                   colnames = "mean_hp",
                                                                   style = bold),
                                                        cell_style(rows = 1,
                                                                   colnames = c("mean_wt", "sd_wt"),
                                                                   style = bold))))

  # Compare just the data
  xlsx_compare <- openxlsx::read.xlsx(xlsxFile = paste0(comp_file_dir, "cars_cell_styles.xlsx"))
  testthat::expect_true(all.equal(openxlsx::read.xlsx(wb), xlsx_compare))

  # to compare workbooks, we have to write and reload the xlsx file
  tmp_dir <- tempdir()
  openxlsx::saveWorkbook(wb,
                         file = paste0(tmp_dir, "/cars_test.xlsx"),
                         overwrite = TRUE)

  wb <- openxlsx::loadWorkbook(file = paste0(tmp_dir, "/cars_test.xlsx"))
  wb_compare <- openxlsx::loadWorkbook(file = paste0(comp_file_dir, "cars_cell_styles.xlsx"))
  testthat::expect_true(all.equal(wb$worksheets,  wb_compare$worksheets))

  # Check that the data was written correctly:
  locs <- tablespan:::get_locations(tbl, start_row = 1, start_col = 1)
  testthat::expect_true(
    max(abs(openxlsx::read.xlsx(wb,
                                rows = locs$row$start_row_data:locs$row$end_row_data,
                                cols = locs$col$start_col_header_lhs:locs$col$end_col_header_rhs,
                                colNames = FALSE) - summarized_table),
        na.rm = TRUE) < 1e-5)
  testthat::expect_true(
    all(is.na(openxlsx::read.xlsx(wb,
                                  rows = locs$row$start_row_data:locs$row$end_row_data,
                                  cols = locs$col$start_col_header_lhs:locs$col$end_col_header_rhs,
                                  colNames = FALSE)) == is.na(summarized_table)))
})

test_that("cars-data_styles", {
  library(tablespan)
  library(testthat)
  library(dplyr)
  library(openxlsx)

  comp_file_dir <- paste0(testthat::test_path(), "/xlsx_files/")

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

  # custom data type styles
  bold <- openxlsx::createStyle(textDecoration = "bold")
  wb <- as_excel(tbl = tbl,
                 styles = tbl_styles(data_styles = create_data_styles(double = list(test = is.double,
                                                                                    style = bold))))

  # Compare just the data
  xlsx_compare <- openxlsx::read.xlsx(xlsxFile = paste0(comp_file_dir, "cars_data_styles.xlsx"))
  testthat::expect_true(all.equal(openxlsx::read.xlsx(wb), xlsx_compare))

  # to compare workbooks, we have to write and reload the xlsx file
  tmp_dir <- tempdir()
  openxlsx::saveWorkbook(wb,
                         file = paste0(tmp_dir, "/cars_test.xlsx"),
                         overwrite = TRUE)

  wb <- openxlsx::loadWorkbook(file = paste0(tmp_dir, "/cars_test.xlsx"))
  wb_compare <- openxlsx::loadWorkbook(file = paste0(comp_file_dir, "cars_data_styles.xlsx"))
  testthat::expect_true(all.equal(wb$worksheets,  wb_compare$worksheets))

  # Check that the data was written correctly:
  locs <- tablespan:::get_locations(tbl, start_row = 1, start_col = 1)
  testthat::expect_true(
    max(abs(openxlsx::read.xlsx(wb,
                                rows = locs$row$start_row_data:locs$row$end_row_data,
                                cols = locs$col$start_col_header_lhs:locs$col$end_col_header_rhs,
                                colNames = FALSE) - summarized_table),
        na.rm = TRUE) < 1e-5)
  testthat::expect_true(
    all(is.na(openxlsx::read.xlsx(wb,
                                  rows = locs$row$start_row_data:locs$row$end_row_data,
                                  cols = locs$col$start_col_header_lhs:locs$col$end_col_header_rhs,
                                  colNames = FALSE)) == is.na(summarized_table)))
})

test_that("cars-additional_spanners", {
  library(tablespan)
  library(testthat)
  library(dplyr)
  library(openxlsx)

  comp_file_dir <- paste0(testthat::test_path(), "/xlsx_files/")

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

  wb <- as_excel(tbl = tbl)

  # Compare just the data
  xlsx_compare <- openxlsx::read.xlsx(xlsxFile = paste0(comp_file_dir, "cars_additional_spanners.xlsx"))
  testthat::expect_true(all.equal(openxlsx::read.xlsx(wb), xlsx_compare))

  # to compare workbooks, we have to write and reload the xlsx file
  tmp_dir <- tempdir()
  openxlsx::saveWorkbook(wb,
                         file = paste0(tmp_dir, "/cars_test.xlsx"),
                         overwrite = TRUE)

  wb <- openxlsx::loadWorkbook(file = paste0(tmp_dir, "/cars_test.xlsx"))
  wb_compare <- openxlsx::loadWorkbook(file = paste0(comp_file_dir, "cars_additional_spanners.xlsx"))
  testthat::expect_true(all.equal(wb$worksheets,  wb_compare$worksheets))

  # Check that the data was written correctly:
  locs <- tablespan:::get_locations(tbl, start_row = 1, start_col = 1)
  testthat::expect_true(
    max(abs(openxlsx::read.xlsx(wb,
                                rows = locs$row$start_row_data:locs$row$end_row_data,
                                cols = locs$col$start_col_header_lhs:locs$col$end_col_header_rhs,
                                colNames = FALSE) - summarized_table),
        na.rm = TRUE) < 1e-5)
  testthat::expect_true(
    all(is.na(openxlsx::read.xlsx(wb,
                                  rows = locs$row$start_row_data:locs$row$end_row_data,
                                  cols = locs$col$start_col_header_lhs:locs$col$end_col_header_rhs,
                                  colNames = FALSE)) == is.na(summarized_table)))
})

test_that("cars-no_row_names", {
  library(tablespan)
  library(testthat)
  library(dplyr)
  library(openxlsx)

  comp_file_dir <- paste0(testthat::test_path(), "/xlsx_files/")

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

  wb <- as_excel(tbl = tbl)

  # Compare just the data
  xlsx_compare <- openxlsx::read.xlsx(xlsxFile = paste0(comp_file_dir, "cars_no_row_names.xlsx"))
  testthat::expect_true(all.equal(openxlsx::read.xlsx(wb), xlsx_compare))

  # to compare workbooks, we have to write and reload the xlsx file
  tmp_dir <- tempdir()
  openxlsx::saveWorkbook(wb,
                         file = paste0(tmp_dir, "/cars_test.xlsx"),
                         overwrite = TRUE)

  wb <- openxlsx::loadWorkbook(file = paste0(tmp_dir, "/cars_test.xlsx"))
  wb_compare <- openxlsx::loadWorkbook(file = paste0(comp_file_dir, "cars_no_row_names.xlsx"))
  testthat::expect_true(all.equal(wb$worksheets,  wb_compare$worksheets))

  # Check that the data was written correctly:
  locs <- tablespan:::get_locations(tbl, start_row = 1, start_col = 1)
  read_data <- openxlsx::read.xlsx(wb,
                                   rows = locs$row$start_row_data:locs$row$end_row_data,
                                   cols = locs$col$start_col_header_rhs:locs$col$end_col_header_rhs,
                                   colNames = FALSE)
  expected_data <- summarized_table |>
    ungroup() |>
    select(N, mean_hp, sd_hp, mean_wt, sd_wt)
  testthat::expect_true(max(abs(read_data - expected_data), na.rm = TRUE) < 1e-5)
  testthat::expect_true(all(is.na(read_data) == is.na(expected_data)))
})

test_that("cars-no_titles", {
  library(tablespan)
  library(testthat)
  library(dplyr)
  library(openxlsx)

  comp_file_dir <- paste0(testthat::test_path(), "/xlsx_files/")

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(N = n(),
              mean_hp = mean(hp),
              sd_hp = sd(hp),
              mean_wt = mean(wt),
              sd_wt = sd(wt))

  # no titles
  tbl <- tablespan(data = summarized_table,
                   formula = 1 ~
                     (Results = N +
                        (`Horse Power` = (Mean = Mean:mean_hp) + (`Standard Deviation` = SD:sd_hp)) +
                        (`Weight` = Mean:mean_wt + SD:sd_wt)),
                   footnote = "Data from the infamous mtcars data set.")

  wb <- as_excel(tbl = tbl)

  # Compare just the data
  xlsx_compare <- openxlsx::read.xlsx(xlsxFile = paste0(comp_file_dir, "cars_no_titles.xlsx"))
  testthat::expect_true(all.equal(openxlsx::read.xlsx(wb), xlsx_compare))

  # to compare workbooks, we have to write and reload the xlsx file
  tmp_dir <- tempdir()
  openxlsx::saveWorkbook(wb,
                         file = paste0(tmp_dir, "/cars_test.xlsx"),
                         overwrite = TRUE)

  wb <- openxlsx::loadWorkbook(file = paste0(tmp_dir, "/cars_test.xlsx"))
  wb_compare <- openxlsx::loadWorkbook(file = paste0(comp_file_dir, "cars_no_titles.xlsx"))
  testthat::expect_true(all.equal(wb$worksheets,  wb_compare$worksheets))

  # Check that the data was written correctly:
  locs <- tablespan:::get_locations(tbl, start_row = 1, start_col = 1)
  read_data <- openxlsx::read.xlsx(wb,
                                   rows = locs$row$start_row_data:locs$row$end_row_data,
                                   cols = locs$col$start_col_header_rhs:locs$col$end_col_header_rhs,
                                   colNames = FALSE)
  expected_data <- summarized_table |>
    ungroup() |>
    select(N, mean_hp, sd_hp, mean_wt, sd_wt)
  testthat::expect_true(max(abs(read_data - expected_data), na.rm = TRUE) < 1e-5)
  testthat::expect_true(all(is.na(read_data) == is.na(expected_data)))
})

test_that("cars-no_titles_no_footnotes", {
  library(tablespan)
  library(testthat)
  library(dplyr)
  library(openxlsx)

  comp_file_dir <- paste0(testthat::test_path(), "/xlsx_files/")

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(N = n(),
              mean_hp = mean(hp),
              sd_hp = sd(hp),
              mean_wt = mean(wt),
              sd_wt = sd(wt))

  # no titles, no footnote
  tbl <- tablespan(data = summarized_table,
                   formula = 1 ~
                     (Results = N +
                        (`Horse Power` = (Mean = Mean:mean_hp) + (`Standard Deviation` = SD:sd_hp)) +
                        (`Weight` = Mean:mean_wt + SD:sd_wt)))

  wb <- as_excel(tbl = tbl)

  # Compare just the data
  xlsx_compare <- openxlsx::read.xlsx(xlsxFile = paste0(comp_file_dir, "cars_no_titles_no_footnote.xlsx"))
  testthat::expect_true(all.equal(openxlsx::read.xlsx(wb), xlsx_compare))

  # to compare workbooks, we have to write and reload the xlsx file
  tmp_dir <- tempdir()
  openxlsx::saveWorkbook(wb,
                         file = paste0(tmp_dir, "/cars_test.xlsx"),
                         overwrite = TRUE)

  wb <- openxlsx::loadWorkbook(file = paste0(tmp_dir, "/cars_test.xlsx"))
  wb_compare <- openxlsx::loadWorkbook(file = paste0(comp_file_dir, "cars_no_titles_no_footnote.xlsx"))
  testthat::expect_true(all.equal(wb$worksheets,  wb_compare$worksheets))

  # Check that the data was written correctly:
  locs <- tablespan:::get_locations(tbl, start_row = 1, start_col = 1)
  read_data <- openxlsx::read.xlsx(wb,
                                   rows = locs$row$start_row_data:locs$row$end_row_data,
                                   cols = locs$col$start_col_header_rhs:locs$col$end_col_header_rhs,
                                   colNames = FALSE)
  expected_data <- summarized_table |>
    ungroup() |>
    select(N, mean_hp, sd_hp, mean_wt, sd_wt)
  testthat::expect_true(max(abs(read_data - expected_data), na.rm = TRUE) < 1e-5)
  testthat::expect_true(all(is.na(read_data) == is.na(expected_data)))
})


test_that("cars-table_with_higher_spanner_left_right", {
  library(tablespan)
  library(testthat)
  library(dplyr)
  library(openxlsx)

  comp_file_dir <- paste0(testthat::test_path(), "/xlsx_files/")

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(N = n(),
              mean_hp = mean(hp),
              sd_hp = sd(hp),
              mean_wt = mean(wt),
              sd_wt = sd(wt))

  # Spanner where we need additional lines
  tbl <- tablespan(data = summarized_table,
                   formula = Cylinder:cyl + Engine:vs ~
                     (Results = N +
                        (`Inner result` = (`Horse Power` = (Mean = Mean:mean_hp) + (`Standard Deviation` = SD:sd_hp)) +
                           (`Weight` = Mean:mean_wt + SD:sd_wt))),
                   title = "Motor Trend Car Road Tests",
                   subtitle = "A table created with tablespan",
                   footnote = "Data from the infamous mtcars data set.")

  wb <- as_excel(tbl = tbl)

  # Compare just the data
  xlsx_compare <- openxlsx::read.xlsx(xlsxFile = paste0(comp_file_dir, "cars_additional_spanners_left_right.xlsx"))
  testthat::expect_true(all.equal(openxlsx::read.xlsx(wb), xlsx_compare))

  # to compare workbooks, we have to write and reload the xlsx file
  tmp_dir <- tempdir()
  openxlsx::saveWorkbook(wb,
                         file = paste0(tmp_dir, "/cars_test.xlsx"),
                         overwrite = TRUE)

  wb <- openxlsx::loadWorkbook(file = paste0(tmp_dir, "/cars_test.xlsx"))
  wb_compare <- openxlsx::loadWorkbook(file = paste0(comp_file_dir, "cars_additional_spanners_left_right.xlsx"))
  testthat::expect_true(all.equal(wb$worksheets,  wb_compare$worksheets))

  # Check that the data was written correctly:
  locs <- tablespan:::get_locations(tbl, start_row = 1, start_col = 1)
  testthat::expect_true(
    max(abs(openxlsx::read.xlsx(wb,
                                rows = locs$row$start_row_data:locs$row$end_row_data,
                                cols = locs$col$start_col_header_lhs:locs$col$end_col_header_rhs,
                                colNames = FALSE) - summarized_table),
        na.rm = TRUE) < 1e-5)
  testthat::expect_true(
    all(is.na(openxlsx::read.xlsx(wb,
                                  rows = locs$row$start_row_data:locs$row$end_row_data,
                                  cols = locs$col$start_col_header_lhs:locs$col$end_col_header_rhs,
                                  colNames = FALSE)) == is.na(summarized_table)))

})

test_that("cars - missing rownames", {
  library(tablespan)
  library(testthat)
  library(dplyr)
  library(openxlsx)

  comp_file_dir <- paste0(testthat::test_path(), "/xlsx_files/")

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(N = n(),
              mean_hp = mean(hp),
              sd_hp = sd(hp),
              mean_wt = mean(wt),
              sd_wt = sd(wt))

  summarized_table[1,1] <- NA
  summarized_table[2,1] <- NA

  tbl <- tablespan(data = summarized_table,
                   formula = Cylinder:cyl + Engine:vs ~
                     N +
                     (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
                     (`Weight` = Mean:mean_wt + SD:sd_wt),
                   title = "Motor Trend Car Road Tests",
                   subtitle = "A table created with tablespan",
                   footnote = "Data from the infamous mtcars data set.")

  wb <- as_excel(tbl = tbl)

  # Compare just the data
  xlsx_compare <- openxlsx::read.xlsx(xlsxFile = paste0(comp_file_dir, "cars_missing_rownames.xlsx"))
  testthat::expect_true(all.equal(openxlsx::read.xlsx(wb), xlsx_compare))

  # to compare workbooks, we have to write and reload the xlsx file
  tmp_dir <- tempdir()
  openxlsx::saveWorkbook(wb,
                         file = paste0(tmp_dir, "/cars_test.xlsx"),
                         overwrite = TRUE)

  wb <- openxlsx::loadWorkbook(file = paste0(tmp_dir, "/cars_test.xlsx"))
  wb_compare <- openxlsx::loadWorkbook(file = paste0(comp_file_dir, "cars_missing_rownames.xlsx"))
  testthat::expect_true(all.equal(wb$worksheets,  wb_compare$worksheets))

  # Check that the data was written correctly:
  locs <- tablespan:::get_locations(tbl, start_row = 1, start_col = 1)
  testthat::expect_true(
    max(abs(openxlsx::read.xlsx(wb,
                                rows = locs$row$start_row_data:locs$row$end_row_data,
                                cols = locs$col$start_col_header_lhs:locs$col$end_col_header_rhs,
                                colNames = FALSE) - summarized_table),
        na.rm = TRUE) < 1e-5)
  testthat::expect_true(
    all(is.na(openxlsx::read.xlsx(wb,
                                  rows = locs$row$start_row_data:locs$row$end_row_data,
                                  cols = locs$col$start_col_header_lhs:locs$col$end_col_header_rhs,
                                  colNames = FALSE)) == is.na(summarized_table)))

})
