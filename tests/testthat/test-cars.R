test_that("cars", {
  library(basicTables)
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

  tbl <- bt(data = summarized_table,
            formula = Cylinder:cyl + Engine:vs ~
              N +
              (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
              (`Weight` = Mean:mean_wt + SD:sd_wt),
            title = "Motor Trend Car Road Tests",
            subtitle = "A table created with basicTables",
            footnote = "Data from the infamous mtcars data set.")

  wb <- write_bt(tbl = tbl)

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

})

test_that("cars-offset", {
  library(basicTables)
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

  tbl <- bt(data = summarized_table,
            formula = Cylinder:cyl + Engine:vs ~
              N +
              (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
              (`Weight` = Mean:mean_wt + SD:sd_wt),
            title = "Motor Trend Car Road Tests",
            subtitle = "A table created with basicTables",
            footnote = "Data from the infamous mtcars data set.")

  wb <- write_bt(tbl = tbl, start_row = 3, start_col = 5)

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

})

test_that("cars-cell_styles", {
  library(basicTables)
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

  tbl <- bt(data = summarized_table,
            formula = Cylinder:cyl + Engine:vs ~
              N +
              (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
              (`Weight` = Mean:mean_wt + SD:sd_wt),
            title = "Motor Trend Car Road Tests",
            subtitle = "A table created with basicTables",
            footnote = "Data from the infamous mtcars data set.")

  bold <- openxlsx::createStyle(textDecoration = "bold")
  wb <- write_bt(tbl = tbl,
                 styles = bt_styles(cell_styles = list(cell_style(rows = 2:3,
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

})

test_that("cars-data_styles", {
  library(basicTables)
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

  tbl <- bt(data = summarized_table,
            formula = Cylinder:cyl + Engine:vs ~
              N +
              (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
              (`Weight` = Mean:mean_wt + SD:sd_wt),
            title = "Motor Trend Car Road Tests",
            subtitle = "A table created with basicTables",
            footnote = "Data from the infamous mtcars data set.")

  # custom data type styles
  bold <- openxlsx::createStyle(textDecoration = "bold")
  wb <- write_bt(tbl = tbl,
                 styles = bt_styles(data_styles = create_data_styles(double = list(test = is.double,
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

})

test_that("cars-additional_spanners", {
  library(basicTables)
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

  tbl <- bt(data = summarized_table,
            formula = Cylinder:cyl + Engine:vs ~
              (Results = N +
                 (`Horse Power` = (Mean = Mean:mean_hp) + (`Standard Deviation` = SD:sd_hp)) +
                 (`Weight` = Mean:mean_wt + SD:sd_wt)),
            title = "Motor Trend Car Road Tests",
            subtitle = "A table created with basicTables",
            footnote = "Data from the infamous mtcars data set.")

  wb <- write_bt(tbl = tbl)

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

})

test_that("cars-no_row_names", {
  library(basicTables)
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
  tbl <- bt(data = summarized_table,
            formula = 1 ~
              (Results = N +
                 (`Horse Power` = (Mean = Mean:mean_hp) + (`Standard Deviation` = SD:sd_hp)) +
                 (`Weight` = Mean:mean_wt + SD:sd_wt)),
            title = "Motor Trend Car Road Tests",
            subtitle = "A table created with basicTables",
            footnote = "Data from the infamous mtcars data set.")

  wb <- write_bt(tbl = tbl)

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

})

test_that("cars-no_titles", {
  library(basicTables)
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
  tbl <- bt(data = summarized_table,
            formula = 1 ~
              (Results = N +
                 (`Horse Power` = (Mean = Mean:mean_hp) + (`Standard Deviation` = SD:sd_hp)) +
                 (`Weight` = Mean:mean_wt + SD:sd_wt)),
            footnote = "Data from the infamous mtcars data set.")

  wb <- write_bt(tbl = tbl)

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

})

test_that("cars-no_titles_no_footnotes", {
  library(basicTables)
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
  tbl <- bt(data = summarized_table,
            formula = 1 ~
              (Results = N +
                 (`Horse Power` = (Mean = Mean:mean_hp) + (`Standard Deviation` = SD:sd_hp)) +
                 (`Weight` = Mean:mean_wt + SD:sd_wt)))

  wb <- write_bt(tbl = tbl)

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

})
