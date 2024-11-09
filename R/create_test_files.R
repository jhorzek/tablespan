create_test_files_cars <- function(){
  library(testthat)
  library(basicTables)
  library(dplyr)
  library(openxlsx)

  target_dir <- paste0(testthat::test_path(), "/xlsx_files/")

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

  openxlsx::saveWorkbook(wb,
                         file = paste0(target_dir, "cars.xlsx"),
                         overwrite = TRUE)

  # offset
  wb <- write_bt(tbl = tbl, start_row = 3, start_col = 5)

  openxlsx::saveWorkbook(wb,
                         file = paste0(target_dir, "cars_offset.xlsx"),
                         overwrite = TRUE)

  # custom cell styles
  bold <- openxlsx::createStyle(textDecoration = "bold")
  wb <- write_bt(tbl = tbl,
                 styles = bt_styles(cell_styles = list(cell_style(rows = 2:3,
                                                                  colnames = "mean_hp",
                                                                  style = bold),
                                                       cell_style(rows = 1,
                                                                  colnames = c("mean_wt", "sd_wt"),
                                                                  style = bold))))
  openxlsx::saveWorkbook(wb,
                         file = paste0(target_dir, "cars_cell_styles.xlsx"),
                         overwrite = TRUE)

  # custom data type styles
  wb <- write_bt(tbl = tbl,
                 styles = bt_styles(data_styles = create_data_styles(double = list(test = is.double,
                                                                                   style = bold))))
  openxlsx::saveWorkbook(wb,
                         file = paste0(target_dir, "cars_data_styles.xlsx"),
                         overwrite = TRUE)

  # Additional spanners
  tbl <- bt(data = summarized_table,
            formula = Cylinder:cyl + Engine:vs ~
              (Results = N +
                 (`Horse Power` = (Mean = Mean:mean_hp) + (`Standard Deviation` = SD:sd_hp)) +
                 (`Weight` = Mean:mean_wt + SD:sd_wt)),
            title = "Motor Trend Car Road Tests",
            subtitle = "A table created with basicTables",
            footnote = "Data from the infamous mtcars data set.")

  wb <- write_bt(tbl = tbl)

  openxlsx::saveWorkbook(wb,
                         file = paste0(target_dir, "cars_additional_spanners.xlsx"),
                         overwrite = TRUE)

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

  openxlsx::saveWorkbook(wb,
                         file = paste0(target_dir, "cars_no_row_names.xlsx"),
                         overwrite = TRUE)
  # no titles
  tbl <- bt(data = summarized_table,
            formula = 1 ~
              (Results = N +
                 (`Horse Power` = (Mean = Mean:mean_hp) + (`Standard Deviation` = SD:sd_hp)) +
                 (`Weight` = Mean:mean_wt + SD:sd_wt)),
            footnote = "Data from the infamous mtcars data set.")

  wb <- write_bt(tbl = tbl)

  openxlsx::saveWorkbook(wb,
                         file = paste0(target_dir, "cars_no_titles.xlsx"),
                         overwrite = TRUE)

  # no titles, no footnote
  tbl <- bt(data = summarized_table,
            formula = 1 ~
              (Results = N +
                 (`Horse Power` = (Mean = Mean:mean_hp) + (`Standard Deviation` = SD:sd_hp)) +
                 (`Weight` = Mean:mean_wt + SD:sd_wt)))

  wb <- write_bt(tbl = tbl)

  openxlsx::saveWorkbook(wb,
                         file = paste0(target_dir, "cars_no_titles_no_footnote.xlsx"),
                         overwrite = TRUE)
}

