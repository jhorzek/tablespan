create_test_files_cars <- function() {
  library(testthat)
  library(tablespan)
  library(dplyr)
  library(openxlsx)

  target_dir <- paste0(testthat::test_path(), "/xlsx_files/")

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

  wb <- as_excel(tbl = tbl)

  openxlsx::saveWorkbook(
    wb,
    file = paste0(target_dir, "cars.xlsx"),
    overwrite = TRUE
  )

  # Complex merging of rownames
  summarized_table_merge <- summarized_table
  summarized_table_merge[, "vs"] <- 1
  summarized_table_merge[1, "vs"] <- 0
  summarized_table_merge[, "N"] <- 1

  tbl_merge <- tablespan(
    data = summarized_table_merge,
    formula = Cylinder:cyl + Engine:vs + N ~
      (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
        (`Weight` = Mean:mean_wt + SD:sd_wt),
    title = "Motor Trend Car Road Tests",
    subtitle = "A table created with tablespan",
    footnote = "Data from the infamous mtcars data set."
  )

  wb <- as_excel(tbl = tbl_merge)
  openxlsx::saveWorkbook(
    wb,
    file = paste0(target_dir, "cars_complex_merge.xlsx"),
    overwrite = TRUE
  )

  # offset
  wb <- as_excel(tbl = tbl, start_row = 3, start_col = 5)

  openxlsx::saveWorkbook(
    wb,
    file = paste0(target_dir, "cars_offset.xlsx"),
    overwrite = TRUE
  )

  # custom cell styles
  bold <- openxlsx::createStyle(textDecoration = "bold")
  wb <- as_excel(
    tbl = tbl |>
      style_column(mean_hp,
                   rows = 2:3,
                   bold = TRUE) |>
      style_column(c(mean_wt, sd_wt),
                   rows = 1,
                   bold = TRUE)
  )
  openxlsx::saveWorkbook(
    wb,
    file = paste0(target_dir, "cars_cell_styles.xlsx"),
    overwrite = TRUE
  )

  # custom data type styles
  wb <- as_excel(
    tbl = tbl |>
      style_column(columns = dplyr::where(is.double),
                   bold = TRUE, italic = TRUE)
  )
  openxlsx::saveWorkbook(
    wb,
    file = paste0(target_dir, "cars_data_styles.xlsx"),
    overwrite = TRUE
  )

  # gradient
  wb <- as_excel(
    tbl = tbl |>
      style_column(columns = dplyr::where(is.double),
                   color_scale = c("#123456" = min(summarized_table |> select(where(is.double)), na.rm = TRUE),
                                   "#B46983" = max(summarized_table |> select(where(is.double)), na.rm = TRUE)))
  )

  openxlsx::saveWorkbook(
    wb,
    file = paste0(target_dir, "cars_data_gradient_2.xlsx"),
    overwrite = TRUE
  )

  wb <- as_excel(
    tbl = tbl |>
      style_column(columns = dplyr::where(is.double),
                   color_scale = c("#123456" = min(summarized_table |> select(where(is.double)), na.rm = TRUE),
                                   "#ffffff" = 50,
                                   "#B46983" = max(summarized_table |> select(where(is.double)), na.rm = TRUE)))
  )

  openxlsx::saveWorkbook(
    wb,
    file = paste0(target_dir, "cars_data_gradient_3.xlsx"),
    overwrite = TRUE
  )

  # Additional spanners
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

  wb <- as_excel(tbl = tbl)

  openxlsx::saveWorkbook(
    wb,
    file = paste0(target_dir, "cars_additional_spanners.xlsx"),
    overwrite = TRUE
  )

  # Spanner where we need additional lines
  tbl <- tablespan(
    data = summarized_table,
    formula = Cylinder:cyl + Engine:vs ~
      (Results = N +
        (`Inner result` = (`Horse Power` = (Mean = Mean:mean_hp) +
          (`Standard Deviation` = SD:sd_hp)) +
          (`Weight` = Mean:mean_wt + SD:sd_wt))),
    title = "Motor Trend Car Road Tests",
    subtitle = "A table created with tablespan",
    footnote = "Data from the infamous mtcars data set."
  )

  wb <- as_excel(tbl = tbl)

  openxlsx::saveWorkbook(
    wb,
    file = paste0(target_dir, "cars_additional_spanners_left_right.xlsx"),
    overwrite = TRUE
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

  wb <- as_excel(tbl = tbl)

  openxlsx::saveWorkbook(
    wb,
    file = paste0(target_dir, "cars_no_row_names.xlsx"),
    overwrite = TRUE
  )
  # no titles
  tbl <- tablespan(
    data = summarized_table,
    formula = 1 ~
      (Results = N +
        (`Horse Power` = (Mean = Mean:mean_hp) +
          (`Standard Deviation` = SD:sd_hp)) +
        (`Weight` = Mean:mean_wt + SD:sd_wt)),
    footnote = "Data from the infamous mtcars data set."
  )

  wb <- as_excel(tbl = tbl)

  openxlsx::saveWorkbook(
    wb,
    file = paste0(target_dir, "cars_no_titles.xlsx"),
    overwrite = TRUE
  )

  # no titles, no footnote
  tbl <- tablespan(
    data = summarized_table,
    formula = 1 ~
      (Results = N +
        (`Horse Power` = (Mean = Mean:mean_hp) +
          (`Standard Deviation` = SD:sd_hp)) +
        (`Weight` = Mean:mean_wt + SD:sd_wt))
  )

  wb <- as_excel(tbl = tbl)

  openxlsx::saveWorkbook(
    wb,
    file = paste0(target_dir, "cars_no_titles_no_footnote.xlsx"),
    overwrite = TRUE
  )

  ## Test data with missing row names. See https://github.com/jhorzek/tablespan/issues/40
  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(
      N = n(),
      mean_hp = mean(hp),
      sd_hp = sd(hp),
      mean_wt = mean(wt),
      sd_wt = sd(wt)
    )

  summarized_table[1, 1] <- NA
  summarized_table[2, 1] <- NA

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

  wb <- as_excel(tbl = tbl)

  openxlsx::saveWorkbook(
    wb,
    file = paste0(target_dir, "cars_missing_rownames.xlsx"),
    overwrite = TRUE
  )
}
