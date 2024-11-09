test_that("errors", {
  library(tabelle)
  library(testthat)
  library(dplyr)
  library(openxlsx)

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(N = n(),
              mean_hp = mean(hp),
              sd_hp = sd(hp),
              mean_wt = mean(wt),
              sd_wt = sd(wt))

  tbl <- tabelle(data = summarized_table,
                 formula = Cylinder:cyl + Engine:vs ~
                   N +
                   (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
                   (`Weight` = Mean:mean_wt + SD:sd_wt),
                 title = "Motor Trend Car Road Tests",
                 subtitle = "A table created with tabelle",
                 footnote = "Data from the infamous mtcars data set.")

  testthat::expect_error(tabelle(data = summarized_table,
                                 formula = Cylinder:cyl + Engine:vs ~ 1))

  testthat::expect_error(tabelle(data = summarized_table,
                                 formula = new_name:(Cylinder:cyl + Engine:vs) ~ N))

  testthat::expect_error(tabelle(data = summarized_table,
                                 formula = (Cylinder:cyl + Engine:vs) ~ N))

  testthat::expect_error(tabelle(data = summarized_table,
                                 formula = Cylinder:cyl * Engine:vs ~ N))

  # unknown rows
  testthat::expect_error(write_tab(tbl = tabelle(data = summarized_table,
                                                 formula = Cylinder:cyl + Engine:vs ~
                                                   N + Unknown +
                                                   (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
                                                   (`Weight` = Mean:mean_wt + SD:sd_wt),
                                                 title = "Motor Trend Car Road Tests",
                                                 subtitle = "A table created with tabelle",
                                                 footnote = "Data from the infamous mtcars data set.")))
  testthat::expect_error(write_tab(tbl = tabelle(data = summarized_table,
                                                 formula = Cylinder:cyl + Engine:vs + Unknown ~
                                                   N +
                                                   (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
                                                   (`Weight` = Mean:mean_wt + SD:sd_wt),
                                                 title = "Motor Trend Car Road Tests",
                                                 subtitle = "A table created with tabelle",
                                                 footnote = "Data from the infamous mtcars data set.")))

  # incorrect specification of styles
  bold <- openxlsx::createStyle(textDecoration = "bold")
  testthat::expect_error(tab_styles(cell_styles = bold))
  testthat::expect_error(write_tab(tbl = tbl, styles = bold))

  testthat::expect_error(cell_style(rows = "1",
                                    colnames = "colname",
                                    style = bold))
  testthat::expect_error(cell_style(rows = 1,
                                    colnames = 1,
                                    style = bold))
  testthat::expect_error(cell_style(rows = 1,
                                    colnames = "colname",
                                    style = "bold"))

  # style outside of range
  tbl <- tabelle(data = summarized_table,
                 formula = Cylinder:cyl + Engine:vs ~
                   N +
                   (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
                   (`Weight` = Mean:mean_wt + SD:sd_wt),
                 title = "Motor Trend Car Road Tests",
                 subtitle = "A table created with tabelle",
                 footnote = "Data from the infamous mtcars data set.")
  bold <- openxlsx::createStyle(textDecoration = "bold")

  testthat::expect_error(write_tab(tbl = tbl,
                                   styles = tab_styles(cell_styles = list(cell_style(rows = 30,
                                                                                     colnames = "mean_hp",
                                                                                     style = bold)))))

  testthat::expect_error(write_tab(tbl = tbl,
                                   styles = tab_styles(cell_styles = list(cell_style(rows = 3,
                                                                                     colnames = "Unknown",
                                                                                     style = bold)))))
})
