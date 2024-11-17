test_that("print", {
  library(dplyr)
  library(tablespan)
  data("mtcars")

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(N = n(),
              mean_hp = mean(hp),
              sd_hp = sd(hp),
              mean_wt = mean(wt),
              sd_wt = sd(wt))
  testthat::expect_no_error(print.Tablespan(tablespan(data = summarized_table,
                                                      formula = cyl ~ mean_hp + sd_hp)))

  testthat::expect_no_error(print.Tablespan(tablespan(data = summarized_table,
                                                      formula = cyl ~ (Horsepower = mean_hp + sd_hp))))

  testthat::expect_no_error(print.Tablespan(tablespan(data = summarized_table,
                                                      formula = cyl ~ (Horsepower = Mean:mean_hp + SD:sd_hp))))

  testthat::expect_no_error(print.Tablespan(tablespan(data = summarized_table,
                                                      formula = Cylinder:cyl + Engine:vs ~
                                                        N +
                                                        (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
                                                        (`Weight` = Mean:mean_wt + SD:sd_wt),
                                                      title = "Motor Trend Car Road Tests",
                                                      subtitle = "A table created with tablespan",
                                                      footnote = "Data from the infamous mtcars data set.")))

  testthat::expect_no_error(print.Tablespan(tablespan(data = summarized_table,
                                                      formula = 1 ~ (Horsepower = Mean:mean_hp + SD:sd_hp))))
})
