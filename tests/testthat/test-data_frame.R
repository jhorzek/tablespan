test_that("testing data.frame", {
  library(dplyr)
  library(tablespan)
  library(tibble)
  data("mtcars")

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(N = n(),
              mean_hp = mean(hp),
              sd_hp = sd(hp),
              mean_wt = mean(wt),
              sd_wt = sd(wt))

  testthat::expect_no_warning(print.Tablespan(tablespan(data = summarized_table,
                                                        formula = cyl ~ mean_hp + sd_hp)))
  testthat::expect_warning(print.Tablespan(tablespan(data = tibble::as_data_frame(summarized_table),
                                                     formula = cyl ~ mean_hp + sd_hp)))
})
