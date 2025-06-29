test_that("multiplication works", {
  library(tablespan)
  library(dplyr)
  data("mtcars")

  # We want to report the following table:
  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(N = n(),
              mean_hp = mean(hp),
              sd_hp = sd(hp),
              mean_wt = mean(wt),
              sd_wt = sd(wt))

  tablespan(summarized_table[1,],
            formula = as.formula(paste0("1 ~ ", paste0(colnames(summarized_table), collapse = " + "))))
})
