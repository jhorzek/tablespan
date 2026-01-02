test_that("print", {
  library(dplyr)
  library(tablespan)
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
  testthat::expect_no_error(print.Tablespan(tablespan(
    data = summarized_table,
    formula = cyl ~ mean_hp + sd_hp
  )))

  testthat::expect_no_error(print.Tablespan(tablespan(
    data = summarized_table,
    formula = cyl ~ (Horsepower = mean_hp + sd_hp)
  )))

  testthat::expect_no_error(print.Tablespan(tablespan(
    data = summarized_table,
    formula = cyl ~ (Horsepower = Mean:mean_hp + SD:sd_hp)
  )))

  testthat::expect_no_error(print.Tablespan(tablespan(
    data = summarized_table,
    formula = Cylinder:cyl + Engine:vs ~
      N +
      (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
      (`Weight` = Mean:mean_wt + SD:sd_wt),
    title = "Motor Trend Car Road Tests",
    subtitle = "A table created with tablespan",
    footnote = "Data from the infamous mtcars data set."
  )))

  testthat::expect_no_error(print.Tablespan(tablespan(
    data = summarized_table,
    formula = 1 ~ (Horsepower = Mean:mean_hp + SD:sd_hp)
  )))
})

test_that("print (no hux)", {
  library(dplyr)
  library(tablespan)
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
  testthat::expect_no_error(print.Tablespan(
    tablespan(data = summarized_table, formula = cyl ~ mean_hp + sd_hp),
    use_hux = FALSE
  ))

  testthat::expect_no_error(print.Tablespan(
    tablespan(
      data = summarized_table,
      formula = cyl ~ (Horsepower = mean_hp + sd_hp)
    ),
    use_hux = FALSE
  ))

  testthat::expect_no_error(print.Tablespan(
    tablespan(
      data = summarized_table,
      formula = cyl ~ (Horsepower = Mean:mean_hp + SD:sd_hp)
    ),
    use_hux = FALSE
  ))

  testthat::expect_no_error(print.Tablespan(
    tablespan(
      data = summarized_table,
      formula = Cylinder:cyl + Engine:vs ~
        N +
        (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
        (`Weight` = Mean:mean_wt + SD:sd_wt),
      title = "Motor Trend Car Road Tests",
      subtitle = "A table created with tablespan",
      footnote = "Data from the infamous mtcars data set."
    ),
    use_hux = FALSE
  ))

  testthat::expect_no_error(print.Tablespan(
    tablespan(
      data = summarized_table,
      formula = 1 ~ (Horsepower = Mean:mean_hp + SD:sd_hp)
    ),
    use_hux = FALSE
  ))
})

test_that("print factors", {
  library(tibble)
  library(tablespan)

  tibble(
    x = factor(LETTERS[1:5], levels = LETTERS[1:5]),
    y = 1:5,
    z = seq(0, 1, length.out = 5)
  ) |>
    tablespan(x + z ~ y) |>
    tablespan:::print.Tablespan() |>
    testthat::expect_output("| A   0    | 1   |")
})
