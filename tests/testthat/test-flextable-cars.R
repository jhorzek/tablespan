library(testthat)
library(dplyr)
library(tablespan)
library(flextable)

compare_flextables <- function(ft1, ft2) {
  expect_equal(unname(ft1$col_keys), unname(ft2$col_keys))
  expect_equal(unname(ft1$header$dataset), unname(ft2$header$dataset))
  expect_equal(unname(ft1$body$dataset), unname(ft2$body$dataset))
  expect_equal(unname(ft1$footer$dataset), unname(ft2$footer$dataset))

  expect_equal(unname(ft1$header$styles), unname(ft2$header$styles))
  expect_equal(unname(ft1$body$styles), unname(ft2$body$styles))
  expect_equal(unname(ft1$footer$styles), unname(ft2$footer$styles))
}

summarized_table <- mtcars |>
  group_by(cyl, vs) |>
  summarise(
    N = n(),
    mean_hp = mean(hp),
    sd_hp = sd(hp),
    mean_wt = mean(wt),
    sd_wt = sd(wt),
    .groups = "drop"
  )

test_that("cars", {
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

  ft_tbl <- as_flextable(tbl)

  ref_table <- data.frame(
    key = c("cyl", "vs", "N", "mean_hp", "sd_hp", "mean_wt", "sd_wt"),
    label = c("Cylinder", "Engine", "N", "Mean", "SD", "Mean", "SD")
  )

  expected <- summarized_table |>
    flextable::flextable() |>
    flextable::set_header_df(mapping = ref_table, key = "key") |>
    add_header_row(
      values = c("", "", "", "Horse Power", "Weight"),
      colwidths = c(1, 1, 1, 2, 2)
    ) |>
    flextable::theme_booktabs() |>
    add_header_lines(
      values = c("Motor Trend Car Road Tests", "A table created with tablespan")
    ) |>
    flextable::align(align = "left", part = "header") |>
    flextable::add_footer_lines(
      values = "Data from the infamous mtcars data set."
    ) |>
    flextable::align(align = "left", part = "footer") |>
    autofit()

  for (nm in names(summarized_table)) {
    if (is.numeric(summarized_table[[nm]])) {
      expected <- expected |>
        colformat_double(
          j = nm,
          digits = tablespan:::smart_round(summarized_table[[nm]])
        )
    }
  }

  compare_flextables(ft_tbl, expected)
})

test_that("cars-additional_spanners", {
  tbl <- tablespan(
    data = summarized_table,
    formula = Cylinder:cyl + Engine:vs ~
      (Results = N +
        (`Horse Power` =
          (Mean = Mean:mean_hp) +
          (`Standard Deviation` = SD:sd_hp)) +
        (`Weight` = Mean:mean_wt + SD:sd_wt)),
    title = "Motor Trend Car Road Tests",
    subtitle = "A table created with tablespan",
    footnote = "Data from the infamous mtcars data set."
  )

  ft_tbl <- as_flextable(tbl)

  ref_table <- data.frame(
    key = c("cyl", "vs", "N", "mean_hp", "sd_hp", "mean_wt", "sd_wt"),
    label = c("Cylinder", "Engine", "N", "Mean", "SD", "Mean", "SD")
  )

  expected <- summarized_table |>
    flextable::flextable() |>
    flextable::set_header_df(mapping = ref_table, key = "key") |>
    add_header_row(
      values = c("", "", "", "Mean", "Standard Deviation", "Weight"),
      colwidths = c(1, 1, 1, 1, 1, 2)
    ) |>
    add_header_row(
      values = c("", "", "", "Horse Power", "", ""),
      colwidths = c(1, 1, 1, 2, 1, 1)
    ) |>
    add_header_row(
      values = c("", "", "Results"),
      colwidths = c(1, 1, 5)
    ) |>
    flextable::theme_booktabs() |>
    add_header_lines(
      values = c("Motor Trend Car Road Tests", "A table created with tablespan")
    ) |>
    flextable::align(align = "left", part = "header") |>
    flextable::add_footer_lines(
      values = "Data from the infamous mtcars data set."
    ) |>
    flextable::align(align = "left", part = "footer") |>
    autofit()

  for (nm in names(summarized_table)) {
    if (is.numeric(summarized_table[[nm]])) {
      expected <- expected |>
        colformat_double(
          j = nm,
          digits = tablespan:::smart_round(summarized_table[[nm]])
        )
    }
  }

  compare_flextables(ft_tbl, expected)
})

test_that("cars-no_row_names", {
  tbl <- tablespan(
    data = summarized_table,
    formula = 1 ~
      (Results = N +
        (`Horse Power` =
          (Mean = Mean:mean_hp) +
          (`Standard Deviation` = SD:sd_hp)) +
        (`Weight` = Mean:mean_wt + SD:sd_wt)),
    title = "Motor Trend Car Road Tests",
    subtitle = "A table created with tablespan",
    footnote = "Data from the infamous mtcars data set."
  )

  ft_tbl <- as_flextable(tbl)

  ref_table <- data.frame(
    key = c("N", "mean_hp", "sd_hp", "mean_wt", "sd_wt"),
    label = c("N", "Mean", "SD", "Mean", "SD")
  )

  summarized_sub <- summarized_table |>
    select(all_of(c("N", "mean_hp", "sd_hp", "mean_wt", "sd_wt")))

  expected <- summarized_sub |>
    flextable::flextable() |>
    flextable::set_header_df(mapping = ref_table, key = "key") |>
    add_header_row(
      values = c("", "Mean", "Standard Deviation", "Weight"),
      colwidths = c(1, 1, 1, 2)
    ) |>
    add_header_row(
      values = c("", "Horse Power", "", ""),
      colwidths = c(1, 2, 1, 1)
    ) |>
    add_header_row(
      values = c("Results"),
      colwidths = c(5)
    ) |>
    flextable::theme_booktabs() |>
    add_header_lines(
      values = c("Motor Trend Car Road Tests", "A table created with tablespan")
    ) |>
    flextable::align(align = "left", part = "header") |>
    flextable::add_footer_lines(
      values = "Data from the infamous mtcars data set."
    ) |>
    flextable::align(align = "left", part = "footer") |>
    autofit()

  for (nm in names(summarized_sub)) {
    if (is.numeric(summarized_sub[[nm]])) {
      expected <- expected |>
        colformat_double(
          j = nm,
          digits = tablespan:::smart_round(summarized_sub[[nm]])
        )
    }
  }

  compare_flextables(ft_tbl, expected)
})

test_that("cars-no_titles", {
  tbl <- tablespan(
    data = summarized_table,
    formula = Cylinder:cyl + Engine:vs ~
      N +
      (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
      (`Weight` = Mean:mean_wt + SD:sd_wt),
    footnote = "Data from the infamous mtcars data set."
  )

  ft_tbl <- as_flextable(tbl)

  ref_table <- data.frame(
    key = c("cyl", "vs", "N", "mean_hp", "sd_hp", "mean_wt", "sd_wt"),
    label = c("Cylinder", "Engine", "N", "Mean", "SD", "Mean", "SD")
  )

  expected <- summarized_table |>
    flextable::flextable() |>
    flextable::set_header_df(mapping = ref_table, key = "key") |>
    add_header_row(
      values = c("", "", "", "Horse Power", "Weight"),
      colwidths = c(1, 1, 1, 2, 2)
    ) |>
    flextable::theme_booktabs() |>
    flextable::align(align = "left", part = "header") |>
    flextable::add_footer_lines(
      values = "Data from the infamous mtcars data set."
    ) |>
    flextable::align(align = "left", part = "footer") |>
    autofit()

  for (nm in names(summarized_table)) {
    if (is.numeric(summarized_table[[nm]])) {
      expected <- expected |>
        colformat_double(
          j = nm,
          digits = tablespan:::smart_round(summarized_table[[nm]])
        )
    }
  }

  compare_flextables(ft_tbl, expected)
})

test_that("cars-no_titles_no_footnotes", {
  tbl <- tablespan(
    data = summarized_table,
    formula = Cylinder:cyl + Engine:vs ~
      N +
      (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
      (`Weight` = Mean:mean_wt + SD:sd_wt)
  )

  ft_tbl <- as_flextable(tbl)

  ref_table <- data.frame(
    key = c("cyl", "vs", "N", "mean_hp", "sd_hp", "mean_wt", "sd_wt"),
    label = c("Cylinder", "Engine", "N", "Mean", "SD", "Mean", "SD")
  )

  expected <- summarized_table |>
    flextable::flextable() |>
    flextable::set_header_df(mapping = ref_table, key = "key") |>
    add_header_row(
      values = c("", "", "", "Horse Power", "Weight"),
      colwidths = c(1, 1, 1, 2, 2)
    ) |>
    flextable::theme_booktabs() |>
    flextable::align(align = "left", part = "header") |>
    flextable::align(align = "left", part = "footer") |>
    autofit()

  for (nm in names(summarized_table)) {
    if (is.numeric(summarized_table[[nm]])) {
      expected <- expected |>
        colformat_double(
          j = nm,
          digits = tablespan:::smart_round(summarized_table[[nm]])
        )
    }
  }

  compare_flextables(ft_tbl, expected)
})

test_that("cars-duplicated_spanner_names", {
  model_1 <- summary(lm(mpg ~ wt + qsec, mtcars))$coefficients |>
    as.data.frame()
  model_2 <- summary(lm(mpg ~ wt + qsec, mtcars))$coefficients |>
    as.data.frame()

  model_1$Parameter <- rownames(model_1)
  model_2$Parameter <- rownames(model_2)

  combined_models <- full_join(model_1, model_2, by = "Parameter")

  tbl <- combined_models |>
    as_tibble() |>
    tablespan(
      formula = Parameter ~
        (`Model 1` = Estimate:Estimate.x +
          (Significance = `t-value`:`t value.x` + `p-value`:`Pr(>|t|).x`)) +
        (`Model 2` = Estimate:Estimate.y +
          (Significance = `t-value`:`t value.y` + `p-value`:`Pr(>|t|).y`))
    )

  testthat::expect_no_error(as_flextable(tbl))
})

test_that("cars - flextable styling", {
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

  ft_tbl <- as_flextable(tbl)

  ref_table <- data.frame(
    key = c("cyl", "vs", "N", "mean_hp", "sd_hp", "mean_wt", "sd_wt"),
    label = c("Cylinder", "Engine", "N", "Mean", "SD", "Mean", "SD")
  )

  expected_base <- summarized_table |>
    flextable::flextable() |>
    flextable::set_header_df(mapping = ref_table, key = "key") |>
    add_header_row(
      values = c("", "", "", "Horse Power", "Weight"),
      colwidths = c(1, 1, 1, 2, 2)
    ) |>
    flextable::theme_booktabs() |>
    add_header_lines(
      values = c("Motor Trend Car Road Tests", "A table created with tablespan")
    ) |>
    flextable::align(align = "left", part = "header") |>
    flextable::add_footer_lines(
      values = "Data from the infamous mtcars data set."
    ) |>
    flextable::align(align = "left", part = "footer") |>
    autofit()

  for (nm in names(summarized_table)) {
    if (is.numeric(summarized_table[[nm]])) {
      expected_base <- expected_base |>
        colformat_double(
          j = nm,
          digits = tablespan:::smart_round(summarized_table[[nm]])
        )
    }
  }

  compare_flextables(ft_tbl, expected_base)

  # title
  compare_flextables(
    tbl |>
      style_title(text_color = "#000000", background_color = "#983439") |>
      as_flextable(),
    expected_base |>
      flextable::bg(i = 1, bg = "#983439", part = "header") |>
      flextable::color(i = 1, color = "#000000", part = "header")
  )

  compare_flextables(
    tbl |>
      style_title(background_color = "#983439", text_color = "#ffffff") |>
      as_flextable(),
    expected_base |>
      flextable::bg(i = 1, bg = "#983439", part = "header") |>
      flextable::color(i = 1, color = "#ffffff", part = "header")
  )

  compare_flextables(
    tbl |>
      style_title(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      style_subtitle(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      style_header(background_color = "#B65455", bold = TRUE) |>
      as_flextable(),
    expected_base |>
      flextable::bg(i = 1:2, bg = "#983439", part = "header") |>
      flextable::color(i = 1:2, color = "#ffffff", part = "header") |>
      flextable::bold(i = 1:2, part = "header") |>
      flextable::italic(i = 1:2, part = "header") |>
      flextable::bg(i = 3:4, bg = "#B65455", part = "header") |>
      flextable::bold(i = 3:4, part = "header")
  )

  compare_flextables(
    tbl |>
      style_title(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      style_subtitle(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      style_header(background_color = "#B65455", bold = TRUE) |>
      style_column(
        columns = dplyr::where(is.double),
        rows = 2:3,
        italic = TRUE,
        text_color = "#B54321"
      ) |>
      as_flextable(),
    expected_base |>
      flextable::bg(i = 1:2, bg = "#983439", part = "header") |>
      flextable::color(i = 1:2, color = "#ffffff", part = "header") |>
      flextable::bold(i = 1:2, part = "header") |>
      flextable::italic(i = 1:2, part = "header") |>
      flextable::bg(i = 3:4, bg = "#B65455", part = "header") |>
      flextable::bold(i = 3:4, part = "header") |>
      flextable::color(
        i = 2:3,
        j = c(1:2, 4:7),
        color = "#B54321",
        part = "body"
      ) |>
      flextable::italic(i = 2:3, j = c(1:2, 4:7), part = "body")
  )

  color_scale = c(
    "#123456" = min(
      summarized_table |> select(where(is.double)),
      na.rm = TRUE
    ),
    "#B46983" = max(
      summarized_table |> select(where(is.double)),
      na.rm = TRUE
    )
  )

  testthat::expect_no_error(
    tbl |>
      style_title(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      style_subtitle(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      style_header(background_color = "#B65455", bold = TRUE) |>
      style_column(
        columns = dplyr::where(is.double),
        rows = 2:3,
        italic = TRUE,
        text_color = "#B54321"
      ) |>
      style_column(
        columns = dplyr::where(is.double),
        color_scale = color_scale
      ) |>
      as_flextable()
  )
})
