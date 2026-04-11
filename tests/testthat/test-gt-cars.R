library(tablespan)
library(testthat)
library(dplyr)

test_tables <- build_tablespan_test_tables()

test_that("cars", {
  gt_tbl <- as_gt(tbl = test_tables$tables$cars)

  expected <- test_tables$data$summarized_table |>
    gt::gt(groupname_col = NULL) |>
    gt::tab_header(
      title = "Motor Trend Car Road Tests",
      subtitle = "A table created with tablespan"
    ) |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_footnote(footnote = "Data from the infamous mtcars data set.") |>
    gt::tab_spanner(
      label = "Horse Power",
      id = "__BASE_LEVEL__Horse Power",
      columns = dplyr::all_of(c("mean_hp", "sd_hp"))
    ) |>
    gt::tab_spanner(
      label = "Weight",
      id = "__BASE_LEVEL__Weight",
      columns = dplyr::all_of(c("mean_wt", "sd_wt"))
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("right"),
        weight = gt::px(1),
        color = "gray"
      ),
      locations = gt::cells_body(columns = all_of(c("vs")))
    ) |>
    gt::cols_label(
      cyl = "Cylinder",
      vs = "Engine",
      mean_hp = "Mean",
      sd_hp = "SD",
      mean_wt = "Mean",
      sd_wt = "SD"
    ) |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "")

  for (i in colnames(test_tables$data$summarized_table)) {
    expected <- expected |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = tablespan:::smart_round(
          x = test_tables$data$summarized_table[[i]]
        )
      )
  }

  compare_html_tables(gt::as_raw_html(gt_tbl), gt::as_raw_html(expected))
})

test_that("cars - no autostyle", {
  gt_tbl <- as_gt(tbl = test_tables$tables$cars, auto_format = FALSE)

  expected <- test_tables$data$summarized_table |>
    gt::gt(groupname_col = NULL) |>
    gt::tab_header(
      title = "Motor Trend Car Road Tests",
      subtitle = "A table created with tablespan"
    ) |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_footnote(footnote = "Data from the infamous mtcars data set.") |>
    gt::tab_spanner(
      label = "Horse Power",
      id = "__BASE_LEVEL__Horse Power",
      columns = dplyr::all_of(c("mean_hp", "sd_hp"))
    ) |>
    gt::tab_spanner(
      label = "Weight",
      id = "__BASE_LEVEL__Weight",
      columns = dplyr::all_of(c("mean_wt", "sd_wt"))
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("right"),
        weight = gt::px(1),
        color = "gray"
      ),
      locations = gt::cells_body(columns = all_of(c("vs")))
    ) |>
    gt::cols_label(
      cyl = "Cylinder",
      vs = "Engine",
      mean_hp = "Mean",
      sd_hp = "SD",
      mean_wt = "Mean",
      sd_wt = "SD"
    )

  for (i in colnames(test_tables$data$summarized_table)) {
    expected <- expected |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = tablespan:::smart_round(
          x = test_tables$data$summarized_table[[i]]
        )
      )
  }

  compare_html_tables(gt::as_raw_html(gt_tbl), gt::as_raw_html(expected))
})

test_that("cars-additional_spanners", {
  gt_tbl <- as_gt(tbl = test_tables$tables$cars_additional_spanners)

  expected <- test_tables$data$summarized_table |>
    gt::gt(groupname_col = NULL) |>
    gt::tab_header(
      title = "Motor Trend Car Road Tests",
      subtitle = "A table created with tablespan"
    ) |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_footnote(footnote = "Data from the infamous mtcars data set.") |>
    gt::tab_spanner(
      label = "Mean",
      id = "__BASE_LEVEL__Results_Horse Power_Mean",
      columns = dplyr::all_of(c("mean_hp"))
    ) |>
    gt::tab_spanner(
      label = "Standard Deviation",
      id = "__BASE_LEVEL__Results_Horse Power_Standard Deviation",
      columns = dplyr::all_of(c("sd_hp"))
    ) |>
    gt::tab_spanner(
      label = "Horse Power",
      id = "__BASE_LEVEL__Results_Horse Power",
      spanners = c(
        "__BASE_LEVEL__Results_Horse Power_Mean",
        "__BASE_LEVEL__Results_Horse Power_Standard Deviation"
      )
    ) |>
    gt::tab_spanner(
      label = "Weight",
      id = "__BASE_LEVEL__Results_Weight",
      columns = dplyr::all_of(c("mean_wt", "sd_wt"))
    ) |>
    gt::tab_spanner(
      label = "Results",
      id = "__BASE_LEVEL__Results",
      columns = dplyr::all_of("N"),
      spanners = c(
        "__BASE_LEVEL__Results_Horse Power",
        "__BASE_LEVEL__Results_Weight"
      )
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("right"),
        weight = gt::px(1),
        color = "gray"
      ),
      locations = gt::cells_body(columns = all_of(c("vs")))
    ) |>
    gt::cols_label(
      cyl = "Cylinder",
      vs = "Engine",
      mean_hp = "Mean",
      sd_hp = "SD",
      mean_wt = "Mean",
      sd_wt = "SD"
    ) |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "")

  for (i in colnames(test_tables$data$summarized_table)) {
    expected <- expected |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = tablespan:::smart_round(
          x = test_tables$data$summarized_table[[i]]
        )
      )
  }

  compare_html_tables(gt::as_raw_html(gt_tbl), gt::as_raw_html(expected))
})

test_that("cars-no_row_names", {
  gt_tbl <- as_gt(tbl = test_tables$tables$cars_no_row_names)

  expected <- test_tables$data$summarized_table |>
    ungroup() |>
    select(-dplyr::all_of(c("cyl", "vs"))) |>
    gt::gt(groupname_col = NULL) |>
    gt::tab_header(
      title = "Motor Trend Car Road Tests",
      subtitle = "A table created with tablespan"
    ) |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_footnote(footnote = "Data from the infamous mtcars data set.") |>
    gt::tab_spanner(
      label = "Mean",
      id = "__BASE_LEVEL__Results_Horse Power_Mean",
      columns = dplyr::all_of(c("mean_hp"))
    ) |>
    gt::tab_spanner(
      label = "Standard Deviation",
      id = "__BASE_LEVEL__Results_Horse Power_Standard Deviation",
      columns = dplyr::all_of(c("sd_hp"))
    ) |>
    gt::tab_spanner(
      label = "Horse Power",
      id = "__BASE_LEVEL__Results_Horse Power",
      spanners = c(
        "__BASE_LEVEL__Results_Horse Power_Mean",
        "__BASE_LEVEL__Results_Horse Power_Standard Deviation"
      )
    ) |>
    gt::tab_spanner(
      label = "Weight",
      id = "__BASE_LEVEL__Results_Weight",
      columns = dplyr::all_of(c("mean_wt", "sd_wt"))
    ) |>
    gt::tab_spanner(
      label = "Results",
      id = "__BASE_LEVEL__Results",
      columns = dplyr::all_of("N"),
      spanners = c(
        "__BASE_LEVEL__Results_Horse Power",
        "__BASE_LEVEL__Results_Weight"
      )
    ) |>
    gt::cols_label(
      mean_hp = "Mean",
      sd_hp = "SD",
      mean_wt = "Mean",
      sd_wt = "SD"
    ) |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "")

  for (i in c("mean_hp", "sd_hp", "mean_wt", "sd_wt")) {
    expected <- expected |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = tablespan:::smart_round(
          x = test_tables$data$summarized_table[[i]]
        )
      )
  }

  compare_html_tables(gt::as_raw_html(gt_tbl), gt::as_raw_html(expected))
})

test_that("cars-no_titles", {
  gt_tbl <- as_gt(tbl = test_tables$tables$cars_no_titles)

  expected <- test_tables$data$summarized_table |>
    gt::gt(groupname_col = NULL) |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_footnote(footnote = "Data from the infamous mtcars data set.") |>
    gt::tab_spanner(
      label = "Horse Power",
      id = "__BASE_LEVEL__Horse Power",
      columns = dplyr::all_of(c("mean_hp", "sd_hp"))
    ) |>
    gt::tab_spanner(
      label = "Weight",
      id = "__BASE_LEVEL__Weight",
      columns = dplyr::all_of(c("mean_wt", "sd_wt"))
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("right"),
        weight = gt::px(1),
        color = "gray"
      ),
      locations = gt::cells_body(columns = all_of(c("vs")))
    ) |>
    gt::cols_label(
      cyl = "Cylinder",
      vs = "Engine",
      mean_hp = "Mean",
      sd_hp = "SD",
      mean_wt = "Mean",
      sd_wt = "SD"
    ) |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "")

  for (i in colnames(expected$`_data`)) {
    expected <- expected |>
      gt::fmt_number(
        columns = dplyr::all_of(i),
        decimals = tablespan:::smart_round(
          x = test_tables$data$summarized_table[[i]]
        )
      )
  }

  compare_html_tables(gt::as_raw_html(gt_tbl), gt::as_raw_html(expected))
})

test_that("cars-no_titles_no_footnotes", {
  gt_tbl <- as_gt(tbl = test_tables$tables$cars_no_titles_no_footnotes)

  expected <- test_tables$data$summarized_table |>
    gt::gt(groupname_col = NULL) |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_spanner(
      label = "Horse Power",
      id = "__BASE_LEVEL__Horse Power",
      columns = dplyr::all_of(c("mean_hp", "sd_hp"))
    ) |>
    gt::tab_spanner(
      label = "Weight",
      id = "__BASE_LEVEL__Weight",
      columns = dplyr::all_of(c("mean_wt", "sd_wt"))
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("right"),
        weight = gt::px(1),
        color = "gray"
      ),
      locations = gt::cells_body(columns = all_of(c("vs")))
    ) |>
    gt::cols_label(
      cyl = "Cylinder",
      vs = "Engine",
      mean_hp = "Mean",
      sd_hp = "SD",
      mean_wt = "Mean",
      sd_wt = "SD"
    ) |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "")

  for (i in colnames(test_tables$data$summarized_table)) {
    expected <- expected |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = tablespan:::smart_round(
          x = test_tables$data$summarized_table[[i]]
        )
      )
  }

  compare_html_tables(gt::as_raw_html(gt_tbl), gt::as_raw_html(expected))
})

test_that("cars-duplicated_spanner_names", {
  gt_tbl <- as_gt(tbl = test_tables$tables$cars_duplicated_spanner_names)

  expected <- test_tables$data$combined_models |>
    select(all_of(c(
      "Parameter",
      "Estimate.x",
      "t value.x",
      "Pr(>|t|).x",
      "Estimate.y",
      "t value.y",
      "Pr(>|t|).y"
    ))) |>
    gt::gt(groupname_col = NULL) |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_spanner(
      label = "Significance",
      id = "__BASE_LEVEL__Model 1_Significance",
      columns = dplyr::all_of(c("t value.x", "Pr(>|t|).x"))
    ) |>
    gt::tab_spanner(
      label = "Significance",
      id = "__BASE_LEVEL__Model 2_Significance",
      columns = dplyr::all_of(c("t value.y", "Pr(>|t|).y"))
    ) |>
    gt::tab_spanner(
      label = "Model 1",
      id = "__BASE_LEVEL__Model 1",
      columns = dplyr::all_of(c("Estimate.x")),
      spanners = "__BASE_LEVEL__Model 1_Significance"
    ) |>
    gt::tab_spanner(
      label = "Model 2",
      id = "__BASE_LEVEL__Model 2",
      columns = dplyr::all_of(c("Estimate.y")),
      spanners = "__BASE_LEVEL__Model 2_Significance"
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("right"),
        weight = gt::px(1),
        color = "gray"
      ),
      locations = gt::cells_body(columns = all_of(c("Parameter")))
    ) |>
    gt::cols_label(
      Estimate.x = "Estimate",
      `t value.x` = "t-value",
      `Pr(>|t|).x` = "p-value",
      Estimate.y = "Estimate",
      `t value.y` = "t-value",
      `Pr(>|t|).y` = "p-value",
    ) |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "")

  for (i in c(
    "Estimate.x",
    "t value.x",
    "Pr(>|t|).x",
    "Estimate.y",
    "t value.y",
    "Pr(>|t|).y"
  )) {
    expected <- expected |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = tablespan:::smart_round(
          x = test_tables$data$combined_models[[i]]
        )
      )
  }

  compare_html_tables(gt::as_raw_html(gt_tbl), gt::as_raw_html(expected))
})


test_that("cars - gt styling", {
  gt_tbl_base <- as_gt(tbl = test_tables$tables$cars)

  expected_base <- test_tables$data$summarized_table |>
    gt::gt(groupname_col = NULL) |>
    gt::tab_header(
      title = "Motor Trend Car Road Tests",
      subtitle = "A table created with tablespan"
    ) |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_footnote(footnote = "Data from the infamous mtcars data set.") |>
    gt::tab_spanner(
      label = "Horse Power",
      id = "__BASE_LEVEL__Horse Power",
      columns = dplyr::all_of(c("mean_hp", "sd_hp"))
    ) |>
    gt::tab_spanner(
      label = "Weight",
      id = "__BASE_LEVEL__Weight",
      columns = dplyr::all_of(c("mean_wt", "sd_wt"))
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("right"),
        weight = gt::px(1),
        color = "gray"
      ),
      locations = gt::cells_body(columns = all_of(c("vs")))
    ) |>
    gt::cols_label(
      cyl = "Cylinder",
      vs = "Engine",
      mean_hp = "Mean",
      sd_hp = "SD",
      mean_wt = "Mean",
      sd_wt = "SD"
    ) |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "")

  for (i in colnames(test_tables$data$summarized_table)) {
    expected_base <- expected_base |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = tablespan:::smart_round(
          x = test_tables$data$summarized_table[[i]]
        )
      )
  }
  compare_html_tables(
    gt::as_raw_html(gt_tbl_base),
    gt::as_raw_html(expected_base)
  )

  # title
  compare_html_tables(
    test_tables$tables$cars_title_style_black_red |>
      as_gt() |>
      gt::as_raw_html(),
    expected_base |>
      gt::tab_style(
        style = gt::cell_text(color = "#000000"),
        locations = gt::cells_title("title")
      ) |>
      gt::tab_style(
        style = gt::cell_fill(color = "#983439"),
        locations = gt::cells_title("title")
      ) |>
      gt::as_raw_html()
  )

  compare_html_tables(
    test_tables$tables$cars_title_style_white_red |>
      as_gt() |>
      gt::as_raw_html(),
    expected_base |>
      gt::tab_style(
        style = gt::cell_text(color = "#ffffff"),
        locations = gt::cells_title("title")
      ) |>
      gt::tab_style(
        style = gt::cell_fill(color = "#983439"),
        locations = gt::cells_title("title")
      ) |>
      gt::as_raw_html()
  )

  compare_html_tables(
    test_tables$tables$cars_title_style_bold_italic |>
      as_gt() |>
      gt::as_raw_html(),
    expected_base |>
      gt::tab_style(
        style = gt::cell_text(
          color = "#ffffff",
          weight = "bold",
          style = "italic"
        ),
        locations = gt::cells_title("title")
      ) |>
      gt::tab_style(
        style = gt::cell_fill(color = "#983439"),
        locations = gt::cells_title("title")
      ) |>
      gt::as_raw_html()
  )

  compare_html_tables(
    test_tables$tables$cars_title_subtitle_style_bold_italic |>
      as_gt() |>
      gt::as_raw_html(),
    expected_base |>
      gt::tab_style(
        style = gt::cell_text(
          color = "#ffffff",
          weight = "bold",
          style = "italic"
        ),
        locations = gt::cells_title("title")
      ) |>
      gt::tab_style(
        style = gt::cell_fill(color = "#983439"),
        locations = gt::cells_title("title")
      ) |>
      gt::tab_style(
        style = gt::cell_text(
          color = "#ffffff",
          weight = "bold",
          style = "italic"
        ),
        locations = gt::cells_title("subtitle")
      ) |>
      gt::tab_style(
        style = gt::cell_fill(color = "#983439"),
        locations = gt::cells_title("subtitle")
      ) |>
      gt::as_raw_html()
  )

  compare_html_tables(
    test_tables$tables$cars_title_subtitle_footnote_size |>
      as_gt() |>
      gt::as_raw_html(),
    expected_base |>
      gt::tab_style(
        style = gt::cell_text(
          color = "#ffffff",
          weight = "bold",
          style = "italic"
        ),
        locations = gt::cells_title("title")
      ) |>
      gt::tab_style(
        style = gt::cell_fill(color = "#983439"),
        locations = gt::cells_title("title")
      ) |>
      gt::tab_style(
        style = gt::cell_text(
          color = "#ffffff",
          weight = "bold",
          style = "italic"
        ),
        locations = gt::cells_title("subtitle")
      ) |>
      gt::tab_style(
        style = gt::cell_fill(color = "#983439"),
        locations = gt::cells_title("subtitle")
      ) |>
      gt::tab_style(
        style = gt::cell_text(size = gt::px(1.3333343412075 * 8)),
        locations = gt::cells_footnotes()
      ) |>
      gt::as_raw_html()
  )

  compare_html_tables(
    test_tables$tables$cars_title_subtitle_footnote_header_style |>
      as_gt() |>
      gt::as_raw_html(),
    expected_base |>
      gt::tab_style(
        style = gt::cell_text(
          color = "#ffffff",
          weight = "bold",
          style = "italic"
        ),
        locations = gt::cells_title("title")
      ) |>
      gt::tab_style(
        style = gt::cell_fill(color = "#983439"),
        locations = gt::cells_title("title")
      ) |>
      gt::tab_style(
        style = gt::cell_text(
          color = "#ffffff",
          weight = "bold",
          style = "italic"
        ),
        locations = gt::cells_title("subtitle")
      ) |>
      gt::tab_style(
        style = gt::cell_fill(color = "#983439"),
        locations = gt::cells_title("subtitle")
      ) |>
      gt::tab_style(
        style = gt::cell_text(size = gt::px(1.3333343412075 * 8)),
        locations = gt::cells_footnotes()
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "bold"),
          gt::cell_fill(color = "#B65455")
        ),
        locations = gt::cells_column_labels()
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "bold"),
          gt::cell_fill(color = "#B65455")
        ),
        locations = gt::cells_column_spanners()
      ) |>
      gt::as_raw_html()
  )

  compare_html_tables(
    test_tables$tables$cars_tsf_hf_cell_style |>
      as_gt() |>
      gt::as_raw_html(),
    expected_base |>
      gt::fmt_number(
        columns = dplyr::where(is.double),
        rows = 2:3,
        decimals = 1
      ) |>
      gt::tab_style(
        style = gt::cell_text(
          color = "#ffffff",
          weight = "bold",
          style = "italic"
        ),
        locations = gt::cells_title("title")
      ) |>
      gt::tab_style(
        style = gt::cell_fill(color = "#983439"),
        locations = gt::cells_title("title")
      ) |>
      gt::tab_style(
        style = gt::cell_text(
          color = "#ffffff",
          weight = "bold",
          style = "italic"
        ),
        locations = gt::cells_title("subtitle")
      ) |>
      gt::tab_style(
        style = gt::cell_fill(color = "#983439"),
        locations = gt::cells_title("subtitle")
      ) |>
      gt::tab_style(
        style = gt::cell_text(style = "italic"),
        locations = gt::cells_footnotes()
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "bold"),
          gt::cell_fill(color = "#B65455")
        ),
        locations = gt::cells_column_labels()
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "bold"),
          gt::cell_fill(color = "#B65455")
        ),
        locations = gt::cells_column_spanners()
      ) |>
      gt::tab_style(
        style = list(gt::cell_text(
          color = "#B54321",
          style = "italic"
        )),
        locations = gt::cells_body(
          columns = dplyr::where(is.double),
          rows = 2:3
        )
      ) |>
      gt::as_raw_html()
  )

  testthat::expect_no_error(
    test_tables$tables$cars_tsf_hf_cs2_cell_style |>
      as_gt()
  )

  testthat::expect_no_error(
    test_tables$tables$cars_tsf_hf_cs3_cell_style |>
      as_gt()
  )
})

test_that("date_text_base", {
  gt_tbl <- as_gt(tbl = test_tables$tables$date_text_base)

  expected <- test_tables$data$mixed_date_text_data |>
    gt::gt(groupname_col = NULL) |>
    gt::tab_header(
      title = "Date/Text Formatting Example",
      subtitle = "Fixture for format_date and format_text"
    ) |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_footnote(footnote = "Synthetic data for tests") |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("right"),
        weight = gt::px(1),
        color = "gray"
      ),
      locations = gt::cells_body(columns = all_of(c("group")))
    ) |>
    gt::cols_label(
      group = "Group",
      event_date = "Date",
      comment = "Comment",
      amount = "Amount"
    ) |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "")

  for (i in c("amount")) {
    expected <- expected |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = tablespan:::smart_round(
          x = test_tables$data$mixed_date_text_data[[i]]
        )
      )
  }

  compare_html_tables(gt::as_raw_html(gt_tbl), gt::as_raw_html(expected))
})

test_that("date_text_formatted", {
  testthat::expect_warning(
    gt_tbl <- as_gt(tbl = test_tables$tables$date_text_formatted)
  )

  expected <- test_tables$data$mixed_date_text_data |>
    gt::gt(groupname_col = NULL) |>
    gt::tab_header(
      title = "Date/Text Formatting Example",
      subtitle = "Fixture for format_date and format_text"
    ) |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_footnote(footnote = "Synthetic data for tests") |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("right"),
        weight = gt::px(1),
        color = "gray"
      ),
      locations = gt::cells_body(columns = all_of(c("group")))
    ) |>
    gt::cols_label(
      group = "Group",
      event_date = "Date",
      comment = "Comment",
      amount = "Amount"
    ) |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "") |>
    gt::fmt_date(
      columns = all_of("event_date"),
      date_style = "iso"
    ) |>
    gt::fmt_auto(columns = all_of("comment")) |>
    gt::fmt_number(
      columns = all_of("amount"),
      decimals = 1
    )

  for (i in c("amount")) {
    expected <- expected |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = 1
      )
  }

  compare_html_tables(gt::as_raw_html(gt_tbl), gt::as_raw_html(expected))
})

test_that("cars_header_cells_styled", {
  gt_tbl <- as_gt(tbl = test_tables$tables$cars_header_cells_styled)

  expected <- test_tables$data$summarized_table |>
    gt::gt(groupname_col = NULL) |>
    gt::tab_header(
      title = "Motor Trend Car Road Tests",
      subtitle = "A table created with tablespan"
    ) |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_footnote(footnote = "Data from the infamous mtcars data set.") |>
    gt::tab_spanner(
      label = "Horse Power",
      id = "__BASE_LEVEL__Horse Power",
      columns = dplyr::all_of(c("mean_hp", "sd_hp"))
    ) |>
    gt::tab_spanner(
      label = "Weight",
      id = "__BASE_LEVEL__Weight",
      columns = dplyr::all_of(c("mean_wt", "sd_wt"))
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("right"),
        weight = gt::px(1),
        color = "gray"
      ),
      locations = gt::cells_body(columns = all_of(c("vs")))
    ) |>
    gt::cols_label(
      cyl = "Cylinder",
      vs = "Engine",
      mean_hp = "Mean",
      sd_hp = "SD",
      mean_wt = "Mean",
      sd_wt = "SD"
    ) |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "")

  for (i in colnames(test_tables$data$summarized_table)) {
    expected <- expected |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = tablespan:::smart_round(
          x = test_tables$data$summarized_table[[i]]
        )
      )
  }

  compare_html_tables(gt::as_raw_html(gt_tbl), gt::as_raw_html(expected))
})

test_that("cars_hline_styled", {
  gt_tbl <- as_gt(tbl = test_tables$tables$cars_hline_styled)

  expected <- test_tables$data$summarized_table |>
    gt::gt(groupname_col = NULL) |>
    gt::tab_header(
      title = "Motor Trend Car Road Tests",
      subtitle = "A table created with tablespan"
    ) |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_footnote(footnote = "Data from the infamous mtcars data set.") |>
    gt::tab_spanner(
      label = "Horse Power",
      id = "__BASE_LEVEL__Horse Power",
      columns = dplyr::all_of(c("mean_hp", "sd_hp"))
    ) |>
    gt::tab_spanner(
      label = "Weight",
      id = "__BASE_LEVEL__Weight",
      columns = dplyr::all_of(c("mean_wt", "sd_wt"))
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("right"),
        weight = gt::px(1),
        color = "gray"
      ),
      locations = gt::cells_body(columns = all_of(c("vs")))
    ) |>
    gt::cols_label(
      cyl = "Cylinder",
      vs = "Engine",
      mean_hp = "Mean",
      sd_hp = "SD",
      mean_wt = "Mean",
      sd_wt = "SD"
    ) |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "")

  for (i in colnames(test_tables$data$summarized_table)) {
    expected <- expected |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = tablespan:::smart_round(
          x = test_tables$data$summarized_table[[i]]
        )
      )
  }

  compare_html_tables(gt::as_raw_html(gt_tbl), gt::as_raw_html(expected))
})

test_that("cars_vline_styled", {
  gt_tbl <- as_gt(tbl = test_tables$tables$cars_vline_styled)

  expected <- test_tables$data$summarized_table |>
    gt::gt(groupname_col = NULL) |>
    gt::tab_header(
      title = "Motor Trend Car Road Tests",
      subtitle = "A table created with tablespan"
    ) |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_footnote(footnote = "Data from the infamous mtcars data set.") |>
    gt::tab_spanner(
      label = "Horse Power",
      id = "__BASE_LEVEL__Horse Power",
      columns = dplyr::all_of(c("mean_hp", "sd_hp"))
    ) |>
    gt::tab_spanner(
      label = "Weight",
      id = "__BASE_LEVEL__Weight",
      columns = dplyr::all_of(c("mean_wt", "sd_wt"))
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("right"),
        weight = gt::px(1),
        color = "gray"
      ),
      locations = gt::cells_body(columns = all_of(c("vs")))
    ) |>
    gt::cols_label(
      cyl = "Cylinder",
      vs = "Engine",
      mean_hp = "Mean",
      sd_hp = "SD",
      mean_wt = "Mean",
      sd_wt = "SD"
    ) |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "")

  for (i in colnames(test_tables$data$summarized_table)) {
    expected <- expected |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = tablespan:::smart_round(
          x = test_tables$data$summarized_table[[i]]
        )
      )
  }

  compare_html_tables(gt::as_raw_html(gt_tbl), gt::as_raw_html(expected))
})

test_that("cars_header_cells_hline_vline_styled", {
  gt_tbl <- as_gt(tbl = test_tables$tables$cars_header_cells_hline_vline_styled)

  expected <- test_tables$data$summarized_table |>
    gt::gt(groupname_col = NULL) |>
    gt::tab_header(
      title = "Motor Trend Car Road Tests",
      subtitle = "A table created with tablespan"
    ) |>
    gt::opt_align_table_header(align = c("left")) |>
    gt::tab_footnote(footnote = "Data from the infamous mtcars data set.") |>
    gt::tab_spanner(
      label = "Horse Power",
      id = "__BASE_LEVEL__Horse Power",
      columns = dplyr::all_of(c("mean_hp", "sd_hp"))
    ) |>
    gt::tab_spanner(
      label = "Weight",
      id = "__BASE_LEVEL__Weight",
      columns = dplyr::all_of(c("mean_wt", "sd_wt"))
    ) |>
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("right"),
        weight = gt::px(1),
        color = "gray"
      ),
      locations = gt::cells_body(columns = all_of(c("vs")))
    ) |>
    gt::cols_label(
      cyl = "Cylinder",
      vs = "Engine",
      mean_hp = "Mean",
      sd_hp = "SD",
      mean_wt = "Mean",
      sd_wt = "SD"
    ) |>
    gt::fmt_auto() |>
    gt::sub_missing(missing_text = "")

  for (i in colnames(test_tables$data$summarized_table)) {
    expected <- expected |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = tablespan:::smart_round(
          x = test_tables$data$summarized_table[[i]]
        )
      )
  }

  compare_html_tables(gt::as_raw_html(gt_tbl), gt::as_raw_html(expected))
})
