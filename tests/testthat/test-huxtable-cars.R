test_that("cars", {
  library(tablespan)
  library(huxtable)
  library(testthat)
  library(dplyr)

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

  hux_tbl <- as_huxtable(x = tbl) |>
    huxtable::as_html()

  expected <-
    '<table class="huxtable" data-quarto-disable-processing="true"  style="margin-left: auto; margin-right: auto;">
<col><col><col><col><col><col><col><tbody>
<tr>
<td class="huxtable-cell" colspan="7" style="border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;">Motor Trend Car Road Tests</td></tr>
<tr>
<td class="huxtable-cell" colspan="7" style="border-style: solid solid solid solid; border-width: 0pt 0pt 0.8pt 0pt;">A table created with tablespan</td></tr>
<tr>
<td class="huxtable-cell" style="border-style: solid solid solid solid; border-width: 0.8pt 0pt 0.4pt 0.4pt;"></td><td class="huxtable-cell" style="border-style: solid solid solid solid; border-width: 0.8pt 0.4pt 0.4pt 0pt;"></td><td class="huxtable-cell" style="border-style: solid solid solid solid; border-width: 0.8pt 0.4pt 0.4pt 0.4pt;"></td><td class="huxtable-cell" colspan="2" style="border-style: solid solid solid solid; border-width: 0.8pt 0.4pt 0.4pt 0.4pt;">Horse Power</td><td class="huxtable-cell" colspan="2" style="border-style: solid solid solid solid; border-width: 0.8pt 0.4pt 0.4pt 0.4pt;">Weight</td></tr>
<tr>
<td class="huxtable-cell" style="border-style: solid solid solid solid; border-width: 0.4pt 0.4pt 0.4pt 0.4pt;">Cylinder</td><td class="huxtable-cell" style="border-style: solid solid solid solid; border-width: 0.4pt 0.4pt 0.4pt 0.4pt;">Engine</td><td class="huxtable-cell" style="border-style: solid solid solid solid; border-width: 0.4pt 0.4pt 0.4pt 0.4pt;">N</td><td class="huxtable-cell" style="border-style: solid solid solid solid; border-width: 0.4pt 0.4pt 0.4pt 0.4pt;">Mean</td><td class="huxtable-cell" style="border-style: solid solid solid solid; border-width: 0.4pt 0.4pt 0.4pt 0.4pt;">SD</td><td class="huxtable-cell" style="border-style: solid solid solid solid; border-width: 0.4pt 0.4pt 0.4pt 0.4pt;">Mean</td><td class="huxtable-cell" style="border-style: solid solid solid solid; border-width: 0.4pt 0.4pt 0.4pt 0.4pt;">SD</td></tr>
<tr>
<td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0.4pt 0pt 0pt 0.4pt;">4</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0.4pt 0.4pt 0pt 0pt;">0</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0.4pt 0pt 0pt 0.4pt;">1</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0.4pt 0pt 0pt 0pt;">91.00</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0.4pt 0pt 0pt 0pt;">&nbsp;&nbsp;&nbsp;&nbsp;</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0.4pt 0pt 0pt 0pt;">2.1400</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0.4pt 0.4pt 0pt 0pt;">&nbsp;&nbsp;&nbsp;</td></tr>
<tr>
<td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0.4pt;">4</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0.4pt 0pt 0pt;">1</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0.4pt;">10</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;">81.80</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;">21.872</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;">2.3003</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0.4pt 0pt 0pt;">0.60</td></tr>
<tr>
<td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0.4pt;">6</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0.4pt 0pt 0pt;">0</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0.4pt;">3</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;">131.67</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;">37.528</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;">2.7550</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0.4pt 0pt 0pt;">0.13</td></tr>
<tr>
<td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0.4pt;">6</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0.4pt 0pt 0pt;">1</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0.4pt;">4</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;">115.25</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;">9.179</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0pt 0pt;">3.3887</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0.4pt 0pt 0pt;">0.12</td></tr>
<tr>
<td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0.8pt 0.4pt;">8</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0.4pt 0.8pt 0pt;">0</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0.8pt 0.4pt;">14</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0.8pt 0pt;">209.21</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0.8pt 0pt;">50.977</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0pt 0.8pt 0pt;">3.9992</td><td class="huxtable-cell" style="text-align: right;  border-style: solid solid solid solid; border-width: 0pt 0.4pt 0.8pt 0pt;">0.76</td></tr>
<tr>
<td class="huxtable-cell" colspan="7" style="border-style: solid solid solid solid; border-width: 0.8pt 0pt 0pt 0pt;">Data from the infamous mtcars data set.</td></tr>
</tbody>
</table>
'

  testthat::expect_identical(
    object = huxtable::as_html(hux_tbl),
    expected = expected
  )

  expected <- summarized_table |>
    huxtable::as_huxtable(add_colnames = FALSE, add_rownames = FALSE) |>
    huxtable::insert_row(
      c("Cylinder", "Engine", "N", "Mean", "SD", "Mean", "SD"),
      after = 0
    ) |>
    huxtable::insert_row(
      c("", "", "", "Horse Power", "", "Weight", ""),
      after = 0
    ) |>
    huxtable::merge_cells(row = 1, col = 4:5) |>
    huxtable::merge_cells(row = 1, col = 6:7) |>
    tablespan:::hux_add_merged_row(text = "A table created with tablespan") |>
    tablespan:::hux_add_merged_row(
      text = "Motor Trend Car Road Tests",
      border = NULL
    ) |>
    huxtable::add_footnote(text = "Data from the infamous mtcars data set.") |>
    huxtable::set_all_borders(row = 3:4, col = 1:7) |>
    huxtable::set_right_border(col = 2) |>
    huxtable::set_bottom_border(col = 1:7, row = 9) |>
    huxtable::set_left_border(row = 3:9, col = 1) |>
    huxtable::set_right_border(row = 3:9, col = 7)

  # Remove vertical borders between empty neighbor cells
  for (row in 3:4) {
    for (col in 1:ncol(expected)) {
      if (col == 1) {
        next
      }
      left_empty <- expected[row, col - 1] %in% c("", NA)
      right_empty <- expected[row, col] %in% c("", NA)
      if (left_empty & right_empty) {
        huxtable::right_border(expected)[row, col - 1] <- 0
        huxtable::left_border(expected)[row, col] <- 0
      }
    }
  }

  # Remove horizontal borders between empty neighbor cells
  for (col in 3:4) {
    for (row in 1:nrow(expected)) {
      if (row == nrow(expected)) {
        next
      }
      top_empty <- expected[row, col] %in% c("", NA)
      bottom_empty <- expected[row + 1, col] %in% c("", NA)
      if (top_empty & bottom_empty) {
        huxtable::bottom_border(expected)[row, col] <- 0
        huxtable::top_border(expected)[row + 1, col] <- 0
      }
    }
  }

  for (co in colnames(summarized_table)) {
    expected <- expected |>
      huxtable::set_number_format(
        i = which(colnames(summarized_table) == co),
        j = 5:9,
        value =
      )
  }

  # border between row names and data
  expected <- expected |>
    huxtable::set_right_border(col = 2)

  huxtable::add_colnames()
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

  for (i in colnames(summarized_table)) {
    expected <- expected |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = tablespan:::smart_round(x = summarized_table[[i]])
      )
  }

  testthat::expect_true(compare_tables(tbl_1 = gt_tbl, tbl_2 = expected))
})

test_that("cars - no autostyle", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  compare_tables <- function(tbl_1, tbl_2) {
    tbl_1_html <- tbl_1 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    tbl_2_html <- tbl_2 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    return(tbl_1_html == tbl_2_html)
  }

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

  gt_tbl <- as_gt(tbl = tbl, auto_format = FALSE)

  expected <- summarized_table |>
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

  for (i in colnames(summarized_table)) {
    expected <- expected |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = tablespan:::smart_round(x = summarized_table[[i]])
      )
  }

  testthat::expect_true(compare_tables(tbl_1 = gt_tbl, tbl_2 = expected))
})

test_that("cars-additional_spanners", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  compare_tables <- function(tbl_1, tbl_2) {
    tbl_1_html <- tbl_1 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    tbl_2_html <- tbl_2 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    return(tbl_1_html == tbl_2_html)
  }

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
      (Results = N +
        (`Horse Power` = (Mean = Mean:mean_hp) +
          (`Standard Deviation` = SD:sd_hp)) +
        (`Weight` = Mean:mean_wt + SD:sd_wt)),
    title = "Motor Trend Car Road Tests",
    subtitle = "A table created with tablespan",
    footnote = "Data from the infamous mtcars data set."
  )

  gt_tbl <- as_gt(tbl = tbl)

  expected <- summarized_table |>
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

  for (i in colnames(summarized_table)) {
    expected <- expected |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = tablespan:::smart_round(x = summarized_table[[i]])
      )
  }

  testthat::expect_true(compare_tables(tbl_1 = gt_tbl, tbl_2 = expected))
})

test_that("cars-no_row_names", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  compare_tables <- function(tbl_1, tbl_2) {
    tbl_1_html <- tbl_1 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    tbl_2_html <- tbl_2 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    return(tbl_1_html == tbl_2_html)
  }

  summarized_table <- mtcars |>
    group_by(cyl, vs) |>
    summarise(
      N = n(),
      mean_hp = mean(hp),
      sd_hp = sd(hp),
      mean_wt = mean(wt),
      sd_wt = sd(wt)
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

  gt_tbl <- as_gt(tbl = tbl)

  expected <- summarized_table |>
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
        decimals = tablespan:::smart_round(x = summarized_table[[i]])
      )
  }

  testthat::expect_true(compare_tables(tbl_1 = gt_tbl, tbl_2 = expected))
})

test_that("cars-no_titles", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  compare_tables <- function(tbl_1, tbl_2) {
    tbl_1_html <- tbl_1 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    tbl_2_html <- tbl_2 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    return(tbl_1_html == tbl_2_html)
  }

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
    footnote = "Data from the infamous mtcars data set."
  )

  gt_tbl <- as_gt(tbl = tbl)

  expected <- summarized_table |>
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

  for (i in colnames(summarized_table)) {
    expected <- expected |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = tablespan:::smart_round(x = summarized_table[[i]])
      )
  }

  testthat::expect_true(compare_tables(tbl_1 = gt_tbl, tbl_2 = expected))
})

test_that("cars-no_titles_no_footnotes", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  compare_tables <- function(tbl_1, tbl_2) {
    tbl_1_html <- tbl_1 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    tbl_2_html <- tbl_2 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    return(tbl_1_html == tbl_2_html)
  }

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
      (`Weight` = Mean:mean_wt + SD:sd_wt)
  )

  gt_tbl <- as_gt(tbl = tbl)

  expected <- summarized_table |>
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

  for (i in colnames(summarized_table)) {
    expected <- expected |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = tablespan:::smart_round(x = summarized_table[[i]])
      )
  }

  testthat::expect_true(compare_tables(tbl_1 = gt_tbl, tbl_2 = expected))
})

test_that("cars-duplicated_spanner_names", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  compare_tables <- function(tbl_1, tbl_2) {
    tbl_1_html <- tbl_1 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    tbl_2_html <- tbl_2 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    return(tbl_1_html == tbl_2_html)
  }

  model_1 <- lm(mpg ~ wt + qsec, data = mtcars) |>
    summary() |>
    (\(.x) as.data.frame(.x$coefficients))()
  model_2 <- lm(mpg ~ wt + qsec, data = mtcars) |>
    summary() |>
    (\(.x) as.data.frame(.x$coefficients))()

  model_1$Parameter <- rownames(model_1)
  model_2$Parameter <- rownames(model_2)

  combined_models <- full_join(model_1, model_2, by = "Parameter")

  tbl <- combined_models |>
    dplyr::as_tibble() |>
    tablespan(
      formula = Parameter ~
        (`Model 1` = Estimate:Estimate.x +
          (Significance = `t-value`:`t value.x` + `p-value`:`Pr(>|t|).x`)) +
        (`Model 2` = Estimate:Estimate.y +
          (Significance = `t-value`:`t value.y` + `p-value`:`Pr(>|t|).y`))
    )

  gt_tbl <- as_gt(tbl)

  expected <- combined_models |>
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
        decimals = tablespan:::smart_round(x = combined_models[[i]])
      )
  }

  testthat::expect_true(compare_tables(tbl_1 = gt_tbl, tbl_2 = expected))
})


test_that("cars - gt styling", {
  library(tablespan)
  library(testthat)
  library(dplyr)

  compare_tables <- function(tbl_1, tbl_2) {
    tbl_1_html <- tbl_1 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    tbl_2_html <- tbl_2 |>
      gt::as_raw_html(inline_css = TRUE) |>
      # the following is taken from https://github.com/rstudio/gt/blob/1e4bae1af102c171a19316bca512db4260592645/tests/testthat/test-as_raw_html.R#L6
      # and removes the unique id:
      gsub(pattern = "id=\"[a-z]*?\"", replacement = "", x = _)

    return(tbl_1_html == tbl_2_html)
  }

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

  gt_tbl_base <- as_gt(tbl = tbl)

  expected_base <- summarized_table |>
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

  for (i in colnames(summarized_table)) {
    expected_base <- expected_base |>
      gt::fmt_number(
        columns = all_of(i),
        decimals = tablespan:::smart_round(x = summarized_table[[i]])
      )
  }

  testthat::expect_true(compare_tables(
    tbl_1 = gt_tbl_base,
    tbl_2 = expected_base
  ))

  # title
  testthat::expect_true(compare_tables(
    tbl |>
      style_title(text_color = "#000000", background_color = "#983439") |>
      as_gt(),
    expected_base |>
      gt::tab_style(
        style = gt::cell_text(color = "#000000"),
        locations = gt::cells_title("title")
      ) |>
      gt::tab_style(
        style = gt::cell_fill(color = "#983439"),
        locations = gt::cells_title("title")
      )
  ))
  testthat::expect_true(compare_tables(
    tbl |>
      style_title(background_color = "#983439", text_color = "#ffffff") |>
      as_gt(),
    expected_base |>
      gt::tab_style(
        style = gt::cell_text(color = "#ffffff"),
        locations = gt::cells_title("title")
      ) |>
      gt::tab_style(
        style = gt::cell_fill(color = "#983439"),
        locations = gt::cells_title("title")
      )
  ))

  testthat::expect_true(compare_tables(
    tbl |>
      style_title(
        background_color = "#983439",
        text_color = "#ffffff",
        bold = TRUE,
        italic = TRUE
      ) |>
      as_gt(),
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
      )
  ))

  testthat::expect_true(compare_tables(
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
      as_gt(),
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
      )
  ))

  testthat::expect_true(compare_tables(
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
      style_footnote(gt_style = gt::cell_text(weight = "lighter")) |>
      as_gt(),
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
        style = gt::cell_text(weight = "lighter"),
        locations = gt::cells_footnotes()
      )
  ))

  testthat::expect_true(compare_tables(
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
      style_footnote(gt_style = gt::cell_text(weight = "lighter")) |>
      style_header(background_color = "#B65455", bold = TRUE) |>
      as_gt(),
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
        style = gt::cell_text(weight = "lighter"),
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
      )
  ))

  testthat::expect_true(compare_tables(
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
      style_footnote(gt_style = gt::cell_text(weight = "lighter")) |>
      style_header(background_color = "#B65455", bold = TRUE) |>
      format_column(
        columns = dplyr::where(is.double),
        rows = 2:3,
        format_gt = function(x, columns, rows, ...) {
          gt::fmt_number(x, columns = columns, rows = rows, decimals = 1)
        },
        format_openxlsx = "GENERAL"
      ) |>
      style_column(
        columns = dplyr::where(is.double),
        rows = 2:3,
        italic = TRUE,
        text_color = "#B54321"
      ) |>
      as_gt(),
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
        style = gt::cell_text(weight = "lighter"),
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
      )
  ))

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
  testthat::expect_true(compare_tables(
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
      style_footnote(gt_style = gt::cell_text(weight = "lighter")) |>
      style_header(background_color = "#B65455", bold = TRUE) |>
      format_column(
        columns = dplyr::where(is.double),
        rows = 2:3,
        format_gt = function(x, columns, rows, ...) {
          gt::fmt_number(x, columns = columns, rows = rows, decimals = 1)
        },
        format_openxlsx = "GENERAL"
      ) |>
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
      as_gt(),
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
        style = gt::cell_text(weight = "lighter"),
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
      gt::data_color(
        columns = dplyr::where(is.double),
        method = "numeric",
        palette = names(color_scale),
        domain = color_scale
      )
  ))

  color_scale = c(
    "#123456" = min(
      summarized_table |> select(where(is.double)),
      na.rm = TRUE
    ),
    "#ffffff" = 50,
    "#B46983" = max(
      summarized_table |> select(where(is.double)),
      na.rm = TRUE
    )
  )

  lower_scale <- scales::col_numeric(
    palette = names(color_scale)[1:2],
    domain = color_scale[1:2]
  )
  upper_scale <- scales::col_numeric(
    palette = names(color_scale)[2:3],
    domain = color_scale[2:3]
  )

  testthat::expect_true(compare_tables(
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
      style_footnote(gt_style = gt::cell_text(weight = "lighter")) |>
      style_header(background_color = "#B65455", bold = TRUE) |>
      format_column(
        columns = dplyr::where(is.double),
        rows = 2:3,
        format_gt = function(x, columns, rows, ...) {
          gt::fmt_number(x, columns = columns, rows = rows, decimals = 1)
        },
        format_openxlsx = "GENERAL"
      ) |>
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
      as_gt(),
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
        style = gt::cell_text(weight = "lighter"),
        locations = gt::cells_footnotes()
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "bold", color = "#000000"),
          gt::cell_fill(color = "#B65455")
        ),
        locations = gt::cells_column_labels()
      ) |>
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "bold", color = "#000000"),
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
      gt::data_color(
        columns = dplyr::where(is.double),
        fn = function(x) {
          color <- suppressWarnings(ifelse(
            x < color_scale[2],
            lower_scale(x),
            upper_scale(x)
          ))
          color <- ifelse(is.na(color), "#D3D3D3", color)
          return(color)
        }
      )
  ))
})
