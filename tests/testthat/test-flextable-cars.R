library(testthat)
library(dplyr)
library(tablespan)
library(flextable)

test_tables <- build_tablespan_test_tables()

compare_flextables <- function(ft1, ft2) {
    expect_equal(unname(ft1$col_keys), unname(ft2$col_keys))
    expect_equal(unname(ft1$header$dataset), unname(ft2$header$dataset))
    expect_equal(unname(ft1$body$dataset), unname(ft2$body$dataset))
    expect_equal(unname(ft1$footer$dataset), unname(ft2$footer$dataset))

    expect_equal(unname(ft1$header$styles), unname(ft2$header$styles))
    expect_equal(unname(ft1$body$styles), unname(ft2$body$styles))
    expect_equal(unname(ft1$footer$styles), unname(ft2$footer$styles))
}

build_expected_flextable <- function(
    data,
    ref_table,
    header_rows = list(),
    title = NULL,
    subtitle = NULL,
    footnote = NULL,
    body_vline_col = NULL
) {
    expected <- data |>
        flextable::flextable() |>
        flextable::set_header_df(mapping = ref_table, key = "key")

    for (header_row in header_rows) {
        expected <- expected |>
            flextable::add_header_row(
                values = header_row$values,
                colwidths = header_row$colwidths
            )
    }

    expected <- expected |>
        flextable::theme_booktabs()

    if (!is.null(body_vline_col)) {
        expected <- expected |>
            flextable::vline(
                j = body_vline_col,
                border = officer::fp_border(),
                part = "body"
            )
    }

    if (!is.null(title) || !is.null(subtitle)) {
        expected <- expected |>
            flextable::add_header_lines(values = c(title, subtitle))
    }

    expected <- expected |>
        flextable::align(align = "left", part = "header")

    if (!is.null(footnote)) {
        expected <- expected |>
            flextable::add_footer_lines(values = footnote)
    }

    expected <- expected |>
        flextable::align(align = "left", part = "footer") |>
        flextable::autofit()

    expected
}

format_numeric_columns <- function(ft_tbl, data, columns = names(data)) {
    for (nm in columns) {
        if (is.numeric(data[[nm]])) {
            ft_tbl <- ft_tbl |>
                flextable::colformat_double(
                    j = nm,
                    digits = tablespan:::smart_round(data[[nm]])
                )
        }
    }

    ft_tbl
}

test_that("cars", {
    ft_tbl <- as_flextable(test_tables$tables$cars)

    expected <- build_expected_flextable(
        data = test_tables$data$summarized_table,
        ref_table = data.frame(
            key = c("cyl", "vs", "N", "mean_hp", "sd_hp", "mean_wt", "sd_wt"),
            label = c("Cylinder", "Engine", "N", "Mean", "SD", "Mean", "SD")
        ),
        header_rows = list(list(
            values = c("", "", "", "Horse Power", "Weight"),
            colwidths = c(1, 1, 1, 2, 2)
        )),
        title = test_tables$metadata$title,
        subtitle = test_tables$metadata$subtitle,
        footnote = test_tables$metadata$footnote,
        body_vline_col = 2
    ) |>
        format_numeric_columns(test_tables$data$summarized_table)

    compare_flextables(ft_tbl, expected)
})

test_that("cars-additional_spanners", {
    ft_tbl <- as_flextable(test_tables$tables$cars_additional_spanners)

    expected <- build_expected_flextable(
        data = test_tables$data$summarized_table,
        ref_table = data.frame(
            key = c("cyl", "vs", "N", "mean_hp", "sd_hp", "mean_wt", "sd_wt"),
            label = c("Cylinder", "Engine", "N", "Mean", "SD", "Mean", "SD")
        ),
        header_rows = list(
            list(
                values = c("", "", "", "Mean", "Standard Deviation", "Weight"),
                colwidths = c(1, 1, 1, 1, 1, 2)
            ),
            list(
                values = c("", "", "", "Horse Power", "", ""),
                colwidths = c(1, 1, 1, 2, 1, 1)
            ),
            list(
                values = c("", "", "Results"),
                colwidths = c(1, 1, 5)
            )
        ),
        title = test_tables$metadata$title,
        subtitle = test_tables$metadata$subtitle,
        footnote = test_tables$metadata$footnote,
        body_vline_col = 2
    ) |>
        format_numeric_columns(test_tables$data$summarized_table)

    compare_flextables(ft_tbl, expected)
})

test_that("cars-no_row_names", {
    ft_tbl <- as_flextable(test_tables$tables$cars_no_row_names)

    summarized_sub <- test_tables$data$summarized_table |>
        dplyr::select(dplyr::all_of(c(
            "N",
            "mean_hp",
            "sd_hp",
            "mean_wt",
            "sd_wt"
        )))

    expected <- build_expected_flextable(
        data = summarized_sub,
        ref_table = data.frame(
            key = c("N", "mean_hp", "sd_hp", "mean_wt", "sd_wt"),
            label = c("N", "Mean", "SD", "Mean", "SD")
        ),
        header_rows = list(
            list(
                values = c("", "Mean", "Standard Deviation", "Weight"),
                colwidths = c(1, 1, 1, 2)
            ),
            list(
                values = c("", "Horse Power", "", ""),
                colwidths = c(1, 2, 1, 1)
            ),
            list(
                values = c("Results"),
                colwidths = c(5)
            )
        ),
        title = test_tables$metadata$title,
        subtitle = test_tables$metadata$subtitle,
        footnote = test_tables$metadata$footnote,
        body_vline_col = NULL
    ) |>
        format_numeric_columns(summarized_sub)

    compare_flextables(ft_tbl, expected)
})

test_that("cars-no_titles", {
    ft_tbl <- as_flextable(test_tables$tables$cars_no_titles)

    expected <- build_expected_flextable(
        data = test_tables$data$summarized_table,
        ref_table = data.frame(
            key = c("cyl", "vs", "N", "mean_hp", "sd_hp", "mean_wt", "sd_wt"),
            label = c("Cylinder", "Engine", "N", "Mean", "SD", "Mean", "SD")
        ),
        header_rows = list(list(
            values = c("", "", "", "Horse Power", "Weight"),
            colwidths = c(1, 1, 1, 2, 2)
        )),
        title = NULL,
        subtitle = NULL,
        footnote = test_tables$metadata$footnote,
        body_vline_col = 2
    ) |>
        format_numeric_columns(test_tables$data$summarized_table)

    compare_flextables(ft_tbl, expected)
})

test_that("cars-no_titles_no_footnotes", {
    ft_tbl <- as_flextable(test_tables$tables$cars_no_titles_no_footnotes)

    expected <- build_expected_flextable(
        data = test_tables$data$summarized_table,
        ref_table = data.frame(
            key = c("cyl", "vs", "N", "mean_hp", "sd_hp", "mean_wt", "sd_wt"),
            label = c("Cylinder", "Engine", "N", "Mean", "SD", "Mean", "SD")
        ),
        header_rows = list(list(
            values = c("", "", "", "Horse Power", "Weight"),
            colwidths = c(1, 1, 1, 2, 2)
        )),
        title = NULL,
        subtitle = NULL,
        footnote = NULL,
        body_vline_col = 2
    ) |>
        format_numeric_columns(test_tables$data$summarized_table)

    compare_flextables(ft_tbl, expected)
})

test_that("cars-duplicated_spanner_names", {
    testthat::expect_no_error(as_flextable(
        test_tables$tables$cars_duplicated_spanner_names
    ))
})

test_that("cars - flextable styling", {
    tbl <- test_tables$tables$cars
    ft_tbl <- as_flextable(tbl)

    expected_base <- build_expected_flextable(
        data = test_tables$data$summarized_table,
        ref_table = data.frame(
            key = c("cyl", "vs", "N", "mean_hp", "sd_hp", "mean_wt", "sd_wt"),
            label = c("Cylinder", "Engine", "N", "Mean", "SD", "Mean", "SD")
        ),
        header_rows = list(list(
            values = c("", "", "", "Horse Power", "Weight"),
            colwidths = c(1, 1, 1, 2, 2)
        )),
        title = test_tables$metadata$title,
        subtitle = test_tables$metadata$subtitle,
        footnote = test_tables$metadata$footnote,
        body_vline_col = 2
    ) |>
        format_numeric_columns(test_tables$data$summarized_table)

    compare_flextables(ft_tbl, expected_base)

    compare_flextables(
        as_flextable(test_tables$tables$cars_title_style_black_red),
        expected_base |>
            flextable::bg(i = 1, bg = "#983439", part = "header") |>
            flextable::color(i = 1, color = "#000000", part = "header")
    )

    compare_flextables(
        as_flextable(test_tables$tables$cars_title_style_white_red),
        expected_base |>
            flextable::bg(i = 1, bg = "#983439", part = "header") |>
            flextable::color(i = 1, color = "#ffffff", part = "header")
    )

    compare_flextables(
        as_flextable(
            test_tables$tables$cars_title_subtitle_footnote_header_style
        ),
        expected_base |>
            flextable::bg(i = 1:2, bg = "#983439", part = "header") |>
            flextable::color(i = 1:2, color = "#ffffff", part = "header") |>
            flextable::bold(i = 1:2, part = "header") |>
            flextable::italic(i = 1:2, part = "header") |>
            flextable::bg(i = 3:4, bg = "#B65455", part = "header") |>
            flextable::bold(i = 3:4, part = "header") |>
            flextable::fontsize(i = 1, size = 8, part = "footer")
    )

    compare_flextables(
        as_flextable(
            test_tables$tables$cars_tsf_hf_cell_style
        ),
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
            flextable::colformat_double(
                i = 2:3,
                j = c(1:2, 4:7),
                digits = 1
            ) |>
            flextable::italic(i = 2:3, j = c(1:2, 4:7), part = "body") |>
            flextable::italic(i = 1, part = "footer")
    )

    testthat::expect_no_error(as_flextable(
        test_tables$tables$cars_tsf_hf_cs2_cell_style
    ))
    testthat::expect_no_error(as_flextable(
        test_tables$tables$cars_tsf_hf_cs3_cell_style
    ))
})

test_that("date_text_base", {
    ft_tbl <- as_flextable(test_tables$tables$date_text_base)

    expected <- build_expected_flextable(
        data = test_tables$data$mixed_date_text_data,
        ref_table = data.frame(
            key = c("group", "event_date", "comment", "amount"),
            label = c("Group", "Date", "Comment", "Amount")
        ),
        title = "Date/Text Formatting Example",
        subtitle = "Fixture for format_date and format_text",
        footnote = "Synthetic data for tests",
        body_vline_col = 1
    ) |>
        format_numeric_columns(
            test_tables$data$mixed_date_text_data,
            columns = c("amount")
        )

    compare_flextables(ft_tbl, expected)
})

test_that("date_text_formatted", {
    ft_tbl <- as_flextable(test_tables$tables$date_text_formatted)

    expected <- build_expected_flextable(
        data = test_tables$data$mixed_date_text_data,
        ref_table = data.frame(
            key = c("group", "event_date", "comment", "amount"),
            label = c("Group", "Date", "Comment", "Amount")
        ),
        title = "Date/Text Formatting Example",
        subtitle = "Fixture for format_date and format_text",
        footnote = "Synthetic data for tests",
        body_vline_col = 1
    ) |>
        flextable::colformat_date(j = "event_date", fmt = "%d-%m-%Y") |>
        flextable::colformat_double(j = "amount", digits = 1)

    compare_flextables(ft_tbl, expected)
})

test_that("cars_header_cells_styled", {
    testthat::expect_no_error(as_flextable(
        test_tables$tables$cars_header_cells_styled
    ))
})

test_that("cars_hline_styled", {
    testthat::expect_no_error(as_flextable(
        test_tables$tables$cars_hline_styled
    ))
})

test_that("cars_vline_styled", {
    testthat::expect_no_error(as_flextable(
        test_tables$tables$cars_vline_styled
    ))
})

test_that("cars_header_cells_hline_vline_styled", {
    testthat::expect_no_error(as_flextable(
        test_tables$tables$cars_header_cells_hline_vline_styled
    ))
})
