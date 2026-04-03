build_tablespan_test_tables <- function() {
    title <- "Motor Trend Car Road Tests"
    subtitle <- "A table created with tablespan"
    footnote <- "Data from the infamous mtcars data set."
    tables <- list()

    summarized_table <- mtcars |>
        dplyr::summarise(
            N = n(),
            mean_hp = mean(hp),
            sd_hp = sd(hp),
            mean_wt = mean(wt),
            sd_wt = sd(wt),
            .by = c(cyl, vs)
        ) |>
        tibble::as_tibble()

    tables$cars <- tablespan::tablespan(
        data = summarized_table,
        formula = Cylinder:cyl + Engine:vs ~
            N +
            (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
            (`Weight` = Mean:mean_wt + SD:sd_wt),
        title = title,
        subtitle = subtitle,
        footnote = footnote
    )

    tables$cars_additional_spanners <- tablespan::tablespan(
        data = summarized_table,
        formula = Cylinder:cyl + Engine:vs ~
            (Results = N +
                (`Horse Power` = (Mean = Mean:mean_hp) +
                    (`Standard Deviation` = SD:sd_hp)) +
                (`Weight` = Mean:mean_wt + SD:sd_wt)),
        title = title,
        subtitle = subtitle,
        footnote = footnote
    )

    tables$cars_no_row_names <- tablespan::tablespan(
        data = summarized_table,
        formula = 1 ~
            (Results = N +
                (`Horse Power` = (Mean = Mean:mean_hp) +
                    (`Standard Deviation` = SD:sd_hp)) +
                (`Weight` = Mean:mean_wt + SD:sd_wt)),
        title = title,
        subtitle = subtitle,
        footnote = footnote
    )

    tables$cars_no_titles <- tablespan::tablespan(
        data = summarized_table,
        formula = Cylinder:cyl + Engine:vs ~
            N +
            (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
            (`Weight` = Mean:mean_wt + SD:sd_wt),
        footnote = footnote
    )

    tables$cars_no_titles_no_footnotes <- tablespan::tablespan(
        data = summarized_table,
        formula = Cylinder:cyl + Engine:vs ~
            N +
            (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
            (`Weight` = Mean:mean_wt + SD:sd_wt)
    )

    model_1 <- stats::lm(mpg ~ wt + qsec, data = mtcars) |>
        summary() |>
        (\(.x) as.data.frame(.x$coefficients))()
    model_2 <- stats::lm(mpg ~ wt + qsec, data = mtcars) |>
        summary() |>
        (\(.x) as.data.frame(.x$coefficients))()

    model_1$Parameter <- rownames(model_1)
    model_2$Parameter <- rownames(model_2)

    combined_models <- dplyr::full_join(model_1, model_2, by = "Parameter")

    tables$cars_duplicated_spanner_names <- dplyr::as_tibble(combined_models) |>
        tablespan::tablespan(
            formula = Parameter ~
                (`Model 1` = Estimate:Estimate.x +
                    (Significance = `t-value`:`t value.x` +
                        `p-value`:`Pr(>|t|).x`)) +
                (`Model 2` = Estimate:Estimate.y +
                    (Significance = `t-value`:`t value.y` +
                        `p-value`:`Pr(>|t|).y`))
        )

    tables$cars_title_style_black_red <- tables$cars |>
        tablespan::style_title(
            text_color = "#000000",
            background_color = "#983439"
        )

    tables$cars_title_style_white_red <- tables$cars |>
        tablespan::style_title(
            background_color = "#983439",
            text_color = "#ffffff"
        )

    tables$cars_title_style_bold_italic <- tables$cars |>
        tablespan::style_title(
            background_color = "#983439",
            text_color = "#ffffff",
            bold = TRUE,
            italic = TRUE
        )

    tables$cars_title_subtitle_style_bold_italic <- tables$cars |>
        tablespan::style_title(
            background_color = "#983439",
            text_color = "#ffffff",
            bold = TRUE,
            italic = TRUE
        ) |>
        tablespan::style_subtitle(
            background_color = "#983439",
            text_color = "#ffffff",
            bold = TRUE,
            italic = TRUE
        )

    tables$cars_title_subtitle_footnote_size <- tables$cars |>
        tablespan::style_title(
            background_color = "#983439",
            text_color = "#ffffff",
            bold = TRUE,
            italic = TRUE
        ) |>
        tablespan::style_subtitle(
            background_color = "#983439",
            text_color = "#ffffff",
            bold = TRUE,
            italic = TRUE
        ) |>
        tablespan::style_footnote(font_size = 8)

    tables$cars_title_subtitle_footnote_header_style <- tables$cars |>
        tablespan::style_title(
            background_color = "#983439",
            text_color = "#ffffff",
            bold = TRUE,
            italic = TRUE
        ) |>
        tablespan::style_subtitle(
            background_color = "#983439",
            text_color = "#ffffff",
            bold = TRUE,
            italic = TRUE
        ) |>
        tablespan::style_footnote(font_size = 8) |>
        tablespan::style_header(background_color = "#B65455", bold = TRUE)

    tables$cars_title_subtitle_footnote_header_format_cell_style <- tables$cars |>
        tablespan::style_title(
            background_color = "#983439",
            text_color = "#ffffff",
            bold = TRUE,
            italic = TRUE
        ) |>
        tablespan::style_subtitle(
            background_color = "#983439",
            text_color = "#ffffff",
            bold = TRUE,
            italic = TRUE
        ) |>
        tablespan::style_footnote(italic = TRUE) |>
        tablespan::style_header(background_color = "#B65455", bold = TRUE) |>
        tablespan::format_column(
            columns = dplyr::where(is.double),
            rows = 2:3,
            fmt = tablespan::format_number(decimals = 1)
        ) |>
        tablespan::style_column(
            columns = dplyr::where(is.double),
            rows = 2:3,
            italic = TRUE,
            text_color = "#B54321"
        )

    color_scale_2 <- c(
        "#123456" = min(
            summarized_table |>
                dplyr::select(dplyr::where(is.double)),
            na.rm = TRUE
        ),
        "#B46983" = max(
            summarized_table |>
                dplyr::select(dplyr::where(is.double)),
            na.rm = TRUE
        )
    )

    tables$cars_title_subtitle_footnote_header_format_color_scale_2_cell_style <- tables$cars |>
        tablespan::style_title(
            background_color = "#983439",
            text_color = "#ffffff",
            bold = TRUE,
            italic = TRUE
        ) |>
        tablespan::style_subtitle(
            background_color = "#983439",
            text_color = "#ffffff",
            bold = TRUE,
            italic = TRUE
        ) |>
        tablespan::style_footnote(font_size = 8) |>
        tablespan::style_header(background_color = "#B65455", bold = TRUE) |>
        tablespan::format_column(
            columns = dplyr::where(is.double),
            rows = 2:3,
            fmt = tablespan::format_number(decimals = 1)
        ) |>
        tablespan::style_column(
            columns = dplyr::where(is.double),
            color_scale = color_scale_2
        ) |>
        tablespan::style_column(
            columns = dplyr::where(is.double),
            rows = 2:3,
            italic = TRUE,
            text_color = "#B54321"
        )

    color_scale_3 <- c(
        "#123456" = min(
            summarized_table |>
                dplyr::select(dplyr::where(is.double)),
            na.rm = TRUE
        ),
        "#ffffff" = 50,
        "#B46983" = max(
            summarized_table |>
                dplyr::select(dplyr::where(is.double)),
            na.rm = TRUE
        )
    )

    tables$cars_title_subtitle_footnote_header_format_color_scale_3_cell_style <- tables$cars |>
        tablespan::style_title(
            background_color = "#983439",
            text_color = "#ffffff",
            bold = TRUE,
            italic = TRUE
        ) |>
        tablespan::style_subtitle(
            background_color = "#983439",
            text_color = "#ffffff",
            bold = TRUE,
            italic = TRUE
        ) |>
        tablespan::style_footnote(font_size = 8) |>
        tablespan::style_header(background_color = "#B65455", bold = TRUE) |>
        tablespan::format_column(
            columns = dplyr::where(is.double),
            rows = 2:3,
            fmt = tablespan::format_number(decimals = 1)
        ) |>
        tablespan::style_column(
            columns = dplyr::where(is.double),
            color_scale = color_scale_3
        ) |>
        tablespan::style_column(
            columns = dplyr::where(is.double),
            rows = 2:3,
            italic = TRUE,
            text_color = "#B54321"
        )

    mixed_date_text_data <- tibble::tibble(
        group = c("A", "A", "B", "B"),
        event_date = as.Date(c(
            "2025-01-15",
            "2025-03-02",
            "2025-05-27",
            "2025-06-11"
        )),
        comment = c("ok", "manual review", "hold", "complete"),
        amount = c(10.432, 11.5, 9.8, 15.07)
    )

    tables$date_text_base <- tablespan::tablespan(
        data = mixed_date_text_data,
        formula = Group:group ~ Date:event_date +
            Comment:comment +
            Amount:amount,
        title = "Date/Text Formatting Example",
        subtitle = "Fixture for format_date and format_text",
        footnote = "Synthetic data for tests"
    )

    tables$date_text_formatted <- tables$date_text_base |>
        tablespan::format_column(
            columns = dplyr::all_of("event_date"),
            fmt = tablespan::format_date("%d-%m-%Y")
        ) |>
        tablespan::format_column(
            columns = dplyr::all_of("comment"),
            fmt = tablespan::format_text()
        ) |>
        tablespan::format_column(
            columns = dplyr::all_of("amount"),
            fmt = tablespan::format_number(decimals = 1)
        )

    tables$cars_header_cells_styled <- tables$cars |>
        tablespan::style_header_cells(
            text_color = "#345364",
            border_color = "#345364",
            top = TRUE,
            bottom = TRUE,
            left = TRUE,
            right = TRUE,
            bold = TRUE
        )

    tables$cars_hline_styled <- tables$cars |>
        tablespan::style_hline(color = "#928505")

    tables$cars_vline_styled <- tables$cars |>
        tablespan::style_vline(color = "#928505")

    tables$cars_header_cells_hline_vline_styled <- tables$cars |>
        tablespan::style_header_cells(
            text_color = "#345364",
            border_color = "#345364",
            top = TRUE,
            bottom = TRUE,
            left = TRUE,
            right = TRUE,
            bold = TRUE
        ) |>
        tablespan::style_hline(color = "#928505") |>
        tablespan::style_vline(color = "#2F4B7C")

    return(list(
        metadata = list(
            title = title,
            subtitle = subtitle,
            footnote = footnote
        ),
        data = list(
            summarized_table = summarized_table,
            combined_models = combined_models,
            mixed_date_text_data = mixed_date_text_data
        ),
        tables = tables
    ))
}
