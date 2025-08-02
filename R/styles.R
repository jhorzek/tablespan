initialize_styles <- function(tbl) {
  tbl$styles <- list()
  if (!is.null(tbl$table_data$row_data)) {
    data <- cbind(tbl$table_data$row_data, tbl$table_data$col_data)
  } else {
    data <- tbl$table_data$col_data
  }

  for (column_name in colnames(data)) {
    tbl <- set_style(
      tbl = tbl,
      style = create_style(
        format = format_auto(data_col = data[[column_name]])
      ),
      columns = dplyr::all_of(column_name),
      rows = seq_len(length(data[[column_name]]))
    )
  }

  return(tbl)
}

format_auto <- function(data_col) {
  if (is.integer(data_col)) {
    return(format_number(decimals = 0))
  } else if (is.numeric(data_col)) {
    return(format_number(decimals = 3))
  } else {
    return(format_text())
  }
}

format_number <- function(decimals = 3, sep_mark = ",", dec_mark = ".") {
  return(
    list(
      "gt" = function(data, columns, rows) {
        gt::fmt_number(
          data = data,
          columns = columns,
          rows = rows,
          decimals = decimals,
          sep_mark = sep_mark,
          dec_mark = dec_mark
        )
      },
      "openxlsx" = paste0(ifelse(
        sep_mark == "",
        "0",
        paste0("#", sep_mark, "##0", dec_mark, rep("0", decimals))
      ))
    )
  )
}

format_text <- function() {
  return(list(
    gt = gt::fmt_auto,
    openxlsx = "TEXT"
  ))
}

set_style <- function(tbl, style, columns = NULL, rows = NULL) {
  # replaces all existing styles
  if (!is(style, "tbl_style")) {
    stop("style must be a tbl_style. Use style() to create a new style object")
  }
  columns_expr <- rlang::enquo(columns)
  browser()
  if (!is.null(tbl$table_data$row_data)) {
    data <- cbind(tbl$table_data$row_data, tbl$table_data$col_data)
  } else {
    data <- tbl$table_data$col_data
  }

  column_names <- data |>
    dplyr::select(!!columns_expr) |>
    colnames()

  for (column_name in column_names) {
    tbl$styles[[column_name]] <- list(list(style = style, rows = rows))
  }

  return(tbl)
}

add_style <- function(tbl, style, columns = NULL, rows = NULL) {
  # adds a style to the list of existing styles
  if (!is(style, "tbl_style")) {
    stop("style must be a tbl_style. Use style() to create a new style object")
  }
  columns_expr <- rlang::enquo(columns)
  if (!is.null(tbl$table_data$row_data)) {
    data <- cbind(tbl$table_data$row_data, tbl$table_data$col_data)
  } else {
    data <- tbl$table_data$col_data
  }

  column_names <- data |>
    dplyr::select(!!columns_expr) |>
    colnames()

  for (column_name in column_names) {
    tbl$styles[[column_name]] <- c(
      tbl$styles[[column_name]],
      list(style = style, rows = rows)
    )
  }
  return(tbl)
}


create_style <- function(
  background_color = "#ffffff",
  text_color = "#000000",
  font_size = 11,
  bold = FALSE,
  italic = FALSE,
  format = auto_format,
  openxlsx_style = NULL,
  gt_style = NULL
) {
  if (is.null(gt_style)) {
    gt_style <- create_style_gt(
      format = format,
      font_size = font_size,
      text_color = text_color,
      bold = bold,
      italic = italic,
      background_color = background_color
    )
  }

  if (is.null(openxlsx_style)) {
    openxlsx_style <- create_style_openxlsx(
      format = format,
      font_size = font_size,
      text_color = text_color,
      bold = bold,
      italic = italic,
      background_color = background_color
    )
  }

  style <- list(
    gt = gt_style,
    openxlsx = openxlsx_style
  )
  class(style) <- "tbl_style"
  return(style)
}


create_style_gt <- function(
  format,
  font_size,
  text_color,
  bold,
  italic,
  background_color
) {
  gt_style <- function(data, column, rows) {
    style <- if (italic) "italic" else NULL
    weight <- if (bold) "bold" else NULL
    data |>
      format$gt(columns = gt::all_of(column), rows = rows) |>
      gt::tab_style(
        data = _,
        style = list(
          gt::cell_text(
            size = font_size,
            color = text_color,
            style = style,
            weight = weight
          ),
          gt::cell_fill(color = background_color)
        ),
        locations = gt::cells_body(
          columns = gt::all_of(column),
          rows = rows
        )
      )
  }

  return(gt_style)
}

create_style_openxlsx <- function(
  format,
  font_size,
  text_color,
  bold,
  italic,
  background_color
) {
  browser()
  textDecoration <- NULL
  if (bold) {
    textDecoration <- c("Bold")
  }
  if (italic) {
    textDecoration <- c(textDecoration, "italic")
  }

  openxlsx_style <- openxlsx::createStyle(
    numFmt = format$openxlsx,
    fontSize = font_size,
    fontColour = text_color,
    bgFill = background_color,
    textDecoration = textDecoration
  )

  return(openxlsx_style)
}
