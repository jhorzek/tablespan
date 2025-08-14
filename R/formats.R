#' initialize_styles
#'
#' Internal function that initializes all styling objects of a tablespan table
#'
#' @param tbl tablespan table
#' @returns tablespan table with added styles field
#' @noRd
initialize_formats <- function(tbl) {
  data <- extract_data(tbl)
  tbl$formats <- list()

  for (column_name in colnames(data)) {
    auto_formatting <- format_auto(data_col = data[[column_name]])

    tbl <- format_column(
      tbl = tbl,
      format_gt = auto_formatting$gt,
      format_openxlsx = auto_formatting$openxlsx,
      columns = dplyr::all_of(column_name),
      rows = seq_len(length(data[[column_name]]))
    )
    # Weird thing, but if we don't access the environment directly,
    # something seems to overwrite the local variables. Accessing it may force
    # the evaluation and prevent this overwrite...
    environment(
      environment(tbl$formats$columns[[column_name]][[1]]$format$gt)$format
    )
  }

  return(tbl)
}

#' format_column
#'
#' Change the formatting of a column or single cells within columns.
#'
#' @param tbl tablespan table
#' @param columns the columns to style. Must be a tidyselect selector expression (e.g., starts_with("hp_"))
#' @param rows indices of the rows which should be styled. When set to NULL, the style is applied to all rows
#' @param format_gt formatting used for gt. This must be a function with the following signature: function(tbl, columns, rows, ...)
#' and return the tbl with applied formatting. See examples.
#' @param format_openxlsx an argument passed to the numFmt field for openxlsx::createStyle.
#' @param stack When set to TRUE, the style is added on top of the existing styles. This is mostly relevant
#' for openxlsx. When set to FALSE, the new style replaces all previous styling.
#' @returns the tablespan table with added styles
#' @export
#' @examples
#' library(tablespan)
#' library(dplyr)
#' data("mtcars")
#'
#' # We want to report the following table:
#' summarized_table <- mtcars |>
#'   group_by(cyl, vs) |>
#'   summarise(N = n(),
#'             mean_hp = mean(hp),
#'             sd_hp = sd(hp),
#'             mean_wt = mean(wt),
#'             sd_wt = sd(wt))
#'
#' # Create a tablespan:
#' tbl <- tablespan(data = summarized_table,
#'                  formula = Cylinder:cyl + Engine:vs ~
#'                    N +
#'                    (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
#'                    (`Weight` = Mean:mean_wt + SD:sd_wt),
#'                  title = "Motor Trend Car Road Tests",
#'                  subtitle = "A table created with tablespan",
#'                  footnote = "Data from the infamous mtcars data set.")
#'
#' tbl |>
#'   format_column(columns = mean_hp,
#'                 rows = c(1,3),
#'                 format_gt = function(tbl, columns, rows, ...){
#'                              return(gt::fmt_number(tbl,
#'                                        columns = columns,
#'                                        rows = rows,
#'                                        decimals = 4))},
#'                 format_openxlsx = "0.0000") |>
#'   as_gt()
format_column <- function(
  tbl,
  columns = dplyr::everything(),
  rows = NULL,
  format_gt = gt::fmt_auto,
  format_openxlsx = "GENERAL",
  stack = TRUE
) {
  columns_expr <- rlang::enquo(columns)
  data <- extract_data(tbl)

  column_names <- data |>
    dplyr::select(!!columns_expr) |>
    colnames()

  gt_format <- create_format_gt_function(
    format = format_gt
  )

  openxlsx_format <- create_format_openxlsx(
    num_format = format_openxlsx
  )

  formats <- list(
    gt = gt_format,
    openxlsx = openxlsx_format
  )

  for (column_name in column_names) {
    if (stack) {
      tbl$formats$columns[[column_name]] <- append(
        tbl$formats$columns[[column_name]],
        list(list(
          format = formats,
          rows = rows
        ))
      )
    } else {
      tbl$formats$columns[[column_name]] <- list(list(
        format = formats,
        rows = rows
      ))
    }
  }

  return(tbl)
}

#' create_format_gt_function
#'
#' Create a new style function to be applied to the body of the table.
#'
#' @param format gt formatting
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param gt_style optional custom gt style. When provided, all other arguments are ignored
#' @noRd
#' @examples
#' library(tablespan)
#' library(dplyr)
#' data("mtcars")
#'
#' # We want to report the following table:
#' summarized_table <- mtcars |>
#'   group_by(cyl, vs) |>
#'   summarise(N = n(),
#'             mean_hp = mean(hp),
#'             sd_hp = sd(hp),
#'             mean_wt = mean(wt),
#'             sd_wt = sd(wt))
#'
#' # Create a tablespan:
#' tbl <- tablespan(data = summarized_table,
#'                  formula = Cylinder:cyl + Engine:vs ~
#'                    N +
#'                    (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
#'                    (`Weight` = Mean:mean_wt + SD:sd_wt),
#'                  title = "Motor Trend Car Road Tests",
#'                  subtitle = "A table created with tablespan",
#'                  footnote = "Data from the infamous mtcars data set.")
#'
#' tbl |>
#'   format_column(columns = mean_hp,
#'                 rows = c(1,3),
#'                 format_gt = function(tbl, columns, rows, ...){
#'                              return(gt::fmt_number(tbl,
#'                                        columns = columns,
#'                                        rows = rows,
#'                                        decimals = 4))},
#'                 format_openxlsx = "0.0000") |>
#'   as_gt()
create_format_gt_function <- function(
  format
) {
  gt_formatter <- function(data, column, rows) {
    return(
      data |>
        format(columns = gt::all_of(column), rows = rows)
    )
  }

  return(gt_formatter)
}

#' create_format_openxlsx
#'
#' Create a new format to be applied to the body of the table.
#'
#' @param num_format number format
#' @noRd
#' @examples
#' library(tablespan)
#' library(dplyr)
#' data("mtcars")
#'
#' # We want to report the following table:
#' summarized_table <- mtcars |>
#'   group_by(cyl, vs) |>
#'   summarise(N = n(),
#'             mean_hp = mean(hp),
#'             sd_hp = sd(hp),
#'             mean_wt = mean(wt),
#'             sd_wt = sd(wt))
#'
#' # Create a tablespan:
#' tbl <- tablespan(data = summarized_table,
#'                  formula = Cylinder:cyl + Engine:vs ~
#'                    N +
#'                    (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
#'                    (`Weight` = Mean:mean_wt + SD:sd_wt),
#'                  title = "Motor Trend Car Road Tests",
#'                  subtitle = "A table created with tablespan",
#'                  footnote = "Data from the infamous mtcars data set.")
#'
#' tbl |>
#'   format_column(columns = mean_hp,
#'                 rows = c(1,3),
#'                 format_gt = function(tbl, columns, rows, ...){
#'                              return(gt::fmt_number(tbl,
#'                                        columns = columns,
#'                                        rows = rows,
#'                                        decimals = 4))},
#'                 format_openxlsx = "0.0000") |>
#'   as_excel()
create_format_openxlsx <- function(
  num_format
) {
  openxlsx_style <- openxlsx::createStyle(
    numFmt = num_format
  )

  return(openxlsx_style)
}


#' format_auto
#'
#' Tries to identify the data type and implement a sensible default styling.
#' @param data_col the column for which a format should be created
#' @returns a list with styling for gt and excel tables
#' @noRd
format_auto <- function(data_col) {
  if (is.integer(data_col)) {
    return(format_number(decimals = 0))
  } else if (is.numeric(data_col)) {
    return(format_number(decimals = 2))
  } else {
    return(format_text())
  }
}

#' format_number
#'
#' Implements simple formatting for numbers in gt and excel exports of tablespan.
#' @param decimals the number of decimals to show
#' @param sep_mark optional symbol used to separate thousands
#' @param dec_mark symbol used to separate decimals
#' @returns a list with styles for gt and openxlsx
#' @noRd
format_number <- function(decimals = 2, sep_mark = ",", dec_mark = ".") {
  if (decimals == 0) {
    openxlsx_format <- "0"
  } else {
    openxlsx_format <- paste0(ifelse(
      sep_mark == "",
      paste0("0", dec_mark, paste0(rep("0", decimals), collapse = "")),
      paste0(
        "#",
        sep_mark,
        "##0",
        dec_mark,
        paste0(rep("0", decimals), collapse = "")
      )
    ))
  }
  int_decimals = force(decimals)
  int_sep_mark = force(sep_mark)
  int_dec_mark = force(dec_mark)
  return(
    list(
      "gt" = function(data, columns, rows) {
        gt::fmt_number(
          data = data,
          columns = columns,
          rows = rows,
          decimals = int_decimals,
          sep_mark = int_sep_mark,
          dec_mark = int_dec_mark
        )
      },
      "openxlsx" = openxlsx_format
    )
  )
}

#' format_text
#'
#' Implements simple formatting for text in gt and excel exports of tablespan.
#' @returns a list with styles for gt and openxlsx
#' @noRd
format_text <- function() {
  return(list(
    gt = function(data, columns, rows) {
      gt::fmt_auto(
        data = data,
        columns = columns,
        rows = rows
      )
    },
    openxlsx = "TEXT"
  ))
}
