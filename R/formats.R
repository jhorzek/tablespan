#' initialize_styles
#'
#' Internal function that initializes all styling objects of a tablespan table
#'
#' @param tbl tablespan table
#' @param max_digits maximal number of digits
#' @returns tablespan table with added styles field
#' @noRd
initialize_formats <- function(tbl, max_digits) {
  data <- get_table_data(tbl)
  tbl$formats <- list()

  for (column_name in colnames(data)) {
    tbl$formats$columns[[column_name]] <- list(
      list(
        fmt = format_auto(
          data_col = data[[column_name]],
          max_digits = max_digits
        ),
        row = 1:nrow(data)
      )
    )
  }

  return(tbl)
}

smart_round <- function(x, max_digits = 4) {
  if (all(is.na(x))) {
    return(0)
  }
  tol <- 10^(-(max_digits + 1))
  # integers
  if (all(abs(x - round(x)) < tol, na.rm = TRUE)) {
    return(0)
  }

  # For all numbers, we first find the first digit after the zero where anything is happing at all.
  # First, we will just extract the decimal places
  decimal_places <- abs(x - round(x, 0))
  # Now we can use log10 to find the first digit where anything meaningful happens.
  # We add a small tolerance to make sure we are not taking the log of 0
  first_non_zero_decimal <- ifelse(
    decimal_places > tol,
    ceiling(-log10(decimal_places + tol)),
    0
  )
  # Now we know where the first numbers after the decimal emerge. We will allow for some more
  # precision just to make sure
  return(min(max_digits, max(first_non_zero_decimal, na.rm = TRUE) + 1))
}


#' format_column
#'
#' Change the formatting of a column or single cells within columns.
#'
#' @param tbl tablespan table
#' @param columns the columns to style. Must be a tidyselect selector expression (e.g., starts_with("hp_"))
#' @param rows indices of the rows which should be styled. When set to NULL, the style is applied to all rows
#' @param fmt fromatting object. Use format_number to format numeric values, format_text for text elements, and format_date
#' for dates.
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
#' if(require_gt(throw = FALSE))
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
  fmt,
  stack = TRUE
) {
  if (!is(fmt, "tablespan_format")) {
    stop("fmt must be a tablespan_format object (e.g., format_numeric).")
  }

  columns_expr <- rlang::enquo(columns)
  data <- get_table_data(tbl)

  column_names <- data |>
    dplyr::select(!!columns_expr) |>
    colnames()

  for (column_name in column_names) {
    if (stack) {
      tbl$formats$columns[[column_name]] <- append(
        tbl$formats$columns[[column_name]],
        list(list(
          fmt = fmt,
          rows = rows
        ))
      )
    } else {
      tbl$formats$columns[[column_name]] <- list(list(
        fmt = fmt,
        rows = rows
      ))
    }
  }

  return(tbl)
}


#' format_auto
#'
#' Tries to identify the data type and implement a sensible default styling.
#' @param data_col the column for which a format should be created
#' @param max_digits maximal number of digits for floats
#' @returns a list with styling for gt and excel tables
#' @noRd
format_auto <- function(data_col, max_digits) {
  if (is.numeric(data_col)) {
    return(format_number(
      decimals = smart_round(x = data_col, max_digits = max_digits)
    ))
  } else if (inherits(data_col, 'Date')) {
    return(format_date())
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
format_number <- function(decimals, sep_mark = ",", dec_mark = ".") {
  styles_list <- list(
    type = "number",
    args = list(
      decimals = decimals,
      sep_mark = sep_mark,
      dec_mark = dec_mark
    )
  )
  class(styles_list) <- "tablespan_format"
  return(styles_list)
}


#' format_text
#'
#' Implements simple formatting for text in gt and excel exports of tablespan.
#' @returns a list with styles for gt and openxlsx
#' @noRd
format_text <- function() {
  formats <- list(type = "text", args = list())
  class(formats) <- "tablespan_format"
  return(formats)
}

format_date <- function(format = "%Y-%m-%d") {
  styles_list <- list(
    type = "date",
    args = list(format = "%Y-%m-%d")
  )
  class(styles_list) <- "tablespan_format"
  return(styles_list)
}


#' format_text_hux
#'
#' Creates a formatting function for huxtable that applies automatic text formatting.
#'
#' @returns a function that applies automatic text formatting to a huxtable, or NULL if huxtable is not available
#' @noRd
format_text_hux <- function() {
  return(NULL)
}
