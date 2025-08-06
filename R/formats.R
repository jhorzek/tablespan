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
#'   style_column(columns = mean_hp,
#'               format = format_number(decimals = 5)) |>
#'   as_gt()
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
      "openxlsx" = openxlsx_format
    )
  )
}

#' format_text
#'
#' Implements simple formatting for text in gt and excel exports of tablespan.
#' @returns a list with styles for gt and openxlsx
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
#'   style_column(columns = mean_hp,
#'               format = format_text()) |>
#'   as_gt()
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
