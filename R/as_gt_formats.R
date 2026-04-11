#' get_formats_gt
#'
#' Internal function that initializes all styling objects of a tablespan table
#'
#' @param tbl tablespan table
#' @returns list with formats
#' @noRd
get_formats_gt <- function(tbl) {
  formats <- list()
  force(tbl)
  data <- get_table_data(tbl)

  for (column_name in colnames(data)) {
    for (form in tbl$formats$columns[[column_name]]) {
      if (form$fmt$type == "number") {
        # We need to create a new environment for each function
        # here because NSE messes up the variables
        formats$columns[[column_name]][[
          length(formats$columns[[column_name]]) + 1
        ]] <- local({
          decimals <- form$fmt$args$decimals
          dec_mark <- form$fmt$args$dec_mark
          sep_mark <- form$fmt$args$sep_mark
          list(
            gt = function(tbl, columns, rows, ...) {
              gt::fmt_number(
                tbl,
                columns = columns,
                rows = rows,
                decimals = decimals,
                dec_mark = dec_mark,
                sep_mark = sep_mark
              )
            },
            rows = form$row
          )
        })
      } else if (form$fmt$type == "text") {
        formats$columns[[column_name]][[
          length(formats$columns[[column_name]]) + 1
        ]] <- list(
          gt = function(tbl, columns, rows, ...) {
            gt::fmt_auto(tbl, columns = columns, rows = rows)
          },
          rows = form$row
        )
      } else if (form$fmt$type == "date") {
        # Create a new environment for each function
        formats$columns[[column_name]][[
          length(formats$columns[[column_name]]) + 1
        ]] <- local({
          format_str <- form$fmt$args$format
          list(
            gt = function(tbl, columns, rows, ...) {
              gt::fmt_date(
                tbl,
                columns = columns,
                rows = rows,
                date_style = format_to_gt_date_style(format_str)
              )
            },
            rows = form$row
          )
        })
      } else {
        formats$columns[[column_name]][[
          length(formats$columns[[column_name]]) + 1
        ]] <- list(
          gt = function(tbl, columns, rows, ...) {
            gt::fmt_auto(tbl, columns = columns, rows = rows)
          },
          rows = form$row
        )
      }
    }
  }

  return(formats)
}

format_to_gt_date_style <- function(format_str) {
  # mapping from R's format to gt
  mappings <- list(
    "%Y-%m-%d" = "iso", # 2023-12-31
    "%Y/%m/%d" = "iso", # 2023/12/31
    "%d/%m/%Y" = "day_month_year", # 31/12/2023
    "%m/%d/%Y" = "month_day_year", # 12/31/2023
    "%b %d, %Y" = "month_day_year", # Dec 31, 2023
    "%B %d, %Y" = "month_day_year", # December 31, 2023
    "%a %b %d" = "day_month", # Sun Dec 31
    "%A %B %d" = "day_month", # Sunday December 31
    "%Y" = "year", # 2023
    "%b %Y" = "month_year", # Dec 2023
    "%B %Y" = "month_year" # December 2023
  )

  if (format_str %in% names(mappings)) {
    return(mappings[[format_str]])
  }

  warning(
    "Could not find a date format correspoding to ",
    format_str,
    ". using 'ios'."
  )

  return("iso")
}
