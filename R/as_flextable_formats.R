#' get_formats_flex
#'
#' Internal function that initializes all styling objects of a tablespan table
#'
#' @param tbl tablespan table
#' @returns list with formats
#' @noRd
get_formats_flex <- function(tbl) {
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
            flex = function(tbl, row, col, part) {
              flextable::colformat_double(
                x = tbl,
                i = row,
                j = col,
                big.mark = sep_mark,
                decimal.mark = dec_mark,
                digits = decimals
              )
            },
            rows = form$row
          )
        })
      } else if (form$fmt$type == "text") {
        formats$columns[[column_name]][[
          length(formats$columns[[column_name]]) + 1
        ]] <- list(
          flex = function(tbl, row, col, part) {
            return(tbl)
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
            flex = function(tbl, row, col, part) {
              flextable::colformat_date(
                x = tbl,
                i = row,
                j = col,
                fmt_date = format_str
              )
            },
            rows = form$row
          )
        })
      } else {
        formats$columns[[column_name]][[
          length(formats$columns[[column_name]]) + 1
        ]] <- list(
          flex = function(tbl, row, col, part) {
            return(tbl)
          },
          rows = form$row
        )
      }
    }
  }

  return(formats)
}
