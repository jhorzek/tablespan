#' get_formats_hux
#'
#' Internal function that initializes all styling objects of a tablespan table
#'
#' @param tbl tablespan table
#' @returns list with formats
#' @noRd
get_formats_hux <- function(tbl) {
  formats <- list()
  force(tbl)
  data <- extract_data(tbl)

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
            hux = function(tbl, row, col) {
              return(
                tbl |>
                  huxtable::set_number_format(
                    row = row,
                    col = col,
                    value = paste0("%5.", decimals, "f")
                  )
              )
            },
            rows = form$row
          )
        })
      } else if (form$fmt$type == "text") {
        formats$columns[[column_name]][[
          length(formats$columns[[column_name]]) + 1
        ]] <- list(
          hux = function(tbl, row, col) {
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
            hux = function(tbl, row, col) {
              huxtable::set_number_format(
                x = tbl,
                i = row,
                j = col,
                value = format_str
              )
            },
            rows = form$row
          )
        })
      } else {
        formats$columns[[column_name]][[
          length(formats$columns[[column_name]]) + 1
        ]] <- list(
          hux = function(tbl, row, col) {
            return(tbl)
          },
          rows = form$row
        )
      }
    }
  }

  return(formats)
}
