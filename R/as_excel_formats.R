get_formats_openxlsx <- function(tbl) {
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
          num_form <- format_number_openxlsx(
            decimals = form$fmt$args$decimals,
            sep_mark = form$fmt$args$sep_mark,
            dec_mark = form$fmt$args$dec_mark
          )
          list(
            openxlsx = function(wb, sheet, row, col) {
              return(openxlsx::addStyle(
                wb = wb,
                sheet = sheet,
                style = num_form,
                rows = row,
                cols = col,
                stack = TRUE,
                gridExpand = FALSE
              ))
            },
            rows = form$row
          )
        })
      } else if (form$fmt$type == "text") {
        formats$columns[[column_name]][[
          length(formats$columns[[column_name]]) + 1
        ]] <- list(
          openxlsx = function(wb, sheet, row, col) {
            return(openxlsx::addStyle(
              wb = wb,
              sheet = sheet,
              style = format_text_openxlsx(),
              rows = row,
              cols = col,
              stack = TRUE,
              gridExpand = FALSE
            ))
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
            openxlsx = function(wb, sheet, row, col) {
              return(openxlsx::addStyle(
                wb = wb,
                sheet = sheet,
                style = format_str,
                rows = row,
                cols = col,
                stack = TRUE,
                gridExpand = FALSE
              ))
            },
            rows = form$row
          )
        })
      } else {
        formats$columns[[column_name]][[
          length(formats$columns[[column_name]]) + 1
        ]] <- list(
          openxlsx = function(wb, row, col) {
            return(wb)
          },
          rows = form$row
        )
      }
    }
  }

  return(formats)
}

#' format_number_openxlsx
#'
#' Creates an openxlsx number format string for formatting numbers with specified decimal places,
#' thousands separator, and decimal mark.
#'
#' @param decimals number of decimal places to display
#' @param sep_mark character used as thousands separator (default: ",")
#' @param dec_mark character used as decimal mark (default: ".")
#'
#' @returns a character string representing the openxlsx number format, or NULL if openxlsx is not available
#' @noRd
format_number_openxlsx <- function(decimals, sep_mark, dec_mark) {
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

  return(openxlsx::createStyle(numFmt = openxlsx_format))
}

#' format_text_openxlsx
#'
#' Creates an openxlsx text format for formatting text cells.
#'
#' @returns a character string representing the openxlsx text format
#' @noRd
format_text_openxlsx <- function() {
  return(openxlsx::createStyle(numFmt = "TEXT"))
}
