get_formats_openxlsx <- function(tbl) {
  require_openxlsx()

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
          format_style <- format_date_openxlsx(format_str)
          list(
            openxlsx = function(wb, sheet, row, col) {
              return(openxlsx::addStyle(
                wb = wb,
                sheet = sheet,
                style = format_style,
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
  require_openxlsx()
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
  require_openxlsx()
  return(openxlsx::createStyle(numFmt = "TEXT"))
}

format_date_openxlsx <- function(format_str) {
  require_openxlsx()
  return(openxlsx::createStyle(numFmt = format_str))
}

format_to_openxlsx_date_style <- function(format_str = NULL) {
  # Default ISO format
  iso_format <- "yyyy-mm-dd"

  # Mapping from R strftime -> Excel format
  mappings <- c(
    "%Y-%m-%d" = "yyyy-mm-dd",
    "%Y/%m/%d" = "yyyy/mm/dd",
    "%d/%m/%Y" = "dd/mm/yyyy",
    "%m/%d/%Y" = "mm/dd/yyyy",
    "%d.%m.%Y" = "dd.mm.yyyy",
    "%Y" = "yyyy",
    "%b %Y" = "mmm yyyy",
    "%B %Y" = "mmmm yyyy",
    "%d-%b-%Y" = "dd-mmm-yyyy",
    "%d-%B-%Y" = "dd-mmmm-yyyy"
  )

  # If NULL or empty: default ISO
  if (is.null(format_str) || !nzchar(format_str)) {
    return(iso_format)
  }

  # Direct match
  if (format_str %in% names(mappings)) {
    return(unname(mappings[format_str]))
  }

  # Basic token replacement (fallback for simple formats)
  converted <- format_str

  converted <- gsub("%Y", "yyyy", converted)
  converted <- gsub("%y", "yy", converted)
  converted <- gsub("%m", "mm", converted)
  converted <- gsub("%d", "dd", converted)
  converted <- gsub("%b", "mmm", converted)
  converted <- gsub("%B", "mmmm", converted)

  # If nothing changed, warn and return ISO
  if (identical(converted, format_str)) {
    warning(
      "Unknown format: ",
      format_str,
      ". Falling back to ISO format (yyyy-mm-dd)."
    )
    return(iso_format)
  }

  return(converted)
}
