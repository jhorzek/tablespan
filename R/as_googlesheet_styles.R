gs_get_style_requests <- function(
  tbl,
  google_sheet,
  sheet,
  locations
) {
  require_googlesheets4()

  style_requests <- initialize_styles_googlesheet(
    tbl = tbl,
    locations = locations,
    google_sheet = google_sheet,
    sheet = sheet
  ) |>
    style_column_googlesheet(
      style_requests = _,
      tbl = tbl,
      locations = locations,
      google_sheet = google_sheet,
      sheet = sheet
    )

  return(style_requests)
}

initialize_styles_googlesheet <- function(tbl, locations, google_sheet, sheet) {
  require_googlesheets4()

  style_requests <- list()

  if (!is.null(tbl$title)) {
    style_requests[[length(style_requests) + 1]] <-
      gs_create_style_request(
        sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
        row = c(locations$row$start_row_title, locations$row$end_row_title),
        col = c(locations$col$start_col_title, locations$col$end_col_title),
        bold = if (is.null(tbl$styles$title$bold)) {
          TRUE
        } else {
          tbl$styles$title$bold
        },
        italic = if (is.null(tbl$styles$title$italic)) {
          FALSE
        } else {
          tbl$styles$title$italic
        },
        font_size = if (is.null(tbl$styles$title$font_size)) {
          12
        } else {
          tbl$styles$title$font_size
        },
        background_color = if (is.null(tbl$styles$title$background_color)) {
          NULL
        } else {
          tbl$styles$title$background_color
        },
        text_color = if (is.null(tbl$styles$title$text_color)) {
          NULL
        } else {
          tbl$styles$title$text_color
        },
        format = NULL
      )
  }

  if (!is.null(tbl$subtitle)) {
    style_requests[[length(style_requests) + 1]] <-
      gs_create_style_request(
        sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
        row = c(
          locations$row$start_row_subtitle,
          locations$row$end_row_subtitle
        ),
        col = c(
          locations$col$start_col_subtitle,
          locations$col$end_col_subtitle
        ),
        bold = if (is.null(tbl$styles$subtitle$bold)) {
          TRUE
        } else {
          tbl$styles$subtitle$bold
        },
        italic = if (is.null(tbl$styles$subtitle$italic)) {
          FALSE
        } else {
          tbl$styles$subtitle$italic
        },
        font_size = if (is.null(tbl$styles$subtitle$font_size)) {
          10
        } else {
          tbl$styles$subtitle$font_size
        },
        background_color = if (is.null(tbl$styles$subtitle$background_color)) {
          NULL
        } else {
          tbl$styles$subtitle$background_color
        },
        text_color = if (is.null(tbl$styles$subtitle$text_color)) {
          NULL
        } else {
          tbl$styles$subtitle$text_color
        },
        format = NULL
      )
  }

  style_requests[[length(style_requests) + 1]] <-
    gs_create_style_request(
      sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
      row = c(
        locations$row$start_row_header,
        locations$row$end_row_header
      ),
      col = c(
        if (is.null(tbl$header$lhs)) {
          locations$col$start_col_header_rhs
        } else {
          locations$col$start_col_header_lhs
        },
        locations$col$end_col_header_rhs
      ),
      bold = if (is.null(tbl$styles$header$bold)) {
        TRUE
      } else {
        tbl$styles$header$bold
      },
      italic = if (is.null(tbl$styles$header$italic)) {
        FALSE
      } else {
        tbl$styles$header$italic
      },
      font_size = if (is.null(tbl$styles$header$font_size)) {
        10
      } else {
        tbl$styles$header$font_size
      },
      background_color = if (is.null(tbl$styles$header$background_color)) {
        NULL
      } else {
        tbl$styles$header$background_color
      },
      text_color = if (is.null(tbl$styles$header$text_color)) {
        NULL
      } else {
        tbl$styles$header$text_color
      },
      format = NULL
    )

  style_requests[[length(style_requests) + 1]] <-
    gs_border_request(
      sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
      row = c(
        locations$row$start_row_header,
        locations$row$end_row_header
      ),
      col = c(
        if (is.null(tbl$header$lhs)) {
          locations$col$start_col_header_rhs
        } else {
          locations$col$start_col_header_lhs
        },
        locations$col$end_col_header_rhs
      ),
      top = if (tbl$styles$header_cells$top) {
        gs_border_style(
          style = "SOLID",
          width = 1,
          color = gs_color(tbl$styles$header_cells$border_color)
        )
      } else {
        NULL
      },
      bottom = if (tbl$styles$header_cells$bottom) {
        gs_border_style(
          style = "SOLID",
          width = 1,
          color = gs_color(tbl$styles$header_cells$border_color)
        )
      } else {
        NULL
      },
      left = if (tbl$styles$header_cells$left) {
        gs_border_style(
          style = "SOLID",
          width = 1,
          color = gs_color(tbl$styles$header_cells$border_color)
        )
      } else {
        NULL
      },
      right = if (tbl$styles$header_cells$right) {
        gs_border_style(
          style = "SOLID",
          width = 1,
          color = gs_color(tbl$styles$header_cells$border_color)
        )
      } else {
        NULL
      }
    )

  if (!is.null(tbl$footnote)) {
    style_requests[[length(style_requests) + 1]] <-
      gs_create_style_request(
        sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
        row = c(
          locations$row$start_row_footnote,
          locations$row$end_row_footnote
        ),
        col = c(
          locations$col$start_col_footnote,
          locations$col$end_col_footnote
        ),
        bold = if (is.null(tbl$styles$subtitle$bold)) {
          TRUE
        } else {
          tbl$styles$subtitle$bold
        },
        italic = if (is.null(tbl$styles$subtitle$italic)) {
          FALSE
        } else {
          tbl$styles$subtitle$italic
        },
        font_size = if (is.null(tbl$styles$subtitle$font_size)) {
          10
        } else {
          tbl$styles$subtitle$font_size
        },
        background_color = if (is.null(tbl$styles$subtitle$background_color)) {
          NULL
        } else {
          tbl$styles$subtitle$background_color
        },
        text_color = if (is.null(tbl$styles$subtitle$text_color)) {
          NULL
        } else {
          tbl$styles$subtitle$text_color
        },
        format = NULL
      )
  }

  return(style_requests)
}

style_column_googlesheet <- function(
  style_requests,
  tbl,
  locations,
  google_sheet,
  sheet
) {
  require_googlesheets4()

  table_data <- get_table_data(tbl = tbl)
  column_names <- names(tbl$styles$columns)

  for (column_name in column_names) {
    for (column_style in tbl$styles$columns[[column_name]]) {
      rows_series <- get_subseries_minmax(column_style$rows)

      for (i in seq_len(nrow(rows_series))) {
        style_requests[[
          length(style_requests) + 1
        ]] <- create_style_googlesheet(
          google_sheet = google_sheet,
          sheet = sheet,
          row = locations$row$start_row_data +
            unlist(rows_series[i, , drop = TRUE]) -
            1,
          col = locations$col$start_col_title +
            which(colnames(table_data) == column_name) -
            1,
          font_size = column_style$style$font_size,
          text_color = column_style$style$text_color,
          bold = column_style$style$bold,
          italic = column_style$style$italic,
          background_color = column_style$style$background_color,
          color_scale = column_style$style$color_scale
        )
      }
    }
  }

  return(style_requests)
}


#' create_style_googlesheet
#'
#' Create a style for Google Sheets export that can be applied to table elements.
#'
#' This function generates a list of style functions that can be used to apply formatting
#' to cells in a Google Sheet.
#'
#' @param font_size numeric value specifying the font size to apply
#' @param text_color character value specifying the text color as a hex code (e.g., "#000000")
#' @param bold logical value indicating whether to apply bold formatting
#' @param italic logical value indicating whether to apply italic formatting
#' @param background_color character value specifying the background color as a hex code
#' @returns A list containing one or more style functions that can be applied to Google Sheets
#' @noRd
#' @examples
#' # Create a style with bold text and yellow background
#' style <- create_style_googlesheet(
#'   bold = TRUE,
#'   background_color = "#FFFF00"
#' )
create_style_googlesheet <- function(
  google_sheet,
  sheet,
  row,
  col,
  font_size,
  text_color,
  bold,
  italic,
  background_color,
  color_scale = NULL
) {
  require_googlesheets4()

  styles <- list(
    gs_create_style_request(
      sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
      row = row,
      col = col,
      bold = bold,
      italic = italic,
      font_size = font_size,
      background_color = background_color,
      text_color = text_color
    )
  )

  if (!is.null(color_scale)) {
    styles[[length(styles) + 1]] <- gs_create_color_scale_request(
      sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
      row = row,
      col = col,
      color_scale = color_scale
    )
  }

  return(styles)
}


#' Create Google Sheets API color scale request
#'
#' Generates a Google Sheets API request to apply a color scale (gradient) conditional
#' formatting to a range of cells. This function supports both 2-color and 3-color scales.
#'
#' @param sheetId The ID of the sheet within the Google Sheet
#' @param row Row or row range (1-indexed) to apply the color scale to
#' @param col Column or column range (1-indexed) to apply the color scale to
#' @param color_scale A named vector of length 2 or 3 specifying the color scale.
#' Values should be numeric and colors should be hex codes. Example:
#' `c("#EE2F43" = -1, "#FFFFFF" = 0, "#37E65A" = 1)`. NA values will be automatically
#' filled with appropriate values from the data.
#' @return A list containing a Google Sheets API request that can be used to apply
#' the specified color scale conditional formatting
#' @noRd
gs_create_color_scale_request <- function(
  sheetId,
  row,
  col,
  color_scale
) {
  # We assume that row and col are row and column ranges
  if (!length(row) %in% 1:2) {
    stop("row must be either one or two values")
  }
  if (!length(col) %in% 1:2) {
    stop("col must be either one or two values")
  }

  # We have to translate the 1-indexed R to a 0-indexed googlesheets request.
  # Additionally, googlesheets has non-inclusive indexes with [start, end), so we must
  # add 1 to the end (so end stays the same, start is reduced by 1):

  row_start <- if (length(row) == 1) row - 1 else row[1] - 1
  row_end <- if (length(row) == 1) row else row[2]

  col_start <- if (length(col) == 1) col - 1 else col[1] - 1
  col_end <- if (length(col) == 1) col else col[2]

  if (length(color_scale) == 2) {
    return(list(
      addConditionalFormatRule = list(
        rule = list(
          ranges = list(
            sheetId = sheetId,
            startRowIndex = row_start,
            endRowIndex = row_end,
            startColumnIndex = col_start,
            endColumnIndex = col_end
          ),
          gradientRule = list(
            minpoint = list(
              color = gs_color(color = names(color_scale)[1]),
              "type" = "NUMBER",
              "value" = as.character(unname(color_scale[1]))
            ),
            maxpoint = list(
              color = gs_color(color = names(color_scale)[2]),
              "type" = "NUMBER",
              "value" = as.character(unname(color_scale[2]))
            )
          )
        ),
        index = 0
      )
    ))
  } else if (length(color_scale) == 3) {
    return(list(
      addConditionalFormatRule = list(
        rule = list(
          ranges = list(
            sheetId = sheetId,
            startRowIndex = row_start,
            endRowIndex = row_end,
            startColumnIndex = col_start,
            endColumnIndex = col_end
          ),
          gradientRule = list(
            minpoint = list(
              color = gs_color(color = names(color_scale)[1]),
              "type" = "NUMBER",
              "value" = as.character(unname(color_scale[1]))
            ),
            midpoint = list(
              color = gs_color(color = names(color_scale)[2]),
              "type" = "NUMBER",
              "value" = as.character(unname(color_scale[2]))
            ),
            maxpoint = list(
              color = gs_color(color = names(color_scale)[3]),
              "type" = "NUMBER",
              "value" = as.character(unname(color_scale[3]))
            )
          )
        ),
        index = 0
      )
    ))
  } else {
    stop("Unknown color_scale")
  }
}
