initialize_styles_googlesheet <- function(tbl) {
  require_googlesheets4()

  styles <- list()

  default <- list(function(google_sheet, sheet, row, col) {
    return(list())
  })

  default_styles$title$googlesheet <- list(
    function(google_sheet, sheet, row, col) {
      gs_create_style_request(
        sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
        row,
        col,
        bold = TRUE,
        italic = FALSE,
        font_size = 12,
        background_color = NULL,
        text_color = NULL,
        format = NULL
      )
    }
  )
  default_styles$subtitle$googlesheet <- list(
    function(google_sheet, sheet, row, col) {
      gs_create_style_request(
        sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
        row,
        col,
        bold = TRUE,
        italic = FALSE,
        font_size = 10,
        background_color = NULL,
        text_color = NULL,
        format = NULL
      )
    }
  )

  default_styles$header$googlesheet <- list(
    function(google_sheet, sheet, row, col) {
      gs_create_style_request(
        sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
        row,
        col,
        bold = TRUE,
        italic = FALSE,
        font_size = 10,
        background_color = NULL,
        text_color = NULL,
        format = NULL
      )
    }
  )

  default_styles$header_cells$googlesheet <- list(
    function(google_sheet, sheet, row, col) {
      gs_border_request(
        sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
        row = row,
        col = col,
        top = NULL,
        bottom = gs_border_style(
          style = "SOLID",
          width = 1,
          color = list(red = 0, green = 0, blue = 0)
        ),
        left = gs_border_style(
          style = "SOLID",
          width = 1,
          color = list(red = 0, green = 0, blue = 0)
        ),
        right = gs_border_style(
          style = "SOLID",
          width = 1,
          color = list(red = 0, green = 0, blue = 0)
        )
      )
    }
  )

  default_styles$footnote$googlesheet <- list(
    function(google_sheet, sheet, row, col) {
      gs_create_style_request(
        sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
        row,
        col,
        bold = FALSE,
        italic = FALSE,
        font_size = 10,
        background_color = NULL,
        text_color = NULL,
        format = NULL
      )
    }
  )

  default_styles$hline$googlesheet <- list(
    gs_border_style(color = "#000000")
  )

  default_styles$vline$googlesheet <- list(gs_border_style(color = "#000000"))

  return(default_styles)
}

style_title_googlesheet <- function(styles_gs, tbl) {
  require_googlesheets4()
  force(tbl)

  styles_gs$title$googlesheet[[
    length(styles_gs$title$googlesheet) + 1
  ]] <- create_style_googlesheet(
    font_size = tbl$styles$title$font_size,
    text_color = tbl$styles$title$text_color,
    bold = tbl$styles$title$bold,
    italic = tbl$styles$title$italic,
    background_color = tbl$styles$title$background_color
  )
  return(styles_gs)
}

style_subtitle_googlesheet <- function(styles_gs, tbl) {
  require_googlesheets4()
  force(tbl)

  styles_gs$subtitle$googlesheet[[
    length(styles_gs$subtitle$googlesheet) + 1
  ]] <- create_style_googlesheet(
    font_size = tbl$styles$subtitle$font_size,
    text_color = tbl$styles$subtitle$text_color,
    bold = tbl$styles$subtitle$bold,
    italic = tbl$styles$subtitle$italic,
    background_color = tbl$styles$subtitle$background_color
  )
  return(styles_gs)
}

style_header_googlesheet <- function(styles_gs, tbl) {
  require_googlesheets4()
  force(tbl)

  styles_gs$header$googlesheet[[
    length(styles_gs$header$googlesheet) + 1
  ]] <- create_style_googlesheet(
    font_size = tbl$styles$header$font_size,
    text_color = tbl$styles$header$text_color,
    bold = tbl$styles$header$bold,
    italic = tbl$styles$header$italic,
    background_color = tbl$styles$header$background_color
  )
  return(styles_gs)
}

style_header_cells_googlesheet <- function(styles_gs, tbl) {
  require_googlesheets4()
  force(tbl)

  tbl$styles$header_cells$googlesheet <- create_style_googlesheet(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    googlesheet_style = googlesheet_style
  )
  tbl$styles$header_cells$googlesheet[[
    length(tbl$styles$header_cells$googlesheet) + 1
  ]] <-
    function(google_sheet, sheet, row, col) {
      gs_border_request(
        sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
        row = row,
        col = col,
        top = gs_border_style(
          style = if (tbl$styles$header_cells$top) "SOLID" else "None",
          width = 1,
          color = tbl$styles$header_cells$border_color
        ),
        bottom = gs_border_style(
          style = if (tbl$styles$header_cells$bottom) "SOLID" else "None",
          width = 1,
          color = tbl$styles$header_cells$border_color
        ),
        left = gs_border_style(
          style = if (tbl$styles$header_cells$left) "SOLID" else "None",
          width = 1,
          color = tbl$styles$header_cells$border_color
        ),
        right = gs_border_style(
          style = if (tbl$styles$header_cells$right) "SOLID" else "None",
          width = 1,
          color = tbl$styles$header_cells$border_color
        )
      )
    }

  return(styles_gs)
}

style_footnote_googlesheet <- function(styles_gs, tbl) {
  require_googlesheets4()
  force(tbl)

  styles_gs$footnote$googlesheet[[
    length(styles_gs$footnote$googlesheet) + 1
  ]] <- create_style_googlesheet(
    font_size = tbl$styles$footnote$font_size,
    text_color = tbl$styles$footnote$text_color,
    bold = tbl$styles$footnote$bold,
    italic = tbl$styles$footnote$italic,
    background_color = tbl$styles$footnote$background_color
  )
  return(styles_gs)
}

style_hline_googlesheet <- function(styles_gs, tbl) {
  require_googlesheets4()
  force(tbl)

  styles_gs$hline$googlesheet[[
    length(styles_gs$footnote$googlesheet) + 1
  ]] <- gs_border_style(color = tbl$styles$hline$color)

  return(styles_gs)
}

style_vline_googlesheet <- function(styles_gs, tbl) {
  require_googlesheets4()
  force(tbl)

  styles_gs$vline$googlesheet[[
    length(styles_gs$footnote$googlesheet) + 1
  ]] <- gs_border_style(color = tbl$vline$hline$color)

  return(styles_gs)
}

style_column_googlesheet <- function(styles_gs, tbl) {
  require_googlesheets4()
  force(tbl)

  column_names <- names(tbl$styles$columns)

  for (column_name in column_names) {
    styles_gs$columns[[column_name]] <- list()
    for (column_style in tbl$styles$columns[[column_name]]) {
      styles_gs$columns[[column_name]][[
        length(styles_gs$columns[[column_name]]) + 1
      ]] <-
        list(
          style = list(
            googlesheet = create_style_googlesheet(
              font_size = column_style$style$font_size,
              text_color = column_style$style$text_color,
              bold = column_style$style$bold,
              italic = column_style$style$italic,
              background_color = column_style$style$background_color,
              color_scale = column_style$style$color_scale
            )
          ),
          rows = column_style$rows
        )
    }
  }

  return(styles_gs)
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
  font_size,
  text_color,
  bold,
  italic,
  background_color,
  color_scale = NULL
) {
  require_googlesheets4()

  styles <- list(
    function(google_sheet, sheet, row, col) {
      return(gs_create_style_request(
        sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
        row = row,
        col = col,
        bold = bold,
        italic = italic,
        font_size = font_size,
        background_color = background_color,
        text_color = text_color
      ))
    }
  )

  if (!is.null(color_scale)) {
    styles[[length(styles) + 1]] <- create_color_scale_gs(
      color_scale = color_scale
    )
  }

  return(styles)
}

create_color_scale_gs <- function(color_scale) {
  return(
    function(google_sheet, sheet, row, col) {
      return(gs_create_color_scale_request(
        sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
        row = row,
        col = col,
        color_scale
      ))
    }
  )
}
