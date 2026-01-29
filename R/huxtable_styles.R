#' default_styles_hux
#'
#' Sets the default styles for huxtable tables in a tablespan table.
#'
#' This function adds default styling for huxtable tables to the provided default_styles list.
#' If the huxtable package is not available, the original default_styles are returned unchanged.
#'
#' @returns a list with default styles for huxtable tables added to the input default_styles
#' @noRd
default_styles_hux <- function() {
  require_huxtable()

  default <- list(function(tbl_hux, row, col) {
    return(tbl_hux)
  })
  default_styles$title$hux <- default
  default_styles$subtitle$hux <- default
  default_styles$header$hux <- default
  default_styles$header_cells$hux <- default
  default_styles$footnote$hux <- default
  default_styles$hline$hux <- default
  default_styles$vline$hux <- default
  return(default_styles)
}

style_title_hux <- function(hux_styles, tbl) {
  require_huxtable()

  force(tbl)

  hux_styles$title$hux <- create_style_hux(
    font_size = tbl$styles$title$font_size,
    text_color = tbl$styles$title$text_color,
    bold = tbl$styles$title$bold,
    italic = tbl$styles$title$italic,
    background_color = tbl$styles$title$background_color
  )

  return(hux_styles)
}

style_subtitle_hux <- function(hux_styles, tbl) {
  require_huxtable()

  force(tbl)

  hux_styles$subtitle$hux <- create_style_hux(
    font_size = tbl$styles$subtitle$font_size,
    text_color = tbl$styles$subtitle$text_color,
    bold = tbl$styles$subtitle$bold,
    italic = tbl$styles$subtitle$italic,
    background_color = tbl$styles$subtitle$background_color
  )

  return(hux_styles)
}

style_header_hux <- function(hux_styles, tbl) {
  require_gt()

  force(tbl)

  hux_styles$header$hux <- create_style_hux(
    font_size = tbl$styles$header$font_size,
    text_color = tbl$styles$header$text_color,
    bold = tbl$styles$header$bold,
    italic = tbl$styles$header$italic,
    background_color = tbl$styles$header$background_color
  )

  return(hux_styles)
}

style_header_cells_hux <- function(hux_styles, tbl) {
  return(hux_styles)
}

style_footnote_hux <- function(hux_styles, tbl) {
  require_gt()

  force(tbl)

  hux_styles$footnote$hux <- create_style_hux(
    font_size = tbl$styles$footnote$font_size,
    text_color = tbl$styles$footnote$text_color,
    bold = tbl$styles$footnote$bold,
    italic = tbl$styles$footnote$italic,
    background_color = tbl$styles$footnote$background_color
  )

  return(hux_styles)
}

style_hline_gt <- function(hux_styles, tbl) {
  return(hux_styles)
}

style_vline_gt <- function(hux_styles, tbl) {
  return(hux_styles)
}


style_column_hux <- function(gt_styles, tbl) {
  force(tbl)
  column_names <- names(tbl$styles$columns)
  for (column_name in column_names) {
    gt_styles$columns[[column_name]] <- list()
    for (column_style in tbl$styles$columns[[column_name]]) {
      gt_styles$columns[[column_name]][[
        length(gt_styles$columns[[column_name]]) + 1
      ]] <-
        list(
          style = list(
            gt = create_style_gt_function(
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

  return(gt_styles)
}
