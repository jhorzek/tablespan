#' initialize_styles_hux
#'
#' Sets the default styles for huxtable tables in a tablespan table.
#'
#' This function adds default styling for huxtable tables to the provided default_styles list.
#' If the huxtable package is not available, the original default_styles are returned unchanged.
#'
#' @returns a list with default styles for huxtable tables added to the input default_styles
#' @noRd
initialize_styles_hux <- function() {
  require_huxtable()

  default_styles <- list()

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

style_hline_hux <- function(hux_styles, tbl) {
  return(hux_styles)
}

style_vline_hux <- function(hux_styles, tbl) {
  return(hux_styles)
}


style_column_hux <- function(hux_styles, tbl) {
  force(tbl)

  column_names <- names(tbl$styles$columns)

  for (column_name in column_names) {
    hux_styles$columns[[column_name]] <- list()
    for (column_style in tbl$styles$columns[[column_name]]) {
      hux_styles$columns[[column_name]][[
        length(hux_styles$columns[[column_name]]) + 1
      ]] <-
        list(
          style = list(
            hux = create_style_hux(
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

  return(hux_styles)
}

#' create_style_hux
#'
#' Create a new style to be applied to the body of the table.
#'
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param hux_style optional custom huxtable style. When provided, all other arguments are ignored. Must be a function with the following signature:
#' function(tbl, row, col)\{apply some style to the table and return the table\}. Example: function(tbl, row, col)\{tbl |> huxtable::set_bold(row = row, col = col)\}
#' @noRd
create_style_hux <- function(
  font_size,
  text_color,
  bold,
  italic,
  background_color,
  color_scale = NULL
) {
  require_huxtable()

  styles <- list()

  if (!is.null(font_size)) {
    styles[[length(styles) + 1]] <- function(tbl, row, col) {
      return(
        huxtable::set_font_size(
          ht = tbl,
          row = row,
          col = col,
          value = font_size
        )
      )
    }
  }

  if (!is.null(text_color)) {
    styles[[length(styles) + 1]] <- function(tbl, row, col) {
      return(
        huxtable::set_text_color(
          ht = tbl,
          row = row,
          col = col,
          value = text_color
        )
      )
    }
  }

  if (!is.null(background_color)) {
    styles[[length(styles) + 1]] <- function(tbl, row, col) {
      return(
        huxtable::set_background_color(
          ht = tbl,
          row = row,
          col = col,
          value = background_color
        )
      )
    }
  }

  if (bold) {
    styles[[length(styles) + 1]] <- function(tbl, row, col) {
      return(
        huxtable::set_bold(
          ht = tbl,
          row = row,
          col = col
        )
      )
    }
  }

  if (italic) {
    styles[[length(styles) + 1]] <- function(tbl, row, col) {
      return(
        huxtable::set_italic(
          ht = tbl,
          row = row,
          col = col
        )
      )
    }
  }

  if (!is.null(color_scale)) {
    styles[[length(styles) + 1]] <- create_color_scale_hux(
      color_scale = color_scale
    )
  }

  return(styles)
}

create_color_scale_hux <- function(color_scale) {
  return(function(tbl, row, col) {
    tbl |>
      huxtable::map_background_color(
        do.call(
          huxtable::by_colorspace,
          c(
            as.list(names(color_scale)),
            list(
              range = color_scale,
              na_color = NA,
              ignore_na = TRUE,
              colwise = FALSE
            )
          )
        ),
        row = row,
        col = col
      )
  })
}
