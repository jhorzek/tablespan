#' initialize_styles_flex
#'
#' Sets the default styles for flextable tables in a tablespan table.
#'
#' This function adds default styling for flextable tables to the provided default_styles list.
#' If the flextable package is not available, the original default_styles are returned unchanged.
#'
#' @returns a list with default styles for flextable tables added to the input default_styles
#' @noRd
initialize_styles_flex <- function() {
  require_flextable()

  default_styles <- list()

  default <- list(function(tbl, row, col, part) {
    return(tbl)
  })

  default_styles$title$flex <- default
  default_styles$subtitle$flex <- default
  default_styles$header$flex <- default
  default_styles$header_cells$flex <- default
  default_styles$footnote$flex <- default
  default_styles$hline$flex <- default
  default_styles$vline$flex <- default

  return(default_styles)
}

style_title_flex <- function(flex_styles, tbl) {
  require_flextable()

  force(tbl)

  flex_styles$title$flex <- create_style_flex(
    font_size = tbl$styles$title$font_size,
    text_color = tbl$styles$title$text_color,
    bold = tbl$styles$title$bold,
    italic = tbl$styles$title$italic,
    background_color = tbl$styles$title$background_color
  )

  return(flex_styles)
}

style_subtitle_flex <- function(flex_styles, tbl) {
  require_flextable()

  force(tbl)

  flex_styles$subtitle$flex <- create_style_flex(
    font_size = tbl$styles$subtitle$font_size,
    text_color = tbl$styles$subtitle$text_color,
    bold = tbl$styles$subtitle$bold,
    italic = tbl$styles$subtitle$italic,
    background_color = tbl$styles$subtitle$background_color
  )

  return(flex_styles)
}

style_header_flex <- function(flex_styles, tbl) {
  require_gt()

  force(tbl)

  flex_styles$header$flex <- create_style_flex(
    font_size = tbl$styles$header$font_size,
    text_color = tbl$styles$header$text_color,
    bold = tbl$styles$header$bold,
    italic = tbl$styles$header$italic,
    background_color = tbl$styles$header$background_color
  )

  return(flex_styles)
}

style_header_cells_flex <- function(flex_styles, tbl) {
  return(flex_styles)
}

style_footnote_flex <- function(flex_styles, tbl) {
  require_gt()

  force(tbl)

  flex_styles$footnote$flex <- create_style_flex(
    font_size = tbl$styles$footnote$font_size,
    text_color = tbl$styles$footnote$text_color,
    bold = tbl$styles$footnote$bold,
    italic = tbl$styles$footnote$italic,
    background_color = tbl$styles$footnote$background_color
  )

  return(flex_styles)
}

style_hline_flex <- function(flex_styles, tbl) {
  return(flex_styles)
}

style_vline_flex <- function(flex_styles, tbl) {
  return(flex_styles)
}


style_column_flex <- function(flex_styles, tbl) {
  force(tbl)

  column_names <- names(tbl$styles$columns)

  for (column_name in column_names) {
    flex_styles$columns[[column_name]] <- list()
    for (column_style in tbl$styles$columns[[column_name]]) {
      flex_styles$columns[[column_name]][[
        length(flex_styles$columns[[column_name]]) + 1
      ]] <-
        list(
          style = list(
            flex = create_style_flex(
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

  return(flex_styles)
}

#' create_style_flex
#'
#' Create a new style to be applied to the body of the table.
#'
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @noRd
create_style_flex <- function(
  font_size,
  text_color,
  bold,
  italic,
  background_color,
  color_scale = NULL
) {
  require_flextable()

  styles <- list()

  if (!is.null(font_size)) {
    styles[[length(styles) + 1]] <- function(tbl, row, col, part) {
      return(
        flextable::fontsize(
          x = tbl,
          i = row,
          j = col,
          size = font_size,
          part = part
        )
      )
    }
  }

  if (!is.null(text_color)) {
    styles[[length(styles) + 1]] <- function(tbl, row, col, part) {
      return(
        flextable::color(
          x = tbl,
          i = row,
          j = col,
          color = text_color,
          part = part
        )
      )
    }
  }

  if (!is.null(background_color)) {
    styles[[length(styles) + 1]] <- function(tbl, row, col, part) {
      return(
        flextable::bg(
          x = tbl,
          i = row,
          j = col,
          bg = background_color,
          part = part
        )
      )
    }
  }

  if (bold) {
    styles[[length(styles) + 1]] <- function(tbl, row, col, part) {
      return(
        flextable::bold(
          x = tbl,
          i = row,
          j = col,
          part = part
        )
      )
    }
  }

  if (italic) {
    styles[[length(styles) + 1]] <- function(tbl, row, col, part) {
      return(
        flextable::italic(
          x = tbl,
          i = row,
          j = col,
          part = part
        )
      )
    }
  }

  if (!is.null(color_scale)) {
    styles[[length(styles) + 1]] <- create_color_scale_flex(
      color_scale = color_scale
    )
  }

  return(styles)
}

create_color_scale_flex <- function(color_scale) {
  fn <- create_color_scale_function(color_scale = color_scale)

  return(function(tbl, row, col, part) {
    tbl |>
      flextable::bg(i = row, j = col, bg = fn, part = part)
  })
}
