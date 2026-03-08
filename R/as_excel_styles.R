#' default_styles_openxlsx
#'
#' Sets the default styles for openxlsx tables in a tablespan table.
#'
#' This function adds default styling for openxlsx tables to the provided default_styles list.
#' If the openxlsx package is not available, the original default_styles are returned unchanged.
#'
#' @returns a list with default styles for openxlsx tables added to the input default_styles
#' @noRd
default_styles_openxlsx <- function() {
  require_openxlsx()

  default_styles <- list()

  default <- list(function(tbl, row, col) {
    return(tbl)
  })

  default_styles$title$openxlsx <- list(openxlsx::createStyle(
    fgFill = NULL,
    textDecoration = "bold",
    fontSize = 14
  ))
  default_styles$subtitle$openxlsx <- list(openxlsx::createStyle(
    fgFill = NULL,
    textDecoration = "bold"
  ))
  default_styles$header$openxlsx <- list(openxlsx::createStyle(
    fgFill = NULL,
    textDecoration = "bold"
  ))
  default_styles$header_cells$openxlsx <- list(openxlsx::createStyle(
    fontSize = 11,
    halign = "center",
    border = "BottomLeftRight",
    borderColour = "#000000",
    borderStyle = "thin",
    textDecoration = "bold"
  ))
  default_styles$footnote$openxlsx <- list(
    openxlsx::createStyle(fgFill = "#ffffff")
  )
  default_styles$hline$openxlsx <- list(openxlsx::createStyle(
    border = "Top",
    borderColour = "#000000",
    borderStyle = "thin"
  ))
  default_styles$vline$openxlsx <- list(openxlsx::createStyle(
    border = "Left",
    borderColour = "#000000",
    borderStyle = "thin"
  ))
  return(default_styles)
}

style_title_openxlsx <- function(xlsx_styles, tbl) {
  require_openxlsx()

  force(tbl)

  xlsx_styles$title$openxlsx[[
    length(xlsx_styles$title$openxlsx) + 1
  ]] <- create_style_openxlsx(
    font_size = tbl$styles$title$font_size,
    text_color = tbl$styles$title$text_color,
    bold = tbl$styles$title$bold,
    italic = tbl$styles$title$italic,
    background_color = tbl$styles$title$background_color
  )

  return(xlsx_styles)
}

style_subtitle_openxlsx <- function(xlsx_styles, tbl) {
  require_openxlsx()

  force(tbl)

  xlsx_styles$subtitle$openxlsx[[
    length(xlsx_styles$subtitle$openxlsx) + 1
  ]] <- create_style_openxlsx(
    font_size = tbl$styles$subtitle$font_size,
    text_color = tbl$styles$subtitle$text_color,
    bold = tbl$styles$subtitle$bold,
    italic = tbl$styles$subtitle$italic,
    background_color = tbl$styles$subtitle$background_color
  )

  return(xlsx_styles)
}

style_header_openxlsx <- function(xlsx_styles, tbl) {
  require_openxlsx()

  force(tbl)

  xlsx_styles$header$openxlsx[[
    length(xlsx_styles$header$openxlsx) + 1
  ]] <- create_style_openxlsx(
    font_size = tbl$styles$header$font_size,
    text_color = tbl$styles$header$text_color,
    bold = tbl$styles$header$bold,
    italic = tbl$styles$header$italic,
    background_color = tbl$styles$header$background_color
  )

  return(xlsx_styles)
}

style_header_cells_openxlsx <- function(xlsx_styles, tbl) {
  require_openxlsx()

  force(tbl)

  border <- ifelse(tbl$styles$header_cells$bottom, "Bottom", "")
  border <- paste0(border, ifelse(tbl$styles$header_cells$left, "Left", ""))
  border <- paste0(border, ifelse(tbl$styles$header_cells$right, "Right", ""))
  border <- paste0(border, ifelse(tbl$styles$header_cells$top, "Top", ""))

  xlsx_styles$header_cells$openxlsx[[
    length(xlsx_styles$header_cells$openxlsx) + 1
  ]] <- openxlsx::createStyle(
    fontSize = tbl$styles$header_cells$font_size,
    fontColour = tbl$styles$header_cells$text_color,
    halign = "center",
    border = border,
    borderColour = tbl$styles$header_cells$border_color,
    borderStyle = "thin",
    textDecoration = if (tbl$styles$header_cells$bold) "bold" else NULL,
    fgFill = tbl$styles$header_cells$background_color
  )

  return(xlsx_styles)
}

style_footnote_openxlsx <- function(xlsx_styles, tbl) {
  require_openxlsx()

  force(tbl)

  xlsx_styles$footnote$openxlsx[[
    length(xlsx_styles$footnote$openxlsx) + 1
  ]] <- create_style_openxlsx(
    font_size = tbl$styles$footnote$font_size,
    text_color = tbl$styles$footnote$text_color,
    bold = tbl$styles$footnote$bold,
    italic = tbl$styles$footnote$italic,
    background_color = tbl$styles$footnote$background_color
  )

  return(xlsx_styles)
}

style_hline_openxlsx <- function(xlsx_styles, tbl) {
  require_openxlsx()

  force(tbl)

  xlsx_styles$hline$openxlsx[[
    length(xlsx_styles$hline$openxlsx) + 1
  ]] <- openxlsx::createStyle(
    border = "Top",
    borderColour = tbl$styles$hline$color,
    borderStyle = "thin"
  )

  return(xlsx_styles)
}

style_vline_openxlsx <- function(xlsx_styles, tbl) {
  require_openxlsx()

  force(tbl)

  xlsx_styles$vline$openxlsx[[
    length(xlsx_styles$vline$openxlsx) + 1
  ]] <- openxlsx::createStyle(
    border = "Left",
    borderColour = tbl$styles$vline$color,
    borderStyle = "thin"
  )

  return(xlsx_styles)
}

style_column_openxlsx <- function(xlsx_styles, tbl) {
  require_openxlsx()

  force(tbl)

  column_names <- names(tbl$styles$columns)
  for (column_name in column_names) {
    xlsx_styles$columns[[column_name]] <- list()
    for (column_style in tbl$styles$columns[[column_name]]) {
      xlsx_styles$columns[[column_name]][[
        length(xlsx_styles$columns[[column_name]]) + 1
      ]] <-
        list(
          style = list(
            openxlsx = create_style_openxlsx(
              font_size = column_style$style$font_size,
              text_color = column_style$style$text_color,
              bold = column_style$style$bold,
              italic = column_style$style$italic,
              background_color = column_style$style$background_color
            )
          ),
          rows = column_style$rows
        )
      if (!is.null(column_style$style$color_scale)) {
        xlsx_styles$columns[[column_name]][[
          length(xlsx_styles$columns[[column_name]]) + 1
        ]] <-
          list(
            style = list(
              openxlsx = create_color_scale_openxlsx(
                color_scale = column_style$style$color_scale
              )
            ),
            rows = column_style$rows
          )
      }
    }
  }

  return(xlsx_styles)
}

#' create_style_openxlsx
#'
#' Create a new style to be applied to the body of the table.
#'
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @noRd
#' @examples
#' library(tablespan)
#' library(dplyr)
#' data("mtcars")
#'
#' # We want to report the following table:
#' summarized_table <- mtcars |>
#'   group_by(cyl, vs) |>
#'   summarise(N = n(),
#'             mean_hp = mean(hp),
#'             sd_hp = sd(hp),
#'             mean_wt = mean(wt),
#'             sd_wt = sd(wt))
#'
#' # Create a tablespan:
#' tbl <- tablespan(data = summarized_table,
#'                  formula = Cylinder:cyl + Engine:vs ~
#'                    N +
#'                    (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
#'                    (`Weight` = Mean:mean_wt + SD:sd_wt),
#'                  title = "Motor Trend Car Road Tests",
#'                  subtitle = "A table created with tablespan",
#'                  footnote = "Data from the infamous mtcars data set.")
#'
#' if(require_openxlsx(throw = FALSE))
#' tbl |>
#'   style_column(columns = mean_hp,
#'                bold = TRUE) |>
#'   as_excel()
create_style_openxlsx <- function(
  font_size,
  text_color,
  bold,
  italic,
  background_color
) {
  require_openxlsx()

  textDecoration <- NULL
  if (bold) {
    textDecoration <- c("Bold")
  }
  if (italic) {
    textDecoration <- c(textDecoration, "italic")
  }
  openxlsx_style <- openxlsx::createStyle(
    fontSize = font_size,
    fontColour = text_color,
    fgFill = background_color,
    textDecoration = textDecoration
  )

  return(openxlsx_style)
}

#' create_color_scale_openxlsx
#'
#' Create a color scale style for openlslx
#' @param color_scale vector with two or three color values
#' @returns openlslx style object
#' @noRd
create_color_scale_openxlsx <- function(color_scale) {
  require_openxlsx()

  if (is.null(color_scale)) {
    return(NULL)
  }

  return(
    function(wb, sheet, rows, cols) {
      openxlsx::conditionalFormatting(
        wb = wb,
        sheet = sheet,
        cols = cols,
        rows = rows,
        type = "colourScale",
        rule = color_scale,
        style = names(color_scale)
      )
    }
  )
}
