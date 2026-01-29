#' initialize_styles
#'
#' Internal function that initializes all styling objects of a tablespan table
#'
#' @param tbl tablespan table
#' @returns tablespan table with added styles field
#' @noRd
initialize_styles <- function(tbl) {
  tbl <- tbl |>
    default_styles()

  data <- extract_data(tbl)

  for (column_name in colnames(data)) {
    tbl <- style_column(
      tbl = tbl,
      columns = dplyr::all_of(column_name),
      rows = seq_len(length(data[[column_name]]))
    )
  }

  return(tbl)
}

#' default_styles
#'
#' Sets the default styles for a tablespan table.
#'
#' @param tbl tablespan object
#' @returns a list with default styles
#' @noRd
default_styles <- function(tbl) {
  tbl <- tbl |>
    style_title() |>
    style_subtitle() |>
    style_header() |>
    style_header_cells() |>
    style_footnote() |>
    style_hline() |>
    style_vline()

  return(tbl)
}

#' default_styles_flex
#'
#' Sets the default styles for flextable tables in a tablespan table.
#'
#' This function adds default styling for flextable tables to the provided default_styles list.
#' If the flextable package is not available, the original default_styles are returned unchanged.
#'
#' @param default_styles a list containing default styles for different table elements
#' @returns a list with default styles for flextable tables added to the input default_styles
#' @noRd
default_styles_flex <- function(default_styles) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    return(default_styles)
  }

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

#' style_title
#'
#' Set the style used for the title of the tablespan table.
#'
#' The styling for openxlsx and gt works differently:
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the title
#' - gt_style must be a list of gt::tab_style objects to be applied to the table
#'
#' @param tbl tablespan table
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param ... optional additional arguments. Currently not used
#' @returns the tablespan table with added styles
#' @export
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
#' if(require_gt(throw = FALSE))
#'   tbl |>
#'     style_title(bold = TRUE) |>
#'     as_gt()
style_title <- function(
  tbl,
  background_color = NULL,
  text_color = NULL,
  font_size = NULL,
  bold = FALSE,
  italic = FALSE,
  ...
) {
  tbl$styles$title <- c(
    list(
      background_color = background_color,
      text_color = text_color,
      font_size = font_size,
      bold = bold,
      italic = italic
    ),
    list(...)
  )
  return(tbl)
}

#' style_subtitle
#'
#' Set the style used for the subtitle of the tablespan table.
#'
#' The styling for openxlsx and gt works differently:
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the subtitle
#' - gt_style must be a list of gt::tab_style objects to be applied to the table
#'
#'
#' @param tbl tablespan table
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param ... optional additional arguments. Currently not used
#' @returns the tablespan table with added styles
#' @export
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
#' if(require_gt(throw = FALSE))
#' tbl |>
#'   style_subtitle(bold = TRUE) |>
#'   as_gt()
style_subtitle <- function(
  tbl,
  background_color = NULL,
  text_color = NULL,
  font_size = NULL,
  bold = FALSE,
  italic = FALSE,
  ...
) {
  tbl$styles$subtitle <- c(
    list(
      background_color = background_color,
      text_color = text_color,
      font_size = font_size,
      bold = bold,
      italic = italic
    ),
    list(...)
  )
  return(tbl)
}

#' style_header
#'
#' Set the style used for the header of the tablespan table.
#'
#' The styling for openxlsx and gt works differently:
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the header
#' - gt_style must be a list of gt::tab_style objects to be applied to the table
#'
#' @param tbl tablespan table
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param ... optional additional arguments. Currently not used
#' @returns the tablespan table with added styles
#' @export
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
#' if(require_gt(throw = FALSE))
#' tbl |>
#'   style_header(
#'     openxlsx_style = openxlsx::createStyle(
#'       fontSize = 8,
#'       fgFill = "#ffffff"),
#'     gt_style = list(gt::cell_text(size = 8))) |>
#'   as_gt()
style_header <- function(
  tbl,
  background_color = NULL,
  text_color = NULL,
  font_size = NULL,
  bold = FALSE,
  italic = FALSE,
  ...
) {
  tbl$styles$header <- c(
    list(
      background_color = background_color,
      text_color = text_color,
      font_size = font_size,
      bold = bold,
      italic = italic
    ),
    list(...)
  )
  return(tbl)
}

#' style_header_cells
#'
#' Set the style used for the cells in the openxlsx export. This function
#' is used to create the borders around cells in openxlsx.
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the header
#'
#'
#' @param tbl tablespan table
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param border_color set the color of the border for the header cells
#' @param top boolean. Set to TRUE to add a top border
#' @param bottom boolean. Set to TRUE to add a bottom border
#' @param left boolean. Set to TRUE to add a left border
#' @param right boolean. Set to TRUE to add a right border
#' @param ... optional additional arguments. Currently not used
#' @returns the tablespan table with added styles
#' @export
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
#' wb <- tbl |>
#'   style_header_cells(text_color = "#345364") |>
#'   as_excel()
#' # save workbook to see the effect
style_header_cells <- function(
  tbl,
  background_color = NULL,
  text_color = NULL,
  font_size = NULL,
  bold = FALSE,
  italic = FALSE,
  border_color = "#000000",
  top = FALSE,
  bottom = TRUE,
  left = TRUE,
  right = TRUE,
  ...
) {
  tbl$styles$header_cells <- c(
    list(
      background_color = background_color,
      text_color = text_color,
      font_size = font_size,
      bold = bold,
      italic = italic,
      border_color = border_color,
      top = top,
      bottom = bottom,
      left = left,
      right = right
    ),
    list(...)
  )
  return(tbl)
}

#' style_footnote
#'
#' Set the style used for the footnote of the tablespan table.
#'
#' The styling for openxlsx and gt works differently:
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the footnote
#' - gt_style must be a list of gt::tab_style objects to be applied to the table
#'
#' @param tbl tablespan table
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param ... optional additional arguments. Currently not used
#' @returns the tablespan table with added styles
#' @export
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
#' if(require_gt(throw = FALSE))
#' tbl |>
#'   style_footnote(bold = TRUE) |>
#'   as_gt()
style_footnote <- function(
  tbl,
  background_color = NULL,
  text_color = NULL,
  font_size = NULL,
  bold = FALSE,
  italic = FALSE,
  ...
) {
  tbl$styles$footnote <- c(
    list(
      background_color = background_color,
      text_color = text_color,
      font_size = font_size,
      bold = bold,
      italic = italic
    ),
    list(...)
  )
  return(tbl)
}

#' style_hline
#'
#' Set the style used for the horizontal lines of the tablespan table. Currently only
#' supported for excel export.
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the horizontal lines
#'
#' @param tbl tablespan table
#' @param color used for the border
#' @param ... optional additional arguments. Currently not used
#' @returns the tablespan table with added styles
#' @export
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
#' wb <- tbl |>
#'   style_hline(
#'     openxlsx_style = openxlsx::createStyle(
#'       border = "Top",
#'       borderColour = "#928505",
#'       borderStyle = "thin")) |>
#'   as_excel()
#' # save workbook to see effect
style_hline <- function(
  tbl,
  color = "#000000",
  ...
) {
  tbl$styles$hline <- c(
    list(
      color = color
    ),
    list(...)
  )
  return(tbl)
}

#' style_vline
#'
#' Set the style used for the vertical lines of the tablespan table. Currently only
#' supported for excel export.
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the vertical lines
#'
#' @param tbl tablespan table
#' @param color color used for the border
#' @param ... optional additional arguments. Currently not used
#' @returns the tablespan table with added styles
#' @export
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
#' wb <- tbl |>
#'   style_vline(
#'     openxlsx_style = openxlsx::createStyle(
#'       border = "Top",
#'       borderColour = "#928505",
#'       borderStyle = "thin")) |>
#'   as_excel()
#' # save workbook to see effect
style_vline <- function(
  tbl,
  color = "#000000",
  ...
) {
  tbl$styles$vline <- c(
    list(
      color = color
    ),
    list(...)
  )
  return(tbl)
}

#' style_column
#'
#' Change the style of a column or single cells within columns.
#'
#' @param tbl tablespan table
#' @param columns the columns to style. Must be a tidyselect selector expression (e.g., starts_with("hp_"))
#' @param rows indices of the rows which should be styled. When set to NULL, the style is applied to all rows
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param color_scale a named vector of length 2 or 3 to define a color scale. Example for two colors: color_scale = c("#EE2F43" = -1, "#37E65A" = 1).
#' Example for three colors: color_scale = c("#EE2F43" = -1, "#FFFFFF" = 0, "#37E65A" = 1). If a value is set as NA, it will be replaced with the minimum, mean, or maximum respectively
#' (e.g., color_scale = c("#EE2F43" = -1, "#FFFFFF" = 0, "#37E65A" = 1) will be replaced by color_scale = c("#EE2F43" = min(data), "#FFFFFF" = 0, "#37E65A" = max(data))).
#' NOTE: When exporting to gt, make sure to apply the color scale before you change the text color; otherwise, gt will overwrite the text color.
#' @param stack When set to TRUE, the style is added on top of the existing styles. This is mostly relevant
#' for openxlsx. When set to FALSE, the new style replaces all previous styling.
#' @param ... optional additional arguments. Currently not used
#' @returns the tablespan table with added styles
#' @export
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
#' if(require_gt(throw = FALSE))
#' tbl |>
#'   style_column(columns = mean_hp,
#'                bold = TRUE) |>
#'   as_gt()
style_column <- function(
  tbl,
  columns = dplyr::everything(),
  rows = NULL,
  background_color = NULL,
  text_color = NULL,
  font_size = NULL,
  bold = FALSE,
  italic = FALSE,
  color_scale = NULL,
  stack = TRUE,
  ...
) {
  columns_expr <- rlang::enquo(columns)
  data <- extract_data(tbl)

  if (is.null(rows)) {
    rows <- 1:nrow(data)
  }

  column_names <- data |>
    dplyr::select(!!columns_expr) |>
    colnames()

  color_scale <- preprocess_color_scale(
    tbl = tbl,
    color_scale = color_scale,
    column_names = column_names,
    rows = rows
  )

  for (column_name in column_names) {
    if (stack) {
      tbl$styles$columns[[column_name]] <- append(
        tbl$styles$columns[[column_name]],
        list(list(
          "style" = c(
            list(
              background_color = background_color,
              text_color = text_color,
              font_size = font_size,
              bold = bold,
              italic = italic,
              color_scale = color_scale
            ),
            list(...)
          ),
          "rows" = rows
        ))
      )
    } else {
      tbl$styles$columns[[column_name]] <- list(list(
        style = c(
          list(
            background_color = background_color,
            text_color = text_color,
            font_size = font_size,
            bold = bold,
            italic = italic,
            color_scale = color_scale
          ),
          list(...)
        ),
        rows = rows
      ))
    }
  }

  return(tbl)
}
