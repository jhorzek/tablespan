#' tbl_styles
#'
#' Define styles for different elements of the table.
#'
#' @param merge_rownames boolean: Should adjacent rows with identical names be merged?
#' @param merged_rownames_style style applied to the merged rownames
#' @param footnote_style style applied to the table footnote
#' @param data_styles styles applied to the columns in the data set based on their
#' classes (e.g., numeric, character, etc.). data_styles must be a list of lists.
#' Each inner list must have two elements: a "test" that is used to determine the
#' class of a data column (e.g., is.double) and a style that is then applied to
#' the columns where the test returns TRUE. Note that styles will be applied in the
#' order of the list, meaning that a later style may overwrite an earlier style.
#' @param cell_styles an optional list with styles for selected cells in the
#' data frame. See ?cell_style.
#' @param bg_default default color for the background of the table
#' @param bg_title background color for the title
#' @param bg_subtitle background color for the subtitle
#' @param bg_header_lhs background color for the left hand side of the table header
#' @param bg_header_rhs background color for the right hand side of the table header
#' @param bg_rownames background color for the row names
#' @param bg_data background color for the data
#' @param bg_footnote background color for the footnote
#' @param vline styling for all vertical lines added to the table
#' @param hline styling for all horizontal lines added to the table
#' @param cell_default default style added to cells in the table
#' @param cell_title style added to title cells in the table
#' @param cell_subtitle style added to subtitle cells in the table
#' @param cell_header_lhs style added to the left hand side of the header cells in the table
#' @param cell_header_rhs style added to the right hand side of the header cells in the table
#' @param cell_rownames style added to row name cells in the table
#' @param cell_data style added to data cells in the table
#' @param cell_footnote style added to footnote cells in the table
#' @importFrom openxlsx createStyle
#' @returns a list with styles for different elements of the table
#' @export
#' @examples
#' tbl_styles()
tbl_styles <- function(
    bg_default = openxlsx::createStyle(fgFill = "#ffffff"),
    bg_title = bg_default,
    bg_subtitle = bg_default,
    bg_header_lhs = bg_default,
    bg_header_rhs = bg_default,
    bg_rownames = bg_default,
    bg_data = bg_default,
    bg_footnote = bg_default,

    vline = openxlsx::createStyle(border = "Left",
                                  borderColour = "#000000",
                                  borderStyle = "thin"),

    hline = openxlsx::createStyle(border = "Top",
                                  borderColour = "#000000",
                                  borderStyle = "thin"),

    cell_default = openxlsx::createStyle(fontSize = 11),
    cell_title = openxlsx::createStyle(fontSize = 14,
                                       halign = "left",
                                       textDecoration = "bold"),
    cell_subtitle = openxlsx::createStyle(fontSize = 11,
                                          halign = "left",
                                          textDecoration = "bold"),
    cell_header_lhs = openxlsx::createStyle(fontSize = 11,
                                            halign = "center",
                                            border = "BottomLeftRight",
                                            borderColour = "#000000",
                                            borderStyle = "thin",
                                            textDecoration = "bold"),
    cell_header_rhs = openxlsx::createStyle(fontSize = 11,
                                            halign = "center",
                                            border = "BottomLeftRight",
                                            borderColour = "#000000",
                                            borderStyle = "thin",
                                            textDecoration = "bold"),
    cell_rownames = cell_default,
    cell_data = cell_default,
    cell_footnote = openxlsx::createStyle(fontSize = 11,
                                          halign = "left"),

    merge_rownames = TRUE,
    merged_rownames_style = openxlsx::createStyle(valign = "top"),
    footnote_style = openxlsx::createStyle(fontSize = 11,
                                           halign = "left"),
    data_styles = create_data_styles(),
    cell_styles = NULL){
  if(!is.null(cell_styles)){
    if(!is.list(cell_styles))
      stop("cell_styles must be a list.")
  }
  return(as.list(environment()))
}

#' cell_style
#'
#' @param rows indices of the rows to which the style should be applied
#' @param colnames names of the columns to which the style should be applied
#' @param style style created with openxlsx::createStyle() that will be applied to
#' the selected cells
#' @param gridExpand see ?openxlsx::addStyle: Apply style only to the selected
#' elements (gridExpand = FALSE, default) or to all combinations?
#' @param stack should the style be added to existing styles (TRUE) or overwrite
#' existing styles (FALSE)
#' @returns list with specified styles
#' @importFrom methods is
#' @export
#' @examples
#' library(tablespan)
#' data("iris")
#'
#' tbl <- tablespan(data = iris[iris$Species == "setosa", ],
#'           formula = Species ~ (Sepal = Sepal.Length + Sepal.Width) +
#'             (Petal = (Width = Petal.Length) + Petal.Width))
#'
#'
#' # To apply a custom style to some elements use the styles argument. The following
#' # applies the "bold" style to the rows 1-5 of the Sepal.Length column and
#' # the rows 9-10 of the Petal.Width column.
#' bold <- openxlsx::createStyle(textDecoration = "bold")
#'
#' wb <- as_excel(tbl = tbl,
#'                styles = tbl_styles(cell_styles = list(cell_style(rows = 1:5,
#'                                                                 colnames = "Sepal.Length",
#'                                                                 style = bold),
#'                                                      cell_style(rows = 9:10,
#'                                                                 colnames = "Petal.Width",
#'                                                                 style = bold))))
#' # saveWorkbook(wb, "iris.xlsx")
cell_style <- function(rows,
                       colnames,
                       style,
                       gridExpand = FALSE,
                       stack = TRUE){
  if(!is(style, "Style"))
    stop("style must be a Style created with openxlsx::createStyle.")
  if(!is.numeric(rows))
    stop("rows must be numeric")
  if(!is.character(colnames))
    stop("colnames must be characters.")

  return(list(rows = rows,
              colnames = colnames,
              style = style,
              gridExpand = gridExpand,
              stack = stack))
}


#' create_data_styles
#'
#' This function sets some defaults for data_styles. See ?tbl_styles
#'
#' Styles are applied to the columns in the data set based on their
#' classes (e.g., numeric, character, etc.). data_styles must be a list of lists.
#' Each inner list must have two elements: a "test" that is used to determine the
#' class of a data colum (e.g., is.double) and a style that is then applied to
#' the columns where the test returns TRUE. Note that styles will be applied in the
#' order of the list, meaning that a later style may overwrite an earlier style.
#'
#' @param double style for columns of type double
#' @param integer style for columns of type integer
#' @param ... add further styles
#' @returns a list of lists with styles
#' @import openxlsx
#' @export
#' @examples
#' library(tablespan)
#' # Make all booleans bold:
#' create_data_styles(boolean = list(test = is.logical,
#'                    style = openxlsx::createStyle(textDecoration = "bold")))
create_data_styles <- function(double = list(test = is.double,
                                             style = openxlsx::createStyle(numFmt = "0.00")),
                               integer = list(test = is.integer,
                                              style = openxlsx::createStyle(numFmt = "0")),
                               ...){
  return(list(double = double,
              integer = integer,
              ...))
}

#' style_color
#'
#' Provides a simple way to define a color scheme for tables. By default, tables
#' have a "light" theme, where the background is white and text / lines are black.
#' Based on a primary color, style_color will create tables that use the primary
#' color as background for all title, header, and row name cells and adapts the
#' text color based on the primary color. The automatic adaption of the
#' background color is implemented based on Mark Ransom and SudoPlz at
#' <https://stackoverflow.com/questions/3942878/how-to-decide-font-color-in-white-or-black-depending-on-background-color>
#'
#' @param primary_color color to be used for the title, header, and row names
#' background. This must be a hex code for the color.
#'
#' @return a list with styling options
#' @export
#'
#' @examples
#' library(tablespan)
#' library(dplyr)
#' data("mtcars")
#'
#' # First summarize the data:
#' summarized_table <- mtcars |>
#'   group_by(cyl, vs) |>
#'   summarise(N = n(),
#'             mean_hp = mean(hp),
#'             sd_hp = sd(hp),
#'             mean_wt = mean(wt),
#'             sd_wt = sd(wt))
#'
#' # Now, we want to create a table, where we show the grouping variables
#' # as row names and also create spanners for the horse power (hp) and the
#' # weight (wt) variables:
#' tbl <- tablespan(data = summarized_table,
#'           formula = Cylinder:cyl + Engine:vs ~
#'             N +
#'             (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
#'             (`Weight` = Mean:mean_wt + SD:sd_wt),
#'           title = "Motor Trend Car Road Tests",
#'           subtitle = "A table created with tablespan",
#'           footnote = "Data from the infamous mtcars data set.")
#'
#' # We can save this table with the default color scheme:
#' wb <- as_excel(tbl = tbl)
#'
#' # Or adapt the color scheme to our liking:
#' wb <- as_excel(tbl = tbl,
#'                styles = style_color(primary_color = "#2e9199"))
#'
#' # Create the excel table:
#' # openxlsx::saveWorkbook(wb,
#' #                        file = "cars.xlsx",
#' #                        overwrite = TRUE)
style_color <- function(primary_color ="#ffffff"){
  if(!grepl(pattern = "^#?([a-f0-9]{3}|[a-f0-9]{6})$",
            x = primary_color))
    stop("primary_color must be a hex code")
  text_color <- get_text_color(primary_color = primary_color)

  line_color <- ifelse(text_color == "#000000",
                       "#000000",
                       primary_color)

  return(
    tbl_styles(
      bg_default = openxlsx::createStyle(fgFill = "#ffffff"),
      bg_title = openxlsx::createStyle(fgFill = primary_color),
      bg_subtitle = openxlsx::createStyle(fgFill = primary_color),
      bg_header_lhs = openxlsx::createStyle(fgFill = primary_color),
      bg_header_rhs = openxlsx::createStyle(fgFill = primary_color),
      bg_rownames = openxlsx::createStyle(fgFill = primary_color),
      bg_data = openxlsx::createStyle(fgFill = "#ffffff"),
      bg_footnote = openxlsx::createStyle(fgFill = "#ffffff"),

      vline = openxlsx::createStyle(border = "Left",
                                    borderColour = line_color,
                                    borderStyle = "thin"),

      hline = openxlsx::createStyle(border = "Top",
                                    borderColour = line_color,
                                    borderStyle = "thin"),

      cell_default = openxlsx::createStyle(fontSize = 11),
      cell_title = openxlsx::createStyle(fontSize = 14,
                                         halign = "left",
                                         textDecoration = "bold",
                                         fontColour = text_color),
      cell_subtitle = openxlsx::createStyle(fontSize = 11,
                                            halign = "left",
                                            textDecoration = "bold",
                                            fontColour = text_color),
      cell_header_lhs = openxlsx::createStyle(fontSize = 11,
                                              halign = "center",
                                              border = "BottomLeftRight",
                                              borderColour = text_color,
                                              borderStyle = "thin",
                                              textDecoration = "bold",
                                              fontColour = text_color),
      cell_header_rhs = openxlsx::createStyle(fontSize = 11,
                                              halign = "center",
                                              border = "BottomLeftRight",
                                              borderColour = text_color,
                                              borderStyle = "thin",
                                              textDecoration = "bold",
                                              fontColour = text_color),
      cell_rownames = openxlsx::createStyle(fontSize = 11,
                                            fontColour = text_color),
      cell_data = openxlsx::createStyle(fontSize = 11),
      cell_footnote = openxlsx::createStyle(fontSize = 11,
                                            halign = "left"),

      merge_rownames = TRUE,
      merged_rownames_style = openxlsx::createStyle(valign = "top"),
      footnote_style = openxlsx::createStyle(fontSize = 11,
                                             halign = "left"),
      data_styles = create_data_styles(),
      cell_styles = NULL)
  )
}

#' get_text_color
#'
#' Determines if the text should be black or white based on the formula
#' from Mark Ransom and SudoPlz at
# <https://stackoverflow.com/questions/3942878/how-to-decide-font-color-in-white-or-black-depending-on-background-color>
#'
#' @param primary_color color to be used for the title, header, and row names
#' background.
#'
#' @return back or white as hex code
#' @importFrom grDevices col2rgb
#' @noRd
#'
#' @examples
#' tablespan:::get_text_color("#ffffff")
get_text_color <- function(primary_color){
  rgb_colors <- as.vector(col2rgb(primary_color))
  # scale colors to be between 0 and 1:
  rgb_colors <- rgb_colors/255
  rgb_colors <- sapply(rgb_colors, function(x) ifelse(x <= 0.03928,
                                                      x/12.92,
                                                      ((x + .055) / 1.055)^2.4))
  luminance <- (0.2126 * rgb_colors[1]) + (0.7152 * rgb_colors[2]) + (0.0722 * rgb_colors[3])

  if(luminance <= .1769)
    return("#ffffff")
  return("#000000")
}
