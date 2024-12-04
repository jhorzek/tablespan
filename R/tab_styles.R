#' tbl_styles
#'
#' Define styles for different elements of the table.
#'
#' @param background_style color etc. for the entire background of the table
#' @param hline_style style for the horizontal lines used in the table. Note:
#' the style for the lines under spanners is defined in the title_style.
#' @param vline_style style for the vertical lines used in the table. Note:
#' the style for the lines under spanners is defined in the title_style.
#' @param title_style style applied to the table title
#' @param subtitle_style style applied to the table subtitle
#' @param header_style style applied to the table header (column names)
#' @param merge_rownames boolean: Should adjacent rows with identical names be merged?
#' @param merged_rownames_style style applied to the merged rownames
#' @param footnote_style style applied to the table footnote
#' @param data_styles styles applied to the columns in the data set based on their
#' classes (e.g., numeric, character, etc.). data_styles must be a list of lists.
#' Each inner list must have two elements: a "test" that is used to determine the
#' class of a data colum (e.g., is.double) and a style that is then applied to
#' the columns where the test returns TRUE. Note that styles will be applied in the
#' order of the list, meaning that a later style may overwrite an earlier style.
#' @param cell_styles an optional list with styles for selected cells in the
#' data frame.
#' @importFrom openxlsx createStyle
#' @returns a list with styles for different elements of the table
#' @export
#' @examples
#' tbl_styles()
tbl_styles <- function(
    background_style = openxlsx::createStyle(fgFill = "#ffffff"),
    hline_style = openxlsx::createStyle(border = "Top",
                                        borderColour = openxlsx::openxlsx_getOp("borderColour", "black"),
                                        borderStyle = openxlsx::openxlsx_getOp("borderStyle", "double")),
    vline_style = openxlsx::createStyle(border = "Left",
                                        borderColour = openxlsx::openxlsx_getOp("borderColour", "black"),
                                        borderStyle = openxlsx::openxlsx_getOp("borderStyle", "double")),
    title_style = openxlsx::createStyle(fontSize = 14,
                                        halign = "left",
                                        textDecoration = "bold"),
    subtitle_style = openxlsx::createStyle(fontSize = 11,
                                           halign = "left",
                                           textDecoration = "bold"),
    header_style = openxlsx::createStyle(fontSize = 11,
                                         halign = "center",
                                         border = "BottomLeftRight",
                                         borderColour = openxlsx::openxlsx_getOp("borderColour", "black"),
                                         borderStyle = openxlsx::openxlsx_getOp("borderStyle", "double"),
                                         textDecoration = "bold"),
    merge_rownames = TRUE,
    merged_rownames_style = createStyle(valign = "top"),
    footnote_style = openxlsx::createStyle(fontSize = 11,
                                           halign = "left"),
    data_styles = create_data_styles(),
    cell_styles = NULL){

  if(!is.null(cell_styles)){
    if(!is.list(cell_styles))
      stop("cell_styles must be a list.")
  }

  return(list(background_style = background_style,
              hline_style = hline_style,
              vline_style = vline_style,
              title_style = title_style,
              subtitle_style = subtitle_style,
              header_style = header_style,
              merge_rownames = merge_rownames,
              merged_rownames_style = merged_rownames_style,
              footnote_style = footnote_style,
              data_styles = data_styles,
              cell_styles = cell_styles))
}

#' cell_style
#'
#' @param rows indices of the rows to which the style should be applied
#' @param colnames names of the columns to which the style should be applied
#' @param style style created with openxlsx::createStyle() that will be applied to
#' the selected cells
#' @param gridExpand see ?openxlsx::addStyle: Apply style only to the selected
#' elements (set gridExpand = FALSE) or to all combinations?
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
                       colnames, style, gridExpand = TRUE, stack = TRUE){
  if(!is(style, "Style"))
    stop("style must be a Style created with openxlsx::createStyle")
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
