#' bt_styles
#'
#' Define styles for different elements of the table.
#'
#' @param background_style color etc. for the entire background of the table
#' @param hline_style style for the horizontal lines used in the table. Note:
#' the style for the lines under spanners is defined in the title_style.
#' @param vline_style style for the vertical lines used in the table. Note:
#' the style for the lines under spanners is defined in the title_style.
#' @param title_style style applied to the table title
#' @param title_style style applied to the table subtitle
#' @param header_style style applied to the table header (column names)
#' @param footnote_style style applied to the table footnote
#' @importFrom openxlsx createStyle
#' @export
#' @examples
#' bt_styles()
bt_styles <- function(
    background_style = openxlsx::createStyle(fgFill = "#ffffff"),
    hline_style = openxlsx::createStyle(border = "Bottom",
                                        borderColour = openxlsx::openxlsx_getOp("borderColour", "black"),
                                        borderStyle = openxlsx::openxlsx_getOp("borderStyle", "double")),
    vline_style = openxlsx::createStyle(border = "Right",
                                        borderColour = openxlsx::openxlsx_getOp("borderColour", "black"),
                                        borderStyle = openxlsx::openxlsx_getOp("borderStyle", "double")),
    title_style = openxlsx::createStyle(fontSize = 14,
                                        halign = "left",
                                        textDecoration = "bold"),
    subtitle_style = openxlsx::createStyle(fontSize = 14,
                                           halign = "left",
                                           textDecoration = "bold"),
    header_style = openxlsx::createStyle(fontSize = 14,
                                         halign = "center",
                                         border = "BottomLeftRight",
                                         borderColour = openxlsx_getOp("borderColour", "black"),
                                         borderStyle = openxlsx_getOp("borderStyle", "double"),
                                         textDecoration = "bold"),
    footnote_style = openxlsx::createStyle(fontSize = 12,
                                           halign = "left")){

  return(list(background_style = background_style,
              hline_style = hline_style,
              vline_style = vline_style,
              title_style = title_style,
              subtitle_style = subtitle_style,
              header_style = header_style,
              footnote_style = footnote_style))
}
