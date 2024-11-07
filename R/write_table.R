#' write_bt
#'
#' Write a basic table to an excel workbook.
#'
#' @param tbl table created with basicTables::bt
#' @param workbook Excel workbook created with openxlsx::createWorkbook()
#' @param sheet name of the sheet to which the table should be written to
#' @param start_row row at which to start the table
#' @param start_col column at which to start the table
#' @param header_style openxlsx style for the header
#' @import openxlsx
#' @export
#' @examples
#' library(basicTables)
#' data("iris")
#'
#' tbl <- bt(data = iris[iris$Species == "setosa", ],
#'           formula = Species ~ (Sepal = Sepal.Length + Sepal.Width) +
#'             (Petal = (Width = Petal.Length) + Petal.Width))
#'
#' wb <- write_bt(tbl = tbl)
#'
#' # saveWorkbook(wb, "iris.xlsx")
write_bt <- function(tbl,
                     workbook = openxlsx::createWorkbook(),
                     sheet = "BasicTable",
                     start_row = 1,
                     start_col = 1,
                     header_style = openxlsx::createStyle(fontSize = 14,
                                                          halign = "center",
                                                          border = "Bottom",
                                                          borderColour = openxlsx_getOp("borderColour", "black"),
                                                          borderStyle = openxlsx_getOp("borderStyle", "double"),
                                                          textDecoration = "bold")){

  if(is.character(sheet) && !(sheet %in% workbook$sheet_names)){
    openxlsx::addWorksheet(sheetName = sheet,
                           wb = workbook)
  }

  workbook <- write_header(workbook = workbook,
                           sheet = sheet,
                           header = tbl$header,
                           table_data = tbl$table_data,
                           start_row = start_row,
                           start_col = start_col,
                           header_style = header_style)

  write_data(workbook = workbook,
             sheet = sheet,
             header = tbl$header,
             table_data = tbl$table_data,
             start_row = start_row,
             start_col = start_col)

  # write_footer

  return(workbook)
}

#' @import openxlsx
write_header <- function(workbook,
                         sheet,
                         header,
                         table_data,
                         start_row,
                         start_col,
                         header_style){



  if(!is.null(header$lhs)){

    max_level <- max(header$lhs$level, header$rhs$level)

    write_header_entry(workbook = workbook,
                       sheet = sheet,
                       header_entry = header$lhs,
                       max_level = max_level,
                       start_row = start_row,
                       start_col = start_col,
                       header_style = header_style)

    # Separate row names and data with a vertical bar
    sty <- openxlsx::createStyle(border = "Right",
                                 borderColour = openxlsx_getOp("borderColour", "black"),
                                 borderStyle = openxlsx_getOp("borderStyle", "double"))
    openxlsx::addStyle(wb = workbook,
                       sheet = sheet,
                       style = sty,
                       rows = (start_row):(start_row + max(header$lhs$level, header$rhs$level)),
                       cols = start_col + header$lhs$width - 1,
                       stack = TRUE)

    start_data_col <- start_col + header$lhs$width
  }else{
    max_level <- header$rhs$level
    start_data_col <- start_col
  }
  write_header_entry(workbook = workbook,
                     sheet = sheet,
                     header_entry = header$rhs,
                     max_level = max_level,
                     start_row = start_row,
                     start_col = start_data_col,
                     header_style = header_style)

}

write_header_entry <- function(workbook,
                               sheet,
                               header_entry,
                               max_level,
                               start_row,
                               start_col,
                               header_style){

  # write current entry name into table
  if(header_entry$name != "_BASE_LEVEL_"){
    openxlsx::writeData(wb = workbook,
                        sheet = sheet,
                        x = header_entry$name,
                        startRow = start_row + (max_level - header_entry$level) - 1,
                        startCol = start_col)

    if(header_entry$width > 1)
      openxlsx::mergeCells(wb = workbook,
                           sheet = sheet,
                           cols = start_col:(start_col + header_entry$width - 1),
                           rows = start_row + (max_level - header_entry$level) - 1)
    openxlsx::addStyle(wb = workbook,
                       sheet = sheet,
                       style = header_style,
                       rows = start_row + (max_level - header_entry$level) - 1,
                       cols = start_col:(start_col + header_entry$width - 1),
                       gridExpand = TRUE,
                       stack = TRUE)
  }

  # entries may have sub-entries, that also have to be written down
  start_col_entry <- start_col
  for(entry in header_entry$entries){
    write_header_entry(workbook = workbook,
                       sheet = sheet,
                       header_entry = entry,
                       max_level = max_level,
                       start_row = start_row,
                       start_col = start_col_entry,
                       header_style = header_style)
    start_col_entry <- start_col_entry + entry$width
  }
  return(workbook)
}

write_data <- function(workbook,
                       sheet,
                       header,
                       table_data,
                       start_row,
                       start_col,
                       header_style){

  if(!is.null(header$lhs)){
    # We need to skip as many rows as there are levels to our header
    start_data_row <- start_row + max(header$lhs$level, header$rhs$level) - 1
  }else{
    start_data_row <- start_row + header$rhs$level - 1
  }

  if(!is.null(header$lhs)){
    # Row names
    openxlsx::writeData(wb = workbook,
                        sheet = sheet,
                        x = table_data$row_data,
                        startCol = start_col,
                        startRow = start_data_row,
                        rowNames = FALSE,
                        colNames = FALSE)

    # Separate row names and data with a vertical bar
    sty <- openxlsx::createStyle(border = "Right",
                                 borderColour = openxlsx_getOp("borderColour", "black"),
                                 borderStyle = openxlsx_getOp("borderStyle", "double"))
    openxlsx::addStyle(wb = workbook,
                       sheet = sheet,
                       style = sty,
                       rows = start_data_row:(start_data_row + nrow(table_data$row_data) - 1),
                       cols = start_col + ncol(table_data$row_data) - 1,
                       stack = TRUE)

    # We need to skip as many columns as there are row name elements
    start_data_col <- start_col + ncol(table_data$row_data)

  }else{
    start_data_col <- start_col
  }

  openxlsx::writeData(wb = workbook,
                      sheet = sheet,
                      x = table_data$col_data,
                      startCol = start_data_col,
                      startRow = start_data_row,
                      rowNames = FALSE,
                      colNames = FALSE)
}
