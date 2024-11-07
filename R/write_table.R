#' write_bt
#'
#' Write a basic table to an excel workbook.
#'
#' @param tbl table created with basicTables::bt
#' @param workbook Excel workbook created with openxlsx::createWorkbook()
#' @param sheet name of the sheet to which the table should be written to
#' @param start_row row at which to start the table
#' @param start_col column at which to start the table
#' @param styles openxlsx style for the different table elements (see ?basicTables::bt_styles)
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
                     styles = bt_styles()){

  if(is.character(sheet) && !(sheet %in% workbook$sheet_names)){
    openxlsx::addWorksheet(sheetName = sheet,
                           wb = workbook)
  }

  locations <- get_locations(tbl = tbl,
                             start_row = start_row,
                             start_col = start_col)

  fill_background(tbl = tbl,
                  workbook = workbook,
                  sheet = sheet,
                  locations = locations,
                  styles = styles)

  write_title(tbl = tbl,
              workbook = workbook,
              sheet = sheet,
              locations = locations,
              styles = styles)

  write_header(workbook = workbook,
               sheet = sheet,
               header = tbl$header,
               table_data = tbl$table_data,
               locations = locations,
               styles = styles)

  write_data(workbook = workbook,
             sheet = sheet,
             header = tbl$header,
             table_data = tbl$table_data,
             locations = locations,
             styles = styles)

  write_footnote(tbl = tbl,
                 workbook = workbook,
                 sheet = sheet,
                 locations = locations,
                 styles = styles)

  return(workbook)
}

fill_background <- function(tbl,
                            workbook,
                            sheet,
                            locations,
                            styles){
  # To fill the background, we have to find the dimensions
  # of the tabel first.
  min_row <- locations$row |> unlist() |> min()
  max_row <- locations$row |> unlist() |> max()
  min_col <- locations$col |> unlist() |> min()
  max_col <- locations$col |> unlist() |> max()

  openxlsx::addStyle(wb = workbook,
                     sheet = sheet,
                     style = styles$background_style,
                     rows = min_row:max_row,
                     cols = min_col:max_col,
                     gridExpand = TRUE,
                     stack = TRUE)
}

write_title <- function(tbl,
                        workbook,
                        sheet,
                        locations,
                        styles){

  if(!is.null(tbl$title)){
    openxlsx::writeData(wb = workbook,
                        sheet = sheet,
                        x = tbl$title,
                        startRow = locations$row$start_row_title,
                        startCol = locations$col$start_col_title,
                        colNames = FALSE,
                        rowNames = FALSE)
    openxlsx::addStyle(wb = workbook,
                       sheet = sheet,
                       style = styles$title_style,
                       rows = locations$row$start_row_title,
                       cols = locations$col$start_col_title,
                       stack = TRUE)
    #openxlsx::mergeCells(wb = workbook,
    #                     sheet = sheet,
    #                     cols = start_col:(start_col + n_col - 1),
    #                     rows = start_row)
  }

  if(!is.null(tbl$subtitle)){
    openxlsx::writeData(wb = workbook,
                        sheet = sheet,
                        x = tbl$subtitle,
                        startRow = locations$row$start_row_subtitle,
                        startCol = locations$col$start_col_subtitle,
                        colNames = FALSE,
                        rowNames = FALSE)
    openxlsx::addStyle(wb = workbook,
                       sheet = sheet,
                       style = styles$subtitle_style,
                       rows = locations$row$start_row_subtitle,
                       cols = locations$col$start_col_subtitle,
                       stack = TRUE)
    #openxlsx::mergeCells(wb = workbook,
    #                     sheet = sheet,
    #                     cols = start_col:(start_col + n_col - 1),
    #                     rows = start_row + 1)
  }

  if(!is.null(tbl$title) | !is.null(tbl$subtitle)){
    # Add line below table
    openxlsx::addStyle(wb = workbook,
                       sheet = sheet,
                       style = styles$hline_style,
                       rows = locations$row$start_row_title + !is.null(tbl$subtitle),
                       cols = locations$col$start_col_title:locations$col$end_col_title,
                       stack = TRUE)
  }
}

#' @import openxlsx
write_header <- function(workbook,
                         sheet,
                         header,
                         table_data,
                         locations,
                         styles){

  if(!is.null(header$lhs)){

    max_level <- max(header$lhs$level, header$rhs$level)

    write_header_entry(workbook = workbook,
                       sheet = sheet,
                       header_entry = header$lhs,
                       max_level = max_level,
                       start_row = locations$row$start_row_header,
                       start_col = locations$col$start_col_header_lhs,
                       header_style = styles$header_style)

    # Separate row names and data with a vertical bar
    openxlsx::addStyle(wb = workbook,
                       sheet = sheet,
                       style = styles$vline_style,
                       rows = locations$row$start_row_header:locations$row$end_row_header,
                       cols = locations$col$end_col_header_lhs,
                       stack = TRUE)

  }else{
    max_level <- header$rhs$level
  }

  write_header_entry(workbook = workbook,
                     sheet = sheet,
                     header_entry = header$rhs,
                     max_level = max_level,
                     start_row = locations$row$start_row_header,
                     start_col = locations$col$start_col_header_rhs,
                     header_style = styles$header_style)

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
}

write_data <- function(workbook,
                       sheet,
                       header,
                       table_data,
                       locations,
                       styles){

  if(!is.null(header$lhs)){
    # Row names
    openxlsx::writeData(wb = workbook,
                        sheet = sheet,
                        x = table_data$row_data,
                        startCol = locations$col$start_col_header_lhs,
                        startRow = locations$row$end_row_header + 1,
                        rowNames = FALSE,
                        colNames = FALSE)

    # Separate row names and data with a vertical bar
    openxlsx::addStyle(wb = workbook,
                       sheet = sheet,
                       style = styles$vline_style,
                       rows = (locations$row$end_row_header + 1) : (locations$row$start_row_footnote - 1),
                       cols = locations$col$end_col_header_lhs,
                       stack = TRUE)

  }

  openxlsx::writeData(wb = workbook,
                      sheet = sheet,
                      x = table_data$col_data,
                      startCol = locations$col$start_col_header_rhs,
                      startRow = locations$row$end_row_header + 1,
                      rowNames = FALSE,
                      colNames = FALSE)

  # Separate data and outside of table with a vertical bar (right hand side of the table)
  openxlsx::addStyle(wb = workbook,
                     sheet = sheet,
                     style = styles$vline_style,
                     rows = (locations$row$end_row_header + 1) : (locations$row$start_row_footnote - 1),
                     cols = locations$col$end_col_header_rhs,
                     stack = TRUE)

  # Add line below table
  openxlsx::addStyle(wb = workbook,
                     sheet = sheet,
                     style = styles$hline_style,
                     rows = locations$row$start_row_footnote - 1,
                     cols = locations$col$start_col_footnote:locations$col$end_col_footnot,
                     stack = TRUE)
}

write_footnote <- function(tbl,
                           workbook,
                           sheet,
                           locations,
                           styles){

  if(is.null(tbl$footnote))
    return()

  openxlsx::writeData(wb = workbook,
                      sheet = sheet,
                      x = tbl$footnote,
                      startRow = locations$row$start_row_footnote,
                      startCol = locations$col$start_col_footnote,
                      colNames = FALSE,
                      rowNames = FALSE)
  openxlsx::addStyle(wb = workbook,
                     sheet = sheet,
                     style = styles$footnote_style,
                     rows = locations$row$start_row_footnote,
                     cols = locations$col$start_col_footnote,
                     stack = TRUE)
  # openxlsx::mergeCells(wb = workbook,
  #                      sheet = sheet,
  #                      cols = start_col:(start_col + n_col - 1),
  #                      rows = start_row)
}
