#' as_excel
#'
#' Write a tablespan table to an excel workbook.
#'
#' @param tbl table created with tablespan::tablespan
#' @param workbook Excel workbook created with openxlsx::createWorkbook()
#' @param sheet name of the sheet to which the table should be written to
#' @param start_row row at which to start the table
#' @param start_col column at which to start the table
#' @param styles openxlsx style for the different table elements (see ?tablespan::tbl_styles).
#' The styles element also allows applying custom styles to parts of the data shown in the
#' table body.
#' @returns openxlsx workbook object that can be edited and saved with openxlsx
#' @import openxlsx
#' @export
#' @examples
#' library(tablespan)
#' library(dplyr)
#' data("iris")
#'
#' tbl <- tablespan(data = iris[iris$Species == "setosa", ],
#'           formula = Species ~ (Sepal = Sepal.Length + Sepal.Width) +
#'             (Petal = (Width = Petal.Length) + Petal.Width))
#'
#' wb <- as_excel(tbl = tbl)
#'
#' # saveWorkbook(wb, "iris.xlsx")
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
#'
#' # The main use case for tablespan is when you already have a summarized table
#' # that you now want to share using xlsx. The following shows an example using
#' # the dplyr package:
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
#' wb <- as_excel(tbl = tbl)
#'
#' # Create the excel table:
#' # openxlsx::saveWorkbook(wb,
#' #                        file = "cars.xlsx", overwrite = TRUE)
as_excel <- function(tbl,
                     workbook = openxlsx::createWorkbook(),
                     sheet = "Table",
                     start_row = 1,
                     start_col = 1,
                     styles = tbl_styles()){

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

  # We create the outlines last as we may have to overwrite some border colors.
  create_outlines(tbl = tbl,
                  workbook = workbook,
                  sheet = sheet,
                  locations = locations,
                  styles = styles)

  return(workbook)
}

#' fill_background
#'
#' Fills the background of the table.
#'
#' @param tbl table created with tablespan::tablespan
#' @param workbook Excel workbook created with openxlsx::createWorkbook()
#' @param sheet name of the sheet to which the table should be written to
#' @param locations list with overview of row and col locations for different table elements
#' @param styles openxlsx style for the different table elements (see ?tablespan::tbl_styles).
#' The styles element also allows applying custom styles to parts of the data shown in the
#' table body.
#' @import openxlsx
#' @noRd
fill_background <- function(tbl,
                            workbook,
                            sheet,
                            locations,
                            styles){
  # Title
  if(!is.null(tbl$title))
    openxlsx::addStyle(wb = workbook,
                       sheet = sheet,
                       style = styles$bg_title,
                       rows = locations$row$start_row_title:locations$row$end_row_title,
                       cols = locations$col$start_col_title:locations$col$end_col_title,
                       gridExpand = TRUE,
                       stack = TRUE)
  # Subtitle
  if(!is.null(tbl$subtitle))
    openxlsx::addStyle(wb = workbook,
                       sheet = sheet,
                       style = styles$bg_subtitle,
                       rows = locations$row$start_row_subtitle:locations$row$end_row_subtitle,
                       cols = locations$col$start_col_subtitle:locations$col$end_col_subtitle,
                       gridExpand = TRUE,
                       stack = TRUE)
  # Header LHS
  if(!is.null(tbl$header$lhs))
    openxlsx::addStyle(wb = workbook,
                       sheet = sheet,
                       style = styles$bg_header_lhs,
                       rows = locations$row$start_row_header:locations$row$end_row_header,
                       cols = locations$col$start_col_header_lhs:locations$col$end_col_header_lhs,
                       gridExpand = TRUE,
                       stack = TRUE)
  # Header RHS
  openxlsx::addStyle(wb = workbook,
                     sheet = sheet,
                     style = styles$bg_header_rhs,
                     rows = locations$row$start_row_header:locations$row$end_row_header,
                     cols = locations$col$start_col_header_rhs:locations$col$end_col_header_rhs,
                     gridExpand = TRUE,
                     stack = TRUE)
  # Rownames
  if(!is.null(tbl$header$lhs))
    openxlsx::addStyle(wb = workbook,
                       sheet = sheet,
                       style = styles$bg_rownames,
                       rows = locations$row$start_row_data:locations$row$end_row_data,
                       cols = locations$col$start_col_header_lhs:locations$col$end_col_header_lhs,
                       gridExpand = TRUE,
                       stack = TRUE)
  # Data
  openxlsx::addStyle(wb = workbook,
                     sheet = sheet,
                     style = styles$bg_data,
                     rows = locations$row$start_row_data:locations$row$end_row_data,
                     cols = locations$col$start_col_header_rhs:locations$col$end_col_header_rhs,
                     gridExpand = TRUE,
                     stack = TRUE)
  # Footnote
  if(!is.null(tbl$footnote))
    openxlsx::addStyle(wb = workbook,
                       sheet = sheet,
                       style = styles$bg_footnote,
                       rows = locations$row$start_row_footnote:locations$row$end_row_footnote,
                       cols = locations$col$start_col_footnote:locations$col$end_col_footnote,
                       gridExpand = TRUE,
                       stack = TRUE)
}

#' create_outlines
#'
#' Adds vertical and horizontal bars to the table.
#'
#' @param tbl table created with tablespan::tablespan
#' @param workbook Excel workbook created with openxlsx::createWorkbook()
#' @param sheet name of the sheet to which the table should be written to
#' @param locations list with overview of row and col locations for different table elements
#' @param styles openxlsx style for the different table elements (see ?tablespan::tbl_styles).
#' The styles element also allows applying custom styles to parts of the data shown in the
#' table body.
#' @import openxlsx
#' @noRd
create_outlines <- function(tbl,
                            workbook,
                            sheet,
                            locations,
                            styles){
  if(!is.null(tbl$header$lhs)){
    left_most <- locations$col$start_col_header_lhs
  }else{
    left_most <- locations$col$start_col_header_rhs
  }

  # top line
  openxlsx::addStyle(wb = workbook,
                     sheet = sheet,
                     style = styles$hline,
                     rows = locations$row$start_row_header,
                     cols = left_most:locations$col$end_col_header_rhs,
                     stack = TRUE)

  # bottom line
  openxlsx::addStyle(wb = workbook,
                     sheet = sheet,
                     style = styles$hline,
                     rows = locations$row$end_row_data + 1,
                     cols = left_most:locations$col$end_col_header_rhs,
                     stack = TRUE)

  # left line
  openxlsx::addStyle(wb = workbook,
                     sheet = sheet,
                     style = styles$vline,
                     rows = locations$row$start_row_header:locations$row$end_row_data,
                     cols = left_most,
                     stack = TRUE)

  # right line
  openxlsx::addStyle(wb = workbook,
                     sheet = sheet,
                     style = styles$vline,
                     rows = locations$row$start_row_header:locations$row$end_row_data,
                     cols = locations$col$end_col_header_rhs + 1,
                     stack = TRUE)

  # row name separator
  openxlsx::addStyle(wb = workbook,
                     sheet = sheet,
                     style = styles$vline,
                     rows = locations$row$start_row_header:locations$row$end_row_data,
                     cols = locations$col$start_col_header_rhs,
                     stack = TRUE)
}

#' write_title
#'
#' Writes the title and the subtitle to the workbook.
#'
#' @param tbl table created with tablespan::tablespan
#' @param workbook Excel workbook created with openxlsx::createWorkbook()
#' @param sheet name of the sheet to which the table should be written to
#' @param locations list with overview of row and col locations for different table elements
#' @param styles openxlsx style for the different table elements (see ?tablespan::tbl_styles).
#' The styles element also allows applying custom styles to parts of the data shown in the
#' table body.
#' @import openxlsx
#' @noRd
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
                       style = styles$cell_title,
                       rows = locations$row$start_row_title,
                       cols = locations$col$start_col_title,
                       stack = TRUE)
    openxlsx::mergeCells(wb = workbook,
                         sheet = sheet,
                         cols = locations$col$start_col_title:locations$col$end_col_title,
                         rows = locations$row$start_row_title)
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
                       style = styles$cell_subtitle,
                       rows = locations$row$start_row_subtitle,
                       cols = locations$col$start_col_subtitle,
                       stack = TRUE)
    openxlsx::mergeCells(wb = workbook,
                         sheet = sheet,
                         cols = locations$col$start_col_subtitle:locations$col$end_col_subtitle,
                         rows = locations$row$start_row_subtitle)
  }
}

#' write_header
#'
#' Writes the header (column names and names for the rownames) to the workbook.
#'
#' @param workbook Excel workbook created with openxlsx::createWorkbook()
#' @param sheet name of the sheet to which the table should be written to
#' @param header header specification from tablespan table
#' @param table_data data for rownames and the actual data for the body of the table
#' @param locations list with overview of row and col locations for different table elements
#' @param styles openxlsx style for the different table elements (see ?tablespan::tbl_styles).
#' The styles element also allows applying custom styles to parts of the data shown in the
#' table body.
#' @import openxlsx
#' @noRd
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
                       header_style = styles$cell_header_lhs,
                       vline = styles$vline)

  }else{
    max_level <- header$rhs$level
  }

  write_header_entry(workbook = workbook,
                     sheet = sheet,
                     header_entry = header$rhs,
                     max_level = max_level,
                     start_row = locations$row$start_row_header,
                     start_col = locations$col$start_col_header_rhs,
                     header_style = styles$cell_header_rhs,
                     vline = styles$vline)

}

#' write_header_entry
#'
#' Recursive function writing the header elements (column names and names for the rownames)
#' to the workbook.
#'
#' @param workbook Excel workbook created with openxlsx::createWorkbook()
#' @param sheet name of the sheet to which the table should be written to
#' @param header_entry specific header enty to write to the workbook
#' @param max_level the highest level of header entries
#' @param start_row integer specifying row to write to
#' @param start_col integer specifying column to write to
#' @param header_style openxlsx style for the header
#' @param vline openxlsx style for the vertical lines in the header
#' @import openxlsx
#' @noRd
write_header_entry <- function(workbook,
                               sheet,
                               header_entry,
                               max_level,
                               start_row,
                               start_col,
                               header_style,
                               vline){

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
                       header_style = header_style,
                       vline = vline)
    start_col_entry <- start_col_entry + entry$width
  }
}

#' write_data
#'
#' Writes the data to the body of the workbook.
#'
#' @param workbook Excel workbook created with openxlsx::createWorkbook()
#' @param sheet name of the sheet to which the table should be written to
#' @param header header specification from tablespan table
#' @param table_data data for rownames and the actual data for the body of the table
#' @param locations list with overview of row and col locations for different table elements
#' @param styles openxlsx style for the different table elements (see ?tablespan::tbl_styles).
#' The styles element also allows applying custom styles to parts of the data shown in the
#' table body.
#' @import openxlsx
#' @noRd
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
    openxlsx::addStyle(wb = workbook,
                       sheet = sheet,
                       style = styles$cell_rownames,
                       rows = locations$row$start_row_data : (locations$row$end_row_data),
                       cols = locations$col$start_col_header_lhs : locations$col$end_col_header_lhs,
                       stack = TRUE,
                       gridExpand = TRUE)

    for(sty in styles$data_styles){
      for(j in seq_len(ncol(table_data$row_data))){
        if(sty$test(table_data$row_data[[j]])){
          openxlsx::addStyle(wb = workbook,
                             sheet = sheet,
                             style = sty$style,
                             rows = (locations$row$end_row_header + 1) : (locations$row$start_row_footnote - 1),
                             cols = locations$col$start_col_header_lhs + j - 1,
                             stack = TRUE)
        }
      }
    }

    if(styles$merge_rownames){
      merge_rownames(workbook = workbook,
                     sheet = sheet,
                     table_data = table_data,
                     locations = locations,
                     styles = styles)
    }
  }

  openxlsx::writeData(wb = workbook,
                      sheet = sheet,
                      x = table_data$col_data,
                      startCol = locations$col$start_col_header_rhs,
                      startRow = locations$row$end_row_header + 1,
                      rowNames = FALSE,
                      colNames = FALSE)
  openxlsx::addStyle(wb = workbook,
                     sheet = sheet,
                     style = styles$cell_data,
                     rows = locations$row$start_row_data : locations$row$end_row_data,
                     cols = locations$col$start_col_header_rhs : locations$col$end_col_header_rhs,
                     stack = TRUE,
                     gridExpand = TRUE)

  for(sty in styles$data_styles){
    for(j in seq_len(ncol(table_data$col_data))){
      if(sty$test(table_data$col_data[[j]])){
        openxlsx::addStyle(wb = workbook,
                           sheet = sheet,
                           style = sty$style,
                           rows = (locations$row$end_row_header + 1) : (locations$row$start_row_footnote - 1),
                           cols = locations$col$start_col_header_rhs + j - 1,
                           stack = TRUE)
      }
    }
  }

  # Apply custom styles
  if(!is.null(styles$cell_styles)){
    for(sty in styles$cell_styles){
      if(any(!sty$colnames %in% colnames(table_data$col_data)))
        stop("Tryping to style an element that was not found in the data: ",
             paste0(sty$colnames[!sty$colnames %in% colnames(table_data$col_data)], collapse = ", "),
             ".")
      if(any(sty$rows > nrow(table_data$col_data)))
        stop("Trying to style a row outside of the range of the data.")

      openxlsx::addStyle(wb = workbook,
                         sheet = sheet,
                         style = sty$style,
                         rows = locations$row$start_row_data + sty$rows - 1,
                         cols = locations$col$start_col_header_rhs + which(colnames(table_data$col_data) %in% sty$colnames) - 1,
                         stack = sty$stack,
                         gridExpand = sty$gridExpand)
    }
  }
}

#' write_footnote
#'
#' Writes the footnote to the workbook.
#'
#' @param tbl table created with tablespan::tablespan
#' @param workbook Excel workbook created with openxlsx::createWorkbook()
#' @param sheet name of the sheet to which the table should be written to
#' @param locations list with overview of row and col locations for different table elements
#' @param styles openxlsx style for the different table elements (see ?tablespan::tbl_styles).
#' The styles element also allows applying custom styles to parts of the data shown in the
#' table body.
#' @import openxlsx
#' @noRd
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
                     style = styles$cell_footnote,
                     rows = locations$row$start_row_footnote,
                     cols = locations$col$start_col_footnote,
                     stack = TRUE)
  openxlsx::mergeCells(wb = workbook,
                       sheet = sheet,
                       cols = locations$col$start_col_footnote:locations$col$end_col_footnote,
                       rows = locations$row$start_row_footnote)
}

#' merge_rownames
#'
#' Merges consecutive rownames that are identical into a common cell.
#'
#' @param workbook Excel workbook created with openxlsx::createWorkbook()
#' @param sheet name of the sheet to which the table should be written to
#' @param table_data data for rownames and the actual data for the body of the table
#' @param locations list with overview of row and col locations for different table elements
#' @param styles openxlsx style for the different table elements (see ?tablespan::tbl_styles).
#' The styles element also allows applying custom styles to parts of the data shown in the
#' table body.
#' @import openxlsx
#' @noRd
merge_rownames <- function(workbook,
                           sheet,
                           table_data,
                           locations,
                           styles){

  cell_ids <- row_data_cell_ids(table_data$row_data)

  # We merge all cells within a column that have the same id.
  for(co in 1:ncol(table_data$row_data)){
    unique_ids <- unique(cell_ids[,co])
    for(id in unique_ids){
      is_identical <- sapply(cell_ids[,co],
                             function(x) identical(x, id))
      if(sum(is_identical) > 1){
        openxlsx::addStyle(wb = workbook,
                           sheet = sheet,
                           style = styles$merged_rownames_style,
                           rows = locations$row$end_row_header + which(is_identical),
                           cols = locations$col$start_col_header_lhs + co - 1,
                           stack = TRUE)

        openxlsx::mergeCells(wb = workbook,
                             sheet = sheet,
                             rows = locations$row$end_row_header + which(is_identical),
                             cols = locations$col$start_col_header_lhs + co - 1)
      }
    }
  }
}

#' row_data_cell_ids
#'
#' Creates an index for each element in the row_data. This index is used
#' to merge cells that are identical.
#' @param row_data row_data for the table
#' @return matrix with indices. Cells that should be merged get the same index.
#' @noRd
#' @importFrom tibble as_tibble
#' @importFrom tibble is_tibble
#' @examples
#' row_data <- tibble::tibble(cyl = c(4,4,6,6,8),
#'                            vs  = c(0,1,1,0,0))
#' tablespan:::row_data_cell_ids(row_data)
row_data_cell_ids <- function(row_data){
  if(!tibble::is_tibble(row_data))
    row_data <- tibble::as_tibble(row_data)
  ids <- matrix(NA,
                nrow = nrow(row_data),
                ncol = ncol(row_data))
  ids[1,] <- 1
  if(nrow(row_data) == 1)
    return(ids)

  for(ro in 2:nrow(row_data)){
    for(co in 1:ncol(row_data)){
      ids[ro, co] <- ids[ro-1, co] + ifelse(identical(row_data[ro, 1:co],
                                                      row_data[ro-1, 1:co]), 0, 1)
    }
  }
  return(ids)
}
