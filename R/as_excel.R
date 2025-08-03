#' as_excel
#'
#' Write a tablespan table to an excel workbook.
#'
#' @param tbl table created with tablespan::tablespan
#' @param workbook Excel workbook created with openxlsx::createWorkbook()
#' @param sheet name of the sheet to which the table should be written to
#' @param start_row row at which to start the table
#' @param start_col column at which to start the table
#' @param merge_rownames should row names with identical entries be merged?
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
as_excel <- function(
  tbl,
  workbook = openxlsx::createWorkbook(),
  sheet = "Table",
  start_row = 1,
  start_col = 1,
  merge_rownames = TRUE
) {
  if (is.character(sheet) && !(sheet %in% workbook$sheet_names)) {
    openxlsx::addWorksheet(sheetName = sheet, wb = workbook)
  }

  styles <- tbl$styles
  styles$merge_rownames <- merge_rownames

  locations <- get_locations(
    tbl = tbl,
    start_row = start_row,
    start_col = start_col
  )

  initialize_styles_openxlsx(
    tbl = tbl,
    workbook = workbook,
    sheet = sheet,
    locations = locations,
    styles = styles
  )

  write_title(
    tbl = tbl,
    workbook = workbook,
    sheet = sheet,
    locations = locations
  )

  write_header(
    workbook = workbook,
    sheet = sheet,
    header = tbl$header,
    table_data = tbl$table_data,
    locations = locations,
    styles = styles
  )

  write_data(
    workbook = workbook,
    sheet = sheet,
    header = tbl$header,
    table_data = tbl$table_data,
    locations = locations,
    styles = styles
  )

  write_footnote(
    tbl = tbl,
    workbook = workbook,
    sheet = sheet,
    locations = locations
  )

  # We create the outlines last as we may have to overwrite some border colors.
  create_outlines(
    tbl = tbl,
    workbook = workbook,
    sheet = sheet,
    locations = locations,
    styles = styles
  )

  return(workbook)
}

#' initialize_styles_openxlsx
#'
#' Initializes all styles for the different table parts.
#'
#' @param tbl table created with tablespan::tablespan
#' @param workbook Excel workbook created with openxlsx::createWorkbook()
#' @param sheet name of the sheet to which the table should be written to
#' @param locations list with overview of row and col locations for different table elements
#' @param styles openxlsx style for the different table elements
#' @import openxlsx
#' @noRd
initialize_styles_openxlsx <- function(
  tbl,
  workbook,
  sheet,
  locations,
  styles
) {
  # Title
  if (!is.null(tbl$title)) {
    openxlsx::addStyle(
      wb = workbook,
      sheet = sheet,
      style = styles$title$openxlsx,
      rows = locations$row$start_row_title:locations$row$end_row_title,
      cols = locations$col$start_col_title:locations$col$end_col_title,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  # Subtitle
  if (!is.null(tbl$subtitle)) {
    openxlsx::addStyle(
      wb = workbook,
      sheet = sheet,
      style = styles$subtitle$openxlsx,
      rows = locations$row$start_row_subtitle:locations$row$end_row_subtitle,
      cols = locations$col$start_col_subtitle:locations$col$end_col_subtitle,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  # Header LHS
  if (!is.null(tbl$header$lhs)) {
    openxlsx::addStyle(
      wb = workbook,
      sheet = sheet,
      style = styles$header$openxlsx,
      rows = locations$row$start_row_header:locations$row$end_row_header,
      cols = locations$col$start_col_header_lhs:locations$col$end_col_header_lhs,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  # Header RHS
  openxlsx::addStyle(
    wb = workbook,
    sheet = sheet,
    style = styles$header$openxlsx,
    rows = locations$row$start_row_header:locations$row$end_row_header,
    cols = locations$col$start_col_header_rhs:locations$col$end_col_header_rhs,
    gridExpand = TRUE,
    stack = TRUE
  )

  # Footnote
  if (!is.null(tbl$footnote)) {
    openxlsx::addStyle(
      wb = workbook,
      sheet = sheet,
      style = styles$footnote$openxlsx,
      rows = locations$row$start_row_footnote:locations$row$end_row_footnote,
      cols = locations$col$start_col_footnote:locations$col$end_col_footnote,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
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
create_outlines <- function(tbl, workbook, sheet, locations, styles) {
  if (!is.null(tbl$header$lhs)) {
    left_most <- locations$col$start_col_header_lhs
  } else {
    left_most <- locations$col$start_col_header_rhs
  }

  # top line
  openxlsx::addStyle(
    wb = workbook,
    sheet = sheet,
    style = styles$hline$openxlsx,
    rows = locations$row$start_row_header,
    cols = left_most:locations$col$end_col_header_rhs,
    stack = TRUE
  )

  # bottom line
  openxlsx::addStyle(
    wb = workbook,
    sheet = sheet,
    style = styles$hline$openxlsx,
    rows = locations$row$end_row_data + 1,
    cols = left_most:locations$col$end_col_header_rhs,
    stack = TRUE
  )

  # left line
  openxlsx::addStyle(
    wb = workbook,
    sheet = sheet,
    style = styles$vline$openxlsx,
    rows = locations$row$start_row_header:locations$row$end_row_data,
    cols = left_most,
    stack = TRUE
  )

  # right line
  openxlsx::addStyle(
    wb = workbook,
    sheet = sheet,
    style = styles$vline$openxlsx,
    rows = locations$row$start_row_header:locations$row$end_row_data,
    cols = locations$col$end_col_header_rhs + 1,
    stack = TRUE
  )

  # row name separator
  openxlsx::addStyle(
    wb = workbook,
    sheet = sheet,
    style = styles$vline$openxlsx,
    rows = locations$row$start_row_header:locations$row$end_row_data,
    cols = locations$col$start_col_header_rhs,
    stack = TRUE
  )
}

#' write_title
#'
#' Writes the title and the subtitle to the workbook.
#'
#' @param tbl table created with tablespan::tablespan
#' @param workbook Excel workbook created with openxlsx::createWorkbook()
#' @param sheet name of the sheet to which the table should be written to
#' @param locations list with overview of row and col locations for different table elements
#' @import openxlsx
#' @noRd
write_title <- function(tbl, workbook, sheet, locations) {
  if (!is.null(tbl$title)) {
    openxlsx::writeData(
      wb = workbook,
      sheet = sheet,
      x = tbl$title,
      startRow = locations$row$start_row_title,
      startCol = locations$col$start_col_title,
      colNames = FALSE,
      rowNames = FALSE
    )
    openxlsx::mergeCells(
      wb = workbook,
      sheet = sheet,
      cols = locations$col$start_col_title:locations$col$end_col_title,
      rows = locations$row$start_row_title
    )
  }

  if (!is.null(tbl$subtitle)) {
    openxlsx::writeData(
      wb = workbook,
      sheet = sheet,
      x = tbl$subtitle,
      startRow = locations$row$start_row_subtitle,
      startCol = locations$col$start_col_subtitle,
      colNames = FALSE,
      rowNames = FALSE
    )
    openxlsx::mergeCells(
      wb = workbook,
      sheet = sheet,
      cols = locations$col$start_col_subtitle:locations$col$end_col_subtitle,
      rows = locations$row$start_row_subtitle
    )
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
#' @param styles openxlsx style for the different table elements
#' @import openxlsx
#' @noRd
write_header <- function(
  workbook,
  sheet,
  header,
  table_data,
  locations,
  styles
) {
  if (!is.null(header$lhs)) {
    max_level <- max(header$lhs$level, header$rhs$level)

    write_header_entry(
      workbook = workbook,
      sheet = sheet,
      header_entry = header$lhs,
      max_level = max_level,
      start_row = locations$row$start_row_header,
      start_col = locations$col$start_col_header_lhs,
      styles = styles
    )
  } else {
    max_level <- header$rhs$level
  }

  write_header_entry(
    workbook = workbook,
    sheet = sheet,
    header_entry = header$rhs,
    max_level = max_level,
    start_row = locations$row$start_row_header,
    start_col = locations$col$start_col_header_rhs,
    styles = styles
  )
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
#' @param styles openxlsx style for the different table elements
#' @import openxlsx
#' @noRd
write_header_entry <- function(
  workbook,
  sheet,
  header_entry,
  max_level,
  start_row,
  start_col,
  styles
) {
  # write current entry name into table
  if (header_entry$name != "_BASE_LEVEL_") {
    openxlsx::writeData(
      wb = workbook,
      sheet = sheet,
      x = header_entry$name,
      startRow = start_row + (max_level - header_entry$level) - 1,
      startCol = start_col
    )

    if (header_entry$width > 1) {
      openxlsx::mergeCells(
        wb = workbook,
        sheet = sheet,
        cols = start_col:(start_col + header_entry$width - 1),
        rows = start_row + (max_level - header_entry$level) - 1
      )
    }
    openxlsx::addStyle(
      wb = workbook,
      sheet = sheet,
      style = styles$header_cells$openxlsx,
      rows = start_row + (max_level - header_entry$level) - 1,
      cols = start_col:(start_col + header_entry$width - 1),
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  # entries may have sub-entries, that also have to be written down
  start_col_entry <- start_col
  for (entry in header_entry$entries) {
    write_header_entry(
      workbook = workbook,
      sheet = sheet,
      header_entry = entry,
      max_level = max_level,
      start_row = start_row,
      start_col = start_col_entry,
      styles = styles
    )
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
write_data <- function(workbook, sheet, header, table_data, locations, styles) {
  if (!is.null(header$lhs)) {
    # Row names
    openxlsx::writeData(
      wb = workbook,
      sheet = sheet,
      x = table_data$row_data,
      startCol = locations$col$start_col_header_lhs,
      startRow = locations$row$end_row_header + 1,
      rowNames = FALSE,
      colNames = FALSE
    )

    if (styles$merge_rownames) {
      merge_rownames(
        workbook = workbook,
        sheet = sheet,
        table_data = table_data,
        locations = locations,
        styles = styles
      )
    }
  }

  openxlsx::writeData(
    wb = workbook,
    sheet = sheet,
    x = table_data$col_data,
    startCol = locations$col$start_col_header_rhs,
    startRow = locations$row$end_row_header + 1,
    rowNames = FALSE,
    colNames = FALSE
  )

  # Add user defined column styles
  column_styles <- styles$columns
  for (column_name in names(column_styles)) {
    for (style in column_styles[[column_name]]) {
      if (is.null(style$style$openxlsx)) {
        next
      } else {
        if (is.null(style$rows)) {
          data_rows <- locations$row$start_row_data:(locations$row$end_row_data)
        } else {
          data_rows <- locations$row$start_row_data + style$rows - 1
        }
        data_cols <- locations$col$start_col_header_lhs +
          which(names(column_styles) == column_name) -
          1
        if (is(style$style$openxlsx, "Style")) {
          openxlsx::addStyle(
            wb = workbook,
            sheet = sheet,
            style = style$style$openxlsx,
            rows = data_rows,
            cols = data_cols,
            stack = TRUE,
            gridExpand = FALSE
          )
        } else if (is.function(style$style$openxlsx)) {
          # used for color scales
          style$style$openxlsx(
            wb = workbook,
            sheet = sheet,
            rows = data_rows,
            cols = data_cols
          )
        }
      }
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
#' @import openxlsx
#' @noRd
write_footnote <- function(tbl, workbook, sheet, locations, styles) {
  if (is.null(tbl$footnote)) {
    return()
  }

  openxlsx::writeData(
    wb = workbook,
    sheet = sheet,
    x = tbl$footnote,
    startRow = locations$row$start_row_footnote,
    startCol = locations$col$start_col_footnote,
    colNames = FALSE,
    rowNames = FALSE
  )
  openxlsx::mergeCells(
    wb = workbook,
    sheet = sheet,
    cols = locations$col$start_col_footnote:locations$col$end_col_footnote,
    rows = locations$row$start_row_footnote
  )
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
merge_rownames <- function(workbook, sheet, table_data, locations, styles) {
  cell_ids <- row_data_cell_ids(table_data$row_data)

  # We merge all cells within a column that have the same id.
  for (co in 1:ncol(table_data$row_data)) {
    unique_ids <- unique(cell_ids[, co])
    for (id in unique_ids) {
      is_identical <- sapply(cell_ids[, co], function(x) identical(x, id))

      if (sum(is_identical) > 1) {
        openxlsx::addStyle(
          wb = workbook,
          sheet = sheet,
          style = openxlsx::createStyle(valign = "top"),
          rows = locations$row$end_row_header + which(is_identical),
          cols = locations$col$start_col_header_lhs + co - 1,
          stack = TRUE
        )

        openxlsx::mergeCells(
          wb = workbook,
          sheet = sheet,
          rows = locations$row$end_row_header + which(is_identical),
          cols = locations$col$start_col_header_lhs + co - 1
        )
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
row_data_cell_ids <- function(row_data) {
  if (!tibble::is_tibble(row_data)) {
    row_data <- tibble::as_tibble(row_data)
  }
  ids <- matrix(NA, nrow = nrow(row_data), ncol = ncol(row_data))
  ids[1, ] <- 1
  if (nrow(row_data) == 1) {
    return(ids)
  }

  for (ro in 2:nrow(row_data)) {
    for (co in 1:ncol(row_data)) {
      ids[ro, co] <- ids[ro - 1, co] +
        ifelse(identical(row_data[ro, 1:co], row_data[ro - 1, 1:co]), 0, 1)
    }
  }
  return(ids)
}
