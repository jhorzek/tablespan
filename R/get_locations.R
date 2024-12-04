#' get_locations
#'
#' Provides row and column indices for the table elements.
#' @param tbl table created with tablespan::tablespan
#' @param start_row row index at which the table will start
#' @param start_col column index at which the table will start
#' @returns list with locations
#' @noRd
get_locations <- function(tbl,
                          start_row,
                          start_col){

  # Get rows
  if(!is.null(tbl$title)){
    start_row_title <- end_row_title <- start_row
    start_row  <- start_row + 1
  }else{
    start_row_title <- end_row_title <- NULL
  }
  if(!is.null(tbl$subtitle)){
    start_row_subtitle <- end_row_subtitle <- start_row
    start_row  <- start_row + 1
  }else{
    start_row_subtitle <- end_row_subtitle <- NULL
  }

  start_row_header <- start_row

  if(!is.null(tbl$header$lhs)){
    start_row <- start_row + (max(tbl$header$lhs$level, tbl$header$rhs$level) - 1)
  }else{
    start_row <- start_row + tbl$header$rhs$level - 1
  }
  end_row_header <- start_row - 1

  start_row_data <- start_row
  end_row_data <- start_row + nrow(tbl$table_data$col_data) - 1

  start_row <- start_row + nrow(tbl$table_data$col_data)

  start_row_footnote <- end_row_footnote <- start_row

  # Get cols
  start_col_title <- start_col_subtitle <- start_col_header_lhs <- start_col_footnote <- start_col

  n_col <- 0

  if(!is.null(tbl$header$lhs)){
    n_col <- n_col + tbl$header$lhs$width + tbl$header$rhs$width - 1
    start_col_header_rhs <- start_col + tbl$header$lhs$width
    end_col_header_lhs <- start_col + tbl$header$lhs$width - 1
  }else{
    n_col <- n_col + tbl$header$rhs$width - 1
    start_col_header_rhs <- start_col
    end_col_header_lhs <- NULL
  }

  end_col_title <- end_col_subtitle <- end_col_footnote <- end_col_header_rhs <- start_col + n_col

  return(list(
    row = list(
      start_row_title = start_row_title,
      end_row_title = end_row_title,
      start_row_subtitle = start_row_subtitle,
      end_row_subtitle = end_row_subtitle,
      start_row_header = start_row_header,
      end_row_header = end_row_header,
      start_row_data = start_row_data,
      end_row_data = end_row_data,
      start_row_footnote = start_row_footnote,
      end_row_footnote = end_row_footnote
    ),
    col = list(
      start_col_title = start_col_title,
      end_col_title = end_col_title,
      start_col_subtitle = start_col_subtitle,
      end_col_subtitle = end_col_subtitle,
      start_col_header_lhs = start_col_header_lhs,
      end_col_header_lhs = end_col_header_lhs,
      start_col_header_rhs = start_col_header_rhs,
      end_col_header_rhs = end_col_header_rhs,
      start_col_footnote = start_col_footnote,
      end_col_footnote = end_col_footnote
    )
  ))
}
