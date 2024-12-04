#' print.Tablespan
#'
#' @param x result from tablespan
#' @param digits number of digits to round doubles to
#' @param n number of rows to print
#' @param ... additional arguments passed to prmatrix
#' @returns nothing
#' @importFrom utils head
#' @export
#' @examples
#' data("iris")
#' tbl <- tablespan(data = iris[iris$Species == "setosa", ],
#'           formula = Species ~ (Sepal = Sepal.Length + Sepal.Width) +
#'             (Petal = Petal.Length + Petal.Width))
#' print(tbl)
print.Tablespan <- function(x, digits = 2, n = 3, ...){
  if(!is.null(x$header$lhs)){
    max_level <- max(x$header$lhs$level, x$header$rhs$level)
    max_col <- x$header$lhs$width + x$header$rhs$width
  }else{
    max_level <- x$header$rhs$level
    max_col <- x$header$rhs$width
  }

  header_table <- matrix(NA,
                         nrow = max_level + min(n, nrow(x$table_data$col_data)),
                         ncol = max_col + !is.null(x$header$lhs))

  if(!is.null(x$header$lhs)){
    header_table <- print_insert_header_entries(header_partial = x$header$lhs,
                                                max_level = max_level,
                                                column_offset = 1,
                                                header_table = header_table)
  }

  header_table <- print_insert_header_entries(header_partial = x$header$rhs,
                                              max_level = max_level,
                                              column_offset = ifelse(is.null(x$header$lhs),
                                                                     1,
                                                                     x$header$lhs$width + 2),
                                              header_table = header_table)

  # add data
  rws <- max_level:(max_level + min(n, nrow(x$table_data$col_data)) - 1) + 1
  if(!is.null(x$header$lhs)){
    cls <- 1:ncol(x$table_data$row_data)
    header_table[rws, cls] <- x$table_data$row_data |>
      sapply(function(x) if(is.numeric(x) & !is.integer(x)){round(x, digits)}else{x}) |>
      utils::head(n = n)

    # add vertical line
    header_table[, max(cls) + 1] <- "|"

    cls <- max(cls) + 1 + 1:ncol(x$table_data$col_data)
  }else{
    cls <- 1:ncol(x$table_data$col_data)
  }

  header_table[rws, cls] <- x$table_data$col_data |>
    sapply(function(x) if(is.numeric(x) & !is.integer(x)){round(x, digits)}else{x}) |>
    utils::head(n = n)

  # add horizontal line
  header_table[max_level, ] <- header_table |>
    apply(2, function(x) max(nchar(x), na.rm = TRUE)) |>
    sapply(function(x) paste0(rep("-", x), collapse = ""))

  # add ...
  if(n < nrow(x$table_data$col_data)){
    header_table <- header_table |>
      rbind("...")
    if(!is.null(x$header$lhs))
      header_table[nrow(header_table), ncol(x$table_data$row_data) + 1] <- "|"
  }

  # add vertical lines
  header_table <- cbind("|", header_table) |>
    cbind("|")

  # actual printing
  if(!is.null(x$title))
    cat(paste0(x$title, "\n"))
  if(!is.null(x$subtitle))
    cat(paste0(x$subtitle, "\n"))
  prmatrix(header_table, quote = FALSE,
           na.print = "",
           rowlab=rep("", nrow(header_table)),
           collab=rep("", ncol(header_table)),
           ...)
  if(!is.null(x$footnote))
    cat(paste0(x$footnote, "\n"))

}


#' print_insert_header_entries
#'
#' Insert header entries into a matrix for printing.
#'
#' @param header_partial part of the header list
#' @param max_level depth of the header list
#' @param column_offset offset to write data in columns
#' @param header_table table in which the header entries should be inserted
#' @returns header_table with entries
#' @noRd
print_insert_header_entries <- function(header_partial, max_level, column_offset, header_table){
  if(header_partial$name != "_BASE_LEVEL_"){
    header_table[max_level - header_partial$level, column_offset] <- header_partial$name
  }
  if(!is.null(header_partial$entries)){
    for(i in seq_along(header_partial$entries)){
      header_table <- print_insert_header_entries(header_partial = header_partial$entries[[i]],
                                                  max_level = max_level,
                                                  column_offset = column_offset,
                                                  header_table = header_table)
      column_offset <- column_offset  + header_partial$entries[[i]]$width
    }
  }
  return(header_table)
}
