#' bt
#'
#' Create a basic table
#'
#' @param data data set
#' @param formula formula to create table
#' @returns list
#' @export
#' @examples
#' data("iris")
#' bt(data = iris[iris$Species == "setosa", ],
#'    formula = Species ~ (Sepal = Sepal.Length + Sepal.Width) +
#'      (Petal = Petal.Length + Petal.Width))
bt <- function(data,
               formula){
  deparsed <- deparse_formula(formula)

  variables <- get_variables(deparsed)

  check_variables(data = data,
                  variables = variables)

  table_data <- list(row_data = get_row_data(data = data,
                                             row_variables = variables$row_variables),
                     col_data = get_col_data(data = data,
                                             col_variables = variables$col_variables))

  header <- construct_header(deparsed)

  footer <- NULL # construct_footer()

  bt_result <- list(header = header,
                    table_data = table_data,
                    footer = footer)
  class(bt_result) <- "BT"
  return(bt_result)
}

check_variables <- function(data, variables){
  if(!all(variables$row_variables == "1")){
    check_row_variables <- setdiff(variables$row_variables, colnames(data))
    if(length(check_row_variables) != 0)
      stop(paste0("The following variables were not found in the data set: ",
                  paste0(check_row_variables, collapse = ",")))
  }

  check_col_variables <- setdiff(variables$col_variables, colnames(data))
  if(length(check_col_variables) != 0)
    stop(paste0("The following variables were not found in the data set: ",
                paste0(check_col_variables, collapse = ",")))
}

get_col_data <- function(data, col_variables){
  return(data[,col_variables, drop = FALSE])
}

get_row_data <- function(data, row_variables){
  if(!all(row_variables == "1"))
    return(data[,row_variables, drop = FALSE])
  return(NULL)
}

construct_header <- function(deparsed){

  deparsed$rhs <- add_header_width(deparsed$rhs)
  deparsed$rhs <- add_header_level(deparsed$rhs)

  return(deparsed)
}

#' add_header_width
#'
#' basicTables represents headers as (highly) nested lists. To determine how
#' wide each entry in the header must be (i.e., how many cells it will get in
#' the Excel output), we have to get the number of root elements each parent element
#' spans. For example, in the following table, x spans two elements x1 and x2:
#'
#' |    x    |
#' | x1 | x2 |
#'
#' add_header_width adds the span to each element in a table header.
#'
#' @param parsed_partial The left hand side or right hand side of the parsed
#' table
#' @returns the parsed_partial with additional width fields
#' @keywords internal
#' @examples
#' library(basicTables)
#' deparsed <- basicTables:::deparse_formula(formula =
#'  (`Row Name` = `Row 1` + `Row 2`) ~ `Column 1` + (`Column Banner` = `Column 2` + `Column 3`))
#' str(deparsed)
#'
#' deparsed <- add_header_width(deparsed$rhs)
#' str(deparsed)
#' deparsed$width
add_header_width <- function(parsed_partial){
  if(is.null(parsed_partial$entries)){
    parsed_partial$width <- 1
    return(parsed_partial)
  }

  parsed_partial$width <- 0
  for(entry in 1:length(parsed_partial$entries)){
    parsed_partial$entries[[entry]] <- add_header_width(parsed_partial$entries[[entry]])
    parsed_partial$width <- parsed_partial$width + parsed_partial$entries[[entry]]$width
  }

  return(parsed_partial)
}

#' add_header_level
#'
#' basicTables represents headers as (highly) nested lists. To determine the level
#' at which each entry resides, we have to get the number of root elements below each
#' parent element. For example, in the following table, x is on level 2, but x1,
#' x2, y1, and y2 are at level 1:
#'
#' |    x    |
#' | x1 | x2 | y1 | y2
#'
#' add_header_level adds the level to each element in a table header.
#'
#' @param parsed_partial The left hand side or right hand side of the parsed
#' table
#' @returns the parsed_partial with additional level fields
#' @keywords internal
#' @examples
#' library(basicTables)
#' deparsed <- basicTables:::deparse_formula(formula =
#'  (`Row Name` = `Row 1` + `Row 2`) ~ `Column 1` + (`Column Banner` = `Column 2` + `Column 3`))
#' str(deparsed)
#'
#' deparsed <- add_header_level(deparsed$rhs)
#' str(deparsed)
#' deparsed$level
add_header_level <- function(parsed_partial){
  if(is.null(parsed_partial$entries)){
    parsed_partial$level <- 1
    return(parsed_partial)
  }

  parsed_partial$level <- 0
  for(entry in seq_along(parsed_partial$entries)){
    parsed_partial$entries[[entry]] <- add_header_level(parsed_partial$entries[[entry]])
    parsed_partial$level <- max(parsed_partial$level,
                                parsed_partial$entries[[entry]]$level + 1)
  }

  return(parsed_partial)
}
