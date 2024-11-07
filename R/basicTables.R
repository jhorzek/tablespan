#' bt
#'
#' Create a basic table using a data set and a formula to describe the headers.
#'
#' Basic tables creates, as the name suggests, very basic tables. In general, \code{bt}
#' will not create summaries for you or transform your data in any shape or form.
#' Instead, the idea is that you provide an already summarized data frame (e.g.,
#' from dplyr's summary function) and just need some nested headers when writing
#' it to Excel.
#'
#' Following the \code{tibble} approach, \code{bt} assumes that all items that you may
#' want to use as row names are just columns in your data set (see example). That
#' is, \code{bt} will allow you to pick some of your items as row names and then just
#' write them in a separate section to the left of the data.
#'
#' The table headers are defined with a basic formula approach. For example,
#' \code{Species ~ Sepal.Length + Sepal.Width} defines a table with Species as the
#' row names and Sepal.Length and Sepal.Width as columns. The output in Excel will
#' be similar to the following:
#' \preformatted{
#' |Species | Sepal.Length  Sepal.Width|
#' |:-------|------------: -----------:|
#' |setosa  |          5.1          3.5|
#' |setosa  |          4.9          3.0|}
#'
#' Note that the row names (Species) are in a separate block to the left.
#'
#' You can add spanner labels with as follows:
#'
#' \code{Species ~ (Sepal = Sepal.Length + Sepal.Width) + (Petal = Sepal.Length + Sepal.Width)}
#'
#' This will result in an Excel output similar to:
#' \preformatted{
#' |        |           Sepal          |          Petal           |
#' |Species | Sepal.Length| Sepal.Width| Petal.Length| Petal.Width|
#' |:-------|------------:|-----------:|------------:|-----------:|
#' |setosa  |          5.1|         3.5|          1.4|         0.2|}
#'
#' You can also nest spanners (e.g., \code{Species ~ (Sepal = (Length = Sepal.Length) + (Width = Sepal.Width))}.
#'
#' In the example above, there is some redundant information: For example, if we have the spanner
#' label "Sepal", we don't need the "Sepal." part of "Sepal.Length". To remove this
#' redundancy, you can rename the item in the header using \code{new_name:old_name}.
#' For example, \code{Species ~ (Sepal = Length:Sepal.Length + Width:Sepal.Width) + (Petal = Length:Sepal.Length + Width:Sepal.Width)}
#' defines as table similar to the following:
#' \preformatted{
#' |        |      Sepal     |      Petal     |
#' |Species | Length | Width | Length | Width |
#' |:-------|-------:|------:|-------:|------:|
#' |setosa  |     5.1|    3.5|     1.4|    0.2|}
#'
#' Finally, to create a table without row names, use
#' \code{1 ~ (Sepal = Length:Sepal.Length + Width:Sepal.Width) + (Petal = Length:Sepal.Length + Width:Sepal.Width)}
#' This defines as table similar to the following:
#' \preformatted{
#' |      Sepal     |      Petal     |
#' | Length | Width | Length | Width |
#' |-------:|------:|-------:|------:|
#' |     5.1|    3.5|     1.4|    0.2|}
#'
#' @param data data set
#' @param formula formula to create table
#' @returns Object of class BT with header info, data, and footer info.
#' @export
#' @examples
#' data("iris")
#' tbl <- bt(data = iris[iris$Species == "setosa", ],
#'           formula = Species ~ (Sepal = Sepal.Length + Sepal.Width) +
#'                     (Petal = Petal.Length + Petal.Width))
#'
#' # Create Excel table:
#' wb <- write_bt(tbl = tbl)
#'
#' # saveWorkbook(wb, "iris.xlsx")
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
  if(!is.null(variables$row_variables)){
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
  if(!is.null(row_variables))
    return(data[,row_variables, drop = FALSE])
  return(NULL)
}

construct_header <- function(deparsed){

  deparsed$rhs <- add_header_width(deparsed$rhs)
  deparsed$rhs <- add_header_level(deparsed$rhs)

  deparsed$lhs <- add_header_width(deparsed$lhs)
  deparsed$lhs <- add_header_level(deparsed$lhs)

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
#' deparsed <- basicTables:::add_header_width(deparsed$rhs)
#' str(deparsed)
#' deparsed$width
add_header_width <- function(parsed_partial){
  if(is.null(parsed_partial))
    return(NULL)
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
#' deparsed <- basicTables:::add_header_level(deparsed$rhs)
#' str(deparsed)
#' deparsed$level
add_header_level <- function(parsed_partial){
  if(is.null(parsed_partial))
    return(NULL)

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
