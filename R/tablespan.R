#' tablespan
#'
#' Create complex table spanners with a simple formula.
#'
#' \code{tablespan} provides a formula based approach to adding headers and spanners
#' to an existing data.frame. The objective is to provide a unified, easy to use, but good
#' enough approach to building and exporting tables to Excel, HTML, and LaTeX. To this end,
#' \code{tablespan} leverages the awesome packages \code{openxlsx} and \code{gt}.
#'
#' Following the \code{tibble} approach, \code{tablespan} assumes that all items that you may
#' want to use as row names are just columns in your data set (see example). That
#' is, \code{tablespan} will allow you to pick some of your items as row names and then just
#' write them in a separate section to the left of the data.
#'
#' The table headers are defined with a basic formula approach inspired by \code{tables}.
#' For example, \code{Species ~ Sepal.Length + Sepal.Width} defines a table with Species as the
#' row names and Sepal.Length and Sepal.Width as columns. The output will
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
#' This will result in an output similar to:
#' \preformatted{
#' |        |           Sepal          |          Petal           |
#' |Species | Sepal.Length| Sepal.Width| Petal.Length| Petal.Width|
#' |:-------|------------:|-----------:|------------:|-----------:|
#' |setosa  |          5.1|         3.5|          1.4|         0.2|}
#'
#' You can also nest spanners (e.g., \code{Species ~ (Sepal = (Length = Sepal.Length) + (Width = Sepal.Width))}.
#'
#' When exporting tables, you may want to rename some of you columns. For example,
#' you may want to rename Sepal.Length and Petal.Length to Length and Sepal.Width and
#' Petal.Width to Width. With \code{tablespan}, you can rename the item in the header
#' using \code{new_name:old_name}.
#' For example, \code{Species ~ (Sepal = Length:Sepal.Length + Width:Sepal.Width) + (Petal = Length:Sepal.Length + Width:Sepal.Width)}
#' defines a table similar to the following:
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
#' Tables created with \code{tablespan} can be exported to Excel (using \code{openxlsx}),
#' HTML (using \code{gt}), LaTeX (using \code{gt}), and RTF (using \code{gt}).
#'
#' References:
#'
#' \itemize{
#'  \item{gt: Iannone R, Cheng J, Schloerke B, Hughes E, Lauer A, Seo J, Brevoort K, Roy O (2024). gt: Easily Create Presentation-Ready Display Tables. R package version 0.11.1.9000, <https://github.com/rstudio/gt>, <https://gt.rstudio.com>.}
#'  \item{tables: Murdoch D (2024). tables: Formula-Driven Table Generation. R package version 0.9.31, <https://dmurdoch.github.io/tables/>.}
#'  \item{openxlsx: Schauberger P, Walker A (2023). _openxlsx: Read, Write and Edit xlsx Files_. R package version 4.2.5.2, <https://ycphs.github.io/openxlsx/>.}
#' }
#' @param data data set
#' @param formula formula to create table
#' @param title string specifying the title of the table
#' @param subtitle string specifying the subtitle of the table
#' @param footnote string specifying the footnote of the table
#' @returns Object of class Tablespan with title, subtitle, header info, data, and footnote.
#' @importFrom tibble as_tibble
#' @importFrom tibble is_tibble
#' @export
#' @examples
#' library(tablespan)
#' library(dplyr)
#' data("mtcars")
#'
#' # We want to report the following table:
#' summarized_table <- mtcars |>
#'   group_by(cyl, vs) |>
#'   summarise(N = n(),
#'             mean_hp = mean(hp),
#'             sd_hp = sd(hp),
#'             mean_wt = mean(wt),
#'             sd_wt = sd(wt))
#'
#' # Create a tablespan:
#' tbl <- tablespan(data = summarized_table,
#'                  formula = Cylinder:cyl + Engine:vs ~
#'                    N +
#'                    (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
#'                    (`Weight` = Mean:mean_wt + SD:sd_wt),
#'                  title = "Motor Trend Car Road Tests",
#'                  subtitle = "A table created with tablespan",
#'                  footnote = "Data from the infamous mtcars data set.")
#'
#' tbl
#'
#' # Export as Excel table:
#' wb <- as_excel(tbl = tbl)
#'
#' # Save using openxlsx
#' # openxlsx::saveWorkbook(wb, "cars.xlsx")
#'
#' # Export as gt:
#' gt_tbl <- as_gt(tbl = tbl)
#' gt_tbl
tablespan <- function(data,
                      formula,
                      title = NULL,
                      subtitle = NULL,
                      footnote = NULL){

  if(!tibble::is_tibble(data)){
    warning("Tablespan uses tibble internally. Translating data to tibble")
    data <- tibble::as_tibble(data)
  }

  deparsed <- deparse_formula(formula)

  variables <- get_variables(deparsed)

  check_variables(data = data,
                  variables = variables)

  table_data <- list(row_data = get_row_data(data = data,
                                             row_variables = variables$row_variables),
                     col_data = get_col_data(data = data,
                                             col_variables = variables$col_variables))

  header <- construct_header(deparsed)

  bt_result <- list(title = title,
                    subtitle = subtitle,
                    header = header,
                    table_data = table_data,
                    footnote = footnote)
  class(bt_result) <- "Tablespan"
  return(bt_result)
}

#' check_variables
#'
#' Checks if the variables specified in the table formula exist in the data set
#'
#' @param data data set
#' @param variables variable names from formula
#' @noRd
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

#' get_col_data
#'
#' Extracts the data that will be used as row names in the table
#'
#' @param data data set
#' @param col_variables variable names from formula
#' @noRd
get_col_data <- function(data, col_variables){
  return(data[,col_variables, drop = FALSE])
}

#' get_row_data
#'
#' Extracts the data that will be used as body in the table
#'
#' @param data data set
#' @param row_variables variable names from formula
#' @noRd
get_row_data <- function(data, row_variables){
  if(!is.null(row_variables))
    return(data[,row_variables, drop = FALSE])
  return(NULL)
}

#' construct_header
#'
#' Adds width and depth (number of levels) to the deparsed
#' table description.
#'
#' @param deparsed table formula translated in nested list
#' @returns deparsed with widht and levels fields
#' @noRd
construct_header <- function(deparsed){

  deparsed$rhs <- add_header_width(deparsed$rhs)
  deparsed$rhs <- add_header_level(deparsed$rhs)

  deparsed$lhs <- add_header_width(deparsed$lhs)
  deparsed$lhs <- add_header_level(deparsed$lhs)

  return(deparsed)
}

#' add_header_width
#'
#' tablespan represents headers as (highly) nested lists. To determine how
#' wide each entry in the header must be (i.e., how many cells it will get in
#' the output), we have to get the number of root elements each parent element
#' spans. For example, in the following table, x spans two elements x1 and x2:
#' \preformatted{
#' |    x    |
#' | x1 | x2 |}
#'
#' add_header_width adds the span to each element in a table header.
#'
#' @param parsed_partial The left hand side or right hand side of the parsed
#' table
#' @returns the parsed_partial with additional width fields
#' @noRd
#' @examples
#' library(tablespan)
#' deparsed <- tablespan:::deparse_formula(formula =
#'  (`Row Name` = `Row 1` + `Row 2`) ~ `Column 1` + (`Column Banner` = `Column 2` + `Column 3`))
#' str(deparsed)
#'
#' deparsed <- tablespan:::add_header_width(deparsed$rhs)
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
#' tablespan represents headers as (highly) nested lists. To determine the level
#' at which each entry resides, we have to get the number of root elements below each
#' parent element. For example, in the following table, x is on level 2, but x1,
#' x2, y1, and y2 are at level 1:
#'
#' \preformatted{
#' |    x    |
#' | x1 | x2 | y1 | y2}
#'
#' add_header_level adds the level to each element in a table header.
#'
#' @param parsed_partial The left hand side or right hand side of the parsed
#' table
#' @returns the parsed_partial with additional level fields
#' @noRd
#' @examples
#' library(tablespan)
#' deparsed <- tablespan:::deparse_formula(formula =
#'  (`Row Name` = `Row 1` + `Row 2`) ~ `Column 1` + (`Column Banner` = `Column 2` + `Column 3`))
#' str(deparsed)
#'
#' deparsed <- tablespan:::add_header_level(deparsed$rhs)
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
