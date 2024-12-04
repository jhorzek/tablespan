#' deparse_formula
#'
#' Translates an R formula into nested lists for both the left and the right
#' hand side to create table rows and banners. Importantly, this function
#' can only parse a specific subset of formulas that allows for the following
#' grammar:
#'
#' 1) Braces group columns
#' 2) Equal signs assign the left hand side as name for multiple columns
#' 3) plus signs combine columns
#'
#' @param formula an R formula following the pattern above
#' @returns nested lists for the left hand side and right hand side of the formula
#' @noRd
#' @examples
#' library(tablespan)
#' deparsed <- tablespan:::deparse_formula(formula =
#'  (`Row Name` = `Row 1` + `Row 2`) ~ `Column 1` + (`Column Banner` = `Column 2` + `Column 3`))
#' str(deparsed)
deparse_formula <- function(formula){
  if(formula[[2]] == "1"){
    lhs <- NULL
  }else{
    lhs <- deparse_formula_partial(formula_partial = formula[[2]])
  }

  if(formula[[3]] == "1"){
    stop("Missing right hand side of formula.")
  }else{
    rhs <- deparse_formula_partial(formula_partial = formula[[3]])
  }

  return(list(lhs = lhs,
              rhs = rhs))
}

#' deparse_formula_partial
#'
#' Translates the left of right hand side of an R formula into a nested list. Importantly, this function
#' can only parse a specific subset of formulas that allows for the following
#' grammar:
#'
#' 1) Braces group columns
#' 2) Equal signs assign the left hand side as name for multiple columns
#' 3) plus signs combine columns
#'
#' @param formula_partial the left or right hand side of an R formula following the pattern above
#' @param deparsed this function is a recursive function and makes use of the deparsed list.
#' Don't change this manually.
#' @returns a nested list with formula elements
#' @importFrom methods is
#' @noRd
#' @examples
#' library(tablespan)
#' formula <- (`Row Name` = `Row 1` + `Row 2`) ~
#'   `Column 1` + (`Column Banner` = `Column 2` + `Column 3`)
#'
#' deparsed <- tablespan:::deparse_formula_partial(formula_partial = formula[[2]])
#' str(deparsed)
deparse_formula_partial <- function(formula_partial,
                                    deparsed = list(name = "_BASE_LEVEL_", entries = list())){
  # There are three types of symbols on the right hand side:
  # 1) Braces group columns
  # 2) Equal signs assign the left hand side as name for multiple columns
  # 3) plus signs combine columns
  # We want to translate the above in a list. For example, we want
  # (a = b1 + b2)
  # to become
  # list(a = c("b1", "b2"))

  if(length(formula_partial) == 1){
    deparsed$entries <- c(deparsed$entries,
                          list(list(name = as.character(formula_partial),
                                    item_name = as.character(formula_partial),
                                    entries = NULL)))
    return(deparsed)
  }

  if((formula_partial[[1]] == ":")){
    if((length(formula_partial[[2]]) == 1) & (length(formula_partial[[3]]) == 1)){
      deparsed$entries <- c(deparsed$entries,
                            list(list(name = as.character(formula_partial[[2]]),
                                      item_name = as.character(formula_partial[[3]]),
                                      entries = NULL)))
      return(deparsed)
    }else{
      stop(paste0("Renaming with ", as.character(formula_partial) ,
                  " is not allowed. Both sides of the colon must be single names (e.g., a:b)."))
    }
  }

  if(formula_partial[[1]] == "+"){
    deparsed <- deparse_formula_partial(formula_partial[[2]],
                                        deparsed)
    deparsed <- deparse_formula_partial(formula_partial[[3]],
                                        deparsed)
    return(deparsed)
  }else if(formula_partial[[1]] == "="){
    # The left hand side is the name of the split, the right hand side
    # specifies the elements
    deparsed$name <- as.character(formula_partial[[2]])
    deparsed <- deparse_formula_partial(formula_partial[[3]],
                                        deparsed)
    return(deparsed)
  }else if(formula_partial[[1]] == "("){
    # Check if there is a name for the current spanner
    try_name <- try(formula_partial[[2]][[1]] != "=", silent = TRUE)
    if(is(try_name, "try-error") | try_name){
      stop("The following spanner has no label: ", formula_partial, ".")
    }
    # In case of a brace, we have to go one step deeper
    deparsed$entries <- c(deparsed$entries,
                          list(deparse_formula_partial(formula_partial[[2]],
                                                       list(name = NULL,
                                                            entries = list()))))
    return(deparsed)
  }else{
    stop(paste0("Unknown symbol detected: ", paste0(as.character(formula_partial), collapse = "")))
  }
}


#' get_variables
#'
#' Extracts the variable names from a deparsed formula (see ?tablespan:::deparse_formula).
#'
#' @param deparsed_formula result from tablespan:::deparse_formula
#' @returns a list with the names of the variables that build the rows and columns
#' @noRd
#' @examples
#' library(tablespan)
#' deparsed <- tablespan:::deparse_formula(formula =
#'  (`Row Name` = `Row 1` + `Row 2`) ~ `Column 1` + (`Column Banner` = `Column 2` + `Column 3`))
#' str(deparsed)
#' tablespan:::get_variables(deparsed)
get_variables <- function(deparsed_formula){
  if(is.null(deparsed_formula$lhs)){
    # no row variable
    row_variables <- NULL
  }else{
    row_variables <- get_variables_from_list(deparsed_formula_element = deparsed_formula$lhs)
  }

  col_variables <- get_variables_from_list(deparsed_formula_element = deparsed_formula$rhs)

  return(
    list(row_variables = row_variables,
         col_variables = col_variables)
  )
}

#' get_variables_from_list
#'
#' Extracts the variable names from the left or right hand side of a
#' deparsed formula (see ?tablespan:::deparse_formula).
#'
#' @param deparsed_formula_element left or right hand side of the result from
#' tablespan:::deparse_formula
#' @param variables the function is recursive and fills the variable vector
#' @returns a vector with the names of the variables
#' @noRd
#' @examples
#' library(tablespan)
#' deparsed <- tablespan:::deparse_formula(formula =
#'  (`Row Name` = `Row 1` + `Row 2`) ~ `Column 1` + (`Column Banner` = `Column 2` + `Column 3`))
#' str(deparsed)
#' tablespan:::get_variables_from_list(deparsed$lhs)
get_variables_from_list <- function(deparsed_formula_element, variables = c()){
  if(is.null(deparsed_formula_element$entries)){
    variables <- c(variables, deparsed_formula_element$item_name)
    return(variables)
  }else{
    for(entry in deparsed_formula_element$entries)
      variables <- get_variables_from_list(entry, variables)
    return(variables)
  }
}
