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
#' @keywords internal
#' @examples
#' library(basicTables)
#' deparsed <- basicTables:::deparse_formula(formula =
#'  (`Row Name` = `Row 1` + `Row 2`) ~ `Column 1` + (`Column Banner` = `Column 2` + `Column 3`))
#' str(deparsed)
deparse_formula <- function(formula){
  lhs <- deparse_formula_partial(formula_partial = formula[[2]])
  rhs <- deparse_formula_partial(formula_partial = formula[[3]])

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
#' @keywords internal
#' @examples
#' library(basicTables)
#' formula <- (`Row Name` = `Row 1` + `Row 2`) ~
#'   `Column 1` + (`Column Banner` = `Column 2` + `Column 3`)
#'
#' deparsed <- basicTables:::deparse_formula_partial(formula_partial = formula[[2]])
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
                                    entries = NULL)))
    return(deparsed)
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
    # In case of a brace, we have to go one step deeper
    deparsed$entries <- c(deparsed$entries,
                          list(deparse_formula_partial(formula_partial[[2]],
                                                   list(name = NULL,
                                                        entries = list()))))
    return(deparsed)
  }else{
    stop(paste0("Unknown symbol detected: ", formula_partial))
  }
}
