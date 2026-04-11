#' print.Tablespan
#'
#' @param x result from tablespan
#' @param digits number of digits to round doubles to
#' @param n number of rows to print
#' @param use_hux if set to TRUE and huxtable is installed, huxtable will be used
#' to print the tablespan table. This allows for styling to be printed
#' @param ... additional arguments passed to prmatrix or huxtable (if use_hux = TRUE)
#' @returns nothing
#' @importFrom utils head
#' @export
#' @examples
#' data("iris")
#' tbl <- tablespan(data = iris[iris$Species == "setosa", ],
#'           formula = Species ~ (Sepal = Sepal.Length + Sepal.Width) +
#'             (Petal = Petal.Length + Petal.Width))
#' print(tbl)
print.Tablespan <- function(
  x,
  digits = 2,
  n = 3,
  use_hux = require_huxtable(throw = FALSE),
  ...
) {
  if (use_hux) {
    require_huxtable()
    huxtable::print_screen(as_huxtable.Tablespan(x), ...)
    return(invisible(NULL))
  }

  cat(as_string(tbl = x, digits = digits, n = n, ...))
}
