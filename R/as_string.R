#' as_string
#'
#' @param tbl result from tablespan
#' @param digits number of digits to round doubles to
#' @param n number of rows to print
#' to print the tablespan table. This allows for styling to be printed
#' @param ... additional arguments passed to prmatrix or huxtable (if use_hux = TRUE)
#' @returns nothing
#' @importFrom utils head
#' @importFrom utils capture.output
#' @export
#' @examples
#' library(tablespan)
#' library(dplyr)
#' data("mtcars")
#'
#' summarized_table <- mtcars |>
#'   group_by(cyl, vs) |>
#'   summarise(N = n(),
#'             mean_hp = mean(hp),
#'             sd_hp = sd(hp),
#'             mean_wt = mean(wt),
#'             sd_wt = sd(wt))
#'
#' tbl <- tablespan(data = summarized_table,
#'                  formula = (LHS = Cylinder:cyl + Engine:vs) ~
#'                    N +
#'                    (Results = (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
#'                       (`Weight` = Mean:mean_wt + SD:sd_wt)))
#' cat(as_string(tbl))
as_string <- function(
  tbl,
  digits = 2,
  n = 3,
  ...
) {
  if (!is.null(tbl$header$lhs)) {
    max_level <- max(tbl$header$lhs$level, tbl$header$rhs$level)
    max_col <- tbl$header$lhs$width + tbl$header$rhs$width
  } else {
    max_level <- tbl$header$rhs$level
    max_col <- tbl$header$rhs$width
  }

  header_table <- matrix(
    NA,
    nrow = max_level + min(n, nrow(tbl$table_data$col_data)),
    ncol = max_col + !is.null(tbl$header$lhs)
  )

  if (!is.null(tbl$header$lhs)) {
    header_table <- print_insert_header_entries(
      header_partial = tbl$header$lhs,
      max_level = max_level,
      column_offset = 1,
      header_table = header_table
    )
  }

  header_table <- print_insert_header_entries(
    header_partial = tbl$header$rhs,
    max_level = max_level,
    column_offset = ifelse(
      is.null(tbl$header$lhs),
      1,
      tbl$header$lhs$width + 2
    ),
    header_table = header_table
  )

  # add data
  rws <- max_level:(max_level + min(n, nrow(tbl$table_data$col_data)) - 1) + 1
  if (!is.null(tbl$header$lhs)) {
    cls <- 1:ncol(tbl$table_data$row_data)
    header_table[rws, cls] <- tbl$table_data$row_data |>
      sapply(
        function(x) {
          if (is.numeric(x) & !is.integer(x)) {
            as.character(round(x, digits))
          } else {
            as.character(x)
          }
        },
        # we prevent simplification because otherwise sapply drops to a
        # vector when using single-row tibbles. This is why we need the
        # somewhat weird workaround using tibble and as.matrix.
        simplify = FALSE
      ) |>
      tibble::as_tibble() |>
      as.matrix() |>
      utils::head(n = n)

    # add vertical line
    header_table[, max(cls) + 1] <- "|"

    cls <- max(cls) + 1 + 1:ncol(tbl$table_data$col_data)
  } else {
    cls <- 1:ncol(tbl$table_data$col_data)
  }

  header_table[rws, cls] <- tbl$table_data$col_data |>
    sapply(
      function(x) {
        if (is.numeric(x) & !is.integer(x)) {
          as.character(round(x, digits))
        } else {
          as.character(x)
        }
      },
      # we prevent simplification because otherwise sapply drops to a
      # vector when using single-row tibbles. This is why we need the
      # somewhat weird workaround using tibble and as.matrix.
      simplify = FALSE
    ) |>
    tibble::as_tibble() |>
    as.matrix() |>
    utils::head(n = n)

  # add horizontal line
  header_table[max_level, ] <- header_table |>
    apply(2, function(x) max(nchar(x), na.rm = TRUE)) |>
    sapply(function(x) paste0(rep("-", x), collapse = ""))

  # add ...
  if (n < nrow(tbl$table_data$col_data)) {
    header_table <- header_table |>
      rbind("...")
    if (!is.null(tbl$header$lhs)) {
      header_table[nrow(header_table), ncol(tbl$table_data$row_data) + 1] <- "|"
    }
  }

  # add vertical lines
  header_table <- cbind("|", header_table) |>
    cbind("|")

  tbl_string <- capture.output({
    if (!is.null(tbl$title)) {
      cat(paste0(tbl$title, "\n"))
    }
    if (!is.null(tbl$subtitle)) {
      cat(paste0(tbl$subtitle, "\n"))
    }
    prmatrix(
      header_table,
      quote = FALSE,
      na.print = "",
      rowlab = rep("", nrow(header_table)),
      collab = rep("", ncol(header_table)),
      ...
    )
    if (!is.null(tbl$footnote)) {
      cat(paste0(tbl$footnote, "\n"))
    }
  })

  return(paste(tbl_string, collapse = "\n"))
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
print_insert_header_entries <- function(
  header_partial,
  max_level,
  column_offset,
  header_table
) {
  if (header_partial$name != "_BASE_LEVEL_") {
    header_table[
      max_level - header_partial$level,
      column_offset
    ] <- header_partial$name
  }
  if (!is.null(header_partial$entries)) {
    for (i in seq_along(header_partial$entries)) {
      header_table <- print_insert_header_entries(
        header_partial = header_partial$entries[[i]],
        max_level = max_level,
        column_offset = column_offset,
        header_table = header_table
      )
      column_offset <- column_offset + header_partial$entries[[i]]$width
    }
  }
  return(header_table)
}
