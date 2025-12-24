#' as_hux
#'
#' Translates a table created with tablespan to a huxtable. See <https://hughjonesd.github.io/huxtable/index.html>.
#'
#' Huxtable is an extremely versatile table creator for R. Once translated to a huxtable, the tablespan table
#' is easy to export to all formats directly supported by huxtable.
#'
#' @param tbl table created with tablespan::tablespan
#' @returns huxtable that can be further adapted with the gt package.
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
#' if(require_huxtable(throw = FALSE)){
#'   hux_tbl <- as_hux(tbl)
#'   hux_tbl
#' }
as_hux <- function(tbl) {
  require_huxtable()

  if (!is.null(tbl$header$lhs)) {
    tbl_body <- cbind(tbl$table_data$row_data, tbl$table_data$col_data)
  } else {
    tbl_body <- tbl$table_data$col_data
  }

  tbl_hux <- huxtable::as_hux(
    tbl_body,
    add_colnames = FALSE,
    add_rownames = FALSE
  )

  updated_tables <- hux_add_headers(tbl, tbl_hux)
  tbl_hux <- updated_tables$tbl_hux
  header_table <- updated_tables$header_table

  tbl_hux <- hux_add_borders(
    tbl = tbl,
    tbl_hux = tbl_hux,
    header_table = header_table
  )

  tbl_hux <- hux_add_title(tbl = tbl, tbl_hux = tbl_hux)

  tbl_hux <- hux_add_footnote(tbl = tbl, tbl_hux = tbl_hux)

  tbl_hux <- tbl_hux |>
    style_hux(tbl = tbl)

  return(tbl_hux)
}


#' hux_insert_header_entries
#'
#' Insert header entries into a matrix for hux
#'
#' @param header_partial part of the header list
#' @param max_level depth of the header list
#' @param column_offset offset to write data in columns
#' @param header_table table in which the header entries should be inserted
#' @returns header_table with entries
#' @noRd
hux_insert_header_entries <- function(
  header_partial,
  max_level,
  column_offset,
  header_table
) {
  require_huxtable()
  if (header_partial$name != "_BASE_LEVEL_") {
    header_table[
      max_level - header_partial$level,
      column_offset
    ] <- header_partial$name
    # also need to save that we have to merge all cells belonging to this header
    if (header_partial$width > 1) {
      attr(header_table, "to_merge")[[
        length(attr(header_table, "to_merge")) + 1
      ]] <- list(
        "row" = max_level - header_partial$level,
        "columns" = column_offset:(column_offset + header_partial$width - 1)
      )
    }
  }
  if (!is.null(header_partial$entries)) {
    for (i in seq_along(header_partial$entries)) {
      header_table <- hux_insert_header_entries(
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

hux_add_headers <- function(tbl, tbl_hux) {
  require_huxtable()
  if (!is.null(tbl$header$lhs)) {
    max_level <- max(tbl$header$lhs$level, tbl$header$rhs$level)
    max_col <- tbl$header$lhs$width + tbl$header$rhs$width
  } else {
    max_level <- tbl$header$rhs$level
    max_col <- tbl$header$rhs$width
  }

  # add all headers
  header_table <- matrix(
    NA,
    nrow = max_level - 1, # remove base level
    ncol = max_col
  )

  attr(header_table, "to_merge") <- list()

  if (!is.null(tbl$header$lhs)) {
    header_table <- hux_insert_header_entries(
      header_partial = tbl$header$lhs,
      max_level = max_level,
      column_offset = 1,
      header_table = header_table
    )
  }

  header_table <- hux_insert_header_entries(
    header_partial = tbl$header$rhs,
    max_level = max_level,
    column_offset = tbl$header$lhs$width + 1,
    header_table = header_table
  )

  # Add the header columns to the hux table
  for (ro in rev(1:nrow(header_table))) {
    tbl_hux <- tbl_hux |>
      huxtable::insert_row(header_table[ro, ], after = 0)
  }

  # And now we can merge the table headers
  for (to_merge in attr(header_table, "to_merge")) {
    tbl_hux <- tbl_hux |>
      huxtable::merge_cells(row = to_merge$row, col = to_merge$columns)
  }

  return(list(tbl_hux = tbl_hux, header_table = header_table))
}

hux_add_borders <- function(tbl, tbl_hux, header_table) {
  require_huxtable()
  # Add borders
  # All header borders
  tbl_hux <- tbl_hux |>
    huxtable::set_all_borders(row = 1:nrow(header_table), col = 1:ncol(tbl_hux))

  # Remove vertical borders between empty neighbor cells
  for (row in 1:nrow(header_table)) {
    for (col in 1:ncol(tbl_hux)) {
      if (col == 1) {
        next
      }
      left_empty <- tbl_hux[row, col - 1] %in% c("", NA)
      right_empty <- tbl_hux[row, col] %in% c("", NA)
      if (left_empty & right_empty) {
        huxtable::right_border(tbl_hux)[row, col - 1] <- 0
        huxtable::left_border(tbl_hux)[row, col] <- 0
      }
    }
  }

  # Remove horizontal borders between empty neighbor cells
  for (col in 1:ncol(header_table)) {
    for (row in 1:nrow(tbl_hux)) {
      if (row == nrow(tbl_hux)) {
        next
      }
      top_empty <- tbl_hux[row, col] %in% c("", NA)
      bottom_empty <- tbl_hux[row + 1, col] %in% c("", NA)
      if (top_empty & bottom_empty) {
        huxtable::bottom_border(tbl_hux)[row, col] <- 0
        huxtable::top_border(tbl_hux)[row + 1, col] <- 0
      }
    }
  }

  if (!is.null(tbl$header$lhs)) {
    # border between row names and data
    tbl_hux <- tbl_hux |>
      huxtable::set_right_border(col = tbl$header$lhs$width)
  }

  # Border below table
  tbl_hux <- tbl_hux |>
    huxtable::set_bottom_border(col = 1:ncol(tbl_hux), row = nrow(tbl_hux))

  # left and right borders
  tbl_hux <- tbl_hux |>
    huxtable::set_left_border(row = 1:nrow(tbl_hux), col = 1)
  tbl_hux <- tbl_hux |>
    huxtable::set_right_border(row = 1:nrow(tbl_hux), col = ncol(tbl_hux))

  return(tbl_hux)
}

hux_add_merged_row <- function(
  ht,
  text,
  border = 0.8,
  number_format = NA,
  ...
) {
  require_huxtable()
  # Copied from huxtable::add_footnote to style merged rows
  nr <- 1
  nc <- ncol(ht)
  ht <- ht |>
    huxtable::insert_row(rep("", nc))
  ht[nr, 1] <- text
  huxtable::colspan(ht)[nr, 1] <- nc
  ht <- huxtable::set_left_border(ht, nr, 1, 0)
  ht <- huxtable::set_right_border(ht, nr, 1, 0)
  ht <- huxtable::set_bottom_border(ht, nr, 1, 0)
  if (!is.null(border)) {
    ht <- huxtable::set_bottom_border(ht, nr, huxtable::everywhere, border)
  }
  huxtable::wrap(ht)[nr, 1] <- TRUE
  if (!missing(...)) {
    ht <- huxtable::set_cell_properties(ht, nr, 1, ...)
  }
  ht <- huxtable::set_number_format(ht, nr, 1, number_format)
  return(ht)
}

hux_add_title <- function(tbl, tbl_hux) {
  require_huxtable()
  if (!is.null(tbl$subtitle)) {
    tbl_hux <- hux_add_merged_row(ht = tbl_hux, text = tbl$subtitle)
  }
  if (!is.null(tbl$title)) {
    set_border <- if (!is.null(tbl$subtitle)) NULL else .8
    tbl_hux <- hux_add_merged_row(
      ht = tbl_hux,
      text = tbl$title,
      border = set_border
    )
  }
  return(tbl_hux)
}

hux_add_footnote <- function(tbl, tbl_hux) {
  require_huxtable()
  if (!is.null(tbl$footnote)) {
    tbl_hux <- tbl_hux |>
      huxtable::add_footnote(text = tbl$footnote)
  }
  return(tbl_hux)
}

style_hux <- function(tbl_hux, tbl) {
  require_huxtable()
  # Style the title
  if (!is.null(tbl$styles$title$hux) & !is.null(tbl$title)) {
    for (sty in tbl$styles$title$hux) {
      tbl_hux <- sty(tbl_hux, row = 1, col = 1:ncol(tbl_hux))
    }
  }
  if (!is.null(tbl$styles$subtitle$hux) & !is.null(tbl$subtitle)) {
    for (sty in tbl$styles$subtitle$hux) {
      tbl_hux <- sty(
        tbl_hux,
        row = 1 * (!is.null(tbl$title)) + 1 * (!is.null(tbl$subtitle)),
        col = 1:ncol(tbl_hux)
      )
    }
  }

  if (!is.null(tbl$styles$footnote$hux) & !is.null(tbl$footnote)) {
    for (sty in tbl$styles$footnote$hux) {
      tbl_hux <- sty(
        tbl_hux,
        row = nrow(tbl_hux),
        col = 1:ncol(tbl_hux)
      )
    }
  }

  if (!is.null(tbl$styles$header$hux)) {
    start_header <- 1 * (!is.null(tbl$title)) + 1 * (!is.null(tbl$subtitle)) + 1
    if (!is.null(tbl$header$lhs)) {
      end_header <- 1 *
        (!is.null(tbl$title)) +
        1 * (!is.null(tbl$subtitle)) +
        max(tbl$header$lhs$level, tbl$header$rhs$level) -
        1
    } else {
      end_header <- 1 *
        (!is.null(tbl$title)) +
        1 * (!is.null(tbl$subtitle)) +
        tbl$header$rhs$level -
        1
    }
    for (sty in tbl$styles$header$hux) {
      tbl_hux <- sty(
        tbl_hux,
        row = start_header:end_header,
        col = 1:ncol(tbl_hux)
      )
    }
  }

  # we don't style the header here, only the data. Therefore, we
  # have to add an offset for the header and the title
  if (!is.null(tbl$header$lhs)) {
    start_at <- 1 *
      (!is.null(tbl$title)) +
      1 * (!is.null(tbl$subtitle)) +
      max(tbl$header$lhs$level, tbl$header$rhs$level)
  } else {
    start_at <- 1 *
      (!is.null(tbl$title)) +
      1 * (!is.null(tbl$subtitle)) +
      tbl$header$rhs$level
  }

  # Apply any custom styles
  for (column_name in names(tbl$styles$columns)) {
    for (c_style in tbl$styles$columns[[column_name]]) {
      if (is.null(c_style$style$hux)) {
        next
      }
      if (length(c_style$style$hux) == 0) {
        next
      }
      if (is.null(c_style$rows)) {
        end_at <- nrow(tbl_hux) - 1 * (!is.null(tbl$footnote))
        rows <- start_at:end_at
      }
      for (style_fun in c_style$style$hux) {
        tbl_hux <- tbl_hux |>
          style_fun(row = rows, col = column_name)
      }
    }
  }

  # Apply custom formatting to columns
  # Apply formats
  for (column_name in names(tbl$formats$columns)) {
    for (c_format in tbl$formats$columns[[column_name]]) {
      if (is.null(c_format$format$hux)) {
        next
      }

      if (is.null(c_format$rows)) {
        # we also don't style the footnote here
        end_at <- nrow(tbl_hux) - 1 * (!is.null(tbl$footnote))
        rows <- start_at:end_at
      } else {
        # add offset for the title and header
        rows <- start_at + c_format$rows - 1
      }

      tbl_hux <- tbl_hux |>
        c_format$format$hux(col = column_name, row = rows)
    }
  }

  return(tbl_hux)
}

#' require_huxtable
#'
#' Check that huxtable is installed
#' @param throw throw error if the package is not installed
#' @returns boolean or error
#' @export
#' @examples
#' library(tablespan)
#' require_huxtable()
require_huxtable <- function(throw = TRUE) {
  if (!requireNamespace("huxtable", quietly = TRUE)) {
    if (throw) {
      stop(
        "Using as_hux requires the huxtable package. Please install with install.packages('huxtable')"
      )
    }
    return(FALSE)
  }
  return(TRUE)
}
