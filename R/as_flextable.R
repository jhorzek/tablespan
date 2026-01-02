#' as_flextable
#'
#' Translates a table created with tablespan to a flextable. See <https://ardata-fr.github.io/flextable-book/>.
#'
#' Flextable is an extremely versatile table creator for R with great support to export to a variety of formats.
#'
#' @param x table created with tablespan::tablespan
#' @param theme a theme to apply to the flextable. Use one of the flextable::theme_* functions
#' @param ... additional arguments passed to flextable::as_flextable
#' @returns flextable that can be further adapted with the gt package.
#' @exportS3Method flextable::as_flextable
#' @method as_flextable Tablespan
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
#' if(require_flextable(throw = FALSE)){
#'   library(flextable)
#'   flex_tbl <- as_flextable(tbl)
#'   flex_tbl
#' }
as_flextable.Tablespan <- function(x, theme = flextable::theme_booktabs, ...) {
  require_flextable()

  if (!is.null(x$header$lhs)) {
    tbl_body <- cbind(x$table_data$row_data, x$table_data$col_data)
  } else {
    tbl_body <- x$table_data$col_data
  }

  tbl_flex <- flextable::flextable(
    tbl_body,
    ...
  )

  updated_tables <- flex_add_headers(x, tbl_flex)
  tbl_flex <- updated_tables$tbl_flex
  header_table <- updated_tables$header_table
  header_width <- updated_tables$header_width

  # For flextable, the theme_booktabs option is already very close to
  # what we want. Therefore, we will not manually add borders but just
  # rely on that theme by default.
  tbl_flex <- tbl_flex |>
    theme()

  tbl_flex <- flex_add_title(tbl = x, tbl_flex = tbl_flex)

  tbl_flex <- flex_add_footnote(tbl = x, tbl_flex = tbl_flex)

  tbl_flex <- tbl_flex |>
    style_flex(tbl = x)

  tbl_flex <- tbl_flex |>
    flextable::autofit()

  return(tbl_flex)
}


#' flex_insert_header_entries
#'
#' Insert header entries into a matrix for flextable
#'
#' @param header_partial part of the header list
#' @param max_level depth of the header list
#' @param column_offset offset to write data in columns
#' @param header_table table in which the header entries should be inserted
#' @param header_width table with values encoding the width of each cell
#' @returns header_table with entries
#' @noRd
flex_insert_header_entries <- function(
  header_partial,
  max_level,
  column_offset,
  header_table,
  header_width
) {
  require_flextable()
  if (header_partial$name != "_BASE_LEVEL_") {
    header_table[
      max_level - header_partial$level,
      column_offset
    ] <- header_partial$name
    # also need to save that we have to merge all cells belonging to this header
    header_width[
      max_level - header_partial$level,
      column_offset
    ] <- header_partial$width
    if (header_partial$width > 1) {
      header_width[
        max_level - header_partial$level,
        (column_offset + 1):(column_offset + header_partial$width - 1)
      ] <- NA
    }
  }
  if (!is.null(header_partial$entries)) {
    for (i in seq_along(header_partial$entries)) {
      recursive_out <- flex_insert_header_entries(
        header_partial = header_partial$entries[[i]],
        max_level = max_level,
        column_offset = column_offset,
        header_table = header_table,
        header_width = header_width
      )
      header_table <- recursive_out$header_table
      header_width <- recursive_out$header_width
      column_offset <- column_offset + header_partial$entries[[i]]$width
    }
  }
  return(list(header_table = header_table, header_width = header_width))
}

flex_add_headers <- function(tbl, tbl_flex) {
  require_flextable()

  if (!is.null(tbl$header$lhs)) {
    max_level <- max(tbl$header$lhs$level, tbl$header$rhs$level)
    max_col <- tbl$header$lhs$width + tbl$header$rhs$width
    col_keys <- c(
      colnames(tbl$table_data$row_data),
      colnames(tbl$table_data$col_data)
    )
  } else {
    max_level <- tbl$header$rhs$level
    max_col <- tbl$header$rhs$width
    col_keys <- colnames(tbl$table_data$col_data)
  }

  # add all headers
  header_table <- matrix(
    "",
    nrow = max_level - 1, # remove base level
    ncol = max_col
  )

  header_width <- matrix(
    1,
    nrow = max_level - 1, # remove base level
    ncol = max_col
  )

  if (!is.null(tbl$header$lhs)) {
    header_table_width <- flex_insert_header_entries(
      header_partial = tbl$header$lhs,
      max_level = max_level,
      column_offset = 1,
      header_table = header_table,
      header_width = header_width
    )
    header_table <- header_table_width$header_table
    header_width <- header_table_width$header_width
  }

  header_table_width <- flex_insert_header_entries(
    header_partial = tbl$header$rhs,
    max_level = max_level,
    column_offset = ifelse(
      is.null(tbl$header$lhs$width),
      1,
      tbl$header$lhs$width + 1
    ),
    header_table = header_table,
    header_width = header_width
  )
  header_table <- header_table_width$header_table
  header_width <- header_table_width$header_width

  header_table[is.na(header_table)] <- ""

  # Add each row, but in reverse order
  for (i in rev(1:nrow(header_table))) {
    if (i == nrow(header_table)) {
      ref_table <- data.frame(
        key = col_keys,
        label = header_table[i, ]
      )
      tbl_flex <- tbl_flex |>
        flextable::set_header_df(mapping = ref_table, key = "key")
      next
    }

    tbl_flex <- tbl_flex |>
      flextable::add_header_row(
        values = header_table[i, !is.na(header_width[i, ])],
        colwidths = header_width[i, !is.na(header_width[i, ])]
      )
  }

  return(list(
    tbl_flex = tbl_flex,
    header_table = header_table,
    header_width = header_width
  ))
}

flex_add_title <- function(tbl, tbl_flex) {
  if (!is.null(tbl$title) | !is.null(tbl$subtitle)) {
    tbl_flex <- flextable::add_header_lines(
      tbl_flex,
      values = c(tbl$title, tbl$subtitle)
    )
  }
  tbl_flex <- tbl_flex |>
    flextable::align(align = "left", part = "header")
  return(tbl_flex)
}

flex_add_footnote <- function(tbl, tbl_flex) {
  if (!is.null(tbl$footnote)) {
    tbl_flex <- flextable::add_footer_lines(tbl_flex, values = tbl$footnote)
  }
  tbl_flex <- tbl_flex |>
    flextable::align(align = "left", part = "footer")
  return(tbl_flex)
}


#' style_flex
#'
#' Applies custom styling to a flextable based on the structure of the original tablespan table.
#'
#' This function applies various styling elements to the flextable, including:
#' - Title styling
#' - Subtitle styling
#' - Footnote styling
#' - Header styling
#' - Column-specific styling
#' - Column-specific formatting
#'
#' @param tbl_flex flextable object to style
#' @param tbl tablespan table object containing styling information
#' @returns flextable with custom styling applied
#' @noRd
style_flex <- function(tbl, tbl_flex) {
  require_flextable()
  # Style the title
  if (!is.null(tbl$styles$title$flex) & !is.null(tbl$title)) {
    for (sty in tbl$styles$title$flex) {
      tbl_flex <- sty(tbl_flex, row = 1, col = NULL, part = "header")
    }
  }
  if (!is.null(tbl$styles$subtitle$flex) & !is.null(tbl$subtitle)) {
    for (sty in tbl$styles$subtitle$flex) {
      tbl_flex <- sty(
        tbl_flex,
        row = 1 * (!is.null(tbl$title)) + 1 * (!is.null(tbl$subtitle)),
        col = NULL,
        part = "header"
      )
    }
  }

  if (!is.null(tbl$styles$footnote$flex) & !is.null(tbl$footnote)) {
    for (sty in tbl$styles$footnote$flex) {
      tbl_flex <- sty(
        tbl_flex,
        row = 1,
        col = NULL,
        part = "footer"
      )
    }
  }

  if (!is.null(tbl$styles$header$flex)) {
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
    for (sty in tbl$styles$header$flex) {
      tbl_flex <- sty(
        tbl_flex,
        row = start_header:end_header,
        col = NULL,
        part = "header"
      )
    }
  }

  # Apply any custom styles
  for (column_name in names(tbl$styles$columns)) {
    for (c_style in tbl$styles$columns[[column_name]]) {
      if (is.null(c_style$style$flex)) {
        next
      }
      if (length(c_style$style$flex) == 0) {
        next
      }
      if (is.null(c_style$rows)) {
        rows <- 1:nrow(tbl$table_data$col_data)
      } else {
        rows <- c_style$rows
      }
      for (style_fun in c_style$style$flex) {
        tbl_flex <- tbl_flex |>
          style_fun(row = rows, col = column_name, part = "body")
      }
    }
  }

  # Apply custom formatting to columns
  # Apply formats
  for (column_name in names(tbl$formats$columns)) {
    for (c_format in tbl$formats$columns[[column_name]]) {
      if (is.null(c_format$format$flex)) {
        next
      }

      if (is.null(c_format$rows)) {
        # we also don't style the footnote here
        rows <- 1:nrow(tbl$table_data$col_data)
      } else {
        rows <- c_format$rows
      }
      tbl_flex <- tbl_flex |>
        c_format$format$flex(col = column_name, row = rows, part = "body")
    }
  }

  return(tbl_flex)
}

#' require_flextable
#'
#' Check that flextable is installed
#' @param throw throw error if the package is not installed
#' @returns boolean or error
#' @export
#' @examples
#' library(tablespan)
#' require_flextable()
require_flextable <- function(throw = TRUE) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    if (throw) {
      stop(
        "Using as_flextable requires the flextable package. Please install with install.packages('flextable')"
      )
    }
    return(FALSE)
  }
  return(TRUE)
}
