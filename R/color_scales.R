#' add_style_color_scale
#'
#' Add a color scale to the table.
#'
#' @param styles list with existing styles
#' @param color_scale vector with two or three color values
#' @param rows vector with rows to apply the style to
#' @returns styles with appended color scale
#' @noRd
add_style_color_scale <- function(styles, color_scale, rows) {
  if (!length(color_scale) %in% c(2, 3)) {
    stop("color_scale must be of length 2 or 3.")
  }

  style <- list()

  style$gt <- create_color_scale_gt(color_scale = color_scale)
  style$flex <- list(create_color_scale_flex(color_scale = color_scale))
  style$openxlsx <- create_color_scale_openxlsx(color_scale = color_scale)
  style$hux <- list(create_color_scale_hux(color_scale = color_scale))

  styles <- append(
    styles,
    list(list(
      "style" = style,
      "rows" = rows
    ))
  )
  return(styles)
}

#' Preprocess color scale for conditional formatting
#'
#' Processes a color scale vector to ensure it's properly formatted for conditional
#' formatting across different table packages. Handles NA values by automatically
#' filling them with appropriate values (min, mean, or max) from the data.
#'
#' @param tbl A table object (gt, flextable, huxtable, or openxlsx) containing the data
#' @param color_scale A named vector of length 2 or 3 specifying the color scale.
#'   Values should be numeric and colors should be hex codes. Example:
#'   `c("#EE2F43" = -1, "#FFFFFF" = 0, "#37E65A" = 1)`. NA values will be automatically
#'   filled with appropriate values from the data.
#' @param column_names Character vector of column names to apply the color scale to
#' @param rows Numeric vector of row indices to apply the color scale to. If NULL,
#'   applies to all rows.
#' @returns A properly formatted color scale vector with all NA values filled in
#' @noRd
preprocess_color_scale <- function(tbl, color_scale, column_names, rows) {
  if (is.null(color_scale)) {
    return(color_scale)
  }
  if (!length(color_scale) %in% c(2, 3)) {
    stop("color_scale must be of length 2 or 3.")
  }

  if (is.null(names(color_scale))) {
    stop(
      'color_scale must be a named vector (e.g., color_scale = c("#EE2F43" = -1, "#FFFFFF" = 0, "#37E65A" = 1))'
    )
  }

  if (!anyNA(color_scale)) {
    check_color_scale_increasing(color_scale = color_scale)
    return(color_scale)
  }

  # fill in NAs
  if (!is.null(tbl$header$lhs)) {
    data <- cbind(tbl$table_data$row_data, tbl$table_data$col_data)
  } else {
    data <- tbl$table_data$col_data
  }

  data <- data |>
    dplyr::select(dplyr::all_of(column_names))
  if (!is.null(rows)) {
    data <- data |>
      dplyr::slice(rows)
  }

  min_val <- min(data, na.rm = TRUE) - 2e-11 # we add minimal slack to avoid issues with numerical precision
  mean_val <- mean(unlist(c(data)), na.rm = TRUE)
  max_val <- max(data, na.rm = TRUE) + 2e-11 # we add minimal slack to avoid issues with numerical precision

  if (is.na(color_scale[1])) {
    color_scale[1] <- min_val
  }
  if (length(color_scale) == 2) {
    if (is.na(color_scale[2])) {
      color_scale[2] <- max_val
    }
  } else if (length(color_scale) == 3) {
    if (is.na(color_scale[2])) {
      color_scale[2] <- mean_val
    }
    if (is.na(color_scale[3])) {
      color_scale[3] <- max_val
    }
  }

  check_color_scale_increasing(color_scale = color_scale)
  return(color_scale)
}

check_color_scale_increasing <- function(color_scale) {
  for (i in 2:length(color_scale)) {
    if (color_scale[i] <= color_scale[i - 1]) {
      stop(
        "The values of the color_scale must be increasing. Got ",
        color_scale,
        " instead. Please adjust."
      )
    }
  }
}

#' create_color_scale_openxlsx
#'
#' Create a color scale style for openlslx
#' @param color_scale vector with two or three color values
#' @returns openlslx style object
#' @noRd
create_color_scale_openxlsx <- function(color_scale) {
  if (!require_openxlsx(throw = FALSE)) {
    return(NULL)
  }
  return(
    function(wb, sheet, rows, cols) {
      openxlsx::conditionalFormatting(
        wb = wb,
        sheet = sheet,
        cols = cols,
        rows = rows,
        type = "colourScale",
        rule = color_scale,
        style = names(color_scale)
      )
    }
  )
}

#' create_color_scale_gt
#'
#' Create a color scale style for gt
#' @param color_scale vector with two or three color values
#' @returns function to style the gt object
#' @noRd
create_color_scale_gt <- function(color_scale) {
  if (!require_gt(throw = FALSE)) {
    return(NULL)
  }
  fn <- create_color_scale_function(color_scale = color_scale)

  return(
    function(data, column, rows) {
      return(
        data |>
          gt::data_color(
            columns = gt::all_of(column),
            rows = rows,
            fn = fn
          )
      )
    }
  )
}

#' create_color_scale_function
#'
#' Creates a function that takes a value x and returns a corresponding color
#' @param color_scale vector with two or three color values
#' @returns function that can be used to create a color
#' @importFrom scales col_numeric
#' @noRd
create_color_scale_function <- function(color_scale) {
  if (length(color_scale) == 3) {
    # Adapted from Paul at https://stackoverflow.com/questions/64469714/set-asymmetric-midpoint-for-data-color-in-gt-table
    lower_scale <- scales::col_numeric(
      palette = names(color_scale)[1:2],
      domain = color_scale[1:2]
    )
    upper_scale <- scales::col_numeric(
      palette = names(color_scale)[2:3],
      domain = color_scale[2:3]
    )
    color_fun <- function(x) {
      color <- suppressWarnings(ifelse(
        x < color_scale[2],
        lower_scale(x),
        upper_scale(x)
      ))
      color <- ifelse(is.na(color), "#D3D3D300", color)
      return(color)
    }
  } else if (length(color_scale) == 2) {
    color_fun <- scales::col_numeric(
      palette = names(color_scale),
      domain = color_scale,
      na.color = "#D3D3D300"
    )
  } else {
    stop(
      "Could not create a color scale from ",
      color_scale,
      ". Expected 2 or 3 named values (e.g., color_scale = c('#EE2F43' = -1, '#37E65A' = 1)."
    )
  }
  return(color_fun)
}

create_color_scale_hux <- function(color_scale) {
  return(function(tbl, row, col) {
    tbl |>
      huxtable::map_background_color(
        do.call(
          huxtable::by_colorspace,
          c(
            as.list(names(color_scale)),
            list(
              range = color_scale,
              na_color = NA,
              ignore_na = TRUE,
              colwise = FALSE
            )
          )
        ),
        row = row,
        col = col
      )
  })
}

create_color_scale_flex <- function(color_scale) {
  fn <- create_color_scale_function(color_scale = color_scale)

  return(function(tbl, row, col, part) {
    tbl |>
      flextable::bg(i = row, j = col, bg = fn, part = part)
  })
}
