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

  min_val <- min(data, na.rm = TRUE)
  mean_val <- mean(unlist(c(data)), na.rm = TRUE)
  max_val <- max(data, na.rm = TRUE)

  # we add minimal slack to avoid issues with numerical precision
  slack <- abs(max_val - min_val) / (1e8)
  min_val <- min_val - slack
  max_val <- max_val + slack

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
