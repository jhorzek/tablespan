#' initialize_styles_gt
#'
#' Sets the default styles for gt tables in a tablespan table.
#'
#' This function adds default styling for gt tables to the provided styles list.
#' If the gt package is not available, the original styles are returned unchanged.
#'
#' @returns a list with default styles for gt tables added to the input styles
#' @noRd
initialize_styles_gt <- function(tbl) {
  require_gt()

  styles <- list()

  default <- function(gt_tbl) {
    return(
      gt_tbl
    )
  }
  styles$title$gt <- default
  styles$subtitle$gt <- default
  styles$header$gt <- default
  styles$header_cells$gt <- default
  styles$footnote$gt <- default
  styles$hline$gt <- default
  styles$vline$gt <- default
  return(styles)
}

style_title_gt <- function(gt_styles, tbl) {
  require_gt()

  force(tbl)

  gt_styles$title$gt <- function(gt_tbl) {
    return(
      gt_tbl |>
        gt::tab_style(
          style = create_style_gt(
            font_size = tbl$styles$title$font_size,
            text_color = tbl$styles$title$text_color,
            bold = tbl$styles$title$bold,
            italic = tbl$styles$title$italic,
            background_color = tbl$styles$title$background_color
          ),
          locations = gt::cells_title(groups = "title")
        )
    )
  }

  return(gt_styles)
}

style_subtitle_gt <- function(gt_styles, tbl) {
  require_gt()

  force(tbl)

  gt_styles$subtitle$gt <- function(gt_tbl) {
    return(
      gt_tbl |>
        gt::tab_style(
          style = create_style_gt(
            font_size = tbl$styles$subtitle$font_size,
            text_color = tbl$styles$subtitle$text_color,
            bold = tbl$styles$subtitle$bold,
            italic = tbl$styles$subtitle$italic,
            background_color = tbl$styles$subtitle$background_color
          ),
          locations = gt::cells_title(groups = "subtitle")
        )
    )
  }

  return(gt_styles)
}

style_header_gt <- function(gt_styles, tbl) {
  require_gt()

  force(tbl)

  gt_styles$header$gt <- function(gt_tbl) {
    return(
      gt_tbl |>
        gt::tab_style(
          style = create_style_gt(
            font_size = tbl$styles$header$font_size,
            text_color = tbl$styles$header$text_color,
            bold = tbl$styles$header$bold,
            italic = tbl$styles$header$italic,
            background_color = tbl$styles$header$background_color
          ),
          locations = gt::cells_column_labels()
        ) |>
        gt::tab_style(
          style = create_style_gt(
            font_size = tbl$styles$header$font_size,
            text_color = tbl$styles$header$text_color,
            bold = tbl$styles$header$bold,
            italic = tbl$styles$header$italic,
            background_color = tbl$styles$header$background_color
          ),
          locations = gt::cells_column_spanners()
        )
    )
  }
  return(gt_styles)
}

style_header_cells_gt <- function(gt_styles, tbl) {
  force(tbl)

  gt_styles$header_cells$gt <- function(gt_tbl) {
    return(gt_tbl)
  }
  return(gt_styles)
}

style_footnote_gt <- function(gt_styles, tbl) {
  require_gt()

  force(tbl)

  gt_styles$footnote$gt <- function(gt_tbl) {
    return(
      gt_tbl |>
        gt::tab_style(
          style = create_style_gt(
            font_size = tbl$styles$footnote$font_size,
            text_color = tbl$styles$footnote$text_color,
            bold = tbl$styles$footnote$bold,
            italic = tbl$styles$footnote$italic,
            background_color = tbl$styles$footnote$background_color
          ),
          locations = gt::cells_footnotes()
        )
    )
  }

  return(gt_styles)
}

style_hline_gt <- function(gt_styles, tbl) {
  force(tbl)

  gt_styles$hline$gt <- function(gt_tbl) return(gt_tbl)
  return(gt_styles)
}

style_vline_gt <- function(gt_styles, tbl) {
  force(tbl)
  gt_styles$vline$gt <- function(gt_tbl) return(gt_tbl)
  return(gt_styles)
}

style_column_gt <- function(gt_styles, tbl) {
  force(tbl)
  column_names <- names(tbl$styles$columns)
  for (column_name in column_names) {
    gt_styles$columns[[column_name]] <- list()
    for (column_style in tbl$styles$columns[[column_name]]) {
      gt_styles$columns[[column_name]][[
        length(gt_styles$columns[[column_name]]) + 1
      ]] <-
        list(
          style = list(
            gt = create_style_gt_function(
              font_size = column_style$style$font_size,
              text_color = column_style$style$text_color,
              bold = column_style$style$bold,
              italic = column_style$style$italic,
              background_color = column_style$style$background_color,
              color_scale = column_style$style$color_scale
            )
          ),
          rows = column_style$rows
        )
    }
  }

  return(gt_styles)
}

#' create_color_scale_gt
#'
#' Create a color scale style for gt
#' @param color_scale vector with two or three color values
#' @returns function to style the gt object
#' @noRd
create_color_scale_gt <- function(color_scale) {
  require_gt()
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

#' create_style_gt_function
#'
#' Create a new style function to be applied to the body of the table.
#'
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param gt_style optional custom gt style. When provided, all other arguments are ignored
#' @noRd
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
#' if(require_gt(throw = FALSE))
#' tbl |>
#'   style_column(columns = mean_hp,
#'                    bold = TRUE) |>
#'   as_gt()
create_style_gt_function <- function(
  font_size,
  text_color,
  bold,
  italic,
  background_color,
  color_scale
) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    return(NULL)
  }

  if (!is.null(color_scale)) {
    color_scale_fn <- create_color_scale_gt(color_scale = color_scale)
  } else {
    color_scale_fn <- function(data, column, rows) {
      return(data)
    }
  }

  styles <- create_style_gt(
    font_size,
    text_color,
    bold,
    italic,
    background_color
  )
  gt_style <- function(data, column, rows) {
    style <- if (italic) "italic" else NULL
    weight <- if (bold) "bold" else NULL
    data |>
      gt::tab_style(
        data = _,
        style = styles,
        locations = gt::cells_body(
          columns = gt::all_of(column),
          rows = rows
        )
      ) |>
      color_scale_fn(data = _, column = column, rows = rows)
  }

  return(gt_style)
}

#' create_style_gt
#'
#' Create a new style to be applied to the body of the table.
#'
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param gt_style optional custom gt style. When provided, all other arguments are ignored
#' @noRd
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
#' if(require_gt(throw = FALSE))
#' tbl |>
#'   style_column(columns = mean_hp,
#'                    bold = TRUE) |>
#'   as_gt()
create_style_gt <- function(
  font_size,
  text_color,
  bold,
  italic,
  background_color,
  gt_style = NULL
) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    return(NULL)
  }
  if (!is.null(gt_style)) {
    return(gt_style)
  }
  style <- if (italic) "italic" else NULL
  weight <- if (bold) "bold" else NULL
  font_size <- if (!is.null(font_size)) {
    gt::px(1.3333343412075 * font_size)
  } else {
    NULL
  }

  style = list(
    gt::cell_text(
      size = font_size,
      color = text_color,
      style = style,
      weight = weight
    )
  )
  if (!is.null(background_color)) {
    style[[length(style) + 1]] <- gt::cell_fill(color = background_color)
  }
  return(style)
}
