add_style_color_scale <- function(styles, color_scale, rows) {
  if (!length(color_scale) %in% c(2, 3)) {
    stop("color_scale must be of length 2 or 3.")
  }
  styles <- append(
    styles,
    list(list(
      "style" = list(
        gt = create_color_scale_gt(color_scale = color_scale),
        openxlsx = create_color_scale_openxlsx(color_scale = color_scale)
      ),
      "rows" = rows
    ))
  )
}

create_color_scale_openxlsx <- function(color_scale) {
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

#' @importFrom scales col_numeric
#' @noRd
create_color_scale_gt <- function(color_scale) {
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

    return(
      function(data, column, rows) {
        return(
          data |>
            gt::data_color(
              columns = gt::all_of(column),
              rows = rows,
              fn = function(x) {
                color <- ifelse(
                  x < color_scale[2],
                  lower_scale(x),
                  upper_scale(x)
                )
                color <- ifelse(is.na(color), "#D3D3D3", color)
                return(color)
              }
            )
        )
      }
    )
  } else if (length(color_scale) == 2) {
    return(
      function(data, column, rows) {
        return(
          data |>
            gt::data_color(
              columns = gt::all_of(column),
              rows = rows,
              method = "numeric",
              palette = names(color_scale),
              domain = color_scale
            )
        )
      }
    )
  }
}
