#' initialize_styles
#'
#' Internal function that initializes all styling objects of a tablespan table
#'
#' @param tbl tablespan table
#' @returns tablespan table with added styles field
#' @noRd
initialize_styles <- function(tbl) {
  tbl$styles <- default_styles()
  data <- extract_data(tbl)

  for (column_name in colnames(data)) {
    tbl <- style_column(
      tbl = tbl,
      columns = dplyr::all_of(column_name),
      rows = seq_len(length(data[[column_name]]))
    )
  }

  return(tbl)
}

#' default_styles
#'
#' Sets the default styles for a tablespan table.
#'
#' @returns a list with default styles
#' @noRd
default_styles <- function() {
  default <- list(
    title = list(),
    subtitle = list(),
    header = list(),
    header_cells = list(),
    columns = list(),
    footnote = list(),
    hline = list(),
    vline = list()
  ) |>
    default_styles_gt() |>
    default_styles_openxlsx() |>
    default_styles_hux() |>
    default_styles_flex()

  return(default)
}

#' default_styles_gt
#'
#' Sets the default styles for gt tables in a tablespan table.
#'
#' This function adds default styling for gt tables to the provided default_styles list.
#' If the gt package is not available, the original default_styles are returned unchanged.
#'
#' @param default_styles a list containing default styles for different table elements
#' @returns a list with default styles for gt tables added to the input default_styles
#' @noRd
default_styles_gt <- function(default_styles) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    return(default_styles)
  }

  default <- function(tbl) {
    return(
      tbl
    )
  }
  default_styles$title$gt <- default
  default_styles$subtitle$gt <- default
  default_styles$header$gt <- default
  default_styles$header_cells$gt <- default
  default_styles$footnote$gt <- default
  default_styles$hline$gt <- default
  default_styles$vline$gt <- default
  return(default_styles)
}

#' default_styles_hux
#'
#' Sets the default styles for huxtable tables in a tablespan table.
#'
#' This function adds default styling for huxtable tables to the provided default_styles list.
#' If the huxtable package is not available, the original default_styles are returned unchanged.
#'
#' @param default_styles a list containing default styles for different table elements
#' @returns a list with default styles for huxtable tables added to the input default_styles
#' @noRd
default_styles_hux <- function(default_styles) {
  if (!requireNamespace("huxtable", quietly = TRUE)) {
    return(default_styles)
  }

  default <- list(function(tbl, row, col) {
    return(tbl)
  })
  default_styles$title$hux <- default
  default_styles$subtitle$hux <- default
  default_styles$header$hux <- default
  default_styles$header_cells$hux <- default
  default_styles$footnote$hux <- default
  default_styles$hline$hux <- default
  default_styles$vline$hux <- default
  return(default_styles)
}

#' default_styles_flex
#'
#' Sets the default styles for flextable tables in a tablespan table.
#'
#' This function adds default styling for flextable tables to the provided default_styles list.
#' If the flextable package is not available, the original default_styles are returned unchanged.
#'
#' @param default_styles a list containing default styles for different table elements
#' @returns a list with default styles for flextable tables added to the input default_styles
#' @noRd
default_styles_flex <- function(default_styles) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    return(default_styles)
  }

  default <- list(function(tbl, row, col, part) {
    return(tbl)
  })
  default_styles$title$flex <- default
  default_styles$subtitle$flex <- default
  default_styles$header$flex <- default
  default_styles$header_cells$flex <- default
  default_styles$footnote$flex <- default
  default_styles$hline$flex <- default
  default_styles$vline$flex <- default
  return(default_styles)
}

#' default_styles_openxlsx
#'
#' Sets the default styles for openxlsx tables in a tablespan table.
#'
#' This function adds default styling for openxlsx tables to the provided default_styles list.
#' If the openxlsx package is not available, the original default_styles are returned unchanged.
#'
#' @param default_styles a list containing default styles for different table elements
#' @returns a list with default styles for openxlsx tables added to the input default_styles
#' @noRd
default_styles_openxlsx <- function(default_styles) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    return(default_styles)
  }

  default <- list(function(tbl, row, col) {
    return(tbl)
  })
  default_styles$title$openxlsx <- openxlsx::createStyle(
    fgFill = "#ffffff",
    textDecoration = "bold",
    fontSize = 14
  )
  default_styles$subtitle$openxlsx <- openxlsx::createStyle(
    fgFill = "#ffffff",
    textDecoration = "bold"
  )
  default_styles$header$openxlsx <- openxlsx::createStyle(
    fgFill = "#ffffff",
    textDecoration = "bold"
  )
  default_styles$header_cells$openxlsx <- openxlsx::createStyle(
    fontSize = 11,
    halign = "center",
    border = "BottomLeftRight",
    borderColour = "#000000",
    borderStyle = "thin",
    textDecoration = "bold"
  )
  default_styles$footnote$openxlsx <- openxlsx::createStyle(fgFill = "#ffffff")
  default_styles$hline$openxlsx <- openxlsx::createStyle(
    border = "Top",
    borderColour = "#000000",
    borderStyle = "thin"
  )
  default_styles$vline$openxlsx <- openxlsx::createStyle(
    border = "Left",
    borderColour = "#000000",
    borderStyle = "thin"
  )
  return(default_styles)
}

#' style_title
#'
#' Set the style used for the title of the tablespan table.
#'
#' The styling for openxlsx and gt works differently:
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the title
#' - gt_style must be a list of gt::tab_style objects to be applied to the table
#'
#' @param tbl tablespan table
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param gt_style optional custom gt style. When provided, all other arguments are ignored
#' @param openxlsx_style optional custom openxlsx style. When provided, all other arguments are ignored
#' @param hux_style optional custom huxtable style. When provided, all other arguments are ignored. Must be a function with the following signature:
#' function(tbl, row, col)\{apply some style to the table and return the table\}. Example: function(tbl, row, col)\{tbl |> huxtable::set_bold(row = row, col = col)\}
#' @param flex_style optional custom flextable style. When provided, all other arguments are ignored. Must be a function with the following signature:
#' function(tbl, row, col, part)\{apply some style to the table and return the table\}. Example: function(tbl, row, col, part)\{tbl |> flextable::color(i = row, j = col, color = "red", part = part)\}
#' @returns the tablespan table with added styles
#' @export
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
#' if(require_gt(throw = FALSE))
#'   tbl |>
#'     style_title(bold = TRUE) |>
#'     as_gt()
style_title <- function(
  tbl,
  background_color = NULL,
  text_color = NULL,
  font_size = NULL,
  bold = FALSE,
  italic = FALSE,
  openxlsx_style = NULL,
  gt_style = NULL,
  hux_style = NULL,
  flex_style = NULL
) {
  gt_style <- create_style_gt(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    gt_style = gt_style
  )

  hux_style <- create_style_hux(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    hux_style = hux_style
  )

  flex_style <- create_style_flex(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    flex_style = flex_style
  )

  openxlsx_style <- create_style_openxlsx(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    openxlsx_style = openxlsx_style
  )

  if (requireNamespace("gt", quietly = TRUE)) {
    tbl$styles$title$gt <- function(tbl) {
      return(
        tbl |>
          gt::tab_style(
            style = gt_style,
            locations = gt::cells_title(groups = "title")
          )
      )
    }
  }

  tbl$styles$title$hux <- hux_style
  tbl$styles$title$flex <- flex_style
  tbl$styles$title$openxlsx <- openxlsx_style
  return(tbl)
}

#' style_subtitle
#'
#' Set the style used for the subtitle of the tablespan table.
#'
#' The styling for openxlsx and gt works differently:
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the subtitle
#' - gt_style must be a list of gt::tab_style objects to be applied to the table
#'
#'
#' @param tbl tablespan table
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param gt_style optional custom gt style. When provided, all other arguments are ignored
#' @param openxlsx_style optional custom openxlsx style. When provided, all other arguments are ignored
#' @param hux_style optional custom huxtable style. When provided, all other arguments are ignored. Must be a function with the following signature:
#' function(tbl, row, col)\{apply some style to the table and return the table\}. Example: function(tbl, row, col)\{tbl |> huxtable::set_bold(row = row, col = col)\}
#' @param flex_style optional custom flextable style. When provided, all other arguments are ignored. Must be a function with the following signature:
#' function(tbl, row, col, part)\{apply some style to the table and return the table\}. Example: function(tbl, row, col, part)\{tbl |> flextable::color(i = row, j = col, color = "red", part = part)\}
#' @returns the tablespan table with added styles
#' @export
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
#'   style_subtitle(bold = TRUE) |>
#'   as_gt()
style_subtitle <- function(
  tbl,
  background_color = NULL,
  text_color = NULL,
  font_size = NULL,
  bold = FALSE,
  italic = FALSE,
  openxlsx_style = NULL,
  gt_style = NULL,
  hux_style = NULL,
  flex_style = NULL
) {
  gt_style <- create_style_gt(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    gt_style = gt_style
  )

  hux_style <- create_style_hux(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    hux_style = hux_style
  )

  flex_style <- create_style_flex(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    flex_style = flex_style
  )

  openxlsx_style <- create_style_openxlsx(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    openxlsx_style = openxlsx_style
  )

  if (requireNamespace("gt", quietly = TRUE)) {
    tbl$styles$subtitle$gt <- function(tbl) {
      return(
        tbl |>
          gt::tab_style(
            style = gt_style,
            locations = gt::cells_title(groups = "subtitle")
          )
      )
    }
  }

  tbl$styles$subtitle$flex <- flex_style
  tbl$styles$subtitle$hux <- hux_style
  tbl$styles$subtitle$openxlsx <- openxlsx_style
  return(tbl)
}

#' style_header
#'
#' Set the style used for the header of the tablespan table.
#'
#' The styling for openxlsx and gt works differently:
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the header
#' - gt_style must be a list of gt::tab_style objects to be applied to the table
#'
#' @param tbl tablespan table
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param gt_style optional custom gt style. When provided, all other arguments are ignored
#' @param openxlsx_style optional custom openxlsx style. When provided, all other arguments are ignored
#' @param hux_style optional custom huxtable style. When provided, all other arguments are ignored. Must be a function with the following signature:
#' function(tbl, row, col)\{apply some style to the table and return the table\}. Example: function(tbl, row, col)\{tbl |> huxtable::set_bold(row = row, col = col)\}
#' @param flex_style optional custom flextable style. When provided, all other arguments are ignored. Must be a function with the following signature:
#' function(tbl, row, col, part)\{apply some style to the table and return the table\}. Example: function(tbl, row, col, part)\{tbl |> flextable::color(i = row, j = col, color = "red", part = part)\}
#' @returns the tablespan table with added styles
#' @export
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
#'   style_header(
#'     openxlsx_style = openxlsx::createStyle(
#'       fontSize = 8,
#'       fgFill = "#ffffff"),
#'     gt_style = list(gt::cell_text(size = 8))) |>
#'   as_gt()
style_header <- function(
  tbl,
  background_color = NULL,
  text_color = NULL,
  font_size = NULL,
  bold = FALSE,
  italic = FALSE,
  openxlsx_style = NULL,
  gt_style = NULL,
  hux_style = NULL,
  flex_style = NULL
) {
  gt_style <- create_style_gt(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    gt_style = gt_style
  )

  hux_style <- create_style_hux(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    hux_style = hux_style
  )

  flex_style <- create_style_flex(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    flex_style = flex_style
  )

  openxlsx_style <- create_style_openxlsx(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    openxlsx_style = openxlsx_style
  )

  if (requireNamespace("gt", quietly = TRUE)) {
    tbl$styles$header$gt <- function(tbl) {
      return(
        tbl |>
          gt::tab_style(
            style = gt_style,
            locations = gt::cells_column_labels()
          ) |>
          gt::tab_style(
            style = gt_style,
            locations = gt::cells_column_spanners()
          )
      )
    }
  }

  tbl$styles$header$hux <- hux_style
  tbl$styles$header$flex <- flex_style
  tbl$styles$header$openxlsx <- openxlsx_style
  return(tbl)
}

#' style_header_cells
#'
#' Set the style used for the cells in the openxlsx export. This function
#' is used to create the borders around cells in openxlsx.
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the header
#'
#'
#' @param tbl tablespan table
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param openxlsx_style optional custom openxlsx style. When provided, all other arguments are ignored
#' @returns the tablespan table with added styles
#' @export
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
#' if(require_openxlsx(throw = FALSE))
#' wb <- tbl |>
#'   style_header_cells(text_color = "#345364") |>
#'   as_excel()
#' # save workbook to see the effect
style_header_cells <- function(
  tbl,
  background_color = NULL,
  text_color = NULL,
  font_size = NULL,
  bold = FALSE,
  italic = FALSE,
  openxlsx_style = NULL
) {
  openxlsx_style <- create_style_openxlsx(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    openxlsx_style = openxlsx_style
  )
  # does not exist for gt
  tbl$styles$header_cells$gt <- function(tbl) {
    return(tbl)
  }
  tbl$styles$header_cells$openxlsx <- openxlsx_style
  return(tbl)
}

#' style_footnote
#'
#' Set the style used for the footnote of the tablespan table.
#'
#' The styling for openxlsx and gt works differently:
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the footnote
#' - gt_style must be a list of gt::tab_style objects to be applied to the table
#'
#' @param tbl tablespan table
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param gt_style optional custom gt style. When provided, all other arguments are ignored
#' @param openxlsx_style optional custom openxlsx style. When provided, all other arguments are ignored
#' @param hux_style optional custom huxtable style. When provided, all other arguments are ignored. Must be a function with the following signature:
#' function(tbl, row, col)\{apply some style to the table and return the table\}. Example: function(tbl, row, col)\{tbl |> huxtable::set_bold(row = row, col = col)\}
#' @param flex_style optional custom flextable style. When provided, all other arguments are ignored. Must be a function with the following signature:
#' function(tbl, row, col, part)\{apply some style to the table and return the table\}. Example: function(tbl, row, col, part)\{tbl |> flextable::color(i = row, j = col, color = "red", part = part)\}
#' @returns the tablespan table with added styles
#' @export
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
#'   style_footnote(bold = TRUE) |>
#'   as_gt()
style_footnote <- function(
  tbl,
  background_color = NULL,
  text_color = NULL,
  font_size = NULL,
  bold = FALSE,
  italic = FALSE,
  openxlsx_style = NULL,
  gt_style = NULL,
  hux_style = NULL,
  flex_style = NULL
) {
  gt_style <- create_style_gt(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    gt_style = gt_style
  )

  hux_style <- create_style_hux(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    hux_style = hux_style
  )

  flex_style <- create_style_flex(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    flex_style = flex_style
  )

  openxlsx_style <- create_style_openxlsx(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    openxlsx_style = openxlsx_style
  )

  if (requireNamespace("gt", quietly = TRUE)) {
    tbl$styles$footnote$gt <- function(tbl) {
      return(
        tbl |>
          gt::tab_style(
            style = gt_style,
            locations = gt::cells_footnotes()
          )
      )
    }
  }

  tbl$styles$footnote$gt <- function(tbl) {
    return(
      tbl |>
        gt::tab_style(
          style = gt_style,
          locations = gt::cells_footnotes()
        )
    )
  }
  tbl$styles$footnote$hux <- hux_style
  tbl$styles$footnote$flex <- flex_style
  tbl$styles$footnote$openxlsx <- openxlsx_style
  return(tbl)
}

#' style_hline
#'
#' Set the style used for the horizontal lines of the tablespan table. Currently only
#' supported for excel export.
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the horizontal lines
#'
#' @param tbl tablespan table
#' @param openxlsx_style style used when exporting to openxlsx
#' @returns the tablespan table with added styles
#' @export
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
#' if(require_openxlsx(throw = FALSE))
#' wb <- tbl |>
#'   style_hline(
#'     openxlsx_style = openxlsx::createStyle(
#'       border = "Top",
#'       borderColour = "#928505",
#'       borderStyle = "thin")) |>
#'   as_excel()
#' # save workbook to see effect
style_hline <- function(
  tbl,
  openxlsx_style
) {
  tbl$styles$hline$openxlsx <- openxlsx_style
  return(tbl)
}

#' style_vline
#'
#' Set the style used for the vertical lines of the tablespan table. Currently only
#' supported for excel export.
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the vertical lines
#'
#' @param tbl tablespan table
#' @param openxlsx_style style used when exporting to openxlsx
#' @returns the tablespan table with added styles
#' @export
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
#' if(require_openxlsx(throw = FALSE))
#' wb <- tbl |>
#'   style_vline(
#'     openxlsx_style = openxlsx::createStyle(
#'       border = "Top",
#'       borderColour = "#928505",
#'       borderStyle = "thin")) |>
#'   as_excel()
#' # save workbook to see effect
style_vline <- function(
  tbl,
  openxlsx_style
) {
  tbl$styles$vline$openxlsx <- openxlsx_style
  return(tbl)
}

#' style_column
#'
#' Change the style of a column or single cells within columns.
#'
#' @param tbl tablespan table
#' @param columns the columns to style. Must be a tidyselect selector expression (e.g., starts_with("hp_"))
#' @param rows indices of the rows which should be styled. When set to NULL, the style is applied to all rows
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param color_scale a named vector of length 2 or 3 to define a color scale. Example for two colors: color_scale = c("#EE2F43" = -1, "#37E65A" = 1).
#' Example for three colors: color_scale = c("#EE2F43" = -1, "#FFFFFF" = 0, "#37E65A" = 1)
#' @param gt_style optional custom gt style. When provided, all other arguments are ignored
#' @param openxlsx_style optional custom openxlsx style. When provided, all other arguments are ignored
#' @param hux_style optional custom huxtable style. When provided, all other arguments are ignored. Must be a function with the following signature:
#' function(tbl, row, col)\{apply some style to the table and return the table\}. Example: function(tbl, row, col)\{tbl |> huxtable::set_bold(row = row, col = col)\}
#' @param flex_style optional custom flextable style. When provided, all other arguments are ignored. Must be a function with the following signature:
#' function(tbl, row, col, part)\{apply some style to the table and return the table\}. Example: function(tbl, row, col, part)\{tbl |> flextable::color(i = row, j = col, color = "red", part = part)\}
#' @param stack When set to TRUE, the style is added on top of the existing styles. This is mostly relevant
#' for openxlsx. When set to FALSE, the new style replaces all previous styling.
#' @returns the tablespan table with added styles
#' @export
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
#'                bold = TRUE) |>
#'   as_gt()
style_column <- function(
  tbl,
  columns = dplyr::everything(),
  rows = NULL,
  background_color = NULL,
  text_color = NULL,
  font_size = NULL,
  bold = FALSE,
  italic = FALSE,
  color_scale = NULL,
  openxlsx_style = NULL,
  gt_style = NULL,
  hux_style = NULL,
  flex_style = NULL,
  stack = TRUE
) {
  columns_expr <- rlang::enquo(columns)
  data <- extract_data(tbl)

  column_names <- data |>
    dplyr::select(!!columns_expr) |>
    colnames()

  gt_style <- create_style_gt_function(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    gt_style = gt_style
  )

  flex_style <- create_style_flex(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    flex_style = flex_style
  )

  hux_style <- create_style_hux(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    hux_style = hux_style
  )

  openxlsx_style <- create_style_openxlsx(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    openxlsx_style = openxlsx_style
  )

  style <- list(
    gt = gt_style,
    hux = hux_style,
    flex = flex_style,
    openxlsx = openxlsx_style
  )

  for (column_name in column_names) {
    if (stack) {
      tbl$styles$columns[[column_name]] <- append(
        tbl$styles$columns[[column_name]],
        list(list("style" = style, "rows" = rows))
      )
    } else {
      tbl$styles$columns[[column_name]] <- list(list(
        style = style,
        rows = rows
      ))
    }
    if (!is.null(color_scale)) {
      tbl$styles$columns[[column_name]] <- add_style_color_scale(
        styles = tbl$styles$columns[[column_name]],
        color_scale = color_scale,
        rows = rows
      )
    }
  }

  return(tbl)
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
  gt_style
) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    return(NULL)
  }

  styles <- create_style_gt(
    font_size,
    text_color,
    bold,
    italic,
    background_color,
    gt_style = gt_style
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
      )
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

#' create_style_openxlsx
#'
#' Create a new style to be applied to the body of the table.
#'
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param openxlsx_style optional custom openxlsx style. When provided, all other arguments are ignored
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
#' if(require_openxlsx(throw = FALSE))
#' tbl |>
#'   style_column(columns = mean_hp,
#'                bold = TRUE) |>
#'   as_excel()
create_style_openxlsx <- function(
  font_size,
  text_color,
  bold,
  italic,
  background_color,
  openxlsx_style = NULL
) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    return(NULL)
  }
  if (!is.null(openxlsx_style)) {
    return(openxlsx_style)
  }
  textDecoration <- NULL
  if (bold) {
    textDecoration <- c("Bold")
  }
  if (italic) {
    textDecoration <- c(textDecoration, "italic")
  }
  openxlsx_style <- openxlsx::createStyle(
    fontSize = font_size,
    fontColour = text_color,
    fgFill = background_color,
    textDecoration = textDecoration
  )

  return(openxlsx_style)
}

#' create_style_hux
#'
#' Create a new style to be applied to the body of the table.
#'
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param hux_style optional custom huxtable style. When provided, all other arguments are ignored. Must be a function with the following signature:
#' function(tbl, row, col)\{apply some style to the table and return the table\}. Example: function(tbl, row, col)\{tbl |> huxtable::set_bold(row = row, col = col)\}
#' @noRd
create_style_hux <- function(
  font_size,
  text_color,
  bold,
  italic,
  background_color,
  hux_style
) {
  if (!requireNamespace("huxtable", quietly = TRUE)) {
    return(NULL)
  }
  if (!is.null(hux_style)) {
    return(list(hux_style))
  }
  styles <- list()

  if (!is.null(font_size)) {
    styles[[length(styles) + 1]] <- function(tbl, row, col) {
      return(
        huxtable::set_font_size(
          ht = tbl,
          row = row,
          col = col,
          value = font_size
        )
      )
    }
  }

  if (!is.null(text_color)) {
    styles[[length(styles) + 1]] <- function(tbl, row, col) {
      return(
        huxtable::set_text_color(
          ht = tbl,
          row = row,
          col = col,
          value = text_color
        )
      )
    }
  }

  if (!is.null(background_color)) {
    styles[[length(styles) + 1]] <- function(tbl, row, col) {
      return(
        huxtable::set_background_color(
          ht = tbl,
          row = row,
          col = col,
          value = background_color
        )
      )
    }
  }

  if (bold) {
    styles[[length(styles) + 1]] <- function(tbl, row, col) {
      return(
        huxtable::set_bold(
          ht = tbl,
          row = row,
          col = col
        )
      )
    }
  }

  if (italic) {
    styles[[length(styles) + 1]] <- function(tbl, row, col) {
      return(
        huxtable::set_italic(
          ht = tbl,
          row = row,
          col = col
        )
      )
    }
  }

  return(styles)
}

#' create_style_flex
#'
#' Create a new style to be applied to the body of the table.
#'
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param flex_style optional custom flextable style. When provided, all other arguments are ignored. Must be a function with the following signature:
#' function(tbl, row, col)\{apply some style to the table and return the table\}. Example: function(tbl, row, col)\{tbl |> flextable::color(i = row, j = col, color = "red")\}
#' @noRd
create_style_flex <- function(
  font_size,
  text_color,
  bold,
  italic,
  background_color,
  flex_style
) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    return(NULL)
  }
  if (!is.null(flex_style)) {
    return(list(flex_style))
  }
  styles <- list()

  if (!is.null(font_size)) {
    styles[[length(styles) + 1]] <- function(tbl, row, col, part) {
      return(
        flextable::fontsize(
          x = tbl,
          i = row,
          j = col,
          size = font_size,
          part = part
        )
      )
    }
  }

  if (!is.null(text_color)) {
    styles[[length(styles) + 1]] <- function(tbl, row, col, part) {
      return(
        flextable::color(
          x = tbl,
          i = row,
          j = col,
          color = text_color,
          part = part
        )
      )
    }
  }

  if (!is.null(background_color)) {
    styles[[length(styles) + 1]] <- function(tbl, row, col, part) {
      return(
        flextable::bg(
          x = tbl,
          i = row,
          j = col,
          bg = background_color,
          part = part
        )
      )
    }
  }

  if (bold) {
    styles[[length(styles) + 1]] <- function(tbl, row, col, part) {
      return(
        flextable::bold(
          x = tbl,
          i = row,
          j = col,
          part = part
        )
      )
    }
  }

  if (italic) {
    styles[[length(styles) + 1]] <- function(tbl, row, col, part) {
      return(
        flextable::italic(
          x = tbl,
          i = row,
          j = col,
          part = part
        )
      )
    }
  }

  return(styles)
}
