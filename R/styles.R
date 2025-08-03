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
    tbl <- set_style_column(
      tbl = tbl,
      format = format_auto(data_col = data[[column_name]]),
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
  list(
    title = list(
      gt = function(tbl) {
        return(
          tbl
        )
      },
      "openxlsx" = openxlsx::createStyle(
        fgFill = "#ffffff",
        textDecoration = "bold",
        fontSize = 14
      )
    ),
    subtitle = list(
      gt = function(tbl) {
        return(tbl)
      },
      "openxlsx" = openxlsx::createStyle(
        fgFill = "#ffffff",
        textDecoration = "bold"
      )
    ),
    header = list(
      gt = function(tbl) {
        return(
          tbl
        )
      },
      "openxlsx" = openxlsx::createStyle(
        fgFill = "#ffffff",
        textDecoration = "bold"
      )
    ),
    header_cells = list(
      gt = function(tbl) {
        return(
          tbl
        )
      },
      "openxlsx" = openxlsx::createStyle(
        fontSize = 11,
        halign = "center",
        border = "BottomLeftRight",
        borderColour = "#000000",
        borderStyle = "thin",
        textDecoration = "bold"
      )
    ),
    columns = list(),
    footnote = list(
      gt = function(tbl) {
        return(
          tbl
        )
      },
      "openxlsx" = openxlsx::createStyle(fgFill = "#ffffff")
    ),
    hline = list(
      gt = function(tbl) {
        return(
          tbl
        )
      },
      "openxlsx" = openxlsx::createStyle(
        border = "Top",
        borderColour = "#000000",
        borderStyle = "thin"
      )
    ),
    vline = list(
      gt = function(tbl) {
        return(tbl)
      },
      "openxlsx" = openxlsx::createStyle(
        border = "Left",
        borderColour = "#000000",
        borderStyle = "thin"
      )
    )
  )
}

#' format_auto
#'
#' Tries to identify the data type and implement a sensible default styling.
#' @param data_col the column for which a format should be created
#' @returns a list with styling for gt and excel tables
#' @noRd
format_auto <- function(data_col) {
  if (is.integer(data_col)) {
    return(format_number(decimals = 0))
  } else if (is.numeric(data_col)) {
    return(format_number(decimals = 2))
  } else {
    return(format_text())
  }
}

#' format_number
#'
#' Implements simple formatting for numbers in gt and excel exports of tablespan.
#' @param decimals the number of decimals to show
#' @param sep_mark optional symbol used to separate thousands
#' @param dec_mark symbol used to separate decimals
#' @returns a list with styles for gt and openxlsx
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
#' tbl |>
#'   set_style_column(columns = mean_hp,
#'                    format = format_number(decimals = 5)) |>
#'   as_gt()
format_number <- function(decimals = 2, sep_mark = ",", dec_mark = ".") {
  if (decimals == 0) {
    openxlsx_format <- "0"
  } else {
    openxlsx_format <- paste0(ifelse(
      sep_mark == "",
      paste0("0", dec_mark, paste0(rep("0", decimals), collapse = "")),
      paste0(
        "#",
        sep_mark,
        "##0",
        dec_mark,
        paste0(rep("0", decimals), collapse = "")
      )
    ))
  }

  return(
    list(
      "gt" = function(data, columns, rows) {
        gt::fmt_number(
          data = data,
          columns = columns,
          rows = rows,
          decimals = decimals,
          sep_mark = sep_mark,
          dec_mark = dec_mark
        )
      },
      "openxlsx" = openxlsx_format
    )
  )
}

#' format_text
#'
#' Implements simple formatting for text in gt and excel exports of tablespan.
#' @returns a list with styles for gt and openxlsx
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
#' tbl |>
#'   set_style_column(columns = mean_hp,
#'                   format = format_text()) |>
#'   as_gt()
format_text <- function() {
  return(list(
    gt = function(data, columns, rows) {
      gt::fmt_auto(
        data = data,
        columns = columns,
        rows = rows
      )
    },
    openxlsx = "TEXT"
  ))
}

#' set_style_title
#'
#' Set the style used for the title of the tablespan table.
#'
#' The styling for openxlsx and gt works differently:
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the title
#' - gt_style must be a list of gt::tab_style objects to be applied to the table
#'
#' All functions that start with "set_style_" completely replace existing styling.
#'
#' @param tbl tablespan table
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param gt_style optional custom gt style. When provided, all other arguments are ignored
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
#' tbl |>
#'   set_style_title(
#'     openxlsx_style = openxlsx::createStyle(
#'       fontSize = 18
#'       fgFill = "#ffffff",
#'       textDecoration = "bold"),
#'     gt_style = list(gt::cell_text(size = 18))) |>
#'   as_gt()
set_style_title <- function(
  tbl,
  format = list(gt = gt::fmt_auto, openxlsx = "TEXT"),
  background_color = "#ffffff",
  text_color = "#000000",
  font_size = 11,
  bold = FALSE,
  italic = FALSE,
  openxlsx_style = NULL,
  gt_style = NULL
) {
  gt_style <- create_style_gt(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    gt_style = gt_style
  )

  openxlsx_style <- create_style_openxlsx(
    format = "TEXT",
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    openxlsx_style = openxlsx_style
  )

  tbl$styles$title$gt <- function(tbl) {
    return(
      tbl |>
        gt::tab_style(
          style = gt_style,
          locations = gt::cells_title(groups = "title")
        )
    )
  }
  tbl$styles$title$openxlsx <- openxlsx_style
  return(tbl)
}

#' set_style_subtitle
#'
#' Set the style used for the subtitle of the tablespan table.
#'
#' The styling for openxlsx and gt works differently:
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the subtitle
#' - gt_style must be a list of gt::tab_style objects to be applied to the table
#'
#' All functions that start with "set_style_" completely replace existing styling.
#'
#' @param tbl tablespan table
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param gt_style optional custom gt style. When provided, all other arguments are ignored
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
#' tbl |>
#'   set_style_title(
#'     openxlsx_style = openxlsx::createStyle(
#'       fontSize = 8
#'       fgFill = "#ffffff"),
#'     gt_style = list(gt::cell_text(size = 8))) |>
#'   as_gt()
set_style_subtitle <- function(
  tbl,
  background_color = "#ffffff",
  text_color = "#000000",
  font_size = 11,
  bold = FALSE,
  italic = FALSE,
  openxlsx_style = NULL,
  gt_style = NULL
) {
  gt_style <- create_style_gt(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    gt_style = gt_style
  )

  openxlsx_style <- create_style_openxlsx(
    format = "TEXT",
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    openxlsx_style = openxlsx_style
  )

  tbl$styles$subtitle$gt <- function(tbl) {
    return(
      tbl |>
        gt::tab_style(
          style = gt_style,
          locations = gt::cells_title(groups = "subtitle")
        )
    )
  }
  tbl$styles$subtitle$openxlsx <- openxlsx_style
  return(tbl)
}

#' set_style_header
#'
#' Set the style used for the header of the tablespan table.
#'
#' The styling for openxlsx and gt works differently:
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the header
#' - gt_style must be a list of gt::tab_style objects to be applied to the table
#'
#' All functions that start with "set_style_" completely replace existing styling.
#'
#' @param tbl tablespan table
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param gt_style optional custom gt style. When provided, all other arguments are ignored
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
#' tbl |>
#'   set_style_header(
#'     openxlsx_style = openxlsx::createStyle(
#'       fontSize = 8
#'       fgFill = "#ffffff"),
#'     gt_style = list(gt::cell_text(size = 8))) |>
#'   as_gt()
set_style_header <- function(
  tbl,
  background_color = "#ffffff",
  text_color = "#000000",
  font_size = 11,
  bold = FALSE,
  italic = FALSE,
  openxlsx_style = NULL,
  gt_style = NULL
) {
  gt_style <- create_style_gt(
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    gt_style = gt_style
  )

  openxlsx_style <- create_style_openxlsx(
    format = "TEXT",
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    openxlsx_style = openxlsx_style
  )

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
  tbl$styles$header$openxlsx <- openxlsx_style
  return(tbl)
}

#' set_style_header_cells
#'
#' Set the style used for the cells in the openxlsx export. This function
#' is used to create the borders around cells in openxlsx.
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the header
#'
#' All functions that start with "set_style_" completely replace existing styling.
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
#' wb <- tbl |>
#'   set_style_header_cells(
#'     openxlsx_style = openxlsx::createStyle(
#'        fontSize = 11,
#'        halign = "center",
#'        border = "Bottom",
#'        borderColour = "#000000",
#'        borderStyle = "thin",
#'        textDecoration = "bold"
#'      )) |>
#'   as_excel()
#' # save workbook to see the effect
set_style_header_cells <- function(
  tbl,
  background_color = "#ffffff",
  text_color = "#000000",
  font_size = 11,
  bold = FALSE,
  italic = FALSE,
  openxlsx_style = NULL
) {
  openxlsx_style <- create_style_openxlsx(
    format = "TEXT",
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

#' set_style_footnote
#'
#' Set the style used for the footnote of the tablespan table.
#'
#' The styling for openxlsx and gt works differently:
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the footnote
#' - gt_style must be a list of gt::tab_style objects to be applied to the table
#'
#' All functions that start with "set_style_" completely replace existing styling.
#'
#' @param tbl tablespan table
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param gt_style optional custom gt style. When provided, all other arguments are ignored
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
#' tbl |>
#'   set_style_footnote(
#'     openxlsx_style = openxlsx::createStyle(
#'       fontSize = 8
#'       fgFill = "#ffffff"),
#'     gt_style = list(gt::cell_text(size = 8))) |>
#'   as_gt()
set_style_footnote <- function(
  tbl,
  background_color = "#ffffff",
  text_color = "#000000",
  font_size = 11,
  bold = FALSE,
  italic = FALSE,
  openxlsx_style = NULL,
  gt_style = NULL
) {
  gt_style <- create_style_gt(
    format = "TEXT",
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    gt_style = gt_style
  )

  openxlsx_style <- create_style_openxlsx(
    format = format,
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    openxlsx_style = openxlsx_style
  )
  tbl$styles$footnote$gt <- function(tbl) {
    return(
      tbl |>
        gt::tab_style(
          style = gt_style,
          locations = gt::cells_footnotes()
        )
    )
  }
  tbl$styles$footnote$openxlsx <- openxlsx_style
  return(tbl)
}

#' set_style_hline
#'
#' Set the style used for the horizontal lines of the tablespan table. Currently only
#' supported for excel export.
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the horizontal lines
#'
#' All functions that start with "set_style_" completely replace existing styling.
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
#' wb <- tbl |>
#'   set_style_hline(
#'     openxlsx_style = openxlsx::createStyle(
#'       border = "Top",
#'       borderColour = "#928505",
#'       borderStyle = "thin")) |>
#'   as_excel()
#' # save workbook to see effect
set_style_hline <- function(
  tbl,
  openxlsx_style
) {
  tbl$styles$hline$openxlsx <- openxlsx_style
  return(tbl)
}

#' set_style_vline
#'
#' Set the style used for the vertical lines of the tablespan table. Currently only
#' supported for excel export.
#'
#' - openxlsx_style must be a style object created with openxlsx::createStyle. This style
#' will then be applied to the vertical lines
#'
#' All functions that start with "set_style_" completely replace existing styling.
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
#' wb <- tbl |>
#'   set_style_vline(
#'     openxlsx_style = openxlsx::createStyle(
#'       border = "Top",
#'       borderColour = "#928505",
#'       borderStyle = "thin")) |>
#'   as_excel()
#' # save workbook to see effect
set_style_vline <- function(
  tbl,
  openxlsx_style
) {
  tbl$styles$vline$openxlsx <- openxlsx_style
  return(tbl)
}

#' set_style_column
#'
#' Change the style of a column or single cells within columns.
#'
#' To change the style of the data shown in the body of the table,
#'
#' @param tbl tablespan table
#' @param columns the columns to style. Must be a tidyselect selector expression (e.g., starts_with("hp_"))
#' @param rows indices of the rows which should be styled. When set to NULL, the style is applied to all rows
#' @param format formatting used for openxlsx and gt. The easiest option is using one of the predefined
#' formats (e.g., format_numeric()). Alternatively, pass a list with (1) a field called gt with a function for
#' formatting gt columns and (2) an argument passed to the numFmt field for openxlsx::createStyle. Example: list(gt = gt::fmt_auto, openxlsx = "TEXT")
#' @param background_color hex code for the background color
#' @param text_color hex code for the text color
#' @param font_size font size
#' @param bold set to TRUE for bold
#' @param italic set to TRUE for italic
#' @param gt_style optional custom gt style. When provided, all other arguments are ignored
#' @param openxlsx_style optional custom openxlsx style. When provided, all other arguments are ignored
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
#' tbl |>
#'   set_style_column(columns = mean_hp,
#'                    bold = TRUE) |>
#'   as_gt()
set_style_column <- function(
  tbl,
  columns = dplyr::everything(),
  rows = NULL,
  format = list(
    gt = gt::fmt_auto,
    openxlsx = "GENERAL"
  ),
  background_color = "#ffffff",
  text_color = "#000000",
  font_size = 11,
  bold = FALSE,
  italic = FALSE,
  openxlsx_style = NULL,
  gt_style = NULL,
  stack = TRUE
) {
  columns_expr <- rlang::enquo(columns)
  if (!is.null(tbl$table_data$row_data)) {
    data <- cbind(tbl$table_data$row_data, tbl$table_data$col_data)
  } else {
    data <- tbl$table_data$col_data
  }

  column_names <- data |>
    dplyr::select(!!columns_expr) |>
    colnames()

  gt_style <- create_style_gt_function(
    format = format,
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    gt_style = gt_style
  )

  openxlsx_style <- create_style_openxlsx(
    format = format,
    font_size = font_size,
    text_color = text_color,
    bold = bold,
    italic = italic,
    background_color = background_color,
    openxlsx_style = openxlsx_style
  )

  style <- list(
    gt = gt_style,
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
  }

  return(tbl)
}

#' create_style_gt_function
#'
#' Create a new style function to be applied to the body of the table.
#'
#' @param format formatting used for openxlsx and gt. The easiest option is using one of the predefined
#' formats (e.g., format_numeric()). Alternatively, pass a list with (1) a field called gt with a function for
#' formatting gt columns and (2) an argument passed to the numFmt field for openxlsx::createStyle. Example: list(gt = gt::fmt_auto, openxlsx = "TEXT")
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
#' tbl |>
#'   set_style_column(columns = mean_hp,
#'                    bold = TRUE) |>
#'   as_gt()
create_style_gt_function <- function(
  format,
  font_size,
  text_color,
  bold,
  italic,
  background_color,
  gt_style
) {
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
      format$gt(columns = gt::all_of(column), rows = rows) |>
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
#' @param format formatting used for openxlsx and gt. The easiest option is using one of the predefined
#' formats (e.g., format_numeric()). Alternatively, pass a list with (1) a field called gt with a function for
#' formatting gt columns and (2) an argument passed to the numFmt field for openxlsx::createStyle. Example: list(gt = gt::fmt_auto, openxlsx = "TEXT")
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
#' tbl |>
#'   set_style_column(columns = mean_hp,
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
  if (!is.null(gt_style)) {
    return(gt_style)
  }
  style <- if (italic) "italic" else NULL
  weight <- if (bold) "bold" else NULL

  style = list(
    gt::cell_text(
      size = font_size,
      color = text_color,
      style = style,
      weight = weight
    ),
    gt::cell_fill(color = background_color)
  )
}

#' create_style_openxlsx
#'
#' Create a new style to be applied to the body of the table.
#'
#' @param format formatting used for openxlsx and gt. The easiest option is using one of the predefined
#' formats (e.g., format_numeric()). Alternatively, pass a list with (1) a field called gt with a function for
#' formatting gt columns and (2) an argument passed to the numFmt field for openxlsx::createStyle. Example: list(gt = gt::fmt_auto, openxlsx = "TEXT")
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
#' tbl |>
#'   set_style_column(columns = mean_hp,
#'                    style = bold = TRUE) |>
#'   as_excel()
create_style_openxlsx <- function(
  format,
  font_size,
  text_color,
  bold,
  italic,
  background_color,
  openxlsx_style = NULL
) {
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
    numFmt = format$openxlsx,
    fontSize = font_size,
    fontColour = text_color,
    fgFill = background_color,
    textDecoration = textDecoration
  )

  return(openxlsx_style)
}
