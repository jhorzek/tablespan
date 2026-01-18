#' as_googlesheet
#'
#' Write a tablespan table to a Google Sheet.
#'
#' @param tbl table created with tablespan::tablespan
#' @param google_sheet Google Sheet spreadsheet dribble created with googlesheets4::gs4_get()
#' @param sheet name of the sheet to which the table should be written to
#' @param start_row row at which to start the table
#' @param start_col column at which to start the table
#' @param merge_rownames should row names with identical entries be merged?
#' @param silent if FALSE, messages about the writing process will be shown
#' @returns Google Sheet spreadsheet dribble that can be further edited or saved
#' @export
#' @examples
#' \dontrun{
#' library(tablespan)
#' library(dplyr)
#'
#' # First summarize the data:
#' summarized_table <- mtcars |>
#'   group_by(cyl, vs) |>
#'   summarise(N = n(),
#'             mean_hp = mean(hp),
#'             sd_hp = sd(hp),
#'             mean_wt = mean(wt),
#'             sd_wt = sd(wt))
#'
#' # Now, create a table with grouping variables as row names and spanners:
#' tbl <- tablespan(data = summarized_table,
#'           formula = Cylinder:cyl + Engine:vs ~
#'             N +
#'             (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
#'             (`Weight` = Mean:mean_wt + SD:sd_wt),
#'           title = "Motor Trend Car Road Tests",
#'           subtitle = "A table created with tablespan",
#'           footnote = "Data from the infamous mtcars data set.")
#'
#' if(require_googlesheets4(throw = FALSE)) {
#'   # Get the Google Sheet (replace with your actual sheet URL)
#'   google_sheet <- googlesheets4::gs4_get(ss = "link-to-your-google-sheet")
#'
#'   # Write the table to the sheet
#'   as_googlesheet(tbl = tbl, google_sheet = google_sheet)
#' }
#' }
as_googlesheet <- function(
  tbl,
  google_sheet,
  sheet = "Table",
  start_row = 1,
  start_col = 1,
  merge_rownames = TRUE,
  silent = FALSE
) {
  require_googlesheets4()
  googlesheets4::local_gs4_quiet()
  if (!is(google_sheet, "googlesheets4_spreadsheet")) {
    stop(
      "google_sheet must be a googlesheets4 spreadsheet dribble. Please run with google_sheet = googlesheets4::gs4_get(ss = 'link to your sheet'))"
    )
  }
  if (!sheet %in% googlesheets4::sheet_names(google_sheet)) {
    if (!silent) {
      rlang::inform(message = c("i" = paste0("Adding sheet ", sheet)))
    }
    googlesheets4::sheet_add(ss = google_sheet, sheet = sheet)
  }
  # We will update the google_sheet object in case sheets were newly written or
  # added somewhere else by the user
  google_sheet$sheets <- googlesheets4::sheet_properties(ss = google_sheet)

  if (!is.null(tbl$header$lhs)) {
    tbl_body <- cbind(tbl$table_data$row_data, tbl$table_data$col_data)
  } else {
    tbl_body <- tbl$table_data$col_data
  }

  locations <- get_locations(
    tbl = tbl,
    start_row = start_row,
    start_col = start_col
  )

  tbl_header <- gs_get_header_table(tbl)

  # Add the data
  if (!silent) {
    rlang::inform(message = c("i" = paste0("Writing data to ", sheet)))
  }
  gs_write_table_data(
    tbl_body = tbl_body,
    tbl_header = tbl_header,
    locations = locations,
    google_sheet = google_sheet,
    sheet = sheet
  )

  if (!silent) {
    rlang::inform(message = c("i" = paste0("Writing title to ", sheet)))
  }
  gs_write_table_title(
    title = tbl$title,
    subtitle = tbl$subtitle,
    locations = locations,
    google_sheet = google_sheet,
    sheet = sheet
  )

  if (!silent) {
    rlang::inform(message = c("i" = paste0("Writing footnote to ", sheet)))
  }

  gs_write_table_footnote(
    footnote = tbl$footnote,
    locations = locations,
    google_sheet = google_sheet,
    sheet = sheet
  )

  merge_requests <- c(
    gs_merge_title_cell_requests(
      tbl = tbl,
      locations = locations,
      google_sheet = google_sheet,
      sheet = sheet
    ),
    gs_merge_header_cell_requests(
      tbl_header = tbl_header,
      locations = locations,
      google_sheet = google_sheet,
      sheet = sheet
    )
  )

  header_outline_requests <- gs_header_outline_requests(
    tbl = tbl,
    tbl_header = tbl_header,
    google_sheet = google_sheet,
    sheet = sheet,
    locations = locations
  )

  outline_requests <- gs_outlines_requests(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = sheet,
    locations = locations
  )

  style_requests <- style_requests_googlesheet(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = sheet,
    locations = locations
  )

  format_requests <- format_requests_googlesheet(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = sheet,
    locations = locations
  )

  # Apply all style requests
  if (!silent) {
    rlang::inform(message = c("i" = paste0("Styling ", sheet)))
  }

  gs_run_style_requests(
    google_sheet = google_sheet,
    style_requests = c(
      format_requests,
      header_outline_requests,
      merge_requests,
      style_requests,
      outline_requests
    )
  )
}

#' Create Google Sheets API requests for table outlines
#'
#' Generates the API requests needed to add outline borders to a tablespan table
#' in a Google Sheet. This function creates border requests for the entire table
#' structure including the outer borders and any internal row name separators.
#'
#' @param tbl A table created with tablespan::tablespan
#' @param google_sheet Google Sheet spreadsheet dribble created with googlesheets4::gs4_get()
#' @param sheet Name of the sheet where the table is being written
#' @param locations A list containing the row and column positions of table elements,
#'        generated by get_locations()
#' @return A list of Google Sheets API requests that can be used to apply table outlines
#' @noRd
gs_outlines_requests <- function(tbl, google_sheet, sheet, locations) {
  require_googlesheets4()
  styles <- tbl$styles
  sheet_id <- google_sheet$sheets$id[google_sheet$sheets$name == sheet]

  requests <- list()

  if (!is.null(tbl$header$lhs)) {
    left_most <- locations$col$start_col_header_lhs
  } else {
    left_most <- locations$col$start_col_header_rhs
  }

  # top line
  requests[[length(requests) + 1]] <- gs_border_request(
    sheetId = sheet_id,
    row = locations$row$start_row_header,
    col = c(left_most, locations$col$end_col_header_rhs),
    top = styles$hline$googlesheet
  )

  # bottom line
  requests[[length(requests) + 1]] <- gs_border_request(
    sheetId = sheet_id,
    row = locations$row$end_row_data,
    col = c(left_most, locations$col$end_col_header_rhs),
    bottom = styles$hline$googlesheet
  )

  # left line
  requests[[length(requests) + 1]] <- gs_border_request(
    sheetId = sheet_id,
    row = c(locations$row$start_row_header, locations$row$end_row_data),
    col = left_most,
    left = styles$vline$googlesheet
  )

  # right line
  requests[[length(requests) + 1]] <- gs_border_request(
    sheetId = sheet_id,
    row = c(locations$row$start_row_header, locations$row$end_row_data),
    col = locations$col$end_col_header_rhs,
    right = styles$vline$googlesheet
  )

  # row name separator
  requests[[length(requests) + 1]] <- gs_border_request(
    sheetId = sheet_id,
    row = c(locations$row$start_row_header, locations$row$end_row_data),
    col = locations$col$start_col_header_rhs,
    left = styles$vline$googlesheet,
  )
  return(requests)
}

#' Create Google Sheets API requests for header outlines
#'
#' Generates the API requests needed to add outline borders to the header section
#' of a tablespan table in a Google Sheet. This function creates border requests
#' for each cell in the header, including special handling for merged header cells.
#'
#' @param tbl A table created with tablespan::tablespan
#' @param tbl_header The header table matrix generated by gs_get_header_table()
#' @param google_sheet Google Sheet spreadsheet dribble created with googlesheets4::gs4_get()
#' @param sheet Name of the sheet where the table is being written
#' @param locations A list containing the row and column positions of table elements,
#'        generated by get_locations()
#' @return A list of Google Sheets API requests that can be used to apply header outlines
#' @noRd
gs_header_outline_requests <- function(
  tbl,
  tbl_header,
  google_sheet,
  sheet,
  locations
) {
  outline_requests <- list()

  row_header_start <- locations$row$start_row_header
  col_header_start <- ifelse(
    !is.null(locations$col$start_col_header_lhs),
    locations$col$start_col_header_lhs,
    locations$col$start_col_header_rhs
  )
  for (i in 1:nrow(tbl_header)) {
    for (j in 1:ncol(tbl_header)) {
      if (!is.na(tbl_header[i, j])) {
        outline_requests[[length(outline_requests) + 1]] <- gs_border_request(
          sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
          row = row_header_start + i - 1,
          col = col_header_start + j - 1,
          bottom = tbl$styles$hline$googlesheet,
          left = tbl$styles$vline$googlesheet,
          right = tbl$styles$vline$googlesheet
        )
      }
    }
  }

  merged_element <- attr(tbl_header, "to_merge")
  for (merge_elem in merged_element) {
    outline_requests[[length(outline_requests) + 1]] <- gs_border_request(
      sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
      row = row_header_start + merge_elem$row - 1,
      col = c(
        col_header_start + min(merge_elem$columns) - 1,
        col_header_start + max(merge_elem$columns) - 1
      ),
      bottom = tbl$styles$hline$googlesheet,
      left = tbl$styles$vline$googlesheet,
      right = tbl$styles$vline$googlesheet
    )
  }

  return(outline_requests)
}

#' Write table data to Google Sheet
#'
#' Writes both the header and body data of a tablespan table to a Google Sheet.
#'
#' @param tbl_body The combined row and column data from the tablespan table
#' @param tbl_header The header table matrix generated by gs_get_header_table()
#' @param locations A list containing the row and column positions of table elements,
#'        generated by get_locations()
#' @param google_sheet Google Sheet spreadsheet dribble created with googlesheets4::gs4_get()
#' @param sheet Name of the sheet where the table data should be written
#' @noRd
gs_write_table_data <- function(
  tbl_body,
  tbl_header,
  locations,
  google_sheet,
  sheet
) {
  gs_write_data(
    data = tibble::as_tibble(tbl_header, .name_repair = make.names),
    google_sheet = google_sheet,
    sheet = sheet,
    range = googlesheets4::cell_limits(
      c(
        locations$row$start_row_header,
        ifelse(
          is.null(tbl$header$lhs),
          locations$col$start_col_header_rhs,
          locations$col$start_col_header_lhs
        )
      ),
      c(
        locations$row$end_row_header,
        locations$col$end_col_header_rhs
      )
    )
  )

  gs_write_data(
    data = tbl_body,
    google_sheet = google_sheet,
    sheet = sheet,
    range = googlesheets4::cell_limits(
      c(
        locations$row$start_row_data,
        ifelse(
          is.null(tbl$header$lhs),
          locations$col$start_col_header_rhs,
          locations$col$start_col_header_lhs
        )
      ),
      c(
        locations$row$end_row_data,
        locations$col$end_col_header_rhs
      )
    )
  )
}

#' Write table title and subtitle to Google Sheet
#'
#' Writes the title and subtitle of a tablespan table to a Google Sheet.
#'
#' @param title The title text to be written to the Google Sheet
#' @param subtitle The subtitle text to be written to the Google Sheet
#' @param locations A list containing the row and column positions of table elements,
#'        generated by get_locations()
#' @param google_sheet Google Sheet spreadsheet dribble created with googlesheets4::gs4_get()
#' @param sheet Name of the sheet where the title should be written
#' @noRd
gs_write_table_title <- function(
  title,
  subtitle,
  locations,
  google_sheet,
  sheet
) {
  if (!is.null(title)) {
    gs_write_data(
      data = tibble::tibble(text = title),
      google_sheet = google_sheet,
      sheet = sheet,
      range = googlesheets4::cell_limits(
        c(
          locations$row$start_row_title,
          locations$col$start_col_title
        ),
        c(
          locations$row$end_row_title,
          locations$col$end_col_title
        )
      )
    )
  }

  if (!is.null(subtitle)) {
    gs_write_data(
      data = tibble::tibble(text = subtitle),
      google_sheet = google_sheet,
      sheet = sheet,
      range = googlesheets4::cell_limits(
        c(
          locations$row$start_row_subtitle,
          locations$col$start_col_subtitle
        ),
        c(
          locations$row$end_row_subtitle,
          locations$col$end_col_subtitle
        )
      )
    )
  }
}

#' Write table footnote to Google Sheet
#'
#' Writes the footnote of a tablespan table to a Google Sheet.
#'
#' @param footnote The footnote text to be written to the Google Sheet
#' @param locations A list containing the row and column positions of table elements,
#'        generated by get_locations()
#' @param google_sheet Google Sheet spreadsheet dribble created with googlesheets4::gs4_get()
#' @param sheet Name of the sheet where the footnote should be written
#' @noRd
gs_write_table_footnote <- function(
  footnote,
  locations,
  google_sheet,
  sheet
) {
  if (!is.null(footnote)) {
    gs_write_data(
      data = tibble::tibble(text = footnote),
      google_sheet = google_sheet,
      sheet = sheet,
      range = googlesheets4::cell_limits(
        c(
          locations$row$start_row_footnote,
          locations$col$start_col_footnote
        ),
        c(
          locations$row$end_row_footnote,
          locations$col$end_col_footnote
        )
      )
    )
  }
}

#' Write data to Google Sheet
#'
#' Writes data to a specified range in a Google Sheet.
#'
#' @param data The data to be written to the Google Sheet. Typically a tibble or data frame.
#' @param google_sheet Google Sheet spreadsheet dribble created with googlesheets4::gs4_get()
#' @param sheet Name of the sheet where the data should be written
#' @param range The cell range where the data should be written, created with googlesheets4::cell_limits()
#' @noRd
gs_write_data <- function(data, google_sheet, sheet, range) {
  googlesheets4::range_write(
    ss = google_sheet,
    data = data,
    sheet = sheet,
    range = range,
    col_names = FALSE,
    reformat = TRUE
  )
}

#' Create header table matrix for Google Sheets
#'
#' Generates a matrix representation of the table header that can be written to
#' a Google Sheet.
#'
#' @param tbl A table created with tablespan::tablespan
#' @return A matrix containing the header values with an attribute "to_merge"
#'         that lists which cells should be merged in the Google Sheet
#' @noRd
gs_get_header_table <- function(tbl) {
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
    # we can reuse a function from the huxtable implementation
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
    column_offset = ifelse(
      !is.null(tbl$header$lhs$width),
      tbl$header$lhs$width + 1,
      1
    ),
    header_table = header_table
  )

  return(header_table)
}

#' Check if googlesheets4 package is available
#'
#' This function checks if the googlesheets4 package is available.
#'
#' @param throw logical. If TRUE (default), the function will throw an error if
#'        googlesheets4 is not available. If FALSE, it will return FALSE instead.
#' @return logical. Returns TRUE if googlesheets4 is available, FALSE if it's not
#'         available and throw=FALSE.
#' @export
#' @examples
#' \dontrun{
#' # Check if googlesheets4 is available, throw error if not
#' require_googlesheets4()
#'
#' # Check if googlesheets4 is available, return FALSE if not
#' require_googlesheets4(throw = FALSE)
#' }
require_googlesheets4 <- function(throw = TRUE) {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    if (throw) {
      stop(
        "Using as_googlesheet requires the googlesheets4 package. Please install with install.packages('googlesheets4')"
      )
    }
    return(FALSE)
  }
  return(TRUE)
}

#' Create Google Sheets API style request
#'
#' Generates a Google Sheets API request to apply formatting styles to a range of cells.
#'
#' @param sheetId The ID of the sheet within the Google Sheet
#' @param row Row or row range (1-indexed) to apply the style to
#' @param col Column or column range (1-indexed) to apply the style to
#' @param bold Logical indicating whether text should be bold
#' @param italic Logical indicating whether text should be italic
#' @param font_size Numeric value for the font size
#' @param background_color Color for the cell background. Can be a color name, hex code,
#'        or a list with red, green, and blue components (values between 0 and 1)
#' @param text_color Color for the text. Can be a color name, hex code,
#'        or a list with red, green, and blue components (values between 0 and 1)
#' @param format A list with format information, containing:
#'        \itemize{
#'          \item{type}{The type of format (e.g., "NUMBER", "PERCENT", "CURRENCY", "DATE")}
#'          \item{pattern}{The pattern to apply (e.g., "#,##0.00" for numbers, "mm/dd/yyyy" for dates)}
#'        }
#' @return A list containing a Google Sheets API request that can be used to apply the specified styles
#' @noRd
gs_create_style_request <- function(
  sheetId,
  row,
  col,
  bold = FALSE,
  italic = FALSE,
  font_size = 10,
  background_color = NULL,
  text_color = NULL,
  format = NULL
) {
  # We assume that row and col are row and column ranges
  if (!length(row) %in% 1:2) {
    stop("row must be either one or two values")
  }
  if (!length(col) %in% 1:2) {
    stop("col must be either one or two values")
  }

  # We have to translate the 1-indexed R to a 0-indexed googlesheets request.
  # Additionally, googlesheets has non-inclusive indexes with [start, end), so we must
  # add 1 to the end (so end stays the same, start is reduced by 1):

  row_start <- if (length(row) == 1) row - 1 else row[1] - 1
  row_end <- if (length(row) == 1) row else row[2]

  col_start <- if (length(col) == 1) col - 1 else col[1] - 1
  col_end <- if (length(col) == 1) col else col[2]

  user_format <- list()

  # Text formatting
  text_format <- list()
  if (!is.null(bold)) {
    text_format$bold <- bold
  }
  if (!is.null(italic)) {
    text_format$italic <- italic
  }
  if (!is.null(font_size)) {
    text_format$fontSize <- font_size
  }
  if (!is.null(text_color)) {
    text_format$foregroundColor <- gs_color(text_color)
  }

  if (length(text_format)) {
    user_format$textFormat <- text_format
  }

  # Background color
  if (!is.null(background_color)) {
    user_format$backgroundColor <- gs_color(background_color)
  }

  # Number and date formatting
  if (!is.null(format)) {
    user_format$numberFormat <- list(
      type = format$type,
      pattern = format$pattern
    )
  }

  # We have to tell the api which fields we want to upate
  fields <- character()
  if (
    !is.null(bold) ||
      !is.null(italic) ||
      !is.null(font_size) ||
      !is.null(text_color)
  ) {
    fields <- c(fields, "textFormat")
  }
  if (!is.null(background_color)) {
    fields <- c(fields, "backgroundColor")
  }
  if (!is.null(format)) {
    fields <- c(fields, "numberFormat")
  }
  if (length(fields) > 0) {
    fields <- paste0("userEnteredFormat(", paste0(fields, collapse = ","), ")")
  }

  # Final API request
  # See https://developers.google.com/workspace/sheets/api/samples/formatting#format-header-row
  reqs <- list(
    list(
      repeatCell = list(
        range = list(
          sheetId = sheetId,
          startRowIndex = row_start,
          endRowIndex = row_end,
          startColumnIndex = col_start,
          endColumnIndex = col_end
        ),
        cell = list(
          userEnteredFormat = user_format
        ),
        fields = paste(fields, collapse = ",")
      )
    )
  )

  return(reqs)
}

#' Create Google Sheets API color scale request
#'
#' Generates a Google Sheets API request to apply a color scale (gradient) conditional
#' formatting to a range of cells. This function supports both 2-color and 3-color scales.
#'
#' @param sheetId The ID of the sheet within the Google Sheet
#' @param row Row or row range (1-indexed) to apply the color scale to
#' @param col Column or column range (1-indexed) to apply the color scale to
#' @param color_scale A named vector of length 2 or 3 specifying the color scale.
#'   Values should be numeric and colors should be hex codes. Example:
#'   `c("#EE2F43" = -1, "#FFFFFF" = 0, "#37E65A" = 1)`. NA values will be automatically
#'   filled with appropriate values from the data.
#' @return A list containing a Google Sheets API request that can be used to apply
#'         the specified color scale conditional formatting
#' @noRd
gs_create_color_scale_request <- function(
  sheetId,
  row,
  col,
  color_scale
) {
  # We assume that row and col are row and column ranges
  if (!length(row) %in% 1:2) {
    stop("row must be either one or two values")
  }
  if (!length(col) %in% 1:2) {
    stop("col must be either one or two values")
  }

  # We have to translate the 1-indexed R to a 0-indexed googlesheets request.
  # Additionally, googlesheets has non-inclusive indexes with [start, end), so we must
  # add 1 to the end (so end stays the same, start is reduced by 1):

  row_start <- if (length(row) == 1) row - 1 else row[1] - 1
  row_end <- if (length(row) == 1) row else row[2]

  col_start <- if (length(col) == 1) col - 1 else col[1] - 1
  col_end <- if (length(col) == 1) col else col[2]

  if (length(color_scale) == 2) {
    return(list(
      addConditionalFormatRule = list(
        rule = list(
          ranges = list(
            sheetId = sheetId,
            startRowIndex = row_start,
            endRowIndex = row_end,
            startColumnIndex = col_start,
            endColumnIndex = col_end
          ),
          gradientRule = list(
            minpoint = list(
              color = gs_color(color = names(color_scale)[1]),
              "type" = "NUMBER",
              "value" = as.character(unname(color_scale[1]))
            ),
            maxpoint = list(
              color = gs_color(color = names(color_scale)[2]),
              "type" = "NUMBER",
              "value" = as.character(unname(color_scale[2]))
            )
          )
        ),
        index = 0
      )
    ))
  } else if (length(color_scale) == 3) {
    return(list(
      addConditionalFormatRule = list(
        rule = list(
          ranges = list(
            sheetId = sheetId,
            startRowIndex = row_start,
            endRowIndex = row_end,
            startColumnIndex = col_start,
            endColumnIndex = col_end
          ),
          gradientRule = list(
            minpoint = list(
              color = gs_color(color = names(color_scale)[1]),
              "type" = "NUMBER",
              "value" = as.character(unname(color_scale[1]))
            ),
            midpoint = list(
              color = gs_color(color = names(color_scale)[2]),
              "type" = "NUMBER",
              "value" = as.character(unname(color_scale[2]))
            ),
            maxpoint = list(
              color = gs_color(color = names(color_scale)[3]),
              "type" = "NUMBER",
              "value" = as.character(unname(color_scale[3]))
            )
          )
        ),
        index = 0
      )
    ))
  } else {
    stop("Unknown color_scale")
  }
}

#' Execute Google Sheets API style requests
#'
#' Sends a batch of style requests to the Google Sheets API to apply formatting
#' to a spreadsheet.
#'
#' @param google_sheet Google Sheet spreadsheet dribble created with googlesheets4::gs4_get()
#' @param style_requests A list of Google Sheets API requests generated by the various
#'        gs_* functions (e.g., gs_create_style_request, gs_border_request, etc.)
#' @return Invisibly returns the response from the Google Sheets API. Issues a warning
#'         if the request fails (HTTP status code >= 400).
#' @noRd
gs_run_style_requests <- function(google_sheet, style_requests) {
  req <- googlesheets4::request_generate(
    endpoint = "sheets.spreadsheets.batchUpdate",
    params = list(
      spreadsheetId = google_sheet$spreadsheet_id,
      requests = style_requests
    )
  )

  req_out <- googlesheets4::request_make(req)
  if (req_out$status >= 400) {
    warning("Styling request failed:", print(req_out))
  }
}

gs_initialize_style_requests <- function(
  tbl,
  google_sheet,
  sheet,
  locations,
  styles
) {
  require_googlesheets4()

  style_requests <- list()
  # Title
  if (!is.null(tbl$title)) {
    for (style_fun in styles$title$googlesheet) {
      style_requests[[length(style_requests) + 1]] <- style_fun(
        google_sheet = google_sheet,
        sheet = sheet,
        row = c(locations$row$start_row_title, locations$row$end_row_title),
        col = c(locations$col$start_col_title, locations$col$end_col_title)
      )
    }
  }
  # Subtitle
  if (!is.null(tbl$subtitle)) {
    for (style_fun in styles$subtitle$googlesheet) {
      style_requests[[length(style_requests) + 1]] <- style_fun(
        google_sheet = google_sheet,
        sheet = sheet,
        row = c(
          locations$row$start_row_subtitle,
          locations$row$end_row_subtitle
        ),
        col = c(
          locations$col$start_col_subtitle,
          locations$col$end_col_subtitle
        )
      )
    }
  }

  # Header LHS
  if (!is.null(tbl$header$lhs)) {
    for (style_fun in styles$header$googlesheet) {
      style_requests[[length(style_requests) + 1]] <- style_fun(
        google_sheet = google_sheet,
        sheet = sheet,
        row = c(locations$row$start_row_header, locations$row$end_row_header),
        col = c(
          locations$col$start_col_header_lhs,
          locations$col$end_col_header_lhs
        )
      )
    }

    for (style_fun in styles$header_cells$googlesheet) {
      style_requests[[length(style_requests) + 1]] <- style_fun(
        google_sheet = google_sheet,
        sheet = sheet,
        row = c(locations$row$start_row_header, locations$row$end_row_header),
        col = c(
          locations$col$start_col_header_lhs,
          locations$col$end_col_header_lhs
        )
      )
    }
  }
  # Header RHS
  for (style_fun in styles$header$googlesheet) {
    style_requests[[length(style_requests) + 1]] <- style_fun(
      google_sheet = google_sheet,
      sheet = sheet,
      row = c(locations$row$start_row_header, locations$row$end_row_header),
      col = c(
        locations$col$start_col_header_rhs,
        locations$col$end_col_header_rhs
      )
    )
  }

  for (style_fun in styles$header_cells$googlesheet) {
    style_requests[[length(style_requests) + 1]] <- style_fun(
      google_sheet = google_sheet,
      sheet = sheet,
      row = c(locations$row$start_row_header, locations$row$end_row_header),
      col = c(
        locations$col$start_col_header_rhs,
        locations$col$end_col_header_rhs
      )
    )
  }

  # Footnote
  if (!is.null(tbl$footnote)) {
    for (style_fun in styles$footnote$googlesheet) {
      style_requests[[length(style_requests) + 1]] <- style_fun(
        google_sheet = google_sheet,
        sheet = sheet,
        row = c(
          locations$row$start_row_footnote,
          locations$row$end_row_footnote
        ),
        col = c(
          locations$col$start_col_footnote,
          locations$col$end_col_footnote
        )
      )
    }
  }

  return(style_requests)
}

#' Create Google Sheets API format requests
#'
#' Generates Google Sheets API requests to apply custom formatting to table data
#' columns.
#'
#' @param tbl A table created with tablespan::tablespan containing format
#'        specifications in the tbl$formats$columns element
#' @param google_sheet Google Sheet spreadsheet dribble created with
#'        googlesheets4::gs4_get()
#' @param sheet Name of the sheet where the table is being written
#' @param locations A list containing the row and column positions of table
#'        elements, generated by get_locations()
#' @return A list of Google Sheets API requests that can be used to apply
#'         custom formatting to table data columns
#' @noRd
format_requests_googlesheet <- function(
  tbl = tbl,
  google_sheet = google_sheet,
  sheet = sheet,
  locations = locations
) {
  format_requests <- list()
  # Add user defined formats
  column_formats <- tbl$formats$columns
  for (column_name in names(column_formats)) {
    for (format in column_formats[[column_name]]) {
      if (is.null(format$format$googlesheet)) {
        next
      } else {
        if (is.null(format$rows)) {
          data_rows <- c(
            locations$row$start_row_data,
            (locations$row$end_row_data)
          )
          contiguous_rows <- TRUE
        } else {
          data_rows <- (locations$row$start_row_data + format$rows) - 1
          if (
            all(min(data_rows):max(data_rows) %in% data_rows) &
              all(data_rows %in% min(data_rows):max(data_rows))
          ) {
            data_rows <- c(min(data_rows), max(data_rows))
            contiguous_rows <- TRUE
          } else {
            contiguous_rows <- FALSE
          }
        }
        data_cols <- locations$col$start_col_header_lhs +
          which(names(column_formats) == column_name) -
          1
        if (is(format$format$googlesheet, "gs_format")) {
          if (contiguous_rows) {
            format_requests[[
              length(format_requests) + 1
            ]] <- gs_create_style_request(
              sheetId = google_sheet$sheets$id[
                google_sheet$sheets$name == sheet
              ],
              row = data_rows,
              col = data_cols,
              format = format$format$googlesheet
            )
          } else {
            for (rw in data_rows) {
              format_requests[[
                length(format_requests) + 1
              ]] <- gs_create_style_request(
                sheetId = google_sheet$sheets$id[
                  google_sheet$sheets$name == sheet
                ],
                row = rw,
                col = data_cols,
                format = format$format$googlesheet
              )
            }
          }
        }
      }
    }
  }
  return(format_requests)
}

#' Generate Google Sheets API style requests for a tablespan table
#'
#' Creates a set of Google Sheets API requests to apply all styling
#' to a tablespan table, including title, subtitle, header, body, footnote, and
#' custom column formatting.
#'
#' @param tbl A table created with tablespan::tablespan containing the data and
#'        styling information to be applied
#' @param google_sheet Google Sheet spreadsheet dribble created with
#'        googlesheets4::gs4_get()
#' @param sheet Name of the sheet where the table is being written
#' @param locations A list containing the row and column positions of table elements,
#'        generated by get_locations()
#' @return A list of Google Sheets API requests that can be executed with
#'         gs_run_style_requests() to apply all styling to the table
#' @noRd
style_requests_googlesheet <- function(
  tbl,
  google_sheet,
  sheet,
  locations
) {
  require_googlesheets4()

  style_requests <- gs_initialize_style_requests(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = sheet,
    locations = locations,
    styles = tbl$styles
  )

  # Apply any custom styles
  for (column_name in names(tbl$styles$columns)) {
    for (c_style in tbl$styles$columns[[column_name]]) {
      if (is.null(c_style$style$googlesheet)) {
        next
      }
      if (length(c_style$style$googlesheet) == 0) {
        next
      }
      if (is.null(c_style$rows)) {
        rows <- c(1, nrow(tbl$table_data$col_data))
        rows_continuous <- TRUE
      } else {
        rows <- c_style$rows
        rows_continuous <- FALSE
      }

      # Need to add some offset to account for title etc
      rows <- rows + locations$row$start_row_data - 1

      # googlesheets works with the column indices
      if (!is.null(tbl$table_data$row_data)) {
        column_index <- which(
          c(
            colnames(tbl$table_data$row_data),
            colnames(tbl$table_data$col_data)
          ) ==
            column_name
        ) +
          locations$col$start_col_header_lhs -
          1
      } else {
        column_index <- which(
          colnames(tbl$table_data$col_data) == column_name
        ) +
          locations$col$start_col_header_rhs -
          1
      }

      for (style_fun in c_style$style$googlesheet) {
        if (rows_continuous) {
          style_requests[[length(style_requests) + 1]] <- style_fun(
            google_sheet = google_sheet,
            sheet = sheet,
            row = rows,
            col = column_index
          )
        } else {
          for (rw in rows) {
            style_requests[[length(style_requests) + 1]] <- style_fun(
              google_sheet = google_sheet,
              sheet = sheet,
              row = rw,
              col = column_index
            )
          }
        }
      }
    }
  }

  return(style_requests)
}

#' Create Google Sheets API merge cells request
#'
#' Generates a Google Sheets API request to merge cells in a specified range.
#'
#' @param sheetId The ID of the sheet within the Google Sheet
#' @param row_start Starting row (1-indexed) of the range to merge
#' @param row_end Ending row (1-indexed) of the range to merge
#' @param col_start Starting column (1-indexed) of the range to merge
#' @param col_end Ending column (1-indexed) of the range to merge
#' @return A list containing a Google Sheets API request that can be used to merge
#'         the specified cell range
#' @noRd
gs_merge_cells_request <- function(
  sheetId,
  row_start,
  row_end,
  col_start,
  col_end
) {
  # Change to zero-indexed
  row_start <- row_start - 1
  row_end <- row_end # we don't subtract 1 because google sheets works with [start, end)
  col_start <- col_start - 1
  col_end <- col_end

  # Final API request
  # See https://developers.google.com/workspace/sheets/api/samples/formatting#merge-cells
  merge_request <- list(
    mergeCells = list(
      range = list(
        sheetId = sheetId,
        startRowIndex = row_start,
        endRowIndex = row_end,
        startColumnIndex = col_start,
        endColumnIndex = col_end
      ),
      "mergeType" = "MERGE_ALL"
    )
  )
  return(merge_request)
}

#' Create Google Sheets API merge cell requests for title elements
#'
#' Generates Google Sheets API requests to merge cells for title, subtitle, and
#' footnote elements of a tablespan table.
#'
#' @param tbl A table created with tablespan::tablespan containing the data to be written
#' @param locations A list containing the row and column positions of table elements,
#'        generated by get_locations()
#' @param google_sheet Google Sheet spreadsheet dribble created with googlesheets4::gs4_get()
#' @param sheet Name of the sheet where the table is being written
#' @return A list of Google Sheets API requests that can be used to merge cells for
#'         title elements (title, subtitle, and footnote)
#' @noRd
gs_merge_title_cell_requests <- function(tbl, locations, google_sheet, sheet) {
  merge_requests <- list()
  if (!is.null(tbl$title)) {
    merge_requests[[length(merge_requests) + 1]] <- gs_merge_cells_request(
      sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
      row_start = locations$row$start_row_title,
      row_end = locations$row$end_row_title,
      col_start = locations$col$start_col_title,
      col_end = locations$col$end_col_title
    )
  }

  if (!is.null(tbl$subtitle)) {
    merge_requests[[length(merge_requests) + 1]] <- gs_merge_cells_request(
      sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
      row_start = locations$row$start_row_subtitle,
      row_end = locations$row$end_row_subtitle,
      col_start = locations$col$start_col_subtitle,
      col_end = locations$col$end_col_subtitle
    )
  }

  if (!is.null(tbl$footnote)) {
    merge_requests[[length(merge_requests) + 1]] <- gs_merge_cells_request(
      sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
      row_start = locations$row$start_row_footnote,
      row_end = locations$row$end_row_footnote,
      col_start = locations$col$start_col_footnote,
      col_end = locations$col$end_col_footnote
    )
  }
  return(merge_requests)
}

#' Create Google Sheets API merge cell requests for header elements
#'
#' Generates Google Sheets API requests to merge header cells in a tablespan table.
#' This function processes the "to_merge" attribute from the header table matrix
#' to create appropriate merge requests for multi-level headers.
#'
#' @param tbl_header A matrix containing the header values, with an attribute "to_merge"
#'        that lists which cells should be merged in the Google Sheet. This is typically
#'        generated by gs_get_header_table().
#' @param locations A list containing the row and column positions of table elements,
#'        generated by get_locations().
#' @param google_sheet Google Sheet spreadsheet dribble created with googlesheets4::gs4_get()
#' @param sheet Name of the sheet where the table is being written
#' @return A list of Google Sheets API requests that can be used to merge header cells
#' @noRd
gs_merge_header_cell_requests <- function(
  tbl_header,
  locations,
  google_sheet,
  sheet
) {
  merge_requests <- list()
  to_merge <- attr(tbl_header, "to_merge")

  col_offset <- if (!is.null(locations$col$start_col_header_lhs)) {
    locations$col$start_col_header_lhs - 1
  } else {
    locations$col$start_col_header_rhs - 1
  }

  for (tm in to_merge) {
    merge_requests[[length(merge_requests) + 1]] <- gs_merge_cells_request(
      sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
      row_start = min(tm$row) + locations$row$start_row_header - 1,
      row_end = if (length(tm$row) == 1) {
        tm$row[1] + locations$row$start_row_header - 1
      } else {
        max(tm$row) + locations$row$start_row_header - 1
      },
      col_start = min(tm$columns) + col_offset,
      col_end = max(tm$columns) + col_offset
    )
  }
  return(merge_requests)
}

#' Create a Google Sheets API border style specification
#'
#' Creates a border style specification that can be used with gs_border_request()
#' to apply borders to cells in a Google Sheet.
#'
#' @param style Character. The style of the border. Must be one of:
#'        "SOLID", "DOTTED", "DASHED", "SOLID_MEDIUM", "SOLID_THICK",
#'        "NONE", or "DOUBLE".
#' @param width Numeric. The width of the border in pixels. Default is 1.
#' @param color The color of the border. Can be a color name, hex code,
#'        or a list with red, green, and blue components (values between 0 and 1).
#' @return A list containing the border style specification with elements:
#'         \itemize{
#'           \item{style}{The border style}
#'           \item{width}{The border width}
#'           \item{color}{The border color as a Google Sheets API color object}
#'         }
#' @export
#' @examples
#' library(tablespan)
#' # Create a solid red border
#' border_style <- gs_border_style(style = "SOLID", width = 2, color = "red")
#'
#' # Create a dotted blue border
#' border_style <- gs_border_style(style = "DOTTED", color = "#0000FF")
gs_border_style <- function(
  style = c(
    "SOLID",
    "DOTTED",
    "DASHED",
    "SOLID_MEDIUM",
    "SOLID_THICK",
    "NONE",
    "DOUBLE"
  ),
  width = 1,
  color
) {
  return(list(
    style = match.arg(style),
    width = width,
    color = gs_color(color)
  ))
}

#' Convert color specification to Google Sheets API color format
#'
#' Converts various color specifications (color names, hex codes, RGB lists) to
#' the format expected by the Google Sheets API.
#'
#' @param color Color specification. Can be:
#'        \itemize{
#'          \item{A color name (e.g., "red", "blue")}
#'          \item{A hex code (e.g., "#FF0000", "#00FF0080" for colors with alpha)}
#'          \item{A list with red, green, and blue components (values between 0 and 1)}
#'        }
#' @return A list with red, green, and blue components (values between 0 and 1)
#'         in the format expected by the Google Sheets API
#' @noRd
#' @examples
#' # Using a color name
#' tablespan:::gs_color("red")
#'
#' # Using a hex code
#' tablespan:::gs_color("#00FF00")
#'
#' # Using RGB values (0-1)
#' tablespan:::gs_color(list(red = 0.5, green = 0.2, blue = 0.8))
gs_color <- function(color) {
  if (is.list(color) && all(c("red", "green", "blue") %in% names(color))) {
    return(color)
  } else {
    gs_col <- as.vector(col2rgb(color)) / 255
  }
  return(list(
    "red" = gs_col[1],
    "green" = gs_col[2],
    "blue" = gs_col[3]
  ))
}

#' Create Google Sheets API border request
#'
#' Generates a Google Sheets API request to apply custom borders to a range of cells.
#'
#' @param sheetId The ID of the sheet within the Google Sheet
#' @param row Row or row range (1-indexed) to apply borders to
#' @param col Column or column range (1-indexed) to apply borders to
#' @param top Border style for the top edge of the cell(s). Should be created with
#'        gs_border_style(). NULL means no border will be applied.
#' @param bottom Border style for the bottom edge of the cell(s). Should be created with
#'        gs_border_style(). NULL means no border will be applied.
#' @param left Border style for the left edge of the cell(s). Should be created with
#'        gs_border_style(). NULL means no border will be applied.
#' @param right Border style for the right edge of the cell(s). Should be created with
#'        gs_border_style(). NULL means no border will be applied.
#' @return A list containing a Google Sheets API request that can be used to apply
#'         borders to the specified cell range
#' @noRd
#' @examples
#' library(tablespan)
#' # Create a border style
#' border_style <- tablespan:::gs_border_style(style = "SOLID", width = 1, color = "black")
#'
#' # Create a request to apply borders to a cell range
#' border_request <- tablespan:::gs_border_request(
#'   sheetId = 0,
#'   row = c(1, 5),
#'   col = c(2, 4),
#'   top = border_style,
#'   bottom = border_style,
#'   left = border_style,
#'   right = border_style
#' )
gs_border_request <- function(
  sheetId,
  row,
  col,
  top = NULL,
  bottom = NULL,
  left = NULL,
  right = NULL
) {
  # We assume that row and col are row and column ranges
  if (!length(row) %in% 1:2) {
    stop("row must be either one or two values")
  }
  if (!length(col) %in% 1:2) {
    stop("col must be either one or two values")
  }

  # We have to translate the 1-indexed R to a 0-indexed googlesheets request.
  # Additionally, googlesheets has non-inclusive indexes with [start, end), so we must
  # add 1 to the end (so end stays the same, start is reduced by 1):

  row_start <- if (length(row) == 1) row - 1 else row[1] - 1
  row_end <- if (length(row) == 1) row else row[2]

  col_start <- if (length(col) == 1) col - 1 else col[1] - 1
  col_end <- if (length(col) == 1) col else col[2]

  request <- list(
    updateBorders = list(
      range = list(
        sheetId = sheetId,
        startRowIndex = row_start,
        endRowIndex = row_end,
        startColumnIndex = col_start,
        endColumnIndex = col_end
      )
    )
  )

  # Final API request
  # See https://developers.google.com/workspace/sheets/api/samples/formatting#cell-borders
  if (!is.null(top)) {
    request$updateBorders$top <- list(
      "style" = top$style,
      "width" = top$width,
      "color" = top$color
    )
  }
  if (!is.null(bottom)) {
    request$updateBorders$bottom <- list(
      "style" = bottom$style,
      "width" = bottom$width,
      "color" = bottom$color
    )
  }
  if (!is.null(left)) {
    request$updateBorders$left <- list(
      "style" = left$style,
      "width" = left$width,
      "color" = left$color
    )
  }
  if (!is.null(right)) {
    request$updateBorders$right <- list(
      "style" = right$style,
      "width" = right$width,
      "color" = right$color
    )
  }
  return(request)
}
