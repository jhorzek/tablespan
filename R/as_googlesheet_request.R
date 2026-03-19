#' as_googlesheet_request
#'
#' Creates a googlesheets4 request to write the tablespan table to a Google Sheet.
#'
#' Tablespan will not directly write to the google sheet. Instead, it will return a googlesheets4
#' request that can be used to write the table to a google sheet with googlesheets4::request_make
#'
#' @param tbl table created with tablespan::tablespan
#' @param google_sheet Google Sheet spreadsheet dribble created with googlesheets4::gs4_get()
#' @param sheet name of the sheet to which the table should be written to
#' @param start_row row at which to start the table
#' @param start_col column at which to start the table
#' @param merge_rownames should row names with identical entries be merged?
#' @param dry_run if set to TRUE, no API calls will be made. This is useful when just testing the function
#' @param token optional token for authenticated requests. If NULL and `dry_run = FALSE`,
#'   tablespan will use `googlesheets4::gs4_token()` automatically.
#'   You can also pass an explicit token from `googlesheets4::gs4_token()`.
#' @param silent suppress messages when TRUE
#' @returns A request that can be passed to googlesheets4::request_make to write the
#' tablespan to a google sheet
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
#'   # google_sheet <- googlesheets4::gs4_get(ss = "link-to-your-google-sheet")
#'   google_sheet <- fake_gs4_dribble()
#'
#'   # Create a request to write the data to the googlesheet
#'   req <- as_googlesheet_request(tbl = tbl,
#'                                 google_sheet = google_sheet,
#'                                 sheet = "Sheet1")
#'
#'   # For real (non-dry-run) requests, authenticate first and pass a token:
#'   # googlesheets4::gs4_auth()
#'   # token <- googlesheets4::gs4_token()
#'   # req <- as_googlesheet_request(tbl = tbl,
#'   #                               google_sheet = google_sheet,
#'   #                               sheet = "Sheet1",
#'   #                               dry_run = FALSE,
#'   #                               token = token)
#'
#'   # Make the actual request:
#'   # googlesheets4::request_make(req)
#' }
#' }
as_googlesheet_request <- function(
  tbl,
  google_sheet,
  sheet = "Table",
  start_row = 1,
  start_col = 1,
  merge_rownames = TRUE,
  dry_run = is(google_sheet, "fake_sheet"),
  token = NULL,
  silent = FALSE
) {
  require_googlesheets4()
  googlesheets4::local_gs4_quiet()

  if (!silent) {
    warning("Export to googlesheets is experimental. Use at your own risk.")
  }

  if (!is(google_sheet, "googlesheets4_spreadsheet")) {
    stop(
      "google_sheet must be a googlesheets4 spreadsheet dribble. Please run with google_sheet = googlesheets4::gs4_get(ss = 'link to your sheet'))"
    )
  }

  # Lazy token acquisition: only get token if NOT a dry run AND token wasn't provided
  if (!dry_run && is.null(token)) {
    token <- googlesheets4::gs4_token()
  }

  # Force token to NULL if dry_run for explicit safety
  if (dry_run) {
    token <- NULL
  }

  if (!dry_run) {
    # update google_sheet. This is necessary because otherwise we
    # may not see all sheets in the spreadsheet.
    google_sheet <- googlesheets4::gs4_get(google_sheet)
  }

  if ((!dry_run) && (!sheet %in% googlesheets4::sheet_names(google_sheet))) {
    stop(
      "Could not find sheet ",
      sheet,
      " in the google_sheet dribble. Please first create the sheet with googlesheets4::sheet_add('",
      sheet,
      "')."
    )
  }

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

  # Data writing requests
  write_data_requests <-
    gs_write_table_data(
      tbl = tbl,
      tbl_body = tbl_body,
      tbl_header = tbl_header,
      locations = locations,
      google_sheet = google_sheet,
      sheet = sheet,
      dry_run = dry_run
    )

  write_data_requests <- c(
    write_data_requests,
    gs_write_table_title(
      title = tbl$title,
      subtitle = tbl$subtitle,
      locations = locations,
      google_sheet = google_sheet,
      sheet = sheet,
      dry_run = dry_run
    )
  )

  write_data_requests <- c(
    write_data_requests,
    gs_write_table_footnote(
      footnote = tbl$footnote,
      locations = locations,
      google_sheet = google_sheet,
      sheet = sheet,
      dry_run = dry_run
    )
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

  style_requests <- gs_get_style_requests(
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
  req <- googlesheets4::request_generate(
    endpoint = "sheets.spreadsheets.batchUpdate",
    params = list(
      spreadsheetId = google_sheet$spreadsheet_id,
      requests = c(
        write_data_requests,
        format_requests,
        header_outline_requests,
        merge_requests,
        style_requests,
        outline_requests
      )
    ),
    token = token
  )

  return(req)
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
#' generated by get_locations()
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
    top = gs_border_style(
      style = "SOLID",
      width = 1,
      color = gs_color(tbl$styles$hline$color)
    )
  )

  # bottom line
  requests[[length(requests) + 1]] <- gs_border_request(
    sheetId = sheet_id,
    row = locations$row$end_row_data,
    col = c(left_most, locations$col$end_col_header_rhs),
    bottom = gs_border_style(
      style = "SOLID",
      width = 1,
      color = gs_color(tbl$styles$hline$color)
    )
  )

  # left line
  requests[[length(requests) + 1]] <- gs_border_request(
    sheetId = sheet_id,
    row = c(locations$row$start_row_header, locations$row$end_row_data),
    col = left_most,
    left = gs_border_style(
      style = "SOLID",
      width = 1,
      color = gs_color(tbl$styles$vline$color)
    )
  )

  # right line
  requests[[length(requests) + 1]] <- gs_border_request(
    sheetId = sheet_id,
    row = c(locations$row$start_row_header, locations$row$end_row_data),
    col = locations$col$end_col_header_rhs,
    right = gs_border_style(
      style = "SOLID",
      width = 1,
      color = gs_color(tbl$styles$vline$color)
    )
  )

  # row name separator
  requests[[length(requests) + 1]] <- gs_border_request(
    sheetId = sheet_id,
    row = c(locations$row$start_row_header, locations$row$end_row_data),
    col = locations$col$start_col_header_rhs,
    left = gs_border_style(
      style = "SOLID",
      width = 1,
      color = gs_color(tbl$styles$vline$color)
    )
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
  col_header_start <- if (!is.null(locations$col$start_col_header_lhs)) {
    locations$col$start_col_header_lhs
  } else {
    locations$col$start_col_header_rhs
  }

  for (i in 1:nrow(tbl_header)) {
    for (j in 1:ncol(tbl_header)) {
      if (!is.na(tbl_header[i, j])) {
        outline_requests[[length(outline_requests) + 1]] <- gs_border_request(
          sheetId = google_sheet$sheets$id[google_sheet$sheets$name == sheet],
          row = row_header_start + i - 1,
          col = col_header_start + j - 1,
          bottom = gs_border_style(
            style = "SOLID",
            width = 1,
            color = gs_color(tbl$styles$hline$color)
          ),
          left = gs_border_style(
            style = "SOLID",
            width = 1,
            color = gs_color(tbl$styles$vline$color)
          ),
          right = gs_border_style(
            style = "SOLID",
            width = 1,
            color = gs_color(tbl$styles$vline$color)
          )
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
      bottom = gs_border_style(
        style = "SOLID",
        width = 1,
        color = gs_color(tbl$styles$hline$color)
      ),
      left = gs_border_style(
        style = "SOLID",
        width = 1,
        color = gs_color(tbl$styles$vline$color)
      ),
      right = gs_border_style(
        style = "SOLID",
        width = 1,
        color = gs_color(tbl$styles$vline$color)
      )
    )
  }

  return(outline_requests)
}

#' Write table data to Google Sheet
#'
#' Writes both the header and body data of a tablespan table to a Google Sheet.
#'
#' @param tbl tablespan table
#' @param tbl_body The combined row and column data from the tablespan table
#' @param tbl_header The header table matrix generated by gs_get_header_table()
#' @param locations A list containing the row and column positions of table elements,
#' generated by get_locations()
#' @param google_sheet Google Sheet spreadsheet dribble created with googlesheets4::gs4_get()
#' @param sheet Name of the sheet where the table data should be written
#' @param dry_run should actual requests be made?
#' @noRd
gs_write_table_data <- function(
  tbl,
  tbl_body,
  tbl_header,
  locations,
  google_sheet,
  sheet,
  dry_run
) {
  requests <- list()

  # sanitize data
  if (!is.null(tbl_header)) {
    tbl_header <- tbl_header |>
      tibble::as_tibble(x = _, .name_repair = function(x) {
        make.names(x, unique = TRUE)
      }) |>
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        escape_google_sheets_formula
      ))
  }
  if (!is.null(tbl_body)) {
    tbl_body <- tbl_body |>
      dplyr::mutate(dplyr::across(
        dplyr::everything(),
        escape_google_sheets_formula
      ))
  }

  requests[[length(requests) + 1]] <-
    gs_data_writing_request(
      data = tbl_header,
      ss = google_sheet,
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
      ),
      col_names = FALSE,
      reformat = TRUE,
      dry_run = dry_run
    )

  requests[[length(requests) + 1]] <-
    gs_data_writing_request(
      data = tbl_body,
      ss = google_sheet,
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
      ),
      col_names = FALSE,
      reformat = TRUE,
      dry_run = dry_run
    )

  return(requests)
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
#' @param dry_run should actual requests be made?
#' @noRd
gs_write_table_title <- function(
  title,
  subtitle,
  locations,
  google_sheet,
  sheet,
  dry_run
) {
  requests <- list()
  if (!is.null(title)) {
    requests[[length(requests) + 1]] <-
      gs_data_writing_request(
        data = tibble::tibble(text = title) |>
          dplyr::mutate(dplyr::across(
            dplyr::everything(),
            escape_google_sheets_formula
          )),
        ss = google_sheet,
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
        ),
        col_names = FALSE,
        reformat = TRUE,
        dry_run = dry_run
      )
  }

  if (!is.null(subtitle)) {
    requests[[length(requests) + 1]] <-
      gs_data_writing_request(
        data = tibble::tibble(text = subtitle) |>
          dplyr::mutate(dplyr::across(
            dplyr::everything(),
            escape_google_sheets_formula
          )),
        ss = google_sheet,
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
        ),
        col_names = FALSE,
        reformat = TRUE,
        dry_run = dry_run
      )
  }
  return(requests)
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
#' @param dry_run should actual requests be made?
#' @noRd
gs_write_table_footnote <- function(
  footnote,
  locations,
  google_sheet,
  sheet,
  dry_run
) {
  requests <- list()
  if (!is.null(footnote)) {
    requests[[length(requests) + 1]] <-
      gs_data_writing_request(
        data = tibble::tibble(text = footnote) |>
          dplyr::mutate(dplyr::across(
            dplyr::everything(),
            escape_google_sheets_formula
          )),
        ss = google_sheet,
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
        ),
        col_names = FALSE,
        reformat = TRUE,
        dry_run = dry_run
      )
  }
  return(requests)
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
    header_table <- insert_header_entries_hux(
      header_partial = tbl$header$lhs,
      max_level = max_level,
      column_offset = 1,
      header_table = header_table
    )
  }

  header_table <- insert_header_entries_hux(
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
      type = format$type
    )
    if (!is.null(format$pattern)) {
      user_format$numberFormat$pattern <- format$pattern
    }
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
#' the specified cell range
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
#' generated by get_locations()
#' @param google_sheet Google Sheet spreadsheet dribble created with googlesheets4::gs4_get()
#' @param sheet Name of the sheet where the table is being written
#' @return A list of Google Sheets API requests that can be used to merge cells for
#' title elements (title, subtitle, and footnote)
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
#' that lists which cells should be merged in the Google Sheet. This is typically
#' generated by gs_get_header_table().
#' @param locations A list containing the row and column positions of table elements,
#' generated by get_locations().
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
#' "SOLID", "DOTTED", "DASHED", "SOLID_MEDIUM", "SOLID_THICK",
#' "NONE", or "DOUBLE".
#' @param width Numeric. The width of the border in pixels. Default is 1.
#' @param color The color of the border. Can be a color name, hex code,
#' or a list with red, green, and blue components (values between 0 and 1).
#' @return A list containing the border style specification with elements:
#' style (The border style), width (The border width), and color (The border
#' color as a Google Sheets API color object)
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
#' in the format expected by the Google Sheets API
#' @noRd
#' @importFrom grDevices col2rgb
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
#' gs_border_style(). NULL means no border will be applied.
#' @param bottom Border style for the bottom edge of the cell(s). Should be created with
#' gs_border_style(). NULL means no border will be applied.
#' @param left Border style for the left edge of the cell(s). Should be created with
#' gs_border_style(). NULL means no border will be applied.
#' @param right Border style for the right edge of the cell(s). Should be created with
#' gs_border_style(). NULL means no border will be applied.
#' @return A list containing a Google Sheets API request that can be used to apply
#' borders to the specified cell range
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

get_subseries_minmax <- function(vec) {
  # When calling the googlesheets api, Googlesheets expects the rows / columns to be
  # given as start row, end row; start col, end col. This function
  # creates such start-end series from a vector. For example, 1,2,3,4, 7, 8, 10, 11, 19
  # are split into 1, 7; 7, 9; 10, 11; 19, 19
  # Handle empty vector case
  if (length(vec) == 0) {
    stop("vec must not be empty")
  }

  # Find where the sequence breaks (difference > 1)
  split_points <- which(diff(vec) != 1) # marks the point at which we jump

  # Add start and end points
  split_indices <- c(0, split_points, length(vec))

  series <- data.frame(
    min = rep(NA, length(split_indices) - 1),
    max = rep(NA, length(split_indices) - 1)
  )
  for (i in seq_along(split_indices[-1])) {
    # we take the current index and go until the next one
    # Each series starts AFTER the current split point and ends at the next one
    subseries <- vec[(split_indices[i] + 1):split_indices[i + 1]]
    series[i, ] <- c(min = min(subseries), max = max(subseries))
  }

  return(series)
}

# gs_data_writing_request is adapted from `range_write()` in the
# googlesheets4 R package:
# https://github.com/tidyverse/googlesheets4
#
# Original authors: Jennifer Bryan et al.
# Licensed under the MIT License.
#
# Modifications:
# - Removed request_make()
# - Return request object instead of executing
#' @importFrom utils getFromNamespace
gs_data_writing_request <- function(
  ss,
  data,
  sheet,
  range,
  col_names = TRUE,
  reformat = TRUE,
  dry_run
) {
  # The following is copy-pasted and slightly adapted from googlesheets4::range_write
  # by Jennifer Bryan
  require_googlesheets4()
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop(
      "Using as_googlesheet requires the purrr package. Please install with install.packages('purrr')"
    )
  }

  # Import required functions
  as_range_spec <- getFromNamespace("as_range_spec", "googlesheets4")
  as_RowData <- getFromNamespace("as_RowData", "googlesheets4")
  first_visible_name <- getFromNamespace("first_visible_name", "googlesheets4")
  lookup_sheet <- getFromNamespace("lookup_sheet", "googlesheets4")
  new <- getFromNamespace("new", "googlesheets4")
  prepare_dims <- getFromNamespace("prepare_dims", "googlesheets4")
  prepare_loc <- getFromNamespace("prepare_loc", "googlesheets4")
  prepare_resize_request <- getFromNamespace(
    "prepare_resize_request",
    "googlesheets4"
  )

  if (!dry_run) {
    ssid <- googlesheets4::as_sheets_id(ss)
    x <- googlesheets4::gs4_get(ssid)
  } else if (is(ss, "googlesheets4_spreadsheet")) {
    x <- ss
  } else {
    stop("Could not create the data wrinting request")
  }
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  if (!is.logical(col_names)) {
    stop("colnames must be a boolean")
  }
  if (!is.logical(reformat)) {
    stop("colnames must be a boolean")
  }
  if (!is.character(sheet)) {
    stop("sheet must be a character")
  }

  range_spec <- as_range_spec(
    range,
    sheet = sheet,
    sheets_df = x$sheets,
    nr_df = x$named_ranges
  )
  range_spec$sheet_name <- range_spec$sheet_name %||%
    first_visible_name(x$sheets)
  requests <- list()
  s <- lookup_sheet(range_spec$sheet_name, sheets_df = x$sheets)
  loc <- prepare_loc(range_spec)
  dims_needed <- prepare_dims(loc, data, col_names)
  resize_req <- prepare_resize_request(
    s,
    nrow_needed = dims_needed$nrow,
    ncol_needed = dims_needed$ncol,
    exact = FALSE
  )
  if (!is.null(resize_req)) {
    new_dims <- purrr::pluck(
      resize_req,
      "updateSheetProperties",
      "properties",
      "gridProperties"
    )
    requests <- c(requests, list(resize_req))
  }
  fields <- if (reformat) {
    "userEnteredValue,userEnteredFormat"
  } else {
    "userEnteredValue"
  }
  data_req <- new(
    "UpdateCellsRequest",
    rows = as_RowData(data, col_names = col_names),
    fields = fields,
    !!!loc
  )
  requests <- c(requests, list(list(updateCells = data_req)))
  return(requests)
}

escape_google_sheets_formula <- function(content) {
  if (!is.character(content)) {
    return(content)
  }
  if (is.null(content) || length(content) == 0) {
    return(content)
  }
  formula_starters <- c("=", "+", "-", "@")

  is_formula <- grepl(
    paste0("^[", paste(formula_starters, collapse = ""), "]"),
    content
  )

  # Only escape elements that are formulas
  content[is_formula] <- paste0("'", content[is_formula])

  return(content)
}

#' fake_gs4_dribble
#'
#' Creates a fake googlesheets4 dribble to use as a placeholder in the as_googlesheets_request function.
#'
#' @returns fake dribble
#' @export
#' @examples
#' library(tablespan)
#' fake_sheet <- fake_gs4_dribble()
#' add_fake_sheet(fake_sheet, sheet_name = "new_sheet")
fake_gs4_dribble <- function() {
  fake_dribble <- list(
    spreadsheet_id = "spreadsheet_id",
    spreadsheet_url = "https://docs.google.com/spreadsheets/d/spreadsheet_id/edit",
    name = "Test",
    locale = "en_US",
    time_zone = "Europe/Berlin",
    sheets = tibble::tibble(
      name = "Sheet1",
      index = 0,
      id = 0,
      type = "GRID",
      visible = TRUE,
      grid_rows = 1000,
      grid_columns = 26,
      data = list(NULL)
    )
  )
  attr(fake_dribble, "class") <- c(
    "googlesheets4_spreadsheet",
    "fake_sheet",
    "list"
  )

  return(fake_dribble)
}

#' add_fake_sheet
#'
#' Adds a fake sheet to a fake_sheet dribble.
#'
#' @param fake_sheet fake sheet dribble created with fake_gs4_dribble
#' @param sheet_name name of the new sheet
#' @export
#' @examples
#' library(tablespan)
#' fake_sheet <- fake_gs4_dribble()
#' add_fake_sheet(fake_sheet, sheet_name = "new_sheet")
add_fake_sheet <- function(fake_sheet, sheet_name) {
  if (!is(fake_sheet, "fake_sheet")) {
    stop("fake_sheet must be of class fake_sheet (see ?fake_gs4_dribble).")
  }
  fake_sheet$sheets <- rbind(
    fake_sheet$sheets,
    tibble::tibble(
      name = sheet_name,
      index = nrow(fake_sheet$sheets) + 1,
      id = nrow(fake_sheet$sheets) + 1,
      type = "GRID",
      visible = TRUE,
      grid_rows = 1000,
      grid_columns = 26,
      data = list(NULL)
    )
  )
  return(fake_sheet)
}
