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

  style_requests <- style_requests_googlesheet(
    tbl = tbl,
    google_sheet = google_sheet,
    sheet = sheet,
    locations = locations
  )

  # MERGE
  warning("Merging missing")

  # Apply all style requests
  if (!silent) {
    rlang::inform(message = c("i" = paste0("Styling ", sheet)))
  }
  gs_run_style_requests(
    google_sheet = google_sheet,
    style_requests = style_requests
  )
}

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
    text_color <- as.vector(col2rgb(text_color)) / 255
    text_format$foregroundColor <- list(
      "red" = text_color[1],
      "green" = text_color[2],
      "blue" = text_color[3]
    )
  }

  if (length(text_format)) {
    user_format$textFormat <- text_format
  }

  # Background color
  if (!is.null(background_color)) {
    background_color <- as.vector(col2rgb(background_color)) / 255
    user_format$backgroundColor <- list(
      "red" = background_color[1],
      "green" = background_color[2],
      "blue" = background_color[3]
    )
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
}

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

style_requests_googlesheet <- function(tbl, google_sheet, sheet, locations) {
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
        )
      } else {
        column_index <- which(
          colnames(tbl$table_data$col_data) == column_name
        )
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

  # Apply custom formatting to columns
  # Apply formats
  warning("TODO: Apply custom formats")

  return(style_requests)
}
