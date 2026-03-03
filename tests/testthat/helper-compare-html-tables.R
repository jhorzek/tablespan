library(rvest)
library(dplyr)

normalize_style <- function(style_string) {
  if (is.null(style_string) || is.na(style_string) || style_string == "") {
    return("")
  }

  parts <- unlist(strsplit(style_string, ";"))
  parts <- trimws(parts)
  parts <- parts[parts != ""]

  # split into key and value pairs
  kv <- strsplit(parts, ":")
  kv <- lapply(kv, function(x) c(trimws(x[1]), trimws(x[2])))

  # sort by property name
  kv <- kv[order(sapply(kv, `[[`, 1))]

  # recombine, but sorted
  return(paste(
    sapply(kv, function(x) paste(x, collapse = ":")),
    collapse = ";"
  ))
}

extract_table_row_data <- function(table_node) {
  cells <- table_node |> html_elements("th, td")
  tibble(
    row = as.integer(ifelse(
      is.na(rvest::html_attr(cells, "rowspan")),
      "1",
      rvest::html_attr(cells, "rowspan")
    )),
    text = html_text2(cells),
    style = sapply(html_attr(cells, "style"), normalize_style)
  )
}

parse_table <- function(html) {
  parsed <- read_html(html) |> html_element("table")
  rows <- html_elements(parsed, "tr")

  table_data <- lapply(rows, function(row) {
    extract_table_row_data(row)
  })
  return(table_data)
}


compare_html_tables <- function(tbl_1, tbl_2) {
  testthat::expect_true(all.equal(
    parse_table(tbl_1),
    parse_table(tbl_2)
  ))
}
