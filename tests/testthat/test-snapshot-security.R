count_fixed_matches <- function(text, pattern) {
  matches <- gregexpr(pattern, text, fixed = TRUE)[[1]]
  if (length(matches) == 1 && matches[1] == -1) {
    return(0L)
  }
  length(matches)
}

test_that("googlesheet snapshots contain no sensitive information", {
  snapshots_dir <- testthat::test_path("_snaps")
  snapshot_files <- Sys.glob(file.path(
    snapshots_dir,
    "cars_*",
    "googlesheet-cars.md"
  ))

  expect_gt(length(snapshot_files), 0)

  placeholder_url <- "https://sheets.googleapis.com/v4/spreadsheets/spreadsheet_id:batchUpdate"

  forbidden_patterns <- c(
    "Authorization",
    "Bearer",
    "access_token",
    "refresh_token",
    "oauth",
    "api_key",
    "client_secret",
    "private_key",
    "BEGIN RSA",
    "BEGIN EC",
    "BEGIN OPENSSH",
    "/Users/",
    "/home/",
    "C:\\\\",
    "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}",
    "docs\\.google\\.com/spreadsheets/d/"
  )

  for (snapshot_file in snapshot_files) {
    snapshot_text <- paste(
      readLines(snapshot_file, warn = FALSE),
      collapse = "\n"
    )

    expect_equal(
      count_fixed_matches(snapshot_text, placeholder_url),
      1L,
      info = snapshot_file
    )

    for (pattern in forbidden_patterns) {
      expect_false(
        grepl(pattern, snapshot_text, perl = TRUE),
        info = paste(snapshot_file, "matched forbidden pattern:", pattern)
      )
    }
  }
})
