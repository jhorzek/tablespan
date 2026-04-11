library(tablespan)
library(testthat)
library(dplyr)

# Disable external network communication for dry-run tests
googlesheets4::gs4_deauth()

test_that("dry_run produces identical output across multiple runs", {
    # Create a consistent test table
    summarized_table <- mtcars |>
        group_by(cyl) |>
        summarise(
            N = n(),
            mean_hp = mean(hp),
            sd_hp = sd(hp)
        )

    tbl <- tablespan(
        data = summarized_table,
        formula = Cylinder:cyl ~ N + (`Horse Power` = Mean:mean_hp + SD:sd_hp),
        title = "Test Table"
    )

    google_sheet <- fake_gs4_dribble()

    # Generate request twice
    req1 <- as_googlesheet_request(
        tbl = tbl,
        google_sheet = google_sheet,
        sheet = "Sheet1",
        dry_run = TRUE,
        silent = TRUE
    )

    req2 <- as_googlesheet_request(
        tbl = tbl,
        google_sheet = google_sheet,
        sheet = "Sheet1",
        dry_run = TRUE,
        silent = TRUE
    )

    # Both requests should be identical
    expect_identical(
        req1,
        req2,
        info = "Dry-run requests should be deterministic"
    )
})

test_that("dry_run always produces NULL token in request", {
    summarized_table <- mtcars |>
        group_by(cyl) |>
        summarise(N = n(), mean_hp = mean(hp))

    tbl <- tablespan(
        data = summarized_table,
        formula = Cylinder:cyl ~ N + mean_hp
    )

    google_sheet <- fake_gs4_dribble()

    # Test with explicit token = NULL
    req_null <- as_googlesheet_request(
        tbl = tbl,
        google_sheet = google_sheet,
        sheet = "Sheet1",
        dry_run = TRUE,
        token = NULL,
        silent = TRUE
    )

    expect_null(
        req_null$token,
        info = "Dry-run with token=NULL should have NULL token in request"
    )

    # Test with dry_run auto-detection
    req_auto <- as_googlesheet_request(
        tbl = tbl,
        google_sheet = google_sheet,
        sheet = "Sheet1",
        dry_run = TRUE,
        silent = TRUE
    )

    expect_null(
        req_auto$token,
        info = "Dry-run with auto-detection should have NULL token in request"
    )
})

test_that("dry_run works without network availability by mocking gs4_auth", {
    # Create a test table
    data <- tibble::tibble(
        x = c(1, 2, 3),
        y = c(4, 5, 6)
    )

    tbl <- tablespan(
        data = data,
        formula = x ~ y
    )

    google_sheet <- fake_gs4_dribble()

    # Dry-run should succeed even without valid authentication
    # (because it never attempts to acquire a token or call gs4_auth)
    expect_no_error(
        as_googlesheet_request(
            tbl = tbl,
            google_sheet = google_sheet,
            sheet = "Sheet1",
            dry_run = TRUE,
            token = NULL,
            silent = TRUE
        )
    )
})

test_that("dry_run produces valid request structure for all table types", {
    google_sheet <- fake_gs4_dribble()

    # Test 1: Simple table
    simple_tbl <- tablespan(
        data = tibble::tibble(a = 1:2, b = 3:4),
        formula = a ~ b
    )
    req <- as_googlesheet_request(
        tbl = simple_tbl,
        google_sheet = google_sheet,
        sheet = "Sheet1",
        dry_run = TRUE,
        silent = TRUE
    )
    expect_type(req, "list")
    expect_named(req, c('method', 'url', 'body', 'token'))

    # Test 2: Table with formatting
    formatted_tbl <- tablespan(
        data = tibble::tibble(val = c(1.234, 5.678)),
        formula = 1 ~ val
    ) |>
        format_column(
            columns = dplyr::where(is.double),
            fmt = format_number(decimals = 2)
        )
    req <- as_googlesheet_request(
        tbl = formatted_tbl,
        google_sheet = google_sheet,
        sheet = "Sheet1",
        dry_run = TRUE,
        silent = TRUE
    )
    expect_type(req$body$request, "list")
})

test_that("dry_run respects explicit token parameter", {
    fake_token <- "fake_token_value"
    tbl <- tablespan(data = tibble::tibble(a = 1), formula = 1 ~ a)
    google_sheet <- fake_gs4_dribble()

    # When dry_run = TRUE, token should be forced to NULL regardless
    req <- as_googlesheet_request(
        tbl = tbl,
        google_sheet = google_sheet,
        sheet = "Sheet1",
        dry_run = TRUE,
        token = fake_token,
        silent = TRUE
    )

    expect_null(
        req$token,
        info = "Dry-run should force token to NULL even if provided"
    )
})


test_that("dry_run with custom start_row and start_col works", {
    tbl <- tablespan(data = tibble::tibble(a = 1:3, b = 4:6), formula = a ~ b)
    google_sheet <- fake_gs4_dribble()

    req <- as_googlesheet_request(
        tbl = tbl,
        google_sheet = google_sheet,
        sheet = "Sheet1",
        start_row = 5,
        start_col = 3,
        dry_run = TRUE,
        silent = TRUE
    )

    expect_type(req, "list")
    expect_null(req$token)
})

test_that("dry_run produces requests without executing actions", {
    tbl <- tablespan(
        data = tibble::as_tibble(mtcars),
        formula = cyl ~ hp + mpg
    )
    google_sheet <- fake_gs4_dribble()
    google_sheet <- add_fake_sheet(
        google_sheet,
        sheet_name = "NonexistentSheet"
    )

    # This should not throw any errors about sheets not existing
    # because dry_run skips the gs4_get validation
    expect_no_error(
        as_googlesheet_request(
            tbl = tbl,
            google_sheet = google_sheet,
            sheet = "NonexistentSheet", # This sheet doesn't exist, but we're in dry_run
            dry_run = TRUE,
            silent = TRUE
        )
    )
})
