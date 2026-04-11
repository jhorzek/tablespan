library(tablespan)
library(testthat)
library(openxlsx)
library(dplyr)

test_tables <- build_tablespan_test_tables()

reference_file <- file.path(
    testthat::test_path(),
    "xlsx_files",
    "reference_tables.xlsx"
)

testthat::expect_true(
    file.exists(reference_file),
    info = "Missing tests/testthat/xlsx_files/reference_tables.xlsx"
)

build_sheet_name_map <- function() {
    sheet_keys <- c(
        names(test_tables$tables),
        "cars_tsf_hf_cs3_cell_style_shifted"
    )

    # Excel sheet names are limited to 31 characters.
    sheet_names <- substr(sheet_keys, 1, 29)
    sheet_names <- make.unique(sheet_names, sep = "_")
    names(sheet_names) <- sheet_keys
    sheet_names
}

sheet_name_map <- build_sheet_name_map()

worksheet_signature <- function(sheet) {
    # Compare stable, user-visible worksheet structures and ignore volatile XML metadata.
    obj <- list()
    for (i in c(
        "cols",
        "conditionalFormatting",
        "dataValidations",
        "freezePane",
        "hyperlinks",
        "mergeCells",
        "pageMargins",
        "pageSetup",
        "rowHeights",
        "sheetProtection"
    )) {
        if (i %in% names(sheet)) {
            obj[[i]] <- sheet[[i]]
        }
    }
    return(obj)
}

run_test <- function(
    tbl,
    reference_file,
    sheet_key,
    start_row = 1,
    start_col = 1
) {
    sheet_name <- unname(sheet_name_map[[sheet_key]])

    wb <- openxlsx::createWorkbook()
    wb <- as_excel(
        tbl = tbl,
        workbook = wb,
        sheet = sheet_name,
        start_row = start_row,
        start_col = start_col
    )

    tmp_file <- tempfile(fileext = ".xlsx")
    openxlsx::saveWorkbook(wb, file = tmp_file, overwrite = TRUE)

    wb_actual <- openxlsx::loadWorkbook(tmp_file)
    wb_reference <- openxlsx::loadWorkbook(reference_file)

    actual_data <- openxlsx::read.xlsx(wb_actual, sheet = 1, colNames = FALSE)
    reference_data <- openxlsx::read.xlsx(
        wb_reference,
        sheet = sheet_name,
        colNames = FALSE
    )
    ref_sheet_idx <- match(sheet_name, openxlsx::getSheetNames(reference_file))

    testthat::expect_false(is.na(ref_sheet_idx))

    testthat::expect_equal(actual_data, reference_data)
    testthat::expect_equal(
        worksheet_signature(sheet = wb_actual$worksheets[[1]]),
        worksheet_signature(sheet = wb_reference$worksheets[[ref_sheet_idx]])
    )
}

testthat::test_that("Excel works", {
    for (tbl_name in names(test_tables$tables)) {
        run_test(
            tbl = test_tables$tables[[tbl_name]],
            reference_file = reference_file,
            sheet_key = tbl_name
        )
    }
})

testthat::test_that("Excel works with shift", {
    run_test(
        tbl = test_tables$tables$cars_tsf_hf_cs3_cell_style,
        reference_file = reference_file,
        sheet_key = "cars_tsf_hf_cs3_cell_style_shifted",
        start_row = 5,
        start_col = 3
    )
})
