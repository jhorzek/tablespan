test_that("Merging cells", {
  library(tablespan)
  # We should only merge cells where all left hand side elements are also equal
  row_data <- data.frame(cyl = c(4,4,6,6,8),
                         vs  = c(0,1,1,0,0))
  ids <- tablespan:::row_data_cell_ids(row_data)
  testthat::expect_true(all(ids == matrix(c(1,1,
                                            1,2,
                                            2,3,
                                            2,4,
                                            3,5),
                                          nrow = 5,
                                          ncol = 2,
                                          byrow = TRUE)))
})
