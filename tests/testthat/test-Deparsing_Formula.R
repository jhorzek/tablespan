test_that("Deparsing table formula", {
  library(testthat)
  library(basicTables)
  formula <- (`Row Name` = `Row 1` + `Row 2`) ~ `Column 1` + (`Column Banner` = `Column 2` + `Column 3` + (`Sub Banner` = `Column 4` + `Column 5`) + (`Column 6` + `Column 7`) + (`Column 8`))
  deparsed <- basicTables:::deparse_formula(formula)

  lhs <- list(
    name = "_BASE_LEVEL_",
    entries = list(list(name = "Row Name",
                        entries = list(list(name = "Row 1",
                                            entries = NULL),
                                       list(name = "Row 2",
                                            entries = NULL))))
  )
  rhs <- list(
    name = "_BASE_LEVEL_",
    entries = list(
      list(name = "Column 1",
           entries = NULL),
      list(name = "Column Banner",
           entries = list(
             list(name = "Column 2",
                  entries = NULL),
             list(name = "Column 3",
                  entries = NULL),
             list(name = "Sub Banner",
                  entries = list(
                    list(name = "Column 4",
                         entries = NULL),
                    list(name = "Column 5",
                         entries = NULL)
                  )),
             list(name = NULL,
                  entries = list(
                    list(name = "Column 6",
                         entries = NULL),
                    list(name = "Column 7",
                         entries = NULL)
                  )),
             list(name = NULL,
                  entries = list(
                    list(name = "Column 8",
                         entries = NULL)
                  ))
           ))
    )
  )

  testthat::expect_true(identical(deparsed$lhs, lhs))
  testthat::expect_true(identical(deparsed$rhs, rhs))

})
