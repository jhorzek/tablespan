test_that("Deparsing table formula", {
  library(testthat)
  library(tablespan)
  formula <- (`Row Name` = `New item name`:`Row 1` + `Row 2`) ~ `Column 1` + (`Column Banner` = `Column 2` + `Column 3` + (`Sub Banner` = `Column 4` + `Column 5`))
  deparsed <- tablespan:::deparse_formula(formula)

  lhs <- list(
    name = "_BASE_LEVEL_",
    entries = list(list(name = "Row Name",
                        entries = list(list(name = "New item name",
                                            item_name = "Row 1",
                                            entries = NULL),
                                       list(name = "Row 2",
                                            item_name = "Row 2",
                                            entries = NULL)))))

  rhs <- list(
    name = "_BASE_LEVEL_",
    entries = list(
      list(name = "Column 1",
           item_name = "Column 1",
           entries = NULL),
      list(name = "Column Banner",
           entries = list(
             list(name = "Column 2",
                  item_name = "Column 2",
                  entries = NULL),
             list(name = "Column 3",
                  item_name = "Column 3",
                  entries = NULL),
             list(name = "Sub Banner",
                  entries = list(
                    list(name = "Column 4",
                         item_name = "Column 4",
                         entries = NULL),
                    list(name = "Column 5",
                         item_name = "Column 5",
                         entries = NULL)
                  ))
           ))
    )
  )

  testthat::expect_true(identical(deparsed$lhs, lhs))
  testthat::expect_true(identical(deparsed$rhs, rhs))

})
