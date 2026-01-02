# as_excel

Write a tablespan table to an excel workbook.

## Usage

``` r
as_excel(
  tbl,
  workbook = openxlsx::createWorkbook(),
  sheet = "Table",
  start_row = 1,
  start_col = 1,
  merge_rownames = TRUE
)
```

## Arguments

- tbl:

  table created with tablespan::tablespan

- workbook:

  Excel workbook created with openxlsx::createWorkbook()

- sheet:

  name of the sheet to which the table should be written to

- start_row:

  row at which to start the table

- start_col:

  column at which to start the table

- merge_rownames:

  should row names with identical entries be merged?

## Value

openxlsx workbook object that can be edited and saved with openxlsx

## Examples

``` r
library(tablespan)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
data("iris")

tbl <- tablespan(data = iris[iris$Species == "setosa", ],
          formula = Species ~ (Sepal = Sepal.Length + Sepal.Width) +
            (Petal = (Width = Petal.Length) + Petal.Width))
#> Warning: Tablespan uses tibble internally. Translating data to tibble

wb <- as_excel(tbl = tbl)

# saveWorkbook(wb, "iris.xlsx")

# The main use case for tablespan is when you already have a summarized table
# that you now want to share using xlsx. The following shows an example using
# the dplyr package:

# First summarize the data:
summarized_table <- mtcars |>
  group_by(cyl, vs) |>
  summarise(N = n(),
            mean_hp = mean(hp),
            sd_hp = sd(hp),
            mean_wt = mean(wt),
            sd_wt = sd(wt))
#> `summarise()` has grouped output by 'cyl'. You can override using the `.groups`
#> argument.

# Now, we want to create a table, where we show the grouping variables
# as row names and also create spanners for the horse power (hp) and the
# weight (wt) variables:
tbl <- tablespan(data = summarized_table,
          formula = Cylinder:cyl + Engine:vs ~
            N +
            (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
            (`Weight` = Mean:mean_wt + SD:sd_wt),
          title = "Motor Trend Car Road Tests",
          subtitle = "A table created with tablespan",
          footnote = "Data from the infamous mtcars data set.")
if(require_openxlsx(throw = FALSE))
  wb <- as_excel(tbl = tbl)

# Create the excel table:
# openxlsx::saveWorkbook(wb,
#                        file = "cars.xlsx", overwrite = TRUE)
```
