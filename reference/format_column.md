# format_column

Change the formatting of a column or single cells within columns.

## Usage

``` r
format_column(
  tbl,
  columns = dplyr::everything(),
  rows = NULL,
  format_gt = gt::fmt_auto,
  format_openxlsx = "GENERAL",
  format_hux = NULL,
  format_flex = NULL,
  stack = TRUE
)
```

## Arguments

- tbl:

  tablespan table

- columns:

  the columns to style. Must be a tidyselect selector expression (e.g.,
  starts_with("hp\_"))

- rows:

  indices of the rows which should be styled. When set to NULL, the
  style is applied to all rows

- format_gt:

  formatting used for gt. This must be a function with the following
  signature: function(tbl, columns, rows, ...) and return the tbl with
  applied formatting. See examples.

- format_openxlsx:

  an argument passed to the numFmt field for openxlsx::createStyle.

- format_hux:

  set to NULL to use default formatting. Alternative, use a value that
  can be passed to huxtable::set_number_format()

- format_flex:

  formatting used for flextable. Must be a function with the following
  signature: function(tbl, i, j, part) and must return the tbl object
  with applied formatting.

- stack:

  When set to TRUE, the style is added on top of the existing styles.
  This is mostly relevant for openxlsx. When set to FALSE, the new style
  replaces all previous styling.

## Value

the tablespan table with added styles

## Examples

``` r
library(tablespan)
library(dplyr)
data("mtcars")

# We want to report the following table:
summarized_table <- mtcars |>
  group_by(cyl, vs) |>
  summarise(N = n(),
            mean_hp = mean(hp),
            sd_hp = sd(hp),
            mean_wt = mean(wt),
            sd_wt = sd(wt))
#> `summarise()` has grouped output by 'cyl'. You can override using the `.groups`
#> argument.

# Create a tablespan:
tbl <- tablespan(data = summarized_table,
                 formula = Cylinder:cyl + Engine:vs ~
                   N +
                   (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
                   (`Weight` = Mean:mean_wt + SD:sd_wt),
                 title = "Motor Trend Car Road Tests",
                 subtitle = "A table created with tablespan",
                 footnote = "Data from the infamous mtcars data set.")

if(require_gt(throw = FALSE))
tbl |>
  format_column(columns = mean_hp,
                rows = c(1,3),
                format_gt = function(tbl, columns, rows, ...){
                             return(gt::fmt_number(tbl,
                                       columns = columns,
                                       rows = rows,
                                       decimals = 4))},
                format_openxlsx = "0.0000") |>
  as_gt()


  


Motor Trend Car Road Tests
```

A table created with tablespan

Cylinder

Engine

N

Horse Power

Weight

Mean

SD

Mean

SD

4 

0 

 1 

 91.0000

  

2.1400

  

4 

1 

10 

 81.80  

21.872

2.3003

0.60

6 

0 

 3 

131.6667

37.528

2.7550

0.13

6 

1 

 4 

115.25  

 9.179

3.3887

0.12

8 

0 

14 

209.21  

50.977

3.9992

0.76

Data from the infamous mtcars data set.
