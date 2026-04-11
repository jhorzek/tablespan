# format_column

Change the formatting of a column or single cells within columns.

## Usage

``` r
format_column(
  tbl,
  columns = dplyr::everything(),
  rows = NULL,
  fmt,
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

- fmt:

  fromatting object. Use format_number to format numeric values,
  format_text for text elements, and format_date for dates.

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
#> `summarise()` has regrouped the output.
#> ℹ Summaries were computed grouped by cyl and vs.
#> ℹ Output is grouped by cyl.
#> ℹ Use `summarise(.groups = "drop_last")` to silence this message.
#> ℹ Use `summarise(.by = c(cyl, vs))` for per-operation grouping
#>   (`?dplyr::dplyr_by`) instead.

# Create a tablespan:
tbl <- tablespan(data = summarized_table,
                 formula = Cylinder:cyl + Engine:vs ~
                   N +
                   (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
                   (`Weight` = Mean:mean_wt + SD:sd_wt),
                 title = "Motor Trend Car Road Tests",
                 subtitle = "A table created with tablespan",
                 footnote = "Data from the infamous mtcars data set.")

if(require_gt(throw = FALSE)){
tbl |>
  format_column(columns = mean_hp,
                rows = c(1,3),
                fmt = format_number(decimals = 4)) |>
  as_gt()
}


  


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
