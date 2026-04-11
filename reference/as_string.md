# as_string

as_string

## Usage

``` r
as_string(tbl, digits = 2, n = 3, ...)
```

## Arguments

- tbl:

  result from tablespan

- digits:

  number of digits to round doubles to

- n:

  number of rows to print to print the tablespan table. This allows for
  styling to be printed

- ...:

  additional arguments passed to prmatrix or huxtable (if use_hux =
  TRUE)

## Value

nothing

## Examples

``` r
library(tablespan)
library(dplyr)
data("mtcars")

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

tbl <- tablespan(data = summarized_table,
                 formula = (LHS = Cylinder:cyl + Engine:vs) ~
                   N +
                   (Results = (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
                      (`Weight` = Mean:mean_wt + SD:sd_wt)))
cat(as_string(tbl))
#>                                                         
#>  |                 |     Results                       |
#>  | LHS             |     Horse Power       Weight      |
#>  | Cylinder Engine | N   Mean        SD    Mean   SD   |
#>  | -------- ------ - --  ----------- ----- ------ ---- |
#>  | 4        0      | 1   91                2.14        |
#>  | 4        1      | 10  81.8        21.87 2.3    0.6  |
#>  | 6        0      | 3   131.67      37.53 2.76   0.13 |
#>  | ...      ...    | ... ...         ...   ...    ...  |
```
