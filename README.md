
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tabelle

> Create satisficing Excel tables in R.

The objective of `tabelle` is to provide a “good enough” approach to
creating tables in R.

Due to the limited approach, `tabelle` is a lot less sophisticated than
the awesome packages [`gt`](https://gt.rstudio.com/), or
[`expss`](https://gdemin.github.io/expss/#Introduction). The main
advantage of `tabelle` is its simplicity and its focus on creating .xslx
tables.

## Installation

`tabelle` is not yet available from CRAN. To install from GitHub, run
the following lines:

``` r
library(remotes)
remotes::install_github("jhorzek/tabelle")
```

## Introduction

`tabelle` assumes that you already have a perfectly summarized table
that you now want to share using .xlsx. That is, `tabelle` will not
create any crosstables, compute percentages or similar. It mostly adds
column names with spanners, combines rownames, and writes the result to
.xlsx files.

Let’s assume we want to share the following table:

``` r
library(dplyr)
data("mtcars")

summarized_table <- mtcars |>
  group_by(cyl, vs) |>
  summarise(N = n(),
            mean_hp = mean(hp),
            sd_hp = sd(hp),
            mean_wt = mean(wt),
            sd_wt = sd(wt))
#> `summarise()` has grouped output by 'cyl'. You can override using the `.groups`
#> argument.

print(summarized_table)
#> # A tibble: 5 × 7
#> # Groups:   cyl [3]
#>     cyl    vs     N mean_hp sd_hp mean_wt  sd_wt
#>   <dbl> <dbl> <int>   <dbl> <dbl>   <dbl>  <dbl>
#> 1     4     0     1    91   NA       2.14 NA    
#> 2     4     1    10    81.8 21.9     2.30  0.598
#> 3     6     0     3   132.  37.5     2.76  0.128
#> 4     6     1     4   115.   9.18    3.39  0.116
#> 5     8     0    14   209.  51.0     4.00  0.759
```

Now we want to create a table where we show the grouping variables as
row names and also create spanners for the horse power (`hp`) and the
weight (`wt`) variables. The result should look something like this:

    |                   | Horse Power |   Weight  |
    | Cylinder | Engine | Mean  |  SD | Mean | SD |
    | -------- | ------ | ----- | --- | ---- | -- |
    |                   |                         |

In `tabelle`, the table headers are defined with a formula approach
inspired by the [`tables`](https://dmurdoch.github.io/tables/) package.
For example, `cyl ~ mean_hp + sd_hp` defines a table with `cyl` as the
row names and `mean_hp` and `sd_hp` as columns. The output in Excel will
be similar to the following:

``` r
library(tabelle)
tabelle(data = summarized_table,
        formula = cyl ~ mean_hp + sd_hp)
#>                         
#>  | cyl | mean_hp sd_hp |
#>  | --- - ------- ----- |
#>  | 4   | 91            |
#>  | 4   | 81.8    21.87 |
#>  | 6   | 131.67  37.53 |
#>  | ... | ...     ...   |
```

Note that the row names (`cyl`) are in a separate block to the left.

Spanners are defined using braces and spanner names. For example, the
following defines a spanner for `mean_hp` and `sd_hp` with the name
`Horsepower`: `cyl ~ (Horsepower = mean_hp + sd_hp)`.

``` r
tabelle(data = summarized_table,
        formula = cyl ~ (Horsepower = mean_hp + sd_hp))
#>                            
#>  |     | Horsepower       |
#>  | cyl | mean_hp    sd_hp |
#>  | --- - ---------- ----- |
#>  | 4   | 91               |
#>  | 4   | 81.8       21.87 |
#>  | 6   | 131.67     37.53 |
#>  | ... | ...        ...   |
```

Spanners can also be nested (e.g.,
`cyl ~ (Horsepower = (Mean = mean_hp) + (SD  = sd_hp))`.

``` r
tabelle(data = summarized_table,
        formula = cyl ~ (Horsepower = (Mean = mean_hp) + (SD  = sd_hp)))
#>                            
#>  |     | Horsepower       |
#>  |     | Mean       SD    |
#>  | cyl | mean_hp    sd_hp |
#>  | --- - ---------- ----- |
#>  | 4   | 91               |
#>  | 4   | 81.8       21.87 |
#>  | 6   | 131.67     37.53 |
#>  | ... | ...        ...   |
```

Often, we may want to replace the technical names (e.g., `mean_hp` and
`sd_hp`) used in `R` when exporting to .xlsx. In the example above, we
may want to replace `mean_hp` and `sd_hp` with “Mean” and “SD”. In
`tabelle` renaming variables is achieved with `new_name:old_name`. For
example, `cyl ~ (Horsepower = Mean:mean_hp + SD:sd_hp)` renames
`mean_hp` to `Mean` and `sd_hp` to `SD`:

``` r
tabelle(data = summarized_table,
        formula = cyl ~ (Horsepower = Mean:mean_hp + SD:sd_hp))
#>                            
#>  |     | Horsepower       |
#>  | cyl | Mean       SD    |
#>  | --- - ---------- ----- |
#>  | 4   | 91               |
#>  | 4   | 81.8       21.87 |
#>  | 6   | 131.67     37.53 |
#>  | ... | ...        ...   |
```

The combination of row names, spanners, and renaming of variables allows
creating the full table as follows:

``` r
tbl <- tabelle(data = summarized_table,
               formula = Cylinder:cyl + Engine:vs ~
                 N +
                 (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
                 (`Weight` = Mean:mean_wt + SD:sd_wt),
               title = "Motor Trend Car Road Tests",
               subtitle = "A table created with tabelle",
               footnote = "Data from the infamous mtcars data set.")
tbl
#> Motor Trend Car Road Tests
#> A table created with tabelle
#>                                                         
#>  |                 |     Horse Power       Weight      |
#>  | Cylinder Engine | N   Mean        SD    Mean   SD   |
#>  | -------- ------ - --  ----------- ----- ------ ---- |
#>  | 4        0      | 1   91                2.14        |
#>  | 4        1      | 10  81.8        21.87 2.3    0.6  |
#>  | 6        0      | 3   131.67      37.53 2.76   0.13 |
#>  | ...      ...    | ... ...         ...   ...    ...  |
#> Data from the infamous mtcars data set.
```

This table can now be translated to an xlsx table with
[openxlsx](https://ycphs.github.io/openxlsx/):

``` r
# write_tab creates an openxlsx workbook
wb <- write_tab(tbl = tbl)

# Save the workbook as an xlsx file:
# openxlsx::saveWorkbook(wb,
#                        file = "cars.xlsx", overwrite = TRUE)
```

The result looks as follows:

![](man/figures/basicTables_example_cars.png)

## Additional Settings

### Tables without row names

Using `1` on the left hand side of the formula creates a table without
row names. For example, `1 ~ (Horsepower = Mean:mean_hp + SD:sd_hp)`
defines

``` r
tabelle(data = summarized_table,
        formula = 1 ~ (Horsepower = Mean:mean_hp + SD:sd_hp))
#>                      
#>  | Horsepower       |
#>  | Mean       SD    |
#>  | ---------- ----- |
#>  | 91               |
#>  | 81.8       21.87 |
#>  | 131.67     37.53 |
#>  | ...        ...   |
```

### Styling

While `tabelle` provides limited styling options, some elements can be
adjusted. For example, we may want to print some elements in bold or
format numbers differently. In `tabelle`, styling happens when
translating the table to an `openxlsx` workbook with `write_tab`. To
this end, `tabelle` provides a \`style

#### Formatting Cells

Let’s assume we want all `mean_hp` values with a value $\geq 100$ to be
printed in bold. To this end, we first create a new style object using
`openxlsx`:

``` r
bold <- openxlsx::createStyle(textDecoration = "bold")
```

Next, we create a cell style with `tabelle`:

``` r
hp_ge_100 <- cell_style(rows = which(summarized_table$mean_hp >= 100), 
                        colnames = "mean_hp", 
                        style = bold,
                        gridExpand = FALSE)
```

Note that we specify the indices of the rows that we want to be in bold
and the column name of the item.

Finally, we pass this style as part of a list to `write_tab`:

``` r
# write_tab creates an openxlsx workbook
wb <- write_tab(tbl = tbl, styles = tab_styles(cell_styles = list(hp_ge_100)))

# Save the workbook as an xlsx file:
# openxlsx::saveWorkbook(wb,
#                        file = "cars.xlsx", overwrite = TRUE)
```

![](man/figures/basicTables_example_cars_styled.png)

#### Formatting Data Types

`tabelle` also allows formatting specific data types. Let’s assume that
we want to round all doubles to 3 instead of the default four digits. To
this end, we use the `create_data_styles` function, where we specify (1)
a function that checks for the data type we want to style (here
`is.double`) and (2) a style for all columns that match that style:

``` r
double_style <- create_data_styles(double = list(test = is.double, 
                                                 style = openxlsx::createStyle(numFmt = "0.000")))
wb <- write_tab(tbl = tbl, styles = tab_styles(data_styles = double_style))

# Save the workbook as an xlsx file:
# openxlsx::saveWorkbook(wb,
#                        file = "cars.xlsx", overwrite = TRUE)
```

![](man/figures/basicTables_example_cars_styled_data.png)

## References

- gt: Iannone R, Cheng J, Schloerke B, Hughes E, Lauer A, Seo J,
  Brevoort K, Roy O (2024). gt: Easily Create Presentation-Ready Display
  Tables. R package version 0.11.1.9000,
  <https://github.com/rstudio/gt>, <https://gt.rstudio.com>.
- expss: Gregory D et al. (2024). expss: Tables with Labels in R. R
  package version 0.9.31, <https://gdemin.github.io/expss/>.
- tables: Murdoch D (2024). tables: Formula-Driven Table Generation. R
  package version 0.9.31, <https://dmurdoch.github.io/tables/>.
- openxlsx: Schauberger P, Walker A (2023). *openxlsx: Read, Write and
  Edit xlsx Files*. R package version 4.2.5.2,
  <https://CRAN.R-project.org/package=openxlsx>.
