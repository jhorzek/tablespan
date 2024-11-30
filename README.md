
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tablespan

> Create satisficing tables in R the formula way.

The objective of `tablespan` is to provide a “good enough” approach to
creating tables by leveraging R’s formulas.

`tablespan` builds on the awesome packages
[`openxlsx`](https://ycphs.github.io/openxlsx/) and
[`gt`](https://gt.rstudio.com/), which allows tables created with
`tablespan` to be exported to the following formats:

1.  **Excel** (using [`openxlsx`](https://ycphs.github.io/openxlsx/))
2.  **HTML** (using [`gt`](https://gt.rstudio.com/))
3.  **LaTeX** (using [`gt`](https://gt.rstudio.com/))
4.  **RTF** (using [`gt`](https://gt.rstudio.com/))

## Installation

`tablespan` is not yet available from CRAN. To install from GitHub, run
the following lines:

``` r
library(remotes)
remotes::install_github("jhorzek/tablespan")
```

## Introduction

R has a large set of great packages that allow you to create and export
tables that look exactly like you envisioned. However, sometimes you may
just need a good-enough table that is easy to create and share with
others. This is where `tablespan` can be of help.

Let’s assume that we want to share the following table:

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

We don’t want to share the table as is - the variable names are all a
bit technical and the table could need some spanners summarizing
columns. So, we want to share a table that looks something like this:

    |                   | Horse Power |   Weight  |
    | Cylinder | Engine | Mean  |  SD | Mean | SD |
    | -------- | ------ | ----- | --- | ---- | -- |
    |                   |                         |

`tablespan` allows us to create this table with a single formula.

### Creating a Basic Table

In `tablespan`, the table headers are defined with a formula. For
example, `cyl ~ mean_hp + sd_hp` defines a table with `cyl` as the row
names and `mean_hp` and `sd_hp` as columns:

``` r
library(tablespan)
tablespan(data = summarized_table,
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

### Adding Spanners

Spanners are defined using braces and spanner names. For example, the
following defines a spanner for `mean_hp` and `sd_hp` with the name
`Horsepower`: `cyl ~ (Horsepower = mean_hp + sd_hp)`:

``` r
tablespan(data = summarized_table,
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

Spanners can also be nested:

``` r
tablespan(data = summarized_table,
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

### Renaming Columns

Variable names in an R `data.frame` are often very technical (e.g.,
`mean_hp` and `sd_hp`). When sharing the table, we may want to replace
those names. In the example above, we may want to replace `mean_hp` and
`sd_hp` with “Mean” and “SD”. In `tablespan` renaming variables is
achieved with `new_name:old_name`. For example,
`cyl ~ (Horsepower = Mean:mean_hp + SD:sd_hp)` renames `mean_hp` to
`Mean` and `sd_hp` to `SD`:

``` r
tablespan(data = summarized_table,
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

### Creating the Full Table

The combination of row names, spanners, and renaming of variables allows
creating the full table:

``` r
library(dplyr)
library(tablespan)
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

tbl <- tablespan(data = summarized_table,
                 formula = Cylinder:cyl + Engine:vs ~
                   N +
                   (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
                   (`Weight` = Mean:mean_wt + SD:sd_wt),
                 title = "Motor Trend Car Road Tests",
                 subtitle = "A table created with tablespan",
                 footnote = "Data from the infamous mtcars data set.")
tbl
#> Motor Trend Car Road Tests
#> A table created with tablespan
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

## Exporting to Excel

Tables created with `tablespan` can now be translated to xlsx tables
with [`openxlsx`](https://ycphs.github.io/openxlsx/) using the
`as_excel` function:

``` r
# as_excel creates an openxlsx workbook
wb <- as_excel(tbl = tbl)

# Save the workbook as an xlsx file:
# openxlsx::saveWorkbook(wb,
#                        file = "cars.xlsx", 
#                        overwrite = TRUE)
```

![](man/figures/tablespan_example_cars.png)

### Styling

While `tablespan` provides limited styling options, some elements can be
adjusted. For example, we may want to print some elements in bold or
format numbers differently. In `tablespan`, styling happens when
translating the table to an `openxlsx` workbook with `as_excel`. To this
end, `tablespan` provides a `styles` argument.

#### Formatting Cells

Let’s assume we want all `mean_hp` values with a value $\geq 100$ to be
printed in bold. To this end, we first create a new style object using
`openxlsx`:

``` r
bold <- openxlsx::createStyle(textDecoration = "bold")
```

Next, we create a cell style with `tablespan`:

``` r
hp_ge_100 <- cell_style(rows = which(summarized_table$mean_hp >= 100), 
                        colnames = "mean_hp", 
                        style = bold,
                        gridExpand = FALSE)
```

Note that we specify the indices of the rows that we want to be in bold
and the column name of the item.

Finally, we pass this style as part of a list to `as_excel`:

``` r
# as_excel creates an openxlsx workbook
wb <- as_excel(tbl = tbl, 
               styles = tbl_styles(cell_styles = list(hp_ge_100)))

# Save the workbook as an xlsx file:
# openxlsx::saveWorkbook(wb,
#                        file = "cars.xlsx", 
#                        overwrite = TRUE)
```

![](man/figures/tablespan_example_cars_styled.png)

#### Formatting Data Types

`tablespan` also allows formatting specific data types. Let’s assume
that we want to round all doubles to 3 instead of the default 2 digits.
To this end, we use the `create_data_styles` function, where we specify
(1) a function that checks for the data type we want to style (here
`is.double`) and (2) a style for all columns that match that style:

``` r
double_style <- create_data_styles(double = list(test = is.double, 
                                                 style = openxlsx::createStyle(numFmt = "0.000")))
wb <- as_excel(tbl = tbl, styles = tbl_styles(data_styles = double_style))

# Save the workbook as an xlsx file:
# openxlsx::saveWorkbook(wb,
#                        file = "cars.xlsx", 
#                        overwrite = TRUE)
```

![](man/figures/tablespan_example_cars_styled_data.png)

## Exporting to HTML, LaTeX, and RTF

Tables created with `tablespan` can also be exported to `gt` which
allows saving as HTML, LaTeX, or RTF file. To this end, we simply have
to call `as_gt` on our table:

``` r
# Translate to gt:
gt_tbl <- as_gt(tbl = tbl)
gt_tbl
```

<p align="center">
<img src="man/figures/tablespan_example_gt_cars.png" alt="Standard table" width="50%">
</p>

### Styling Great Tables

The `gt` package provides a wide range of functions to adapt the style
of the table created with `as_gt`. For instance, `opt_stylize` adds a
pre-defined style to the entire table:

``` r
gt_tbl |> 
  gt::opt_stylize(style = 6,
                  color = 'gray')
```

<p align="center">
<img src="man/figures/tablespan_example_gt_cars_styled.png" alt="Styled table" width="50%">
</p>

When adapting the `gt` object, there is an important detail to keep in
mind: To ensure that each table spanner has a unique ID, `tablespan`
will create IDs that differ from the text shown in the spanner. To
demonstrate this, Let’s assume that we want to add a spanner above
`Horse Power` and `Weight`:

``` r
gt_tbl |> 
  gt::tab_spanner(label = "New Spanner", 
                  spanners = c("Horse Power", "Weight"))
#> Error in `gt::tab_spanner()`:
#> ! One or more spanner ID(s) supplied in `spanners` (Horse Power and
#>   Weight), for the new spanner with the ID `New Spanner` doesn't belong to any
#>   existing spanners.
```

This will throw an error because the spanner IDs are different from the
spanner labels. To get the spanner IDs, use `gt::tab_info()`:

``` r
gt_tbl |> 
  gt::tab_info()
```

<div id="cctlqodhdh" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>@import url("https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
#cctlqodhdh table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#cctlqodhdh thead, #cctlqodhdh tbody, #cctlqodhdh tfoot, #cctlqodhdh tr, #cctlqodhdh td, #cctlqodhdh th {
  border-style: none;
}
&#10;#cctlqodhdh p {
  margin: 0;
  padding: 0;
}
&#10;#cctlqodhdh .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: 800px;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #F7F7F7;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#cctlqodhdh .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#cctlqodhdh .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#cctlqodhdh .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#cctlqodhdh .gt_heading {
  background-color: #FFFFFF;
  text-align: left;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#cctlqodhdh .gt_bottom_border {
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#cctlqodhdh .gt_col_headings {
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#cctlqodhdh .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#cctlqodhdh .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#cctlqodhdh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#cctlqodhdh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#cctlqodhdh .gt_column_spanner {
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#cctlqodhdh .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#cctlqodhdh .gt_group_heading {
  padding-top: 12px;
  padding-bottom: 12px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  text-transform: uppercase;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-bottom-style: none;
  border-bottom-width: 1px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#cctlqodhdh .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 80%;
  font-weight: bolder;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-bottom-style: none;
  border-bottom-width: 1px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#cctlqodhdh .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#cctlqodhdh .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#cctlqodhdh .gt_row {
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #F7F7F7;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#cctlqodhdh .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cctlqodhdh .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#cctlqodhdh .gt_row_group_first td {
  border-top-width: 1px;
}
&#10;#cctlqodhdh .gt_row_group_first th {
  border-top-width: 1px;
}
&#10;#cctlqodhdh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cctlqodhdh .gt_first_summary_row {
  border-top-style: none;
  border-top-color: #D3D3D3;
}
&#10;#cctlqodhdh .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#cctlqodhdh .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#cctlqodhdh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cctlqodhdh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: none;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#cctlqodhdh .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: none;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#cctlqodhdh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#cctlqodhdh .gt_table_body {
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #D3D3D3;
}
&#10;#cctlqodhdh .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#cctlqodhdh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cctlqodhdh .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#cctlqodhdh .gt_sourcenote {
  font-size: 10px;
  padding-top: 6px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#cctlqodhdh .gt_left {
  text-align: left;
}
&#10;#cctlqodhdh .gt_center {
  text-align: center;
}
&#10;#cctlqodhdh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#cctlqodhdh .gt_font_normal {
  font-weight: normal;
}
&#10;#cctlqodhdh .gt_font_bold {
  font-weight: bold;
}
&#10;#cctlqodhdh .gt_font_italic {
  font-style: italic;
}
&#10;#cctlqodhdh .gt_super {
  font-size: 65%;
}
&#10;#cctlqodhdh .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#cctlqodhdh .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#cctlqodhdh .gt_indent_1 {
  text-indent: 5px;
}
&#10;#cctlqodhdh .gt_indent_2 {
  text-indent: 10px;
}
&#10;#cctlqodhdh .gt_indent_3 {
  text-indent: 15px;
}
&#10;#cctlqodhdh .gt_indent_4 {
  text-indent: 20px;
}
&#10;#cctlqodhdh .gt_indent_5 {
  text-indent: 25px;
}
&#10;#cctlqodhdh .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#cctlqodhdh div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" style="table-layout:fixed;width:800px;" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <colgroup>
    <col style="width:250px;"/>
    <col style="width:50px;"/>
    <col style="width:280px;"/>
  </colgroup>
  <thead>
    <tr class="gt_heading">
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><span class='gt_from_md'>Information on ID and Label Values</span></td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="a::stub">ID</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="i"><span class='gt_from_md'><em>Idx</em><br />
<em>Lvl</em></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="label">Label</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="Columns">Columns</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_1" scope="row" class="gt_row gt_left gt_stub" style="font-family: 'IBM Plex Mono'; font-size: 14px;">cyl</th>
<td headers="Columns stub_1_1 i" class="gt_row gt_right" style="font-family: 'IBM Plex Mono'; font-size: 14px;">1</td>
<td headers="Columns stub_1_1 label" class="gt_row gt_left" style="font-family: 'IBM Plex Mono'; font-size: 14px;">Cylinder</td></tr>
    <tr><th id="stub_1_2" scope="row" class="gt_row gt_left gt_stub" style="font-family: 'IBM Plex Mono'; font-size: 14px;">vs</th>
<td headers="Columns stub_1_2 i" class="gt_row gt_right" style="font-family: 'IBM Plex Mono'; font-size: 14px;">2</td>
<td headers="Columns stub_1_2 label" class="gt_row gt_left" style="font-family: 'IBM Plex Mono'; font-size: 14px;">Engine</td></tr>
    <tr><th id="stub_1_3" scope="row" class="gt_row gt_left gt_stub" style="font-family: 'IBM Plex Mono'; font-size: 14px;">N</th>
<td headers="Columns stub_1_3 i" class="gt_row gt_right" style="font-family: 'IBM Plex Mono'; font-size: 14px;">3</td>
<td headers="Columns stub_1_3 label" class="gt_row gt_left" style="font-family: 'IBM Plex Mono'; font-size: 14px;">N</td></tr>
    <tr><th id="stub_1_4" scope="row" class="gt_row gt_left gt_stub" style="font-family: 'IBM Plex Mono'; font-size: 14px;">mean_hp</th>
<td headers="Columns stub_1_4 i" class="gt_row gt_right" style="font-family: 'IBM Plex Mono'; font-size: 14px;">4</td>
<td headers="Columns stub_1_4 label" class="gt_row gt_left" style="font-family: 'IBM Plex Mono'; font-size: 14px;">Mean</td></tr>
    <tr><th id="stub_1_5" scope="row" class="gt_row gt_left gt_stub" style="font-family: 'IBM Plex Mono'; font-size: 14px;">sd_hp</th>
<td headers="Columns stub_1_5 i" class="gt_row gt_right" style="font-family: 'IBM Plex Mono'; font-size: 14px;">5</td>
<td headers="Columns stub_1_5 label" class="gt_row gt_left" style="font-family: 'IBM Plex Mono'; font-size: 14px;">SD</td></tr>
    <tr><th id="stub_1_6" scope="row" class="gt_row gt_left gt_stub" style="font-family: 'IBM Plex Mono'; font-size: 14px;">mean_wt</th>
<td headers="Columns stub_1_6 i" class="gt_row gt_right" style="font-family: 'IBM Plex Mono'; font-size: 14px;">6</td>
<td headers="Columns stub_1_6 label" class="gt_row gt_left" style="font-family: 'IBM Plex Mono'; font-size: 14px;">Mean</td></tr>
    <tr><th id="stub_1_7" scope="row" class="gt_row gt_left gt_stub" style="font-family: 'IBM Plex Mono'; font-size: 14px;">sd_wt</th>
<td headers="Columns stub_1_7 i" class="gt_row gt_right" style="font-family: 'IBM Plex Mono'; font-size: 14px;">7</td>
<td headers="Columns stub_1_7 label" class="gt_row gt_left" style="font-family: 'IBM Plex Mono'; font-size: 14px;">SD</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="Rows">Rows</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_8" scope="row" class="gt_row gt_left gt_stub" style="font-family: 'IBM Plex Mono'; font-size: 14px;">&lt;&lt; Index values 1 to 5 &gt;&gt;</th>
<td headers="Rows stub_1_8 i" class="gt_row gt_right" style="font-family: 'IBM Plex Mono'; font-size: 14px;"><br /></td>
<td headers="Rows stub_1_8 label" class="gt_row gt_left" style="font-family: 'IBM Plex Mono'; font-size: 14px;"><br /></td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="3" class="gt_group_heading" scope="colgroup" id="Spanners">Spanners</th>
    </tr>
    <tr class="gt_row_group_first"><th id="stub_1_9" scope="row" class="gt_row gt_left gt_stub" style="font-family: 'IBM Plex Mono'; font-size: 14px;">__BASE_LEVEL__Horse Power</th>
<td headers="Spanners stub_1_9 i" class="gt_row gt_right" style="font-family: 'IBM Plex Mono'; font-size: 14px;">1</td>
<td headers="Spanners stub_1_9 label" class="gt_row gt_left" style="font-family: 'IBM Plex Mono'; font-size: 14px;">Horse Power</td></tr>
    <tr><th id="stub_1_10" scope="row" class="gt_row gt_left gt_stub" style="font-family: 'IBM Plex Mono'; font-size: 14px;">__BASE_LEVEL__Weight</th>
<td headers="Spanners stub_1_10 i" class="gt_row gt_right" style="font-family: 'IBM Plex Mono'; font-size: 14px;">1</td>
<td headers="Spanners stub_1_10 label" class="gt_row gt_left" style="font-family: 'IBM Plex Mono'; font-size: 14px;">Weight</td></tr>
  </tbody>
  &#10;  
</table>
</div>

The IDs for the spanners can be found at the very bottom. To add another
spanner above `Horse Power` and `Weight`, we have to use these IDs:

``` r
gt_tbl |> 
  gt::tab_spanner(label = "New Spanner", 
                  spanners = c("__BASE_LEVEL__Horse Power", 
                               "__BASE_LEVEL__Weight"))
```

## Tables without row names

Using `1` on the left hand side of the formula creates a table without
row names. For example, `1 ~ (Horsepower = Mean:mean_hp + SD:sd_hp)`
defines

``` r
tablespan(data = summarized_table,
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
