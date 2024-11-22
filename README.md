
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tablespan

> Create satisficing tables in R the formula way.

The objective of `tablespan` is to provide a “good enough” approach to
creating tables in R with simple formulas (inspired by
[`tables`](https://dmurdoch.github.io/tables/)). To this end,
`tablespan` builds on the awesome packages
[`openxlsx`](https://ycphs.github.io/openxlsx/) and
[`gt`](https://gt.rstudio.com/).

Tables created with `tablespan` can be exported to:

1.  [Excel](#excel) (using
    [`openxlsx`](https://ycphs.github.io/openxlsx/))
2.  [HTML](#html) (using [`gt`](https://gt.rstudio.com/))
3.  [LaTeX](#html) (using [`gt`](https://gt.rstudio.com/))
4.  [RTF](#html) (using [`gt`](https://gt.rstudio.com/))

## Installation

`tablespan` is not yet available from CRAN. To install from GitHub, run
the following lines:

``` r
library(remotes)
remotes::install_github("jhorzek/tablespan")
```

## Introduction

`tablespan` assumes that you already have a perfectly summarized table
that you now want to share. All you need are some **table** headers with
**spanners** without investing much time into making it look perfect.
This is what `tablespan` was designed for.

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

### Creating a Basic Table

In `tablespan`, the table headers are defined with a formula approach
inspired by the [`tables`](https://dmurdoch.github.io/tables/) package.
For example, `cyl ~ mean_hp + sd_hp` defines a table with `cyl` as the
row names and `mean_hp` and `sd_hp` as columns. The output will look as
follows:

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
`Horsepower`: `cyl ~ (Horsepower = mean_hp + sd_hp)`.

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

Spanners can also be nested (e.g.,
`cyl ~ (Horsepower = (Mean = mean_hp) + (SD  = sd_hp))`.

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
creating the full table as follows:

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
`to_excel` function:

``` r
# to_excel creates an openxlsx workbook
wb <- to_excel(tbl = tbl)

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
translating the table to an `openxlsx` workbook with `to_excel`. To this
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

Finally, we pass this style as part of a list to `to_excel`:

``` r
# to_excel creates an openxlsx workbook
wb <- to_excel(tbl = tbl, 
               styles = tbl_styles(cell_styles = list(hp_ge_100)))

# Save the workbook as an xlsx file:
# openxlsx::saveWorkbook(wb,
#                        file = "cars.xlsx", 
#                        overwrite = TRUE)
```

![](man/figures/tablespan_example_cars_styled.png)

#### Formatting Data Types

`tablespan` also allows formatting specific data types. Let’s assume
that we want to round all doubles to 3 instead of the default four
digits. To this end, we use the `create_data_styles` function, where we
specify (1) a function that checks for the data type we want to style
(here `is.double`) and (2) a style for all columns that match that
style:

``` r
double_style <- create_data_styles(double = list(test = is.double, 
                                                 style = openxlsx::createStyle(numFmt = "0.000")))
wb <- to_excel(tbl = tbl, styles = tbl_styles(data_styles = double_style))

# Save the workbook as an xlsx file:
# openxlsx::saveWorkbook(wb,
#                        file = "cars.xlsx", 
#                        overwrite = TRUE)
```

![](man/figures/tablespan_example_cars_styled_data.png)

## Exporting to HTML, LaTeX, and RTF

Tables created with `tablespan` can also be exported to `gt` which
allows saving as HTML, LaTeX, or RTF file. To this end, we simply have
to call `to_gt` on our table:

``` r
# Translate to gt:
gt_tbl <- to_gt(tbl = tbl)
gt_tbl
```

<div id="dlddicmdmx" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#dlddicmdmx table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#dlddicmdmx thead, #dlddicmdmx tbody, #dlddicmdmx tfoot, #dlddicmdmx tr, #dlddicmdmx td, #dlddicmdmx th {
  border-style: none;
}
&#10;#dlddicmdmx p {
  margin: 0;
  padding: 0;
}
&#10;#dlddicmdmx .gt_table {
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
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#dlddicmdmx .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#dlddicmdmx .gt_title {
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
&#10;#dlddicmdmx .gt_subtitle {
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
&#10;#dlddicmdmx .gt_heading {
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
&#10;#dlddicmdmx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dlddicmdmx .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#dlddicmdmx .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
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
&#10;#dlddicmdmx .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#dlddicmdmx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#dlddicmdmx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#dlddicmdmx .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#dlddicmdmx .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#dlddicmdmx .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
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
&#10;#dlddicmdmx .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#dlddicmdmx .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#dlddicmdmx .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#dlddicmdmx .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#dlddicmdmx .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dlddicmdmx .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#dlddicmdmx .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#dlddicmdmx .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#dlddicmdmx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dlddicmdmx .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#dlddicmdmx .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#dlddicmdmx .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dlddicmdmx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dlddicmdmx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#dlddicmdmx .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#dlddicmdmx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#dlddicmdmx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#dlddicmdmx .gt_footnotes {
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
&#10;#dlddicmdmx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dlddicmdmx .gt_sourcenotes {
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
&#10;#dlddicmdmx .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#dlddicmdmx .gt_left {
  text-align: left;
}
&#10;#dlddicmdmx .gt_center {
  text-align: center;
}
&#10;#dlddicmdmx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#dlddicmdmx .gt_font_normal {
  font-weight: normal;
}
&#10;#dlddicmdmx .gt_font_bold {
  font-weight: bold;
}
&#10;#dlddicmdmx .gt_font_italic {
  font-style: italic;
}
&#10;#dlddicmdmx .gt_super {
  font-size: 65%;
}
&#10;#dlddicmdmx .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#dlddicmdmx .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#dlddicmdmx .gt_indent_1 {
  text-indent: 5px;
}
&#10;#dlddicmdmx .gt_indent_2 {
  text-indent: 10px;
}
&#10;#dlddicmdmx .gt_indent_3 {
  text-indent: 15px;
}
&#10;#dlddicmdmx .gt_indent_4 {
  text-indent: 20px;
}
&#10;#dlddicmdmx .gt_indent_5 {
  text-indent: 25px;
}
&#10;#dlddicmdmx .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#dlddicmdmx div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="7" class="gt_heading gt_title gt_font_normal" style>Motor Trend Car Road Tests</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="7" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>A table created with tablespan</td>
    </tr>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" scope="col" id="cyl">Cylinder</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" scope="col" id="vs">Engine</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" scope="col" id="N">N</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="Horse Power">
        <div class="gt_column_spanner">Horse Power</div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="Weight">
        <div class="gt_column_spanner">Weight</div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="mean_hp">Mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sd_hp">SD</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="mean_wt">Mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sd_wt">SD</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="cyl" class="gt_row gt_right">4 </td>
<td headers="vs" class="gt_row gt_right" style="border-right-width: 1px; border-right-style: solid; border-right-color: gray;">0 </td>
<td headers="N" class="gt_row gt_right"> 1 </td>
<td headers="mean_hp" class="gt_row gt_right"> 91    </td>
<td headers="sd_hp" class="gt_row gt_right"><br /></td>
<td headers="mean_wt" class="gt_row gt_right">2.14 </td>
<td headers="sd_wt" class="gt_row gt_right"><br /></td></tr>
    <tr><td headers="cyl" class="gt_row gt_right">4 </td>
<td headers="vs" class="gt_row gt_right" style="border-right-width: 1px; border-right-style: solid; border-right-color: gray;">1 </td>
<td headers="N" class="gt_row gt_right">10 </td>
<td headers="mean_hp" class="gt_row gt_right"> 81.8  </td>
<td headers="sd_hp" class="gt_row gt_right">21.872</td>
<td headers="mean_wt" class="gt_row gt_right">2.3  </td>
<td headers="sd_wt" class="gt_row gt_right">0.598</td></tr>
    <tr><td headers="cyl" class="gt_row gt_right">6 </td>
<td headers="vs" class="gt_row gt_right" style="border-right-width: 1px; border-right-style: solid; border-right-color: gray;">0 </td>
<td headers="N" class="gt_row gt_right"> 3 </td>
<td headers="mean_hp" class="gt_row gt_right">131.667</td>
<td headers="sd_hp" class="gt_row gt_right">37.528</td>
<td headers="mean_wt" class="gt_row gt_right">2.755</td>
<td headers="sd_wt" class="gt_row gt_right">0.128</td></tr>
    <tr><td headers="cyl" class="gt_row gt_right">6 </td>
<td headers="vs" class="gt_row gt_right" style="border-right-width: 1px; border-right-style: solid; border-right-color: gray;">1 </td>
<td headers="N" class="gt_row gt_right"> 4 </td>
<td headers="mean_hp" class="gt_row gt_right">115.25 </td>
<td headers="sd_hp" class="gt_row gt_right"> 9.179</td>
<td headers="mean_wt" class="gt_row gt_right">3.389</td>
<td headers="sd_wt" class="gt_row gt_right">0.116</td></tr>
    <tr><td headers="cyl" class="gt_row gt_right">8 </td>
<td headers="vs" class="gt_row gt_right" style="border-right-width: 1px; border-right-style: solid; border-right-color: gray;">0 </td>
<td headers="N" class="gt_row gt_right">14 </td>
<td headers="mean_hp" class="gt_row gt_right">209.214</td>
<td headers="sd_hp" class="gt_row gt_right">50.977</td>
<td headers="mean_wt" class="gt_row gt_right">3.999</td>
<td headers="sd_wt" class="gt_row gt_right">0.759</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"> Data from the infamous mtcars data set.</td>
    </tr>
  </tfoot>
</table>
</div>

The `gt` package provides a wide range of functions to adapt the style
of these tables. For instance, `opt_stylize` adds a pre-defined style to
the entire table:

``` r
gt_tbl |> 
  gt::opt_stylize(style = 6,
                  color = 'gray')
```

<div id="owkzwqwtrw" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#owkzwqwtrw table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#owkzwqwtrw thead, #owkzwqwtrw tbody, #owkzwqwtrw tfoot, #owkzwqwtrw tr, #owkzwqwtrw td, #owkzwqwtrw th {
  border-style: none;
}
&#10;#owkzwqwtrw p {
  margin: 0;
  padding: 0;
}
&#10;#owkzwqwtrw .gt_table {
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
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #5F5F5F;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #5F5F5F;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#owkzwqwtrw .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#owkzwqwtrw .gt_title {
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
&#10;#owkzwqwtrw .gt_subtitle {
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
&#10;#owkzwqwtrw .gt_heading {
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
&#10;#owkzwqwtrw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #5F5F5F;
}
&#10;#owkzwqwtrw .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #5F5F5F;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #5F5F5F;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#owkzwqwtrw .gt_col_heading {
  color: #FFFFFF;
  background-color: #5F5F5F;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
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
&#10;#owkzwqwtrw .gt_column_spanner_outer {
  color: #FFFFFF;
  background-color: #5F5F5F;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#owkzwqwtrw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#owkzwqwtrw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#owkzwqwtrw .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #5F5F5F;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#owkzwqwtrw .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#owkzwqwtrw .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #5F5F5F;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #5F5F5F;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#owkzwqwtrw .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #5F5F5F;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #5F5F5F;
  vertical-align: middle;
}
&#10;#owkzwqwtrw .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#owkzwqwtrw .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#owkzwqwtrw .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: none;
  border-top-width: 1px;
  border-top-color: #D5D5D5;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D5D5D5;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D5D5D5;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#owkzwqwtrw .gt_stub {
  color: #333333;
  background-color: #D5D5D5;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D5D5D5;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#owkzwqwtrw .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#owkzwqwtrw .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#owkzwqwtrw .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#owkzwqwtrw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#owkzwqwtrw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #5F5F5F;
}
&#10;#owkzwqwtrw .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#owkzwqwtrw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #5F5F5F;
}
&#10;#owkzwqwtrw .gt_grand_summary_row {
  color: #333333;
  background-color: #D5D5D5;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#owkzwqwtrw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #5F5F5F;
}
&#10;#owkzwqwtrw .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #5F5F5F;
}
&#10;#owkzwqwtrw .gt_striped {
  background-color: #F4F4F4;
}
&#10;#owkzwqwtrw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #5F5F5F;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #5F5F5F;
}
&#10;#owkzwqwtrw .gt_footnotes {
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
&#10;#owkzwqwtrw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#owkzwqwtrw .gt_sourcenotes {
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
&#10;#owkzwqwtrw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#owkzwqwtrw .gt_left {
  text-align: left;
}
&#10;#owkzwqwtrw .gt_center {
  text-align: center;
}
&#10;#owkzwqwtrw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#owkzwqwtrw .gt_font_normal {
  font-weight: normal;
}
&#10;#owkzwqwtrw .gt_font_bold {
  font-weight: bold;
}
&#10;#owkzwqwtrw .gt_font_italic {
  font-style: italic;
}
&#10;#owkzwqwtrw .gt_super {
  font-size: 65%;
}
&#10;#owkzwqwtrw .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#owkzwqwtrw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#owkzwqwtrw .gt_indent_1 {
  text-indent: 5px;
}
&#10;#owkzwqwtrw .gt_indent_2 {
  text-indent: 10px;
}
&#10;#owkzwqwtrw .gt_indent_3 {
  text-indent: 15px;
}
&#10;#owkzwqwtrw .gt_indent_4 {
  text-indent: 20px;
}
&#10;#owkzwqwtrw .gt_indent_5 {
  text-indent: 25px;
}
&#10;#owkzwqwtrw .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#owkzwqwtrw div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="7" class="gt_heading gt_title gt_font_normal" style>Motor Trend Car Road Tests</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="7" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>A table created with tablespan</td>
    </tr>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" scope="col" id="cyl">Cylinder</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" scope="col" id="vs">Engine</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="2" colspan="1" scope="col" id="N">N</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="Horse Power">
        <div class="gt_column_spanner">Horse Power</div>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2" scope="colgroup" id="Weight">
        <div class="gt_column_spanner">Weight</div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="mean_hp">Mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sd_hp">SD</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="mean_wt">Mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="sd_wt">SD</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="cyl" class="gt_row gt_right">4 </td>
<td headers="vs" class="gt_row gt_right" style="border-right-width: 1px; border-right-style: solid; border-right-color: gray;">0 </td>
<td headers="N" class="gt_row gt_right"> 1 </td>
<td headers="mean_hp" class="gt_row gt_right"> 91    </td>
<td headers="sd_hp" class="gt_row gt_right"><br /></td>
<td headers="mean_wt" class="gt_row gt_right">2.14 </td>
<td headers="sd_wt" class="gt_row gt_right"><br /></td></tr>
    <tr><td headers="cyl" class="gt_row gt_right gt_striped">4 </td>
<td headers="vs" class="gt_row gt_right gt_striped" style="border-right-width: 1px; border-right-style: solid; border-right-color: gray;">1 </td>
<td headers="N" class="gt_row gt_right gt_striped">10 </td>
<td headers="mean_hp" class="gt_row gt_right gt_striped"> 81.8  </td>
<td headers="sd_hp" class="gt_row gt_right gt_striped">21.872</td>
<td headers="mean_wt" class="gt_row gt_right gt_striped">2.3  </td>
<td headers="sd_wt" class="gt_row gt_right gt_striped">0.598</td></tr>
    <tr><td headers="cyl" class="gt_row gt_right">6 </td>
<td headers="vs" class="gt_row gt_right" style="border-right-width: 1px; border-right-style: solid; border-right-color: gray;">0 </td>
<td headers="N" class="gt_row gt_right"> 3 </td>
<td headers="mean_hp" class="gt_row gt_right">131.667</td>
<td headers="sd_hp" class="gt_row gt_right">37.528</td>
<td headers="mean_wt" class="gt_row gt_right">2.755</td>
<td headers="sd_wt" class="gt_row gt_right">0.128</td></tr>
    <tr><td headers="cyl" class="gt_row gt_right gt_striped">6 </td>
<td headers="vs" class="gt_row gt_right gt_striped" style="border-right-width: 1px; border-right-style: solid; border-right-color: gray;">1 </td>
<td headers="N" class="gt_row gt_right gt_striped"> 4 </td>
<td headers="mean_hp" class="gt_row gt_right gt_striped">115.25 </td>
<td headers="sd_hp" class="gt_row gt_right gt_striped"> 9.179</td>
<td headers="mean_wt" class="gt_row gt_right gt_striped">3.389</td>
<td headers="sd_wt" class="gt_row gt_right gt_striped">0.116</td></tr>
    <tr><td headers="cyl" class="gt_row gt_right">8 </td>
<td headers="vs" class="gt_row gt_right" style="border-right-width: 1px; border-right-style: solid; border-right-color: gray;">0 </td>
<td headers="N" class="gt_row gt_right">14 </td>
<td headers="mean_hp" class="gt_row gt_right">209.214</td>
<td headers="sd_hp" class="gt_row gt_right">50.977</td>
<td headers="mean_wt" class="gt_row gt_right">3.999</td>
<td headers="sd_wt" class="gt_row gt_right">0.759</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"> Data from the infamous mtcars data set.</td>
    </tr>
  </tfoot>
</table>
</div>

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
