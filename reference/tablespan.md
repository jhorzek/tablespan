# tablespan

Create complex table spanners with a simple formula.

## Usage

``` r
tablespan(
  data,
  formula = 1 ~ .,
  title = NULL,
  subtitle = NULL,
  footnote = NULL,
  max_digits = 4
)
```

## Arguments

- data:

  data set

- formula:

  formula to create table

- title:

  string specifying the title of the table

- subtitle:

  string specifying the subtitle of the table

- footnote:

  string specifying the footnote of the table

- max_digits:

  the maximal number of digits to print for floating point numbers

## Value

Object of class Tablespan with title, subtitle, header info, data, and
footnote.

## Details

`tablespan` provides a formula based approach to adding headers and
spanners to an existing data.frame. The objective is to provide a
unified, easy to use, but good enough approach to building and exporting
tables to Excel, HTML, and LaTeX and other formats. To this end,
`tablespan` leverages the awesome packages `openxlsx`, `gt`, `flextabe`,
and `huxtable`.

Following the `tibble` approach, `tablespan` assumes that all items that
you may want to use as row names are just columns in your data set (see
example). That is, `tablespan` will allow you to pick some of your items
as row names and then just write them in a separate section to the left
of the data.

The table headers are defined with a basic formula approach inspired by
`tables`. For example, `Species ~ Sepal.Length + Sepal.Width` defines a
table with Species as the row names and Sepal.Length and Sepal.Width as
columns. The output will be similar to the following:

    |Species | Sepal.Length  Sepal.Width|
    |:-------|------------: -----------:|
    |setosa  |          5.1          3.5|
    |setosa  |          4.9          3.0|

Note that the row names (Species) are in a separate block to the left.

You can add spanner labels with as follows:

`Species ~ (Sepal = Sepal.Length + Sepal.Width) + (Petal = Sepal.Length + Sepal.Width)`

This will result in an output similar to:

    |        |           Sepal          |          Petal           |
    |Species | Sepal.Length| Sepal.Width| Petal.Length| Petal.Width|
    |:-------|------------:|-----------:|------------:|-----------:|
    |setosa  |          5.1|         3.5|          1.4|         0.2|

You can also nest spanners (e.g.,
`Species ~ (Sepal = (Length = Sepal.Length) + (Width = Sepal.Width))`.

When exporting tables, you may want to rename some of you columns. For
example, you may want to rename Sepal.Length and Petal.Length to Length
and Sepal.Width and Petal.Width to Width. With `tablespan`, you can
rename the item in the header using `new_name:old_name`. For example,
`Species ~ (Sepal = Length:Sepal.Length + Width:Sepal.Width) + (Petal = Length:Sepal.Length + Width:Sepal.Width)`
defines a table similar to the following:

    |        |      Sepal     |      Petal     |
    |Species | Length | Width | Length | Width |
    |:-------|-------:|------:|-------:|------:|
    |setosa  |     5.1|    3.5|     1.4|    0.2|

Finally, to create a table without row names, use
`1 ~ (Sepal = Length:Sepal.Length + Width:Sepal.Width) + (Petal = Length:Sepal.Length + Width:Sepal.Width)`
This defines as table similar to the following:

    |      Sepal     |      Petal     |
    | Length | Width | Length | Width |
    |-------:|------:|-------:|------:|
    |     5.1|    3.5|     1.4|    0.2|

Tables created with `tablespan` can be exported to Excel (using
`openxlsx`), HTML (using `gt`), LaTeX (using `gt`), and RTF (using
`gt`).

References:

- gt: Iannone R, Cheng J, Schloerke B, Hughes E, Lauer A, Seo J,
  Brevoort K, Roy O (2024). gt: Easily Create Presentation-Ready Display
  Tables. R package version 0.11.1.9000,
  \<https://github.com/rstudio/gt\>, \<https://gt.rstudio.com\>.

- tables: Murdoch D (2024). tables: Formula-Driven Table Generation. R
  package version 0.9.31, \<https://dmurdoch.github.io/tables/\>.

- openxlsx: Schauberger P, Walker A (2023). \_openxlsx: Read, Write and
  Edit xlsx Files\_. R package version 4.2.5.2,
  \<https://ycphs.github.io/openxlsx/\>.

- flextable: Gohel D, Skintzos P (2025). \_flextable: Functions for
  Tabular Reporting\_. R package version 0.9.10,
  \<https://CRAN.R-project.org/package=flextable\>.

- huxtable: Hugh-Jones D (2025). \_huxtable: Easily Create and Style
  Tables for LaTeX, HTML and Other Formats\_. R package version 5.8.0,
  \<https://CRAN.R-project.org/package=huxtable\>.

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

tbl
#>             Motor Trend Car Road Tests                                
#>             A table created with tablespan                            
#>           ┌───────────────────┬────┬─────────────────┬───────────────┐
#>           │                   │    │ Horse Power     │ Weight        │
#>           ├──────────┬────────┼────┼────────┬────────┼────────┬──────┤
#>           │ Cylinder │ Engine │ N  │ Mean   │ SD     │ Mean   │ SD   │
#>           ├──────────┴────────┼────┴────────┴────────┴────────┴──────┤
#>           │        4        0 │  1    91.00            2.1400        │
#>           │        4        1 │ 10    81.80   21.872   2.3003   0.60 │
#>           │        6        0 │  3   131.67   37.528   2.7550   0.13 │
#>           │        6        1 │  4   115.25    9.179   3.3887   0.12 │
#>           │        8        0 │ 14   209.21   50.977   3.9992   0.76 │
#>           └───────────────────┴──────────────────────────────────────┘
#>             Data from the infamous mtcars data                        
#>             set.                                                      
#> 
#> Column names: cyl, vs, N, mean_hp, sd_hp, mean_wt, sd_wt

# Add styling:
tbl <- tbl |>
    style_header(background_color = "#000000", text_color = "#ffffff") |>
    style_column(columns = where(is.double), bold = TRUE)

# Export as Excel table:
if(require_openxlsx(throw = FALSE))
  wb <- as_excel(tbl = tbl)

# Save using openxlsx
# openxlsx::saveWorkbook(wb, "cars.xlsx")

# Export as gt:
if(require_gt(throw = FALSE)) {
  as_gt(tbl)
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

 91.00

  

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

131.67

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

\# Export as flextable:
if([require_flextable](https://jhorzek.github.io/tablespan/reference/require_flextable.md)(throw
= FALSE)) {
flextable::[as_flextable](https://davidgohel.github.io/flextable/reference/as_flextable.html)(tbl)
}

| Motor Trend Car Road Tests              |        |     |             |        |        |      |
|-----------------------------------------|--------|-----|-------------|--------|--------|------|
| A table created with tablespan          |        |     |             |        |        |      |
|                                         |        |     | Horse Power |        | Weight |      |
| Cylinder                                | Engine | N   | Mean        | SD     | Mean   | SD   |
| 4                                       | 0      | 1   | 91.00       |        | 2.1400 |      |
| 4                                       | 1      | 10  | 81.80       | 21.872 | 2.3003 | 0.60 |
| 6                                       | 0      | 3   | 131.67      | 37.528 | 2.7550 | 0.13 |
| 6                                       | 1      | 4   | 115.25      | 9.179  | 3.3887 | 0.12 |
| 8                                       | 0      | 14  | 209.21      | 50.977 | 3.9992 | 0.76 |
| Data from the infamous mtcars data set. |        |     |             |        |        |      |

\# Export as gt:
if([require_huxtable](https://jhorzek.github.io/tablespan/reference/require_huxtable.md)(throw
= FALSE)) {
huxtable::[as_huxtable](https://hughjonesd.github.io/huxtable/reference/as_huxtable.html)(tbl)
} \#\> Motor Trend Car Road Tests \#\> A table created with tablespan
\#\> ┌───────────────────┬────┬─────────────────┬───────────────┐ \#\> │
│ │ Horse Power │ Weight │ \#\>
├──────────┬────────┼────┼────────┬────────┼────────┬──────┤ \#\> │
Cylinder │ Engine │ N │ Mean │ SD │ Mean │ SD │ \#\>
├──────────┴────────┼────┴────────┴────────┴────────┴──────┤ \#\> │ 4 0
│ 1 91.00      2.1400     │ \#\> │ 4 1 │ 10 81.80 21.872 2.3003 0.60 │
\#\> │ 6 0 │ 3 131.67 37.528 2.7550 0.13 │ \#\> │ 6 1 │ 4 115.25 9.179
3.3887 0.12 │ \#\> │ 8 0 │ 14 209.21 50.977 3.9992 0.76 │ \#\>
└───────────────────┴──────────────────────────────────────┘ \#\> Data
from the infamous mtcars data \#\> set. \#\> \#\> Column names: cyl, vs,
N, mean_hp, sd_hp, mean_wt, sd_wt
