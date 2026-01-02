# as_huxtable

Translates a table created with tablespan to a huxtable. See
\<https://hughjonesd.github.io/huxtable/index.html\>.

## Usage

``` r
# S3 method for class 'Tablespan'
as_huxtable(x, ...)
```

## Arguments

- x:

  table created with tablespan::tablespan

- ...:

  additional arguments passed to huxtable::as_huxtable

## Value

huxtable that can be further adapted with the gt package.

## Details

Huxtable is an extremely versatile table creator for R. Once translated
to a huxtable, the tablespan table is easy to export to all formats
directly supported by huxtable.

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
#> `summarise()` has grouped output by 'cyl'. You can override using the `.groups`
#> argument.

tbl <- tablespan(data = summarized_table,
                 formula = (LHS = Cylinder:cyl + Engine:vs) ~
                   N +
                   (Results = (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
                      (`Weight` = Mean:mean_wt + SD:sd_wt)))
if(require_huxtable(throw = FALSE)){
  library(huxtable)
  hux_tbl <- as_huxtable(tbl)
  hux_tbl
}
#> 
#> Attaching package: ‘huxtable’
#> The following objects are masked from ‘package:flextable’:
#> 
#>     align, as_flextable, bold, font, height, italic, set_caption,
#>     valign, width
#> The following object is masked from ‘package:dplyr’:
#> 
#>     add_rownames
#>           ┌───────────────────┬────┬─────────────────────────────────┐
#>           │                   │    │ Results                         │
#>           ├───────────────────┤    ├─────────────────┬───────────────┤
#>           │ LHS               │    │ Horse Power     │ Weight        │
#>           ├──────────┬────────┼────┼────────┬────────┼────────┬──────┤
#>           │ Cylinder │ Engine │ N  │ Mean   │ SD     │ Mean   │ SD   │
#>           ├──────────┴────────┼────┴────────┴────────┴────────┴──────┤
#>           │        4        0 │  1    91.00            2.1400        │
#>           │        4        1 │ 10    81.80   21.872   2.3003   0.60 │
#>           │        6        0 │  3   131.67   37.528   2.7550   0.13 │
#>           │        6        1 │  4   115.25    9.179   3.3887   0.12 │
#>           │        8        0 │ 14   209.21   50.977   3.9992   0.76 │
#>           └───────────────────┴──────────────────────────────────────┘
#> 
#> Column names: cyl, vs, N, mean_hp, sd_hp, mean_wt, sd_wt
```
