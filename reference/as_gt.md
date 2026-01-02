# as_gt

Translates a table created with tablespan to a great table (gt). See
\<https://gt.rstudio.com/\>.

## Usage

``` r
as_gt(
  tbl,
  groupname_col = NULL,
  separator_style = NULL,
  auto_format = TRUE,
  ...
)
```

## Arguments

- tbl:

  table created with tablespan::tablespan

- groupname_col:

  Provide column names to group data. See ?gt::gt for more details.

- separator_style:

  style of the vertical line that separates the row names from the data.

- auto_format:

  should the table be formatted automatically?

- ...:

  additional arguments passed to gt::gt().

## Value

gt table that can be further adapted with the gt package.

## Details

Tablespan itself does not provide any printing of tables as HTML table.
However, with as_gt, tablespan can be translated to a great table which
provides html and LaTeX output.

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
if(require_gt(throw = FALSE)){
  gt_tbl <- as_gt(tbl)
  gt_tbl
}


  

```

Results

LHS

N

Horse Power

Weight

Cylinder

Engine

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
