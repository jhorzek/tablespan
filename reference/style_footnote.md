# style_footnote

Set the style used for the footnote of the tablespan table.

## Usage

``` r
style_footnote(
  tbl,
  background_color = NULL,
  text_color = NULL,
  font_size = NULL,
  bold = FALSE,
  italic = FALSE,
  openxlsx_style = NULL,
  gt_style = NULL,
  hux_style = NULL,
  flex_style = NULL
)
```

## Arguments

- tbl:

  tablespan table

- background_color:

  hex code for the background color

- text_color:

  hex code for the text color

- font_size:

  font size

- bold:

  set to TRUE for bold

- italic:

  set to TRUE for italic

- openxlsx_style:

  optional custom openxlsx style. When provided, all other arguments are
  ignored

- gt_style:

  optional custom gt style. When provided, all other arguments are
  ignored

- hux_style:

  optional custom huxtable style. When provided, all other arguments are
  ignored. Must be a function with the following signature:
  function(tbl, row, col){apply some style to the table and return the
  table}. Example: function(tbl, row, col){tbl \|\>
  huxtable::set_bold(row = row, col = col)}

- flex_style:

  optional custom flextable style. When provided, all other arguments
  are ignored. Must be a function with the following signature:
  function(tbl, row, col, part){apply some style to the table and return
  the table}. Example: function(tbl, row, col, part){tbl \|\>
  flextable::color(i = row, j = col, color = "red", part = part)}

## Value

the tablespan table with added styles

## Details

The styling for openxlsx and gt works differently:

\- openxlsx_style must be a style object created with
openxlsx::createStyle. This style will then be applied to the footnote -
gt_style must be a list of gt::tab_style objects to be applied to the
table

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
  style_footnote(bold = TRUE) |>
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
