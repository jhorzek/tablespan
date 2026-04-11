# style_header

Set the style used for the header of the tablespan table.

## Usage

``` r
style_header(
  tbl,
  background_color = NULL,
  text_color = NULL,
  font_size = NULL,
  bold = FALSE,
  italic = FALSE,
  ...
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

- ...:

  optional additional arguments. Currently not used

## Value

the tablespan table with added styles

## Details

The styling for openxlsx and gt works differently:

\- openxlsx_style must be a style object created with
openxlsx::createStyle. This style will then be applied to the header -
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

if(require_gt(throw = FALSE))
tbl |>
  style_header(
    openxlsx_style = openxlsx::createStyle(
      fontSize = 8,
      fgFill = "#ffffff"),
    gt_style = list(gt::cell_text(size = 8))) |>
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
