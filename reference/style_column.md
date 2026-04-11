# style_column

Change the style of a column or single cells within columns.

## Usage

``` r
style_column(
  tbl,
  columns = dplyr::everything(),
  rows = NULL,
  background_color = NULL,
  text_color = NULL,
  font_size = NULL,
  bold = FALSE,
  italic = FALSE,
  color_scale = NULL,
  stack = TRUE,
  ...
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

- color_scale:

  a named vector of length 2 or 3 to define a color scale. Example for
  two colors: color_scale = c("#EE2F43" = -1, "#37E65A" = 1). Example
  for three colors: color_scale = c("#EE2F43" = -1, "#FFFFFF" = 0,
  "#37E65A" = 1). If a value is set as NA, it will be replaced with the
  minimum, mean, or maximum respectively (e.g., color_scale =
  c("#EE2F43" = -1, "#FFFFFF" = 0, "#37E65A" = 1) will be replaced by
  color_scale = c("#EE2F43" = min(data), "#FFFFFF" = 0, "#37E65A" =
  max(data))). NOTE: When exporting to gt, make sure to apply the color
  scale before you change the text color; otherwise, gt will overwrite
  the text color.

- stack:

  When set to TRUE, the style is added on top of the existing styles.
  This is mostly relevant for openxlsx. When set to FALSE, the new style
  replaces all previous styling.

- ...:

  optional additional arguments. Currently not used

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

if(require_gt(throw = FALSE))
tbl |>
  style_column(columns = mean_hp,
               bold = TRUE) |>
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
