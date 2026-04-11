# style_header_cells

Set the style used for the cells in the openxlsx export. This function
is used to create the borders around cells in openxlsx.

## Usage

``` r
style_header_cells(
  tbl,
  background_color = NULL,
  text_color = NULL,
  font_size = NULL,
  bold = FALSE,
  italic = FALSE,
  border_color = "#000000",
  top = FALSE,
  bottom = TRUE,
  left = TRUE,
  right = TRUE,
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

- border_color:

  set the color of the border for the header cells

- top:

  boolean. Set to TRUE to add a top border

- bottom:

  boolean. Set to TRUE to add a bottom border

- left:

  boolean. Set to TRUE to add a left border

- right:

  boolean. Set to TRUE to add a right border

- ...:

  optional additional arguments. Currently not used

## Value

the tablespan table with added styles

## Details

\- openxlsx_style must be a style object created with
openxlsx::createStyle. This style will then be applied to the header

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

if(require_openxlsx(throw = FALSE))
wb <- tbl |>
  style_header_cells(text_color = "#345364") |>
  as_excel()
# save workbook to see the effect
```
