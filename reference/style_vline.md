# style_vline

Set the style used for the vertical lines of the tablespan table.
Currently only supported for excel export.

## Usage

``` r
style_vline(tbl, openxlsx_style)
```

## Arguments

- tbl:

  tablespan table

- openxlsx_style:

  style used when exporting to openxlsx

## Value

the tablespan table with added styles

## Details

\- openxlsx_style must be a style object created with
openxlsx::createStyle. This style will then be applied to the vertical
lines

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

if(require_openxlsx(throw = FALSE))
wb <- tbl |>
  style_vline(
    openxlsx_style = openxlsx::createStyle(
      border = "Top",
      borderColour = "#928505",
      borderStyle = "thin")) |>
  as_excel()
# save workbook to see effect
```
