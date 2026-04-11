# Create a Google Sheets API border style specification

Creates a border style specification that can be used with
gs_border_request() to apply borders to cells in a Google Sheet.

## Usage

``` r
gs_border_style(
  style = c("SOLID", "DOTTED", "DASHED", "SOLID_MEDIUM", "SOLID_THICK", "NONE", "DOUBLE"),
  width = 1,
  color
)
```

## Arguments

- style:

  Character. The style of the border. Must be one of: "SOLID", "DOTTED",
  "DASHED", "SOLID_MEDIUM", "SOLID_THICK", "NONE", or "DOUBLE".

- width:

  Numeric. The width of the border in pixels. Default is 1.

- color:

  The color of the border. Can be a color name, hex code, or a list with
  red, green, and blue components (values between 0 and 1).

## Value

A list containing the border style specification with elements: style
(The border style), width (The border width), and color (The border
color as a Google Sheets API color object)

## Examples

``` r
library(tablespan)
# Create a solid red border
border_style <- gs_border_style(style = "SOLID", width = 2, color = "red")

# Create a dotted blue border
border_style <- gs_border_style(style = "DOTTED", color = "#0000FF")
```
