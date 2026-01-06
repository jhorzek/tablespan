# Exporting Tablespan

``` r
library(tablespan)
```

**tablespan** provides a unified, formula-driven approach to creating
complex tables with multi-level headers (spanners) that can be exported
to a variety of output formats. The core idea is to define the
*structure* of a table once, using a concise formula syntax, and then
reuse that structure across multiple reporting outputs such as Excel,
HTML, LaTeX, Word, Typst, or PowerPoint.

**Exporting** is a core design goal of **tablespan**: instead of
manually recreating tables for different formats, you can generate a
single `Tablespan` object and convert it into the format that best fits
your reporting workflow.

## Motivations for using tablespan

There are two primary motivations for using tablespan:

1.  **Easily create “good enough” tables that can be exported to many
    formats**. `tablespan` is designed to quickly generate basic tables
    that work across a wide variety of outputs. This is ideal for
    internal or semi-formal reporting where the *same table* needs to be
    delivered as:

    - HTML (dashboards)
    - PDF, Word or PowerPoint (reports and presentations)
    - Excel (shared tables)

2.  **Easily build a good starting point table that can then be refined
    with more sophisticated packages (e.g., gt or flextable)**.
    `tablespan` provides a concise syntax for relatively basic tables
    with title, subtitle, header, row names, columns and footnotes. Once
    exported to one of the supported packages (e.g., gt), these tables
    can easily be refined to be publication ready.

## Example: creating a tablespan object

``` r
library(tablespan)
library(dplyr)

data(mtcars)

summarized_table <- mtcars |>
  group_by(cyl, vs) |>
  summarise(
    N = n(),
    mean_hp = mean(hp),
    sd_hp = sd(hp),
    mean_wt = mean(wt),
    sd_wt = sd(wt),
    .groups = "drop")

tbl <- tablespan(
  data = summarized_table,
  formula = Cylinder:cyl + Engine:vs ~
    N +
    (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
    (`Weight` = Mean:mean_wt + SD:sd_wt),
  title = "Motor Trend Car Road Tests",
  subtitle = "A table created with tablespan",
  footnote = "Data from the mtcars data set")
```

At this point, `tbl` is a `Tablespan` object that contains the complete
logical definition of the table and can be exported or further styled.

## Styling with tablespan

Before exporting, basic styling can be applied directly at the tablespan
level.

### Column style with a color scale

For example, we can apply a color scale to the `mean_hp` column:

``` r
tbl <- tbl |>
  style_column(
    columns = mean_hp, 
    text_color = "#FFFFFF",
    color_scale = c("#01016f" = NA, "#d8031c" = NA))
```

Any style applied directly with tablespan will be automatically exported
to all other table formats (e.g., gt).

## Exporting tables

Before exporting, keep the following points in mind:

1.  **The required backend packages must be installed** tablespan
    delegates exporting to other packages. If a backend package is not
    installed, the corresponding export will not work (e.g., make sure
    that gt is installed to export to gt).

2.  **flextable and huxtable must be loaded** For
    [`as_flextable()`](https://hughjonesd.github.io/huxtable/reference/as_flextable.html)
    and
    [`as_huxtable()`](https://hughjonesd.github.io/huxtable/reference/as_huxtable.html)
    to work, the respective packages must be loaded.

## Export to Excel (openxlsx)

**To use the excel export, please make sure that openxlsx is
installed.**

### Supported formats

- `.xlsx`

### Example

``` r
wb <- as_excel(tbl)
# openxlsx::saveWorkbook(wb, "cars.xlsx", overwrite = TRUE)
```

------------------------------------------------------------------------

## Export to gt and further refinement

> “The `gt` package in R is a powerful tool for creating **elegant and
> customizable tables** for data visualization and reporting. It offers
> a user-friendly way to **design and style tables** in RMarkdown
> documents and Shiny applications.”
>
> <https://r-graph-gallery.com/package/gt.html>

**To use the gt export, please make sure that gt is installed.**

**Supported [export
formats](https://gt.rstudio.com/reference/gtsave.html)**:

- HTML
- LaTeX
- RTF
- Word (`.docx`)

### Example

``` r
gt_tbl <- as_gt(tbl)
gt_tbl
```

[TABLE]

Optionally adapt the table with styles not supported directly within
`tablespan`:

``` r
gt_tbl <- gt_tbl |>
  gt::cols_align(
    align = "left",
    columns = 1:2)

gt_tbl
```

[TABLE]

## Export to flextable

> “The flextable package provides a framework to easily create tables
> for reporting and publications. Functions are provided to let users
> create tables, modify and format their content, and define their
> content.”
>
> <https://ardata-fr.github.io/flextable-book/>

**To use the flextabe export, please make sure that flextable is
installed and loaded.**

### Supported formats

- HTML
- PDF
- Word (`.docx`)
- PowerPoint (`.pptx`)

### Example

``` r
ft <- flextable::as_flextable(tbl)
ft
```

| Motor Trend Car Road Tests     |        |     |             |        |        |      |
|--------------------------------|--------|-----|-------------|--------|--------|------|
| A table created with tablespan |        |     |             |        |        |      |
|                                |        |     | Horse Power |        | Weight |      |
| Cylinder                       | Engine | N   | Mean        | SD     | Mean   | SD   |
| 4                              | 0      | 1   | 91.00       |        | 2.1400 |      |
| 4                              | 1      | 10  | 81.80       | 21.872 | 2.3003 | 0.60 |
| 6                              | 0      | 3   | 131.67      | 37.528 | 2.7550 | 0.13 |
| 6                              | 1      | 4   | 115.25      | 9.179  | 3.3887 | 0.12 |
| 8                              | 0      | 14  | 209.21      | 50.977 | 3.9992 | 0.76 |
| Data from the mtcars data set  |        |     |             |        |        |      |

Optionally post-process the table:

``` r
compose(ft,
  j = 5,
  value = flextable::as_paragraph(
    flextable::minibar(value = sd_hp, 
    max = max(sd_hp, na.rm = TRUE))),
  part = "body")
```

| Motor Trend Car Road Tests     |        |     |             |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |        |      |
|--------------------------------|--------|-----|-------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------|------|
| A table created with tablespan |        |     |             |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |        |      |
|                                |        |     | Horse Power |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         | Weight |      |
| Cylinder                       | Engine | N   | Mean        | SD                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      | Mean   | SD   |
| 4                              | 0      | 1   | 91.00       | ![](data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAASwAAAA8CAYAAADc3IdaAAAABmJLR0QAAAAAAAD5Q7t/AAAACXBIWXMAAC4jAAAuIwF4pT92AAAAXUlEQVR4nO3BMQEAAADCoPVPbQlPoAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAICnARmLAAHd/g3OAAAAAElFTkSuQmCC)                                                                                                                                                                                                 | 2.1400 |      |
| 4                              | 1      | 10  | 81.80       | ![](data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAASwAAAA8CAYAAADc3IdaAAAABmJLR0QAAAAAAAD5Q7t/AAAACXBIWXMAAC4jAAAuIwF4pT92AAAA7UlEQVR4nO3UQQ2AMADAQIam+VcwT2Bh4bM03Cnoq2Ot9Vz8ypxznG6AL+7TAQC7DAvIMCwgw7CADMMCMgwLyDAsIMOwgAzDAjIMC8gwLCDDsIAMwwIyDAvIMCwgw7CADMMCMgwLyDAsIMOwgAzDAjIMC8gwLCDDsIAMwwIyDAvIMCwgw7CADMMCMgwLyDAsIMOwgAzDAjIMC8gwLCDDsIAMwwIyDAvIMCwgw7CADMMCMgwLyDAsIMOwgAzDAjIMC8gwLCDDsIAMwwIyDAvIMCwgw7CADMMCMgwLyDAsIMOwgAzDAjIMC8gwLCDjBQvABHhI26cGAAAAAElFTkSuQmCC) | 2.3003 | 0.60 |
| 6                              | 0      | 3   | 131.67      | ![](data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAASwAAAA8CAYAAADc3IdaAAAABmJLR0QAAAAAAAD5Q7t/AAAACXBIWXMAAC4jAAAuIwF4pT92AAAA7ElEQVR4nO3UQQ2AMBAAQYqm86+gnsACL5pNZhTsa9fe+7mA38zMOt1QdZ8OAPjKsIAMwwIyDAvIMCwgw7CADMMCMgwLyDAsIMOwgAzDAjIMC8gwLCDDsIAMwwIyDAvIMCwgw7CADMMCMgwLyDAsIMOwgAzDAjIMC8gwLCDDsIAMwwIyDAvIMCwgw7CADMMCMgwLyDAsIMOwgAzDAjIMC8gwLCDDsIAMwwIyDAvIMCwgw7CADMMCMgwLyDAsIMOwgAzDAjIMC8gwLCDDsIAMwwIyDAvIMCwgw7CADMMCMgwLyDAsIMOwgAzDAjJeIDEEeBb06AQAAAAASUVORK5CYII=) | 2.7550 | 0.13 |
| 6                              | 1      | 4   | 115.25      | ![](data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAASwAAAA8CAYAAADc3IdaAAAABmJLR0QAAAAAAAD5Q7t/AAAACXBIWXMAAC4jAAAuIwF4pT92AAAA7UlEQVR4nO3UQQ2AMADAQIam+VcwT2BhH7KU3Cnoq2Ot9Vw/MOccpxuAb92nAwB2GRaQYVhAhmEBGYYFZBgWkGFYQIZhARmGBWQYFpBhWECGYQEZhgVkGBaQYVhAhmEBGYYFZBgWkGFYQIZhARmGBWQYFpBhWECGYQEZhgVkGBaQYVhAhmEBGYYFZBgWkGFYQIZhARmGBWQYFpBhWECGYQEZhgVkGBaQYVhAhmEBGYYFZBgWkGFYQIZhARmGBWQYFpBhWECGYQEZhgVkGBaQYVhAhmEBGYYFZBgWkGFYQIZhARmGBWQYFpBhWEDGC8O8BHiJtfHhAAAAAElFTkSuQmCC) | 3.3887 | 0.12 |
| 8                              | 0      | 14  | 209.21      | ![](data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAASwAAAA8CAYAAADc3IdaAAAABmJLR0QAAAAAAAD5Q7t/AAAACXBIWXMAAC4jAAAuIwF4pT92AAAA5ElEQVR4nO3UQQ0AIRDAwOP8S1tPYIEfaTKjoK+umdkfQMD/OgDglmEBGYYFZBgWkGFYQIZhARmGBWQYFpBhWECGYQEZhgVkGBaQYVhAhmEBGYYFZBgWkGFYQIZhARmGBWQYFpBhWECGYQEZhgVkGBaQYVhAhmEBGYYFZBgWkGFYQIZhARmGBWQYFpBhWECGYQEZhgVkGBaQYVhAhmEBGYYFZBgWkGFYQIZhARmGBWQYFpBhWECGYQEZhgVkGBaQYVhAhmEBGYYFZBgWkGFYQIZhARmGBWQYFpBhWECGYQEZhgVkHJSAA9uXsGskAAAAAElFTkSuQmCC)             | 3.9992 | 0.76 |
| Data from the mtcars data set  |        |     |             |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |        |      |

## Export to huxtable

> “Huxtable is an R package to create LaTeX and HTML tables, with a
> friendly, modern interface. Features include control over text
> styling, number format, background color, borders, padding and
> alignment. Cells can span multiple rows and/or columns. Tables can be
> manipulated with standard R subsetting or `dplyr` functions.”
>
> <https://hughjonesd.github.io/huxtable/>

**To use the huxtable export, please make sure that huxtable is
installed and loaded.**

### Supported formats

- Console
- LaTeX
- HTML
- RTF
- Excel (`.xlsx`)
- Word (`.docx`)

### Example

``` r
ht <- huxtable::as_huxtable(tbl)
ht
```

|                                |        |     |             |        |        |      |
|--------------------------------|--------|-----|-------------|--------|--------|------|
| Motor Trend Car Road Tests     |        |     |             |        |        |      |
| A table created with tablespan |        |     |             |        |        |      |
|                                |        |     | Horse Power |        | Weight |      |
| Cylinder                       | Engine | N   | Mean        | SD     | Mean   | SD   |
| 4                              | 0      | 1   | 91.00       |        | 2.1400 |      |
| 4                              | 1      | 10  | 81.80       | 21.872 | 2.3003 | 0.60 |
| 6                              | 0      | 3   | 131.67      | 37.528 | 2.7550 | 0.13 |
| 6                              | 1      | 4   | 115.25      | 9.179  | 3.3887 | 0.12 |
| 8                              | 0      | 14  | 209.21      | 50.977 | 3.9992 | 0.76 |
| Data from the mtcars data set  |        |     |             |        |        |      |

Optional post-processing:

``` r
ht <- huxtable::set_background_color(ht, row = 5:9, value = "orange")
ht
```

|                                |        |     |             |        |        |      |
|--------------------------------|--------|-----|-------------|--------|--------|------|
| Motor Trend Car Road Tests     |        |     |             |        |        |      |
| A table created with tablespan |        |     |             |        |        |      |
|                                |        |     | Horse Power |        | Weight |      |
| Cylinder                       | Engine | N   | Mean        | SD     | Mean   | SD   |
| 4                              | 0      | 1   | 91.00       |        | 2.1400 |      |
| 4                              | 1      | 10  | 81.80       | 21.872 | 2.3003 | 0.60 |
| 6                              | 0      | 3   | 131.67      | 37.528 | 2.7550 | 0.13 |
| 6                              | 1      | 4   | 115.25      | 9.179  | 3.3887 | 0.12 |
| 8                              | 0      | 14  | 209.21      | 50.977 | 3.9992 | 0.76 |
| Data from the mtcars data set  |        |     |             |        |        |      |
