# add_fake_sheet

Adds a fake sheet to a fake_sheet dribble.

## Usage

``` r
add_fake_sheet(fake_sheet, sheet_name)
```

## Arguments

- fake_sheet:

  fake sheet dribble created with fake_gs4_dribble

- sheet_name:

  name of the new sheet

## Examples

``` r
library(tablespan)
fake_sheet <- fake_gs4_dribble()
add_fake_sheet(fake_sheet, sheet_name = "new_sheet")
#> 
#> ── <googlesheets4_spreadsheet> ─────────────────────────────────────────────────
#> Spreadsheet name: Test          
#>               ID: spreadsheet_id
#>           Locale: en_US         
#>        Time zone: Europe/Berlin 
#>      # of sheets: 2             
#> 
#> ── <sheets> ────────────────────────────────────────────────────────────────────
#> (Sheet name): (Nominal extent in rows x columns)
#>       Sheet1: 1000 x 26
#>    new_sheet: 1000 x 26
```
