# fake_gs4_dribble

Creates a fake googlesheets4 dribble to use as a placeholder in the
as_googlesheets_request function.

## Usage

``` r
fake_gs4_dribble()
```

## Value

fake dribble

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
