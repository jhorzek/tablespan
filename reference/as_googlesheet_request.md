# as_googlesheet_request

Creates a googlesheets4 request to write the tablespan table to a Google
Sheet.

## Usage

``` r
as_googlesheet_request(
  tbl,
  google_sheet,
  sheet = "Table",
  start_row = 1,
  start_col = 1,
  merge_rownames = TRUE,
  dry_run = is(google_sheet, "fake_sheet"),
  token = NULL,
  escape_formulas = TRUE,
  silent = FALSE
)
```

## Arguments

- tbl:

  table created with tablespan::tablespan

- google_sheet:

  Google Sheet spreadsheet dribble created with googlesheets4::gs4_get()

- sheet:

  name of the sheet to which the table should be written to

- start_row:

  row at which to start the table

- start_col:

  column at which to start the table

- merge_rownames:

  should row names with identical entries be merged?

- dry_run:

  if set to TRUE, no API calls will be made. This is useful when just
  testing the function

- token:

  optional token for authenticated requests. If NULL and \`dry_run =
  FALSE\`, tablespan will use \`googlesheets4::gs4_token()\`
  automatically. You can also pass an explicit token from
  \`googlesheets4::gs4_token()\`.

- escape_formulas:

  should formulas be escaped to prevent execution in Google Sheets?
  Default is TRUE, which means that any cell content starting with "="
  will be escaped by prefixing it with a single quote ('). This ensures
  that the content is treated as text rather than a formula in Google
  Sheets. If set to FALSE, cells starting with "=" will be written as
  formulas and executed in Google Sheets. Use with caution if your data
  may contain content that could be interpreted as formulas.

- silent:

  suppress messages when TRUE

## Value

A request that can be passed to googlesheets4::request_make to write the
tablespan to a google sheet

## Details

Tablespan will not directly write to the google sheet. Instead, it will
return a googlesheets4 request that can be used to write the table to a
google sheet with googlesheets4::request_make

## Examples

``` r
if (FALSE) { # \dontrun{
library(tablespan)
library(dplyr)

# First summarize the data:
summarized_table <- mtcars |>
  group_by(cyl, vs) |>
  summarise(N = n(),
            mean_hp = mean(hp),
            sd_hp = sd(hp),
            mean_wt = mean(wt),
            sd_wt = sd(wt))

# Now, create a table with grouping variables as row names and spanners:
tbl <- tablespan(data = summarized_table,
          formula = Cylinder:cyl + Engine:vs ~
            N +
            (`Horse Power` = Mean:mean_hp + SD:sd_hp) +
            (`Weight` = Mean:mean_wt + SD:sd_wt),
          title = "Motor Trend Car Road Tests",
          subtitle = "A table created with tablespan",
          footnote = "Data from the infamous mtcars data set.")

if(require_googlesheets4(throw = FALSE)) {
  # Get the Google Sheet (replace with your actual sheet URL)
  # google_sheet <- googlesheets4::gs4_get(ss = "link-to-your-google-sheet")
  google_sheet <- fake_gs4_dribble()

  # Create a request to write the data to the googlesheet
  req <- as_googlesheet_request(tbl = tbl,
                                google_sheet = google_sheet,
                                sheet = "Sheet1")

  # For real (non-dry-run) requests, authenticate first and pass a token:
  # googlesheets4::gs4_auth()
  # token <- googlesheets4::gs4_token()
  # req <- as_googlesheet_request(tbl = tbl,
  #                               google_sheet = google_sheet,
  #                               sheet = "Sheet1",
  #                               dry_run = FALSE,
  #                               token = token)

  # Make the actual request:
  # googlesheets4::request_make(req)
}
} # }
```
