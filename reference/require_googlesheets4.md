# Check if googlesheets4 package is available

This function checks if the googlesheets4 package is available.

## Usage

``` r
require_googlesheets4(throw = TRUE)
```

## Arguments

- throw:

  logical. If TRUE (default), the function will throw an error if
  googlesheets4 is not available. If FALSE, it will return FALSE
  instead.

## Value

logical. Returns TRUE if googlesheets4 is available, FALSE if it's not
available and throw=FALSE.

## Examples

``` r
if (FALSE) { # \dontrun{
# Check if googlesheets4 is available, throw error if not
require_googlesheets4()

# Check if googlesheets4 is available, return FALSE if not
require_googlesheets4(throw = FALSE)
} # }
```
