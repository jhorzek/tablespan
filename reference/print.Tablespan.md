# print.Tablespan

print.Tablespan

## Usage

``` r
# S3 method for class 'Tablespan'
print(x, digits = 2, n = 3, use_hux = require_huxtable(throw = FALSE), ...)
```

## Arguments

- x:

  result from tablespan

- digits:

  number of digits to round doubles to

- n:

  number of rows to print

- use_hux:

  if set to TRUE and huxtable is installed, huxtable will be used to
  print the tablespan table. This allows for styling to be printed

- ...:

  additional arguments passed to prmatrix or huxtable (if use_hux =
  TRUE)

## Value

nothing

## Examples

``` r
data("iris")
tbl <- tablespan(data = iris[iris$Species == "setosa", ],
          formula = Species ~ (Sepal = Sepal.Length + Sepal.Width) +
            (Petal = Petal.Length + Petal.Width))
#> Warning: Tablespan uses tibble internally. Translating data to tibble
print(tbl)
#>      ┌─────────┬────────────────────────────┬────────────────────────────┐
#>      │         │ Sepal                      │ Petal                      │
#>      ├─────────┼──────────────┬─────────────┼──────────────┬─────────────┤
#>      │ Species │ Sepal.Length │ Sepal.Width │ Petal.Length │ Petal.Width │
#>      ├─────────┼──────────────┴─────────────┴──────────────┴─────────────┤
#>      │ setosa  │         5.10          3.50           1.40          0.20 │
#>      │ setosa  │         4.90          3.00           1.40          0.20 │
#>      │ setosa  │         4.70          3.20           1.30          0.20 │
#>      │ setosa  │         4.60          3.10           1.50          0.20 │
#>      │ setosa  │         5.00          3.60           1.40          0.20 │
#>      │ setosa  │         5.40          3.90           1.70          0.40 │
#>      │ setosa  │         4.60          3.40           1.40          0.30 │
#>      │ setosa  │         5.00          3.40           1.50          0.20 │
#>      │ setosa  │         4.40          2.90           1.40          0.20 │
#>      │ setosa  │         4.90          3.10           1.50          0.10 │
#>      │ setosa  │         5.40          3.70           1.50          0.20 │
#>      │ setosa  │         4.80          3.40           1.60          0.20 │
#>      │ setosa  │         4.80          3.00           1.40          0.10 │
#>      │ setosa  │         4.30          3.00           1.10          0.10 │
#>      │ setosa  │         5.80          4.00           1.20          0.20 │
#>      │ setosa  │         5.70          4.40           1.50          0.40 │
#>      │ setosa  │         5.40          3.90           1.30          0.40 │
#>      │ setosa  │         5.10          3.50           1.40          0.30 │
#>      │ setosa  │         5.70          3.80           1.70          0.30 │
#>      │ setosa  │         5.10          3.80           1.50          0.30 │
#>      │ setosa  │         5.40          3.40           1.70          0.20 │
#>      │ setosa  │         5.10          3.70           1.50          0.40 │
#>      │ setosa  │         4.60          3.60           1.00          0.20 │
#>      │ setosa  │         5.10          3.30           1.70          0.50 │
#>      │ setosa  │         4.80          3.40           1.90          0.20 │
#>      │ setosa  │         5.00          3.00           1.60          0.20 │
#>      │ setosa  │         5.00          3.40           1.60          0.40 │
#>      │ setosa  │         5.20          3.50           1.50          0.20 │
#>      │ setosa  │         5.20          3.40           1.40          0.20 │
#>      │ setosa  │         4.70          3.20           1.60          0.20 │
#>      │ setosa  │         4.80          3.10           1.60          0.20 │
#>      │ setosa  │         5.40          3.40           1.50          0.40 │
#>      │ setosa  │         5.20          4.10           1.50          0.10 │
#>      │ setosa  │         5.50          4.20           1.40          0.20 │
#>      │ setosa  │         4.90          3.10           1.50          0.20 │
#>      │ setosa  │         5.00          3.20           1.20          0.20 │
#>      │ setosa  │         5.50          3.50           1.30          0.20 │
#>      │ setosa  │         4.90          3.60           1.40          0.10 │
#>      │ setosa  │         4.40          3.00           1.30          0.20 │
#>      │ setosa  │         5.10          3.40           1.50          0.20 │
#>      │ setosa  │         5.00          3.50           1.30          0.30 │
#>      │ setosa  │         4.50          2.30           1.30          0.30 │
#>      │ setosa  │         4.40          3.20           1.30          0.20 │
#>      │ setosa  │         5.00          3.50           1.60          0.60 │
#>      │ setosa  │         5.10          3.80           1.90          0.40 │
#>      │ setosa  │         4.80          3.00           1.40          0.30 │
#>      │ setosa  │         5.10          3.80           1.60          0.20 │
#>      │ setosa  │         4.60          3.20           1.40          0.20 │
#>      │ setosa  │         5.30          3.70           1.50          0.20 │
#>      │ setosa  │         5.00          3.30           1.40          0.20 │
#>      └─────────┴─────────────────────────────────────────────────────────┘
#> 
#> Column names: Species, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
```
