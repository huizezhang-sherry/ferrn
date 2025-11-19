# Better label formatting to avoid overlapping

Better label formatting to avoid overlapping

## Usage

``` r
format_label(labels, accuracy)
```

## Arguments

- labels:

  a numerical vector of labels

- accuracy:

  the accuracy of the label

## Value

a vector of adjusted labels

## Examples

``` r
format_label(c(0.87, 0.87, 0.9, 0.93, 0.95), 0.01)
#> [1] 0.87 0.90 0.93 0.95
format_label(c(0.87, 0.87, 0.9, 0.93, 0.95, 0.96, 0.96), 0.01)
#> [1] 0.87 0.90 0.93 0.95 0.96
```
