# Clean method names

Clean method names

## Usage

``` r
clean_method(dt)
```

## Arguments

- dt:

  a data object

## Value

a tibble with method cleaned

## Examples

``` r
head(clean_method(holes_1d_better), 5)
#> # A tibble: 5 × 8
#>   basis         index_val info          method alpha tries  loop    id
#>   <list>            <dbl> <chr>         <chr>  <dbl> <dbl> <dbl> <int>
#> 1 <dbl [5 × 1]>     0.749 new_basis     CRS      0.5     1     1     1
#> 2 <dbl [5 × 1]>     0.730 random_search CRS      0.5     2     1     2
#> 3 <dbl [5 × 1]>     0.743 random_search CRS      0.5     2     2     3
#> 4 <dbl [5 × 1]>     0.736 random_search CRS      0.5     2     3     4
#> 5 <dbl [5 × 1]>     0.747 random_search CRS      0.5     2     4     5
```
