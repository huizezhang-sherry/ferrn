# Bind the theoretical best record

The theoretical best basis is usually known for a simulated problem.
Augment this information into the data object allows for evaluating the
performance of optimisation against the theory.

## Usage

``` r
bind_theoretical(dt, matrix, index, raw_data)
```

## Arguments

- dt:

  a data object collected by the projection pursuit guided tour
  optimisation in the `tourr` package

- matrix:

  a matrix of the theoretical basis

- index:

  the index function used to calculate the index value

- raw_data:

  a tibble of the original data used to calculate the index value

## Value

a tibble object containing both the searched and theoretical best bases

## See also

Other bind:
[`bind_random()`](https://huizezhang-sherry.github.io/ferrn/reference/bind_random.md),
[`bind_random_matrix()`](https://huizezhang-sherry.github.io/ferrn/reference/bind_random_matrix.md)

## Examples

``` r
best <- matrix(c(0, 1, 0, 0, 0), nrow = 5)
tail(holes_1d_better %>% bind_theoretical(best, tourr::holes(), raw_data = boa5), 1)
#> # A tibble: 1 × 8
#>   basis         index_val info        method alpha tries  loop    id
#>   <list>            <dbl> <chr>       <chr>  <dbl> <dbl> <dbl> <dbl>
#> 1 <dbl [5 × 1]>     0.931 theoretical NA        NA    NA    NA     0
```
