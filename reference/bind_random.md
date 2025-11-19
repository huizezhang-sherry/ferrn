# Bind random bases in the projection bases space

Given the orthonormality constraint, the projection bases live in a high
dimensional hollow sphere. Generating random points on the sphere is
useful to perceive the data object in the high dimensional space.

## Usage

``` r
bind_random(dt, n = 500, seed = 1)
```

## Arguments

- dt:

  a data object collected by the projection pursuit guided tour
  optimisation in the `tourr` package

- n:

  numeric; the number of random bases to generate in each dimension by
  geozoo

- seed:

  numeric; a seed for generating reproducible random bases from geozoo

## Value

a tibble object containing both the searched and random bases

## See also

Other bind:
[`bind_random_matrix()`](https://huizezhang-sherry.github.io/ferrn/reference/bind_random_matrix.md),
[`bind_theoretical()`](https://huizezhang-sherry.github.io/ferrn/reference/bind_theoretical.md)

## Examples

``` r
bind_random(holes_1d_better) %>% tail(5)
#> # A tibble: 5 × 8
#>   basis         index_val info               method      alpha tries  loop    id
#>   <list>            <dbl> <chr>              <chr>       <dbl> <dbl> <dbl> <dbl>
#> 1 <dbl [5 × 1]>        NA randomly_generated randomly_g…    NA    NA    NA     0
#> 2 <dbl [5 × 1]>        NA randomly_generated randomly_g…    NA    NA    NA     0
#> 3 <dbl [5 × 1]>        NA randomly_generated randomly_g…    NA    NA    NA     0
#> 4 <dbl [5 × 1]>        NA randomly_generated randomly_g…    NA    NA    NA     0
#> 5 <dbl [5 × 1]>        NA randomly_generated randomly_g…    NA    NA    NA     0
```
