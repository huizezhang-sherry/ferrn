# Bind random bases in the projection bases space as a matrix

Bind random bases in the projection bases space as a matrix

## Usage

``` r
bind_random_matrix(basis, n = 500, d = 1, front = FALSE, seed = 1)
```

## Arguments

- basis:

  a matrix returned by
  [`get_basis_matrix()`](https://huizezhang-sherry.github.io/ferrn/reference/get.md)

- n:

  numeric; the number of random bases to generate in each dimension by
  geozoo

- d:

  numeric; dimension of the basis, d = 1, 2, ...

- front:

  logical; if the random bases should be bound before or after the
  original bases

- seed:

  numeric; a seed for generating reproducible random bases from geozoo

## Value

matrix

a matrix containing both the searched and random bases

## See also

Other bind:
[`bind_random()`](https://huizezhang-sherry.github.io/ferrn/reference/bind_random.md),
[`bind_theoretical()`](https://huizezhang-sherry.github.io/ferrn/reference/bind_theoretical.md)

## Examples

``` r
data <- get_basis_matrix(holes_1d_geo)
bind_random_matrix(data) %>% tail(5)
#>                 V1          V2          V3          V4         V5
#> [912,]  0.23964796  0.05845296 -0.73710134  0.03404807  0.6282312
#> [913,]  0.50671255 -0.19049853  0.55844763  0.55229465  0.3000992
#> [914,]  0.57680194  0.35147626  0.18255033 -0.67351716  0.2383568
#> [915,]  0.04265493 -0.46800503 -0.08392637 -0.55669631  0.6798510
#> [916,] -0.07483644 -0.58095640  0.06165148 -0.56242581 -0.5803150
```
