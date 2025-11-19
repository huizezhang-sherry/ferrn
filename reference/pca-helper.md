# Helper functions for \`explore_space_pca()\`

Helper functions for \`explore_space_pca()\`

## Usage

``` r
flip_sign(dt, group = NULL, ...)

compute_pca(dt, group = NULL, random = TRUE, flip = TRUE, ...)
```

## Arguments

- dt:

  a data object collected by the projection pursuit guided tour
  optimisation in `tourr`

- group:

  the variable to label different runs of the optimiser(s)

- ...:

  other arguments received from
  [`explore_space_pca()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_pca.md)

- random:

  logical; if random bases from the basis space need to be added to the
  data

- flip:

  logical; if the sign flipping need to be performed

## Value

`flip_sign()`: a list containing a matrix of all the bases, a logical
value indicating whether a flip of sign is performed, and a data frame
of the original dataset.

`compute_pca()`: a list containing the PCA summary and a data frame with
PC coordinates augmented.

## Examples

``` r
dt <- dplyr::bind_rows(holes_1d_geo, holes_1d_better)
 flip_sign(dt, group = method) %>% str(max = 1)
#> signs in all the bases will be flipped in group search_geodesic 
#> List of 3
#>  $ basis: num [1:495, 1:5] -0.341 -0.338 -0.344 -0.335 -0.347 ...
#>   ..- attr(*, "dimnames")=List of 2
#>  $ flip : logi TRUE
#>  $ dt   : tibble [495 × 8] (S3: tbl_df/tbl/data.frame)
compute_pca(dt, group = method)
#> signs in all the bases will be flipped in group search_geodesic 
#> $pca_summary
#> Standard deviations (1, .., p=5):
#> [1] 1.1319039 1.0218899 0.9700389 0.9506578 0.9109385
#> 
#> Rotation (n x k) = (5 x 5):
#>           PC1         PC2         PC3        PC4        PC5
#> V1  0.4407384 -0.42455466 -0.62022104  0.1214241 -0.4754840
#> V2 -0.1380017  0.77121660 -0.43378011  0.4213502 -0.1431060
#> V3 -0.4991679 -0.26809392 -0.60955795 -0.1162005  0.5421195
#> V4 -0.4462420 -0.39048709  0.23458203  0.7490500 -0.1796755
#> V5  0.5817256 -0.02497549  0.02389681  0.4828475  0.6536503
#> 
#> $aug
#> # A tibble: 995 × 14
#>    basis    index_val info  method alpha tries  loop    id row_num   PC1     PC2
#>    <list>       <dbl> <chr> <chr>  <dbl> <dbl> <dbl> <dbl>   <int> <dbl>   <dbl>
#>  1 <dbl[…]>     0.749 new_… PD       0.5     1     1     1       1 -1.85 -0.0647
#>  2 <dbl[…]>     0.749 dire… PD      NA       2     1     2       2 -1.86 -0.0738
#>  3 <dbl[…]>     0.749 dire… PD      NA       2     1     3       3 -1.84 -0.0558
#>  4 <dbl[…]>     0.749 dire… PD      NA       2     1     4       4 -1.84 -0.0589
#>  5 <dbl[…]>     0.749 dire… PD      NA       2     1     5       5 -1.86 -0.0707
#>  6 <dbl[…]>     0.749 dire… PD      NA       2     1     6       6 -1.83 -0.0480
#>  7 <dbl[…]>     0.749 dire… PD      NA       2     1     7       7 -1.87 -0.0815
#>  8 <dbl[…]>     0.749 dire… PD      NA       2     1     8       8 -1.84 -0.0802
#>  9 <dbl[…]>     0.749 dire… PD      NA       2     1     9       9 -1.86 -0.0493
#> 10 <dbl[…]>     0.749 best… PD      NA       2     1    10      10 -1.87 -0.0776
#> # ℹ 985 more rows
#> # ℹ 3 more variables: PC3 <dbl>, PC4 <dbl>, PC5 <dbl>
#> 
```
