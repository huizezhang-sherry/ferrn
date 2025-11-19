# Functions to get components from the data collecting object

Functions to get components from the data collecting object

## Usage

``` r
get_best(dt, group = NULL)

get_start(dt, group = NULL)

get_interp(dt, group = NULL)

get_interp_last(dt, group = NULL)

get_anchor(dt, group = NULL)

get_search(dt)

get_dir_search(dt, ratio = 5, ...)

get_space_param(dt, ...)

get_theo(dt)

get_interrupt(dt, group = NULL, precision = 0.001)

get_search_count(dt, iter = NULL, group = NULL)

get_basis_matrix(dt)
```

## Arguments

- dt:

  a data object collected by the projection pursuit guided tour
  optimisation in the `tourr` package

- group:

  the variable to label different runs of the optimiser(s)

- ratio:

  numeric; a buffer value to deviate directional search points from the
  anchor points

- ...:

  other arguments passed to
  [`compute_pca()`](https://huizezhang-sherry.github.io/ferrn/reference/pca-helper.md)

- precision:

  numeric; if the index value of the last interpolating point and the
  anchor point differ by `precision`, an interruption is registered

- iter:

  the variable to be counted by

## Value

a tibble object containing the best basis found by the optimiser(s)

## Details

`get_best`: extract the best basis found by the optimiser(s)

`get_start`: extract the start point of the optimisation

`get_interp`: extract the interpolation points

`get_interp_last`: extract the last point in each interpolation

`get_anchor`: extract the anchor points on the geodesic path

`get_search`: extract search points in the optimisation (for
`search_geodesic`)

`get_dir_search`: extract directional search points (for
`search_geodesic`)

`get_space_param`: estimate the radius of the background circle based on
the randomly generated points. The space of projected bases is a circle
when reduced to 2D. A radius is estimated using the largest distance
from the bases in the data object to the centre point.

`get_theo`: extract the theoretical basis, if exist

`get_interrupt`: extract the end point of the interpolation and the
target point in the iteration when an interruption happens. The
optimiser can find better basis on the interpolation path, an
interruption is implemented to stop further interpolation from the
highest point to the target point. This discrepancy is highlighted in
the PCA plot.

`get_search_count`: summarise the number of search points in each
iteration

`get_basis_matrix`: extract all the bases as a matrix

## Examples

``` r
get_search(holes_1d_geo)
#> # A tibble: 363 × 8
#>    basis         index_val info                  method  alpha tries  loop    id
#>    <list>            <dbl> <chr>                 <chr>   <dbl> <dbl> <dbl> <int>
#>  1 <dbl [5 × 1]>     0.749 direction_search      search…    NA     2     1     2
#>  2 <dbl [5 × 1]>     0.749 direction_search      search…    NA     2     1     3
#>  3 <dbl [5 × 1]>     0.749 direction_search      search…    NA     2     1     4
#>  4 <dbl [5 × 1]>     0.749 direction_search      search…    NA     2     1     5
#>  5 <dbl [5 × 1]>     0.749 direction_search      search…    NA     2     1     6
#>  6 <dbl [5 × 1]>     0.749 direction_search      search…    NA     2     1     7
#>  7 <dbl [5 × 1]>     0.749 direction_search      search…    NA     2     1     8
#>  8 <dbl [5 × 1]>     0.749 direction_search      search…    NA     2     1     9
#>  9 <dbl [5 × 1]>     0.749 best_direction_search search…    NA     2     1    10
#> 10 <dbl [5 × 1]>     0.749 direction_search      search…    NA     2     1    11
#> # ℹ 353 more rows

get_anchor(holes_1d_geo)
#> # A tibble: 34 × 8
#>    basis         index_val info             method       alpha tries  loop    id
#>    <list>            <dbl> <chr>            <chr>        <dbl> <dbl> <dbl> <int>
#>  1 <dbl [5 × 1]>     0.749 new_basis        search_geod…   0.5     1     1     1
#>  2 <dbl [5 × 1]>     0.753 best_line_search search_geod…  NA       2     1     2
#>  3 <dbl [5 × 1]>     0.793 best_line_search search_geod…  NA       3     1     3
#>  4 <dbl [5 × 1]>     0.805 best_line_search search_geod…  NA       4     1     4
#>  5 <dbl [5 × 1]>     0.836 best_line_search search_geod…  NA       5     1     5
#>  6 <dbl [5 × 1]>     0.890 best_line_search search_geod…  NA       6     1     6
#>  7 <dbl [5 × 1]>     0.917 best_line_search search_geod…  NA       7     1     7
#>  8 <dbl [5 × 1]>     0.929 best_line_search search_geod…  NA       8     1     8
#>  9 <dbl [5 × 1]>     0.929 best_line_search search_geod…  NA       9     1     9
#> 10 <dbl [5 × 1]>     0.933 best_line_search search_geod…  NA       9     2    10
#> # ℹ 24 more rows

get_start(holes_1d_better)
#> # A tibble: 1 × 8
#>   basis         index_val info      method        alpha tries  loop    id
#>   <list>            <dbl> <chr>     <chr>         <dbl> <dbl> <dbl> <int>
#> 1 <dbl [5 × 1]>     0.749 new_basis search_better   0.5     1     1     1

get_interrupt(holes_1d_better)
#> # A tibble: 6 × 8
#>   basis         index_val info          method         alpha tries  loop id   
#>   <list>            <dbl> <chr>         <chr>          <dbl> <dbl> <dbl> <chr>
#> 1 <dbl [5 × 1]>     0.752 new_basis     search_better  0.5       2     6 2    
#> 2 <dbl [5 × 1]>     0.813 new_basis     search_better  0.49      4     7 4    
#> 3 <dbl [5 × 1]>     0.904 new_basis     search_better  0.485     5     3 5    
#> 4 <dbl [5 × 1]>     0.753 interpolation search_better NA         2     9 2    
#> 5 <dbl [5 × 1]>     0.866 interpolation search_better NA         4     7 4    
#> 6 <dbl [5 × 1]>     0.914 interpolation search_better NA         5     6 5    

get_interp(holes_1d_better) %>% head()
#> # A tibble: 6 × 8
#>   basis         index_val info          method        alpha tries  loop    id
#>   <list>            <dbl> <chr>         <chr>         <dbl> <dbl> <dbl> <int>
#> 1 <dbl [5 × 1]>     0.749 interpolation search_better    NA     2     1     1
#> 2 <dbl [5 × 1]>     0.750 interpolation search_better    NA     2     2     2
#> 3 <dbl [5 × 1]>     0.751 interpolation search_better    NA     2     3     3
#> 4 <dbl [5 × 1]>     0.751 interpolation search_better    NA     2     4     4
#> 5 <dbl [5 × 1]>     0.752 interpolation search_better    NA     2     5     5
#> 6 <dbl [5 × 1]>     0.752 interpolation search_better    NA     2     6     6

get_basis_matrix(holes_1d_better) %>% head()
#>               V1         V2          V3        V4        V5
#> [1,]  0.34100587 -0.1129065 -0.14519958 0.0357833 0.9211969
#> [2,]  0.47021229  0.2323680  0.49786511 0.3133040 0.6155294
#> [3,] -0.07081708 -0.3776694 -0.10317006 0.3478981 0.8489250
#> [4,]  0.23230438 -0.2897560  0.18500055 0.5184984 0.7476699
#> [5,] -0.10216793  0.4893826 -0.08432954 0.1108271 0.8547937
#> [6,]  0.04188535 -0.1072834  0.37321788 0.7647209 0.5124902

get_best(dplyr::bind_rows(holes_1d_better, holes_1d_geo), group = method)
#> # A tibble: 2 × 8
#>   basis         index_val info          method          alpha tries  loop    id
#>   <list>            <dbl> <chr>         <chr>           <dbl> <dbl> <dbl> <int>
#> 1 <dbl [5 × 1]>     0.914 interpolation search_better      NA     5     6    55
#> 2 <dbl [5 × 1]>     0.933 interpolation search_geodesic    NA     9     3   152

get_search_count(holes_1d_better)
#> map tries to the x-axis
#> # A tibble: 6 × 2
#>   tries     n
#>   <dbl> <int>
#> 1     1     1
#> 2     2     6
#> 3     3     1
#> 4     4     7
#> 5     5     3
#> 6     6    24
get_search_count(dplyr::bind_rows(holes_1d_better, holes_1d_geo), group = method)
#> map tries to the x-axis
#> # A tibble: 16 × 3
#> # Groups:   tries [10]
#>    tries method              n
#>    <dbl> <chr>           <int>
#>  1     1 search_better       1
#>  2     2 search_better       6
#>  3     3 search_better       1
#>  4     4 search_better       7
#>  5     5 search_better       3
#>  6     6 search_better      24
#>  7     1 search_geodesic     1
#>  8     2 search_geodesic    11
#>  9     3 search_geodesic    11
#> 10     4 search_geodesic    11
#> 11     5 search_geodesic    11
#> 12     6 search_geodesic    11
#> 13     7 search_geodesic    11
#> 14     8 search_geodesic    11
#> 15     9 search_geodesic    22
#> 16    10 search_geodesic   264

get_interp_last(holes_1d_better)
#> # A tibble: 4 × 8
#>   basis         index_val info          method        alpha tries  loop    id
#>   <list>            <dbl> <chr>         <chr>         <dbl> <dbl> <dbl> <int>
#> 1 <dbl [5 × 1]>     0.753 interpolation search_better    NA     2     9     9
#> 2 <dbl [5 × 1]>     0.798 interpolation search_better    NA     3    15    24
#> 3 <dbl [5 × 1]>     0.866 interpolation search_better    NA     4     7    31
#> 4 <dbl [5 × 1]>     0.914 interpolation search_better    NA     5     6    37
get_interp_last(dplyr::bind_rows(holes_1d_better, holes_1d_geo), group = method)
#> # A tibble: 12 × 8
#>    basis         index_val info          method          alpha tries  loop    id
#>    <list>            <dbl> <chr>         <chr>           <dbl> <dbl> <dbl> <int>
#>  1 <dbl [5 × 1]>     0.753 interpolation search_better      NA     2     9     9
#>  2 <dbl [5 × 1]>     0.798 interpolation search_better      NA     3    15    24
#>  3 <dbl [5 × 1]>     0.866 interpolation search_better      NA     4     7    31
#>  4 <dbl [5 × 1]>     0.914 interpolation search_better      NA     5     6    37
#>  5 <dbl [5 × 1]>     0.753 interpolation search_geodesic    NA     2     7     7
#>  6 <dbl [5 × 1]>     0.793 interpolation search_geodesic    NA     3    11    18
#>  7 <dbl [5 × 1]>     0.805 interpolation search_geodesic    NA     4     7    25
#>  8 <dbl [5 × 1]>     0.836 interpolation search_geodesic    NA     5     8    33
#>  9 <dbl [5 × 1]>     0.890 interpolation search_geodesic    NA     6     7    40
#> 10 <dbl [5 × 1]>     0.917 interpolation search_geodesic    NA     7     5    45
#> 11 <dbl [5 × 1]>     0.929 interpolation search_geodesic    NA     8     4    49
#> 12 <dbl [5 × 1]>     0.933 interpolation search_geodesic    NA     9     3    52

res <- holes_1d_geo %>% compute_pca() %>% purrr::pluck("aug")
get_dir_search(res)
#> # A tibble: 330 × 16
#>    basis    index_val info   method alpha tries  loop    id row_num   PC1    PC2
#>    <list>       <dbl> <chr>  <chr>  <dbl> <dbl> <dbl> <dbl>   <int> <dbl>  <dbl>
#>  1 <dbl[…]>     0.749 direc… PD        NA     2     1     2       2 -1.93 -0.494
#>  2 <dbl[…]>     0.749 direc… PD        NA     2     1     3       3 -1.81 -0.515
#>  3 <dbl[…]>     0.749 direc… PD        NA     2     1     4       4 -1.78 -0.507
#>  4 <dbl[…]>     0.749 direc… PD        NA     2     1     5       5 -1.96 -0.501
#>  5 <dbl[…]>     0.749 direc… PD        NA     2     1     6       6 -1.75 -0.555
#>  6 <dbl[…]>     0.749 direc… PD        NA     2     1     7       7 -1.99 -0.454
#>  7 <dbl[…]>     0.749 direc… PD        NA     2     1     8       8 -1.87 -0.402
#>  8 <dbl[…]>     0.749 direc… PD        NA     2     1     9       9 -1.86 -0.606
#>  9 <dbl[…]>     0.749 best_… PD        NA     2     1    10      10 -1.98 -0.478
#> 10 <dbl[…]>     0.749 direc… PD        NA     2     1    11      11 -1.75 -0.530
#> # ℹ 320 more rows
#> # ℹ 5 more variables: PC3 <dbl>, PC4 <dbl>, PC5 <dbl>, anchor_x <dbl>,
#> #   anchor_y <dbl>

best <- matrix(c(0, 1, 0, 0, 0), nrow = 5)
holes_1d_better %>%
  bind_theoretical(best, tourr::holes(), raw_data = boa5) %>%
  get_theo()
#> # A tibble: 1 × 8
#>   basis         index_val info        method alpha tries  loop    id
#>   <list>            <dbl> <chr>       <chr>  <dbl> <dbl> <dbl> <dbl>
#> 1 <dbl [5 × 1]>     0.931 theoretical NA        NA    NA    NA     0
```
