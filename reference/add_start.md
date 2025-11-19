# A ggproto for drawing start points

This is a wrapper function used by
[`explore_space_pca()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_pca.md)
and should be be called directly by the user

## Usage

``` r
add_start(dt, start_size = 5, start_alpha = 1, start_color = NULL, ...)
```

## Arguments

- dt:

  A data object from the running the optimisation algorithm in guided
  tour

- start_size:

  numeric; the size of start point

- start_alpha:

  numeric; the alpha of start point

- start_color:

  the variable to be coloured by

- ...:

  other aesthetics inherent from
  [`explore_space_pca()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_pca.md)

## Value

a wrapper for drawing start points in
[`explore_space_pca()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_pca.md)

## See also

Other draw functions:
[`add_anchor()`](https://huizezhang-sherry.github.io/ferrn/reference/add_anchor.md),
[`add_anno()`](https://huizezhang-sherry.github.io/ferrn/reference/add_anno.md),
[`add_dir_search()`](https://huizezhang-sherry.github.io/ferrn/reference/add_dir_search.md),
[`add_end()`](https://huizezhang-sherry.github.io/ferrn/reference/add_end.md),
[`add_interp()`](https://huizezhang-sherry.github.io/ferrn/reference/add_interp.md),
[`add_interp_last()`](https://huizezhang-sherry.github.io/ferrn/reference/add_interp_last.md),
[`add_interrupt()`](https://huizezhang-sherry.github.io/ferrn/reference/add_interrupt.md),
[`add_search()`](https://huizezhang-sherry.github.io/ferrn/reference/add_search.md),
[`add_space()`](https://huizezhang-sherry.github.io/ferrn/reference/add_space.md),
[`add_theo()`](https://huizezhang-sherry.github.io/ferrn/reference/add_theo.md)

## Examples

``` r
library(ggplot2)
# construct the space and start df for plotting
space <- tibble::tibble(x0 = 0, y0 = 0, r = 5)
holes_1d_geo %>%
  compute_pca() %>%
  purrr::pluck("aug") %>%
  clean_method() %>%
  get_start()
#> # A tibble: 1 × 14
#>   basis    index_val info    method alpha tries  loop    id row_num   PC1    PC2
#>   <list>       <dbl> <chr>   <chr>  <dbl> <dbl> <dbl> <dbl>   <int> <dbl>  <dbl>
#> 1 <dbl[…]>     0.749 new_ba… PD       0.5     1     1     1       1 -1.87 -0.505
#> # ℹ 3 more variables: PC3 <dbl>, PC4 <dbl>, PC5 <dbl>
```
