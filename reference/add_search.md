# A ggproto for drawing search points

This is a wrapper function used by
[`explore_space_pca()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_pca.md)
and should be be called directly by the user

## Usage

``` r
add_search(dt, search_size = 0.5, search_alpha = 0.5, search_color = NULL, ...)
```

## Arguments

- dt:

  A data object from the running the optimisation algorithm in guided
  tour

- search_size:

  numeric; the size of the search points

- search_alpha:

  numeric; the alpha of the anchor points

- search_color:

  the variable to be coloured by

- ...:

  other aesthetics inherent from
  [`explore_space_pca()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_pca.md)

## Value

a wrapper for drawing search points in
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
[`add_space()`](https://huizezhang-sherry.github.io/ferrn/reference/add_space.md),
[`add_start()`](https://huizezhang-sherry.github.io/ferrn/reference/add_start.md),
[`add_theo()`](https://huizezhang-sherry.github.io/ferrn/reference/add_theo.md)
