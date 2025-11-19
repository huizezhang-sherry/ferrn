# A ggproto for drawing finish points

This is a wrapper function used by
[`explore_space_pca()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_pca.md)
and should be be called directly by the user

## Usage

``` r
add_interp_last(
  dt,
  interp_last_size = 3,
  interp_last_alpha = 1,
  interp_last_color = NULL,
  ...
)
```

## Arguments

- dt:

  A data object from the running the optimisation algorithm in guided
  tour

- interp_last_size:

  numeric; the size of the last interpolation points in each iteration

- interp_last_alpha:

  numeric; the alpha of the last interpolation points in each iteration

- interp_last_color:

  the variable to be coloured by

- ...:

  other aesthetics inherent from
  [`explore_space_pca()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_pca.md)

## Value

a wrapper for drawing the last interpolation points of each iteration in
[`explore_space_pca()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_pca.md)

## See also

Other draw functions:
[`add_anchor()`](https://huizezhang-sherry.github.io/ferrn/reference/add_anchor.md),
[`add_anno()`](https://huizezhang-sherry.github.io/ferrn/reference/add_anno.md),
[`add_dir_search()`](https://huizezhang-sherry.github.io/ferrn/reference/add_dir_search.md),
[`add_end()`](https://huizezhang-sherry.github.io/ferrn/reference/add_end.md),
[`add_interp()`](https://huizezhang-sherry.github.io/ferrn/reference/add_interp.md),
[`add_interrupt()`](https://huizezhang-sherry.github.io/ferrn/reference/add_interrupt.md),
[`add_search()`](https://huizezhang-sherry.github.io/ferrn/reference/add_search.md),
[`add_space()`](https://huizezhang-sherry.github.io/ferrn/reference/add_space.md),
[`add_start()`](https://huizezhang-sherry.github.io/ferrn/reference/add_start.md),
[`add_theo()`](https://huizezhang-sherry.github.io/ferrn/reference/add_theo.md)
