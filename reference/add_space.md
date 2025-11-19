# A ggproto for drawing circle

This is a wrapper function used by
[`explore_space_pca()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_pca.md)
and should be be called directly by the user

## Usage

``` r
add_space(
  dt,
  space_alpha = 0.5,
  space_fill = "grey92",
  space_color = "white",
  cent_size = 1,
  cent_alpha = 1,
  cent_color = "black",
  ...
)
```

## Arguments

- dt:

  A data object from the running the optimisation algorithm in guided
  tour

- space_alpha:

  numeric; the alpha of the basis space

- space_fill:

  character; the colour of the space filling

- space_color:

  character; the colour of the space brim

- cent_size:

  numeric; the size of the centre point

- cent_alpha:

  numeric; an alpha of the centre point

- cent_color:

  character; the colour of the centre point

- ...:

  other aesthetics inherent from
  [`explore_space_pca()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_pca.md)

## Value

a wrapper for drawing the space in
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
[`add_start()`](https://huizezhang-sherry.github.io/ferrn/reference/add_start.md),
[`add_theo()`](https://huizezhang-sherry.github.io/ferrn/reference/add_theo.md)

## Examples

``` r
library(ggplot2)
space <- tibble::tibble(x0 = 0, y0 = 0, r = 5)
ggplot() +
  add_space(space) +
  theme_void() +
  theme(aspect.ratio = 1)
```
