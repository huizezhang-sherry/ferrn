# A ggproto for annotating the interrupted path

This is a wrapper function used by
[`explore_space_pca()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_pca.md)
and should be be called directly by the user

## Usage

``` r
add_interrupt(
  dt,
  interrupt_size = 0.5,
  interrupt_alpha = NULL,
  interrupt_color = NULL,
  interrupt_group = NULL,
  interrupt_linetype = "dashed",
  ...
)
```

## Arguments

- dt:

  A data object from the running the optimisation algorithm in guided
  tour

- interrupt_size:

  numeric; the size of the interruption path

- interrupt_alpha:

  numeric; the alpha of the interruption path

- interrupt_color:

  the variable to be coloured by

- interrupt_group:

  the variable to label different interruption

- interrupt_linetype:

  character; the linetype to annotate the interruption

- ...:

  other aesthetics inherent from
  [`explore_space_pca()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_pca.md)

## Value

a wrapper for annotating the interruption in
[`explore_space_pca()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_pca.md)

## See also

Other draw functions:
[`add_anchor()`](https://huizezhang-sherry.github.io/ferrn/reference/add_anchor.md),
[`add_anno()`](https://huizezhang-sherry.github.io/ferrn/reference/add_anno.md),
[`add_dir_search()`](https://huizezhang-sherry.github.io/ferrn/reference/add_dir_search.md),
[`add_end()`](https://huizezhang-sherry.github.io/ferrn/reference/add_end.md),
[`add_interp()`](https://huizezhang-sherry.github.io/ferrn/reference/add_interp.md),
[`add_interp_last()`](https://huizezhang-sherry.github.io/ferrn/reference/add_interp_last.md),
[`add_search()`](https://huizezhang-sherry.github.io/ferrn/reference/add_search.md),
[`add_space()`](https://huizezhang-sherry.github.io/ferrn/reference/add_space.md),
[`add_start()`](https://huizezhang-sherry.github.io/ferrn/reference/add_start.md),
[`add_theo()`](https://huizezhang-sherry.github.io/ferrn/reference/add_theo.md)
