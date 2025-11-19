# Plot the grand tour animation of the bases space in high dimension

Plot the grand tour animation of the bases space in high dimension

## Usage

``` r
explore_space_tour(..., axes = "bottomleft")

prep_space_tour(
  dt,
  group = NULL,
  flip = FALSE,
  n_random = 2000,
  color = NULL,
  rand_size = 1,
  rand_color = "#D3D3D3",
  point_size = 1.5,
  end_size = 5,
  theo_size = 3,
  theo_shape = 17,
  theo_color = "black",
  palette = botanical_palettes$fern,
  ...
)
```

## Arguments

- ...:

  other argument passed to
  [`tourr::animate_xy()`](https://ggobi.github.io/tourr/reference/display_xy.html)
  and `prep_space_tour()`

- axes:

  see \[tourr::animate_xy()\]

- dt:

  a data object collected by the projection pursuit guided tour
  optimisation in `tourr`

- group:

  the variable to label different runs of the optimiser(s)

- flip:

  logical; if the sign flipping need to be performed

- n_random:

  numeric; the number of random basis to generate

- color:

  the variable to be coloured by

- rand_size:

  numeric; the size of random points

- rand_color:

  character; the color hex code for random points

- point_size:

  numeric; the size of points searched by the optimiser(s)

- end_size:

  numeric; the size of end points

- theo_size:

  numeric; the size of theoretical point(s)

- theo_shape:

  numeric; the shape symbol in the basic plot

- theo_color:

  character; the color of theoretical point(s)

- palette:

  the colour palette to be used

## Value

- `explore_space_tour()`:

  an animation of the search path in the high-dimensional sphere

- `prep_space_tour()`:

  a list containing various components needed for producing the
  animation

## See also

Other main plot functions:
[`explore_space_start()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_pca.md),
[`explore_trace_interp()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_trace.md),
[`explore_trace_search()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_trace_search.md)

## Examples

``` r
if (FALSE){
explore_space_tour(dplyr::bind_rows(holes_1d_better, holes_1d_geo),
  group = method, palette = botanical_palettes$fern[c(1, 6)]
)
}
```
