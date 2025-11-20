# Create Huber plots with ggplot2

The Huber plot presents the projection pursuit index values of 2D data
in each 1D projection in polar coordinates, corresponding to each
projection direction. It offers a simpler illustration of more complex
projection from high-dimensional data to lower dimensions in projection
pursuit. The function `prep_huber()` calculates each component required
for the Huber plot (see details), which can then be supplied to various
geom layers in ggplot2.

## Usage

``` r
StatHuber

stat_huber(
  mapping = NULL,
  data = NULL,
  geom = "path",
  position = "identity",
  ...,
  index.fun,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_huber(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  index.fun,
  ref.circle.color = NULL,
  ref.circle.colour = NULL,
  ref.circle.linetype = "dashed",
  ref.circle.linewidth = NULL,
  idx.max.color = NULL,
  idx.max.colour = NULL,
  idx.max.linetype = "dashed",
  idx.max.linewidth = NULL,
  idx.profile.color = NULL,
  idx.profile.colour = NULL,
  idx.profile.linetype = "solid",
  idx.profile.linewidth = NULL,
  proj.points.color = NULL,
  proj.points.colour = NULL,
  proj.points.stroke = NULL,
  proj.points.alpha = NULL,
  proj.points.size = NULL,
  proj.points.shape = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)

GeomHuber

prep_huber_best_proj(data, index_fun)

theme_huber(...)
```

## Format

An object of class `StatHuber` (inherits from `Stat`, `ggproto`, `gg`)
of length 3.

An object of class `GeomHuber` (inherits from `Geom`, `ggproto`, `gg`)
of length 4.

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- geom:

  The geometric object to use to display the data for this layer. When
  using a `stat_*()` function to construct a layer, the `geom` argument
  can be used to override the default coupling between stats and geoms.
  The `geom` argument accepts the following:

  - A `Geom` ggproto subclass, for example `GeomPoint`.

  - A string naming the geom. To give the geom as a string, strip the
    function name of the `geom_` prefix. For example, to use
    [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html),
    give the geom as `"point"`.

  - For more information and other ways to specify the geom, see the
    [layer
    geom](https://ggplot2.tidyverse.org/reference/layer_geoms.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- index.fun, index_fun:

  the projection pursuit index function, see examples

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display. To include legend
  keys for all levels, even when no data exists, use `TRUE`. If `NA`,
  all levels are shown in legend, but unobserved levels are omitted.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used to override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
    give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

- ref.circle.color, ref.circle.colour, ref.circle.linetype,
  ref.circle.linewidth:

  Default aesthetics for the reference circle

- idx.max.color, idx.max.colour, idx.max.linetype, idx.max.linewidth:

  Default aesthetics for the line indicating the best projection
  direction

- idx.profile.color, idx.profile.colour, idx.profile.linetype,
  idx.profile.linewidth:

  Default aesthetics for the index profile line

- proj.points.color, proj.points.colour, proj.points.stroke,
  proj.points.alpha, proj.points.size, proj.points.shape:

  Default aesthetics for the projected data points

## Details

the `prep_huber()` function calculates components required for making
the Huber plots. It returns a list including three elements:

- the `idx_df` data frame: :

  the x/y coordinates of the index value, in polar coordinates. Used for
  plotting the index value at each projection direction, with the
  reference circle.

- the `proj_df` data frame: :

  the best 1D projection. Used for plotting the 1D projection in
  histogram.

- the `slope` value: :

  the slope to plot in the Huber plot to indicate the direction of the
  best 1D projection.

## Examples

``` r
library(ggplot2)
library(tourr)
library(ash)
data(randu)
# simplify the randu data into 2D for illustration
randu_std <- as.data.frame(apply(randu, 2, function(x) (x-mean(x))/sd(x)))
randu_std$yz <- sqrt(35)/6*randu_std$y-randu_std$z/6
randu_df <- randu_std[c(1,4)]

# main example: Huber plot with geom_huber()
randu_df |>
  ggplot()  +
  geom_huber(aes(x = x, y = yz), index.fun = norm_bin(nr = nrow(randu_df))) +
  coord_fixed() +
  theme_huber()


# compute the best projection data for histogram
randu_huber_best <- prep_huber_best_proj(
  randu_df, index_fun = norm_bin(nr = nrow(randu_df))
  )
randu_huber_best |>
  ggplot() +
  geom_histogram(aes(x = x), breaks = seq(-2.2, 2.4, 0.12)) +
  xlab("") + ylab("") +
  theme_bw() +
  theme(axis.text.y = element_blank())
```
