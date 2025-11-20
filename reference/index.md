# Package index

## Main plotting functions

Main diagnostic plotting functions

- [`explore_trace_search()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_trace_search.md)
  : Plot the count in each iteration
- [`explore_trace_interp()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_trace.md)
  : Plot the trace the search progression
- [`explore_space_start()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_pca.md)
  [`explore_space_end()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_pca.md)
  [`explore_space_pca()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_pca.md)
  : Plot the PCA projection of the projection bases space
- [`explore_space_tour()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_tour.md)
  [`prep_space_tour()`](https://huizezhang-sherry.github.io/ferrn/reference/explore_space_tour.md)
  : Plot the grand tour animation of the bases space in high dimension
- [`plot_projection()`](https://huizezhang-sherry.github.io/ferrn/reference/projection.md)
  [`compute_projection()`](https://huizezhang-sherry.github.io/ferrn/reference/projection.md)
  : Plot the projection from the optimisation data collected from
  projection pursuit
- [`flip_sign()`](https://huizezhang-sherry.github.io/ferrn/reference/pca-helper.md)
  [`compute_pca()`](https://huizezhang-sherry.github.io/ferrn/reference/pca-helper.md)
  : Helper functions for \`explore_space_pca()\`
- [`StatHuber`](https://huizezhang-sherry.github.io/ferrn/reference/huber.md)
  [`stat_huber()`](https://huizezhang-sherry.github.io/ferrn/reference/huber.md)
  [`geom_huber()`](https://huizezhang-sherry.github.io/ferrn/reference/huber.md)
  [`GeomHuber`](https://huizezhang-sherry.github.io/ferrn/reference/huber.md)
  [`prep_huber()`](https://huizezhang-sherry.github.io/ferrn/reference/huber.md)
  [`theme_huber()`](https://huizezhang-sherry.github.io/ferrn/reference/huber.md)
  : Create Huber plots with ggplot2

## Calculate projection pursuit index metrics

- [`sample_bases()`](https://huizezhang-sherry.github.io/ferrn/reference/optim.md)
  [`print(`*`<basis_df>`*`)`](https://huizezhang-sherry.github.io/ferrn/reference/optim.md)
  [`tbl_sum(`*`<basis_df>`*`)`](https://huizezhang-sherry.github.io/ferrn/reference/optim.md)
  [`calc_smoothness()`](https://huizezhang-sherry.github.io/ferrn/reference/optim.md)
  [`print(`*`<smoothness_res>`*`)`](https://huizezhang-sherry.github.io/ferrn/reference/optim.md)
  [`tbl_sum(`*`<smoothness_res>`*`)`](https://huizezhang-sherry.github.io/ferrn/reference/optim.md)
  [`calc_squintability()`](https://huizezhang-sherry.github.io/ferrn/reference/optim.md)
  [`print(`*`<squintability_res>`*`)`](https://huizezhang-sherry.github.io/ferrn/reference/optim.md)
  [`tbl_sum(`*`<squintability_res>`*`)`](https://huizezhang-sherry.github.io/ferrn/reference/optim.md)
  [`fit_ks()`](https://huizezhang-sherry.github.io/ferrn/reference/optim.md)
  [`fit_nls()`](https://huizezhang-sherry.github.io/ferrn/reference/optim.md)
  : Function to calculate smoothness and squintability

## Get components

Extracting components from existing data object

- [`get_best()`](https://huizezhang-sherry.github.io/ferrn/reference/get.md)
  [`get_start()`](https://huizezhang-sherry.github.io/ferrn/reference/get.md)
  [`get_interp()`](https://huizezhang-sherry.github.io/ferrn/reference/get.md)
  [`get_interp_last()`](https://huizezhang-sherry.github.io/ferrn/reference/get.md)
  [`get_anchor()`](https://huizezhang-sherry.github.io/ferrn/reference/get.md)
  [`get_search()`](https://huizezhang-sherry.github.io/ferrn/reference/get.md)
  [`get_dir_search()`](https://huizezhang-sherry.github.io/ferrn/reference/get.md)
  [`get_space_param()`](https://huizezhang-sherry.github.io/ferrn/reference/get.md)
  [`get_theo()`](https://huizezhang-sherry.github.io/ferrn/reference/get.md)
  [`get_interrupt()`](https://huizezhang-sherry.github.io/ferrn/reference/get.md)
  [`get_search_count()`](https://huizezhang-sherry.github.io/ferrn/reference/get.md)
  [`get_basis_matrix()`](https://huizezhang-sherry.github.io/ferrn/reference/get.md)
  : Functions to get components from the data collecting object

## Bind additionals

Bind external data to the existing data object

- [`bind_random()`](https://huizezhang-sherry.github.io/ferrn/reference/bind_random.md)
  : Bind random bases in the projection bases space
- [`bind_random_matrix()`](https://huizezhang-sherry.github.io/ferrn/reference/bind_random_matrix.md)
  : Bind random bases in the projection bases space as a matrix
- [`bind_theoretical()`](https://huizezhang-sherry.github.io/ferrn/reference/bind_theoretical.md)
  : Bind the theoretical best record

## Create ggprotos

ggproto for drawing each component in PCA plot

- [`add_anchor()`](https://huizezhang-sherry.github.io/ferrn/reference/add_anchor.md)
  : A ggproto for drawing anchor points
- [`add_anno()`](https://huizezhang-sherry.github.io/ferrn/reference/add_anno.md)
  : A ggproto for annotating the symmetry of the starting points
- [`add_dir_search()`](https://huizezhang-sherry.github.io/ferrn/reference/add_dir_search.md)
  : A ggproto for drawing directional search points
- [`add_end()`](https://huizezhang-sherry.github.io/ferrn/reference/add_end.md)
  : A ggproto for drawing start points
- [`add_interp()`](https://huizezhang-sherry.github.io/ferrn/reference/add_interp.md)
  : A ggproto for drawing interpolation path
- [`add_interp_last()`](https://huizezhang-sherry.github.io/ferrn/reference/add_interp_last.md)
  : A ggproto for drawing finish points
- [`add_interrupt()`](https://huizezhang-sherry.github.io/ferrn/reference/add_interrupt.md)
  : A ggproto for annotating the interrupted path
- [`add_search()`](https://huizezhang-sherry.github.io/ferrn/reference/add_search.md)
  : A ggproto for drawing search points
- [`add_space()`](https://huizezhang-sherry.github.io/ferrn/reference/add_space.md)
  : A ggproto for drawing circle
- [`add_start()`](https://huizezhang-sherry.github.io/ferrn/reference/add_start.md)
  : A ggproto for drawing start points
- [`add_theo()`](https://huizezhang-sherry.github.io/ferrn/reference/add_theo.md)
  : A ggproto for drawing the theoretical basis, if applicable

## Color & theme

An australian botanical color palette

- [`botanical_palettes`](https://huizezhang-sherry.github.io/ferrn/reference/color.md)
  [`botanical_pal()`](https://huizezhang-sherry.github.io/ferrn/reference/color.md)
  : A customised colour palette based on Australian botanies
- [`scale_color_continuous_botanical()`](https://huizezhang-sherry.github.io/ferrn/reference/scale.md)
  [`scale_color_discrete_botanical()`](https://huizezhang-sherry.github.io/ferrn/reference/scale.md)
  [`scale_fill_continuous_botanical()`](https://huizezhang-sherry.github.io/ferrn/reference/scale.md)
  [`scale_fill_discrete_botanical()`](https://huizezhang-sherry.github.io/ferrn/reference/scale.md)
  : continuous scale colour function
- [`theme_fern()`](https://huizezhang-sherry.github.io/ferrn/reference/theme_fern.md)
  : A specific theme for trace plots

## Data

Simulated example data

- [`sine1000`](https://huizezhang-sherry.github.io/ferrn/reference/pipe-sine-boa.md)
  [`sine1000_4d`](https://huizezhang-sherry.github.io/ferrn/reference/pipe-sine-boa.md)
  [`sine1000_6d`](https://huizezhang-sherry.github.io/ferrn/reference/pipe-sine-boa.md)
  [`sine1000_8d`](https://huizezhang-sherry.github.io/ferrn/reference/pipe-sine-boa.md)
  [`pipe1000`](https://huizezhang-sherry.github.io/ferrn/reference/pipe-sine-boa.md)
  [`pipe1000_4d`](https://huizezhang-sherry.github.io/ferrn/reference/pipe-sine-boa.md)
  [`pipe1000_6d`](https://huizezhang-sherry.github.io/ferrn/reference/pipe-sine-boa.md)
  [`pipe1000_8d`](https://huizezhang-sherry.github.io/ferrn/reference/pipe-sine-boa.md)
  [`pipe1000_10d`](https://huizezhang-sherry.github.io/ferrn/reference/pipe-sine-boa.md)
  [`pipe1000_12d`](https://huizezhang-sherry.github.io/ferrn/reference/pipe-sine-boa.md)
  [`boa`](https://huizezhang-sherry.github.io/ferrn/reference/pipe-sine-boa.md)
  [`boa5`](https://huizezhang-sherry.github.io/ferrn/reference/pipe-sine-boa.md)
  [`boa6`](https://huizezhang-sherry.github.io/ferrn/reference/pipe-sine-boa.md)
  : Simulated sine, pipe, and gaussian mixture
- [`holes_1d_geo`](https://huizezhang-sherry.github.io/ferrn/reference/data.md)
  [`holes_1d_better`](https://huizezhang-sherry.github.io/ferrn/reference/data.md)
  [`holes_1d_jellyfish`](https://huizezhang-sherry.github.io/ferrn/reference/data.md)
  [`holes_2d_jellyfish`](https://huizezhang-sherry.github.io/ferrn/reference/data.md)
  [`holes_2d_better`](https://huizezhang-sherry.github.io/ferrn/reference/data.md)
  [`holes_2d_better_max_tries`](https://huizezhang-sherry.github.io/ferrn/reference/data.md)
  : Data objects collected during the projection pursuit optimisation

## Miscellaneous

Other misc functions

- [`clean_method()`](https://huizezhang-sherry.github.io/ferrn/reference/relevel.md)
  : Clean method names
- [`format_label()`](https://huizezhang-sherry.github.io/ferrn/reference/format_label.md)
  : Better label formatting to avoid overlapping
