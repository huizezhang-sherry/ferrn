pkgname <- "ferrn"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ferrn')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_space")
### * add_space

flush(stderr()); flush(stdout())

### Name: add_space
### Title: A ggproto for drawing circle
### Aliases: add_space

### ** Examples

library(ggplot2)
space <- tibble::tibble(x0 = 0, y0 = 0, r = 5)
ggplot() +
  add_space(space) +
  theme_void() +
  theme(aspect.ratio = 1)



cleanEx()
nameEx("add_start")
### * add_start

flush(stderr()); flush(stdout())

### Name: add_start
### Title: A ggproto for drawing start points
### Aliases: add_start

### ** Examples

library(ggplot2)
# construct the space and start df for plotting
space <- tibble::tibble(x0 = 0, y0 = 0, r = 5)
start <- holes_1d_geo %>%
  compute_pca() %>%
  purrr::pluck("aug") %>%
  clean_method() %>%
  get_start()
ggplot() +
  add_space(dt = space) +
  add_start(dt = start, start_color = info) +
  theme_void() +
  theme(aspect.ratio = 1)



cleanEx()
nameEx("bind_random")
### * bind_random

flush(stderr()); flush(stdout())

### Name: bind_random
### Title: Bind random bases in the projection bases space
### Aliases: bind_random

### ** Examples

bind_random(holes_1d_better) %>% tail(5)



cleanEx()
nameEx("bind_random_matrix")
### * bind_random_matrix

flush(stderr()); flush(stdout())

### Name: bind_random_matrix
### Title: Bind random bases in the projection bases space as a matrix
### Aliases: bind_random_matrix

### ** Examples

data <- get_basis_matrix(holes_1d_geo)
bind_random_matrix(data) %>% tail(5)



cleanEx()
nameEx("bind_theoretical")
### * bind_theoretical

flush(stderr()); flush(stdout())

### Name: bind_theoretical
### Title: Bind the theoretical best record
### Aliases: bind_theoretical

### ** Examples

best <- matrix(c(0, 1, 0, 0, 0), nrow = 5)
tail(holes_1d_better %>% bind_theoretical(best, tourr::holes(), raw_data = boa5), 1)



cleanEx()
nameEx("data")
### * data

flush(stderr()); flush(stdout())

### Name: holes_1d_geo
### Title: Simulated data
### Aliases: holes_1d_geo holes_1d_better holes_2d_better
###   holes_2d_better_max_tries boa boa5 boa6
### Keywords: datasets

### ** Examples

library(ggplot2)
library(tidyr)
library(dplyr)
boa %>%
  pivot_longer(cols = x1:x10, names_to = "var", values_to = "value") %>%
  mutate(var = forcats::fct_relevel(as.factor(var), paste0("x", 1:10))) %>%
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(vars(var))



cleanEx()
nameEx("explore_space_pca")
### * explore_space_pca

flush(stderr()); flush(stdout())

### Name: explore_space_pca
### Title: Plot the PCA projection of the projection bases space
### Aliases: explore_space_pca flip_sign compute_pca

### ** Examples

dplyr::bind_rows(holes_1d_geo, holes_1d_better) %>%
  bind_theoretical(matrix(c(0, 1, 0, 0, 0), nrow = 5),
    index = tourr::holes(), raw_data = boa5
  ) %>%
  explore_space_pca(group = method, details = TRUE) +
  scale_color_discrete_botanical()
dplyr::bind_rows(holes_1d_geo, holes_1d_better) %>%
  flip_sign(group = method) %>%
  str(max = 1)
dplyr::bind_rows(holes_1d_geo, holes_1d_better) %>% compute_pca(group = method)



cleanEx()
nameEx("explore_space_tour")
### * explore_space_tour

flush(stderr()); flush(stdout())

### Name: explore_space_tour
### Title: Plot the grand tour animation of the bases space in high
###   dimension
### Aliases: explore_space_tour prep_space_tour

### ** Examples

explore_space_tour(dplyr::bind_rows(holes_1d_better, holes_1d_geo),
  group = method, palette = botanical_palettes$fern[c(1, 6)]
)



cleanEx()
nameEx("explore_trace")
### * explore_trace

flush(stderr()); flush(stdout())

### Name: explore_trace_interp
### Title: Plot the trace the search progression
### Aliases: explore_trace_interp

### ** Examples

# Compare the trace of interpolated points in two algorithms
holes_1d_better %>%
  explore_trace_interp(interp_size = 2) +
  scale_color_continuous_botanical(palette = "fern")



cleanEx()
nameEx("explore_trace_search")
### * explore_trace_search

flush(stderr()); flush(stdout())

### Name: explore_trace_search
### Title: Plot the count in each iteration
### Aliases: explore_trace_search

### ** Examples

# Summary plots for search points in two algorithms
library(patchwork)
library(dplyr)
library(ggplot2)
p1 <- holes_1d_better %>% explore_trace_search() +
  scale_color_continuous_botanical(palette = "fern")
p2 <- holes_2d_better_max_tries %>% explore_trace_search() +
  scale_color_continuous_botanical(palette = "daisy")
p1 / p2



cleanEx()
nameEx("format_label")
### * format_label

flush(stderr()); flush(stdout())

### Name: format_label
### Title: Better label formatting to avoid overlapping
### Aliases: format_label

### ** Examples

format_label(c(0.87, 0.87, 0.9, 0.93, 0.95), 0.01)
format_label(c(0.87, 0.87, 0.9, 0.93, 0.95, 0.96, 0.96), 0.01)



cleanEx()
nameEx("get_anchor")
### * get_anchor

flush(stderr()); flush(stdout())

### Name: get_anchor
### Title: Extract the anchor points on the geodesic path
### Aliases: get_anchor

### ** Examples

holes_1d_better %>% get_anchor()
holes_1d_geo %>% get_anchor()



cleanEx()
nameEx("get_basis_matrix")
### * get_basis_matrix

flush(stderr()); flush(stdout())

### Name: get_basis_matrix
### Title: Extract all the bases as a matrix
### Aliases: get_basis_matrix

### ** Examples

head(get_basis_matrix(holes_1d_better), 5)



cleanEx()
nameEx("get_best")
### * get_best

flush(stderr()); flush(stdout())

### Name: get_best
### Title: Extract the record with the largest index value
### Aliases: get_best

### ** Examples

dplyr::bind_rows(holes_1d_better, holes_1d_geo) %>% get_best(group = method)



cleanEx()
nameEx("get_dir_search")
### * get_dir_search

flush(stderr()); flush(stdout())

### Name: get_dir_search
### Title: Extract directional search points during the optimisation
### Aliases: get_dir_search

### ** Examples

holes_1d_geo %>%
  compute_pca() %>%
  purrr::pluck("aug") %>%
  get_dir_search()



cleanEx()
nameEx("get_interp")
### * get_interp

flush(stderr()); flush(stdout())

### Name: get_interp
### Title: Extract interpolated records
### Aliases: get_interp

### ** Examples

holes_1d_better %>%
  get_interp() %>%
  head()
get_interp(dplyr::bind_rows(holes_1d_better, holes_1d_geo), group = method) %>% head()



cleanEx()
nameEx("get_interp_last")
### * get_interp_last

flush(stderr()); flush(stdout())

### Name: get_interp_last
### Title: Extract the end point at each interpolation
### Aliases: get_interp_last

### ** Examples

holes_1d_better %>% get_interp_last()
get_interp_last(dplyr::bind_rows(holes_1d_better, holes_1d_geo), group = method)



cleanEx()
nameEx("get_interrupt")
### * get_interrupt

flush(stderr()); flush(stdout())

### Name: get_interrupt
### Title: Extract the end point of the interpolation and the target point
###   in the iteration when an interruption happens
### Aliases: get_interrupt

### ** Examples

holes_1d_better %>% get_interrupt()
holes_1d_geo %>% get_interrupt()



cleanEx()
nameEx("get_search")
### * get_search

flush(stderr()); flush(stdout())

### Name: get_search
### Title: Extract search points during the optimisation
### Aliases: get_search

### ** Examples

holes_1d_better %>% get_search()
holes_1d_geo %>% get_search()



cleanEx()
nameEx("get_search_count")
### * get_search_count

flush(stderr()); flush(stdout())

### Name: get_search_count
### Title: Extract the count in each iteration
### Aliases: get_search_count

### ** Examples

get_search_count(holes_1d_better)
get_search_count(dplyr::bind_rows(holes_1d_better, holes_1d_geo), group = method)



cleanEx()
nameEx("get_start")
### * get_start

flush(stderr()); flush(stdout())

### Name: get_start
### Title: Extract the starting records
### Aliases: get_start

### ** Examples

holes_1d_better %>% get_start()



cleanEx()
nameEx("get_theo")
### * get_theo

flush(stderr()); flush(stdout())

### Name: get_theo
### Title: Extract the theoretical best basis, if applicable
### Aliases: get_theo

### ** Examples

best <- matrix(c(0, 1, 0, 0, 0), nrow = 5)
holes_1d_better %>%
  bind_theoretical(best, tourr::holes(), raw_data = boa5) %>%
  get_theo()



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
