#' code to prepare `holes` dataset goes her
set.seed(123456);
holes_1d_geo <- animate_dist(boa5, tour_path = guided_tour(
  holes(), d = 1, search_f =  search_geodesic), rescale = FALSE)

set.seed(123456)
holes_1d_better <-animate_dist(boa5, tour_path = guided_tour(
  holes(), d = 1, search_f =  search_better), rescale = FALSE)

set.seed(123456);
holes_1d_jellyfish <- animate_dist(boa5, tour_path = guided_tour(
    holes(), d = 1, search_f =  search_jellyfish, n_jellies = 50,
     max.tries = 50), rescale = FALSE)

set.seed(123456)
holes_2d_better <- animate_xy(boa6, tour_path = guided_tour(
  holes(), d = 2, search_f =  search_better), rescale = FALSE)

set.seed(123456)
holes_2d_better_max_tries <- animate_xy(boa6, tour_path = guided_tour(
  holes(), d = 2, search_f = search_better, max.tries = 500), rescale = FALSE)

library(tidyverse)
set.seed(1234)
x1 <- rnorm(1000, 0, 1)
x2 <- sample(c(rnorm(500, -3, 1), rnorm(500, 3, 1)), size = 1000)
x3 <- sample(c(rep(-1, 500), rep(1, 500)), size = 1000)
x4 <- sample(c(rnorm(250, -3, 1), rnorm(750, 3, 1)), size = 1000)
x5 <- sample(c(rnorm(330, -5, 1), rnorm(340, 0, 1), rnorm(330, 5, 1)), size = 1000)
x6 <- sample(c(rnorm(450, -5, 1), rnorm(100, 0, 1), rnorm(450, 5, 1)), size = 1000)
x7 <- sample(c(rnorm(500, -5, 1), rnorm(500, 5, 1)), size = 1000)
x8 <- rnorm(1000, 0, 1)
x9 <- rnorm(1000, 0, 1)
x10 <- rnorm(1000, 0, 1)
boa <- tibble(x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5,
              x6 = x6, x7 = x7, x8 = x8, x9 = x9, x10 = x10) %>%
boa <- as_tibble(scale(boa))
boa5 <- select(boa, x1, x2, x8: x10)
boa6 <- select(boa, x1, x2, x7: x10)

usethis::use_data(holes_1d_geo, overwrite = TRUE)
usethis::use_data(holes_1d_better, overwrite = TRUE)
usethis::use_data(holes_1d_jellyfish, overwrite = TRUE)
usethis::use_data(holes_2d_better, overwrite = TRUE)
usethis::use_data(holes_2d_max_tries, overwrite = TRUE)
usethis::use_data(boa, overwrite = TRUE)
usethis::use_data(boa5, overwrite = TRUE)
usethis::use_data(boa6, overwrite = TRUE)

