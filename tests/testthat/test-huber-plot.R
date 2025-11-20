library(ggplot2)
library(tourr)
library(ash)
data(randu)
randu_std <- as.data.frame(apply(randu, 2, function(x) (x-mean(x))/sd(x)))
randu_std$yz <- sqrt(35)/6*randu_std$y - randu_std$z/6
randu_df <- randu_std[c(1,4)]


test_that("basic huber plot", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  p1 <- ggplot()  +
    geom_huber(data = randu_df, aes(x = x, y = yz),
               index.fun = norm_bin(nr = nrow(randu_df))) +
    coord_fixed() +
    theme_huber()


  vdiffr::expect_doppelganger("huber-plot-basic", p1)
})

test_that("geom_huber parameter sweep snapshot", {
  skip_if_not_installed("vdiffr")

  base <- ggplot(randu_df, aes(x = x, y = yz)) +
    coord_fixed() +
    theme_huber()

  p <- base +
    geom_huber(
      index.fun = norm_bin(nr = nrow(randu_df)),
      ref.circle.color     = "red",
      ref.circle.linetype  = "dotted",
      ref.circle.linewidth = 1.5,

      idx.max.color     = "blue",
      idx.max.linetype  = "dotdash",
      idx.max.linewidth = 2,

      idx.profile.color     = "purple",
      idx.profile.linetype  = "solid",
      idx.profile.linewidth = 1.5,

      proj.points.color  = "green",
      proj.points.shape  = 21,
      proj.points.size   = 4,
      proj.points.alpha  = 0.6,
      proj.points.stroke = 1
    )

  vdiffr::expect_doppelganger("geom_huber-all-parameters", p)
})

test_that("huber theme variations", {
  randu_huber_best <- prep_huber_best_proj(
    randu_df, index_fun = norm_bin(nr = nrow(randu_df))
  )
  p1 <- randu_huber_best |>
    ggplot() +
    geom_histogram(aes(x = x), breaks = seq(-2.2, 2.4, 0.12)) +
    xlab("") + ylab("") +
    theme_bw() +
    theme(axis.text.y = element_blank())

  vdiffr::expect_doppelganger("huber-histogram", p1)
})
