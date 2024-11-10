# 1. with and without id, cols,
# 2. add additional command
# 3. test the animation/ add the example of the animation


test_that("multiplication works", {
  skip_if_not_installed("vdiffr")
  library(ggplot2)

  p <- holes_2d_jellyfish |> get_best() |> plot_projection(data = boa6)
  p1 <- holes_2d_jellyfish |> get_best(tries) |> plot_projection(data = boa6, id = tries)
  p2 <- holes_2d_jellyfish |> get_best(tries) |> plot_projection(data = boa6, id = tries, label = FALSE)

  vdiffr::expect_doppelganger("basic", p)
  vdiffr::expect_doppelganger("change id", p1)
  vdiffr::expect_doppelganger("test label", p2)

})

