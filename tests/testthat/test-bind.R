library(dplyr)
best <- matrix(c(0, 1, 0, 0, 0), nrow = 5)
theo <- tail(holes_1d_better %>% bind_theoretical(best, tourr::holes(), raw_data = boa5), 1)
test_that("bind_theoretical_single", {
  expect_type(theo %>% pull(basis), "list")
  expect_true(theo %>% pull(basis) %>% .[[1]] %>% is.matrix())
  expect_equal(theo %>% pull(basis) %>% .[[1]] %>% nrow(), 5)
  expect_equal(theo %>% pull(basis) %>% .[[1]] %>% ncol(), 1)
  expect_true(theo %>% pull(index_val) %>% is.numeric())
  expect_match(theo %>% pull(info), "theoretical")
})


best_pos <- matrix(c(0, 1, 0, 0, 0), nrow = 5)
best_neg <- matrix(c(0, -1, 0, 0, 0), nrow = 5)
theo <- tail(holes_1d_better %>%
  bind_theoretical(best_pos, tourr::holes(), raw_data = boa5) %>%
  bind_theoretical(best_neg, tourr::holes(), raw_data = boa5), 2)
row_dim <- theo %>%
  pull(basis) %>%
  vapply(nrow, numeric(1))
col_dim <- theo %>%
  pull(basis) %>%
  vapply(ncol, numeric(1))

test_that("bind_theoretical_mult", {
  # basis
  expect_output(theo %>% pull(basis) %>% str(), "List of 2")
  expect_true(theo %>% pull(basis) %>% vapply(is.matrix, logical(1)) %>% unique())
  expect_equal(row_dim %>% unique(), 5)
  expect_equal(col_dim %>% unique(), 1)

  # other columns
  expect_true(theo %>% pull(index_val) %>% is.numeric())
  expect_match(theo %>% pull(info), "theoretical")
})

data <- get_basis_matrix(holes_1d_geo)


test_that("bind_random_matrix", {
  expect_true(bind_random_matrix(data) %>% is.matrix())
  expect_equal(bind_random_matrix(data) %>% nrow(), nrow(data) + ncol(data) * 500)
})
