library(dplyr)
test_that("get_best", {
  expect_equal(holes_1d_better %>% get_best() %>% nrow(), 1) # without group
  expect_equal(bind_rows(holes_1d_better, holes_1d_geo) %>% get_best(group = method) %>% nrow(), 2) # with group
})

test_that("get_start", {
  expect_equal(holes_1d_better %>% get_start() %>% nrow(), 1)
})

test_that("get_interp", {
  expect_match(get_interp(holes_1d_better) %>% pull(info) %>% unique(), "interpolation")
  expect_equal(bind_rows(holes_1d_better, holes_1d_geo) %>% get_interp(group = method) %>%
    group_by(method) %>% count() %>% nrow(), 2)
})

test_that("get_basis_matrix", {
  expect_true(holes_1d_better %>% get_basis_matrix() %>% is.matrix())
  expect_equal(
    holes_1d_better %>% get_basis_matrix() %>% ncol(),
    holes_1d_better %>% head(1) %>% pull(basis) %>% .[[1]] %>% nrow()
  )
})
