#' Function to calculate smoothness
#'
#' @param idx character, the name of projection pursuit index function, e.g.
#' "holes" (see the \pkg{tourr} package for index examples)
#' @param size numeric, the number of random basis to generate for calculating smoothness
#' @inheritParams tourr::basis_random
#' @param best a matrix, the theoretical best projection matrix, used to calculate
#' projection distance with the simulated random bases.
#' @param data matrix, the data to be projected
#' @inheritParams GpGp::fit_model
#' @param other_gp_params list, other parameters to be passed to \code{GpGp::fit_model}
calc_smoothness <- function(idx, data = sine1000, size = 300, n = 6, d = 2,
                            best = matrix(c(0, 0, 0, 0, 1, 0,
                                            0, 0, 0, 0, 0, 1), nrow = 6),
                            start_parms = c(0.001, 0.5, 2, 2),
                            other_gp_params = list(NULL)
                            ){

  idx <- dplyr::sym(idx)
  set.seed(123)
  seed <- sample(1: 10000, size = size)
  basis_df <- tibble::tibble(basis = lapply(1:size, function(i){
    set.seed(seed[i]); tourr::basis_random(n = n, d = d)}))
    dplyr::rowwise() |>
    dplyr::mutate(proj_dist = tourr::proj_dist(best, basis),
                  index_val = get(idx)()(as.matrix(data) %*% basis))

  gp_params <- list(
    y = basis_df$index_val, locs = basis_df$proj_dist,
    X = as.matrix(rep(1,nrow(basis_df))),
    start_parms = start_parms,
    covfun_name = "matern_isotropic",
    other_gp_params
  )

  fit <- do.call("GpGp::fit_model", gp_params)
  cov_params <- tibble::as_tibble_row(fit$covparms, .name_repair = "unique")
  colnames(cov_params) <- c("variance", "range", "smoothness", "nugget", "convergence")
  cov_params <-  cov_params |> dplyr::mutate(convergence = fit$conv, idx = as.character(idx))

  list(basis = basis_df, gp_res = fit, cov_params = cov_params)
}


globalVariables(c("basis"))
