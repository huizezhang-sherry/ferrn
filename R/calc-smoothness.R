#' Function to calculate smoothness and squintability
#'
#' @param idx character, the name of projection pursuit index function, e.g.
#' "holes" (see the \pkg{tourr} package for index examples)
#' @param n_basis numeric, the number of random basis to generate for calculating smoothness
#' @inheritParams tourr::basis_random
#' @param best a matrix, the theoretical best projection matrix, used to calculate
#' projection distance with the simulated random bases.
#' @param data matrix, the data to be projected
#' @param other_gp_params list, other parameters to be passed to \code{GpGp::fit_model}
#' @param verbose logical, whether to print optimisation progression when
#' fitting the Gaussian process
 #' @inheritParams GpGp::fit_model
#' @inheritParams base::format
#' @inheritParams base::print
#' @examples
#' library(tourr)
#' calc_smoothness("holes", data = pipe1000)
#'
#' @rdname optim
#' @export
calc_smoothness <- function(idx, data = sine1000, n_basis = 300, n = 6, d = 2,
                            best = matrix(c(0, 0, 0, 0, 1, 0,
                                            0, 0, 0, 0, 0, 1), nrow = 6),
                            start_parms = c(0.001, 0.5, 2, 2),
                            other_gp_params = NULL, verbose = FALSE){

  # sample basis
  cli::cli_inform("sample random bases ...")
  idx <- dplyr::sym(idx)
  set.seed(123)
  seed <- sample(1: 10000, size = n_basis)
  basis_df <- tibble::tibble(basis = lapply(1:n_basis, function(i){
    set.seed(seed[i]); tourr::basis_random(n = n, d = d)})) |>
    dplyr::rowwise() |>
    dplyr::mutate(proj_dist = tourr::proj_dist(best, basis),
                  index_val = get(idx)()(as.matrix(data) %*% basis))

  # construct gp
  cli::cli_inform("fit the gaussian process ...")
  if (verbose) {silent <- FALSE} else {silent <- TRUE}
  gp_params <- list(y = basis_df$index_val, locs = basis_df$proj_dist,
                    X = as.matrix(rep(1,nrow(basis_df))),
                    start_parms = start_parms, covfun_name = "matern_isotropic",
                    silent = silent,
                    other_gp_params
                    )
  fit <- do.call("fit_model", gp_params)

  cov_params <- suppressMessages(tibble::as_tibble_row(fit$covparms, .name_repair = "unique"))
  colnames(cov_params) <- c("variance", "range", "smoothness", "nugget", "convergence")
  cov_params <-  cov_params |> dplyr::mutate(convergence = fit$conv, idx = as.character(idx))

  # return
  res <- tibble::as_tibble(cov_params)
  attr(res, "basis_df") <- basis_df |> dplyr::ungroup()
  attr(res, "gp_res") <- fit
  attr(res, "data") <- tibble::as_tibble(data)
  attr(res, "best_basis") <- best

  class(res) <- c("smoothness_res", class(res))
  return(res)
}


globalVariables(c("basis", "sine1000"))


#' @rdname optim
#' @export
print.smoothness_res <- function(x, width = NULL, ...){
  writeLines(format(x, width = width, ...))
}

#' @importFrom tibble tbl_sum
#' @rdname optim
#' @export
tbl_sum.smoothness_res <- function(x){

  cli::cli_rule()
  dim <- attr(x, "basis_df")$basis[[1]] |> dim()
  line <- glue::glue("No. of basis = ", nrow(attr(x, "basis_df")),
                     ", bases [", dim[1], " x ", dim[2], "]")
  c("Smoothness" = line)
}
