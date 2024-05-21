#' @param proj_dist_threshold only for squintability, the threshold for projection
#' distance for the random basis to be considered in sampling
#' @param method only for squintability, the method to calculate squintability,
#' either through kernel smoothing ("ks") or non-linear least square ("nls")
#' @param step only for squintability, the step size for interpolation,
#' recommend between 0.01 and 0.1
#' @param bin_nobs_threshold numeric, only for squintability, the threshold
#' number of observations for
#' applying binning before fitting the kernel
#' @param bin_size only for squintability, the bin size for binning the data
#' before fitting the kernel
#' @rdname optim
#' @export
calc_squintability <- function(idx, data = sine1000,
                               method = c("ks", "nls"), n_basis, n = 6, d = 2,
                               proj_dist_threshold = 1.5, step = 0.02,
                               best = matrix(c(0, 0, 0, 0, 1, 0,
                                               0, 0, 0, 0, 0, 1), nrow = 6),
                               bin_nobs_threshold = 5000, bin_size = 0.02
                               ){

  ## sample basis
  set.seed(123)
  seed <- sample(1: 10000, size = 1000)
  basis_lst <- list()
  i <- 1
  while (length(basis_lst) < n_basis){
    set.seed(seed[i])
    bb <- tourr::basis_random(n = n, d = d)
    if (tourr::proj_dist(best, bb) > proj_dist_threshold){
      bb_lst <- c(bb_lst, list(bb))
    }
    i <- i +1
  }

  ## interpolate between the best and the random basis
  ## TODO: progress bar here
  basis_df <- tibble::tibble(id = 1:n_basis) |>
    dplyr::mutate(res = lapply(bb_lst, function(bb){
      interp_bb_best(bb = bb, best = best, step = step)
      })) |>
    tidyr::unnest(res) |>
    dplyr::rowwise() |>
    dplyr::mutate(!!dplyr::sym(idx) := get(idx)()(data %*% basis)) |>
    dplyr::ungroup()

  res <- switch(method,
                ks = fit_ks(basis_df, idx = idx, bin_nobs_threshold, bin_size),
                nls = fit_nls(basis_df, idx = idx, bin_nobs_threshold, bin_size)
                )

  list(basis_df = basis_df, measure = res)

}

interp_bb_best <- function(bb, best, step = 0.02){

  if (!identical(dim(bb), dim(best))){
    cli::cli_abort("random basis and the best basis must have the same dimension")
  } else{
    n <- dim(bb)[1]
    d <- dim(bb)[2]
  }

  t <- array(unlist(list(best, bb)), dim = c(n = n, d = d, 2))
  class(t) <- c("history_array")
  tt <- tourr::interpolate(t, step)
  tt_mat <- lapply(1:length(tt), function(ll){
    as.vector(tt[,,ll])|> matrix(nrow = n, ncol = d)})
  dist <- as.vector(lapply(tt_mat,  function(mat){proj_dist(best, mat)}))
  tibble::tibble(basis = tt_mat, dist = dist)
}

fit_ks <- function(basis_df, idx, bin_nobs_threshold, bin_size){

  if (nrow(basis_df) > bin_nobs_threshold){
    cli::cli_abort("apply binning before fitting the kernel smoother with bin_size = {bin_size}")
    basis_df <- basis_df |>
      dplyr::mutate(dist_bin = ceiling(dist / bin_size) * bin_size)
  } else{
    basis_df <- basis_df |> dplyr::mutate(dist_bin = dist)
  }

  fit <- stats::ksmooth(basis_df$dist_bin, basis_df[[idx]])
  ks_dt <- tibble::tibble(x = fit$x, y = fit$y) |>
    dplyr::mutate(dy = c(NA, -diff(y) / diff(x)))
  largest <- ks_dt |> dplyr::filter(dy == max(dy, na.rm = TRUE))
  tibble::tibble(idx = idx, max_x = largest$x, max_dev = largest$dy,
         squintability = max_dev * max_x)

}

fit_nls <- function(basis_df, idx, bin_nobs_threshold, bin_size){

  if (nrow(basis_df) > bin_nobs_threshold){
    cli::cli_abort("apply binning before fitting the kernel smoother with bin_size = {bin_size}")
    basis_df <- basis_df |>
      dplyr::mutate(dist_bin = ceiling(dist / bin_size) * bin_size,
                    dist_bin = dist_bin / pi * 180)
  } else{
    basis_df <- basis_df |> dplyr::mutate(dist_bin = dist / pi * 180)
  }

  model = stats::nls(idx ~ theta1/(1 + exp(-theta2 + theta3 * dist_bin)),
              start = list(theta1 = 1, theta2 = 5, theta3 = 0.1))
  theta_params <- stats::coef(model)
  colnames(theta_params) <- paste0("theta", 1:length(theta_params))
  tibble::tibble(idx = idx) |> dplyr::bind_cols(theta_params)
}

globalVariables(c("dist", "dist_bin", "dist", "y", "x", "dy", "max_dev", "max_x"))
