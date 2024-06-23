#' Function to calculate smoothness and squintability
#'
#' @param idx character, the name of projection pursuit index function, e.g.
#' "holes"
#' @param data a matrix or data frame, the high dimensional data to be projected
#' @param n_basis numeric, the number of random bases to generate
#' @param best a matrix, the theoretical/ empirical best projection matrix to
#' calculate the projection distance from the simulated random bases.
#' @param parallel logic, whether to use parallel computing for calculating the index.
#' Recommend for the stringy index.
#' @param scale logic, whether to scale the index value to 0-1 in squintability
#' @param step_size numeric, step size for interpolating from each random basis to
#' the best basis, recommend 0.005
#' @param seed numeric, seed for sampling random bases
#' @param x objects with specialised printing methods
#' @param basis_df the basis data frame returned from \code{sample_bases}
#' @param start_params list, the starting parameters for the Gaussian process
#' for smoothness
#' @param other_gp_params list, additional parameters to be passed to
#' [GpGp::fit_model()]
#' for calculating smoothness
#' @param verbose logical, whether to print optimisation progression when
#' fitting the Gaussian process
#' @param min_proj_dist only for squintability, the threshold for projection
#' distance for the random basis to be considered in sampling
#' @param method either "ks" (kernel smoothing) or "nls" (non-linear least
#' square) for calculating squintability.
#' @param bin_width numeric, the bin width to average the index value
#' before fitting the kernel, recommend to set as the same as `step` parameter
#' @param other_params list additional parameters for fitting kernel smoothing
#' or non-linear least square, see [stats::ksmooth()] and
#' [stats::nls()] for details
#' @inheritParams base::print
#' @importFrom stats ksmooth
#' @examples
#' \dontrun{
#' library(GpGp)
#' library(fields)
#' library(tourr)
#' basis_smoothness <- sample_bases(idx = "holes")
#' calc_smoothness(basis_smoothness)
#' basis_squint <- sample_bases(idx = "holes", n_basis = 100, step_size = 0.01, min_proj_dist = 1.5)
#' calc_squintability(basis_squint, method = "ks", bin_width = 0.01)
#'}
#' @rdname optim
#' @export
sample_bases <- function(idx, data = sine1000, n_basis = 300, parallel = FALSE,
                         best = matrix(c(0, 0, 0, 0, 1, 0,
                                         0, 0, 0, 0, 0, 1), nrow = 6),
                         min_proj_dist = NA, step_size = NA, seed = 123){

  dim <- dim(best)
  n <- dim[1]
  d <- dim[2]
  idx <- dplyr::sym(idx)
  set.seed(seed)
  seed_vec <- sample(1: 10000, size = 1000)
  bb_lst <- list()
  i <- 1

  while (length(bb_lst) < n_basis){
    set.seed(seed_vec[i]); bb <- tourr::basis_random(n = n, d = d)
    if (!is.na(min_proj_dist)){
      dist <- tourr::proj_dist(bb, best)
      if (dist > min_proj_dist){bb_lst <- c(bb_lst, list(bb))}
      i <- i +1
    } else{
      bb_lst <- c(bb_lst, list(bb)); i <- i + 1
    }
  }

  if (!is.na(step_size)){
    basis_df <- tibble::tibble(id = 1:n_basis) |>
      dplyr::mutate(basis = lapply(bb_lst, function(bb){
        interp_bb_best(bb = bb, best = best, step = step_size)
        })) |>
      tidyr::unnest(basis)
  } else{
    basis_df <- tibble::tibble(id = 1:n_basis, basis = bb_lst)
  }

  df_add_idx_val <- function(data, idx, org_data){
    pb$tick()
    tibble::as_tibble(data) |> dplyr::mutate(index := get(idx)()(org_data %*% basis[[1]]))
  }

  pb <- progress::progress_bar$new(total = nrow(basis_df))
  basis_df <- basis_df |>
    dplyr::mutate(dist = lapply(basis, function(bb){tourr::proj_dist(bb, best)})) |>
    tidyr::unnest(dist) |>
    dplyr::group_split(aa = dplyr::row_number())

  if (parallel){
    if (!requireNamespace("future.apply", quietly = TRUE))
      cli::cli_abort("package {.code future.apply} is required, please install it first")
    cli::cli_inform("Calculating projection index in parallel using future.apply ...")
    basis_df <- future.apply::future_lapply(basis_df, function(basis_df) {
        tibble::as_tibble(basis_df) |>
          dplyr::mutate(index := get(idx)()(data %*% basis[[1]]))
        }) |>
     dplyr::bind_rows() |>
     dplyr::select(-aa)
  } else{
    basis_df <- basis_df |>
      purrr::map_dfr(~df_add_idx_val(.x, idx, data)) |>
      dplyr::select(-aa)
  }

  attr(basis_df, "data") <- tibble::as_tibble(data)
  attr(basis_df, "idx") <- idx
  attr(basis_df, "n_basis") <- n_basis
  attr(basis_df, "best") <- best
  attr(basis_df, "step_size") <- step_size
  attr(basis_df, "min_proj_dist") <- min_proj_dist
  attr(basis_df, "seed") <- seed
  class(basis_df) <- c("basis_df", class(basis_df))

  return(basis_df)
}

#' @rdname optim
#' @export
print.basis_df <- function(x, width = NULL, ...){
  writeLines(format(x, width = width, ...))
}


#' @importFrom tibble tbl_sum
#' @rdname optim
#' @export
tbl_sum.basis_df <- function(x){

  dim <- x$basis[[1]] |> dim()
  if (!is.na(attr(x, "step_size"))) {
    line <- glue::glue(length(unique(x$id)), " -> ", nrow(x))
  } else{
    line <- glue::glue(nrow(x))
    }
  c("PP index" = attr(x, "idx"),"No. of bases" = line,
    "interpolation step size" = attr(x, "step_size"),
    "Min. proj. dist." = attr(x, "min_proj_dist"))
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
    as.vector(tt[,,ll])|> matrix(nrow = n, ncol = d)
  })
  tibble::tibble(basis = tt_mat)
}

#' @rdname optim
#' @export
calc_smoothness <- function(basis_df, start_params = c(0.001, 0.5, 2, 2),
                            other_gp_params = NULL, verbose = FALSE){

  if (!inherits(basis_df, "basis_df")){
    cli::cli_abort("Please use {.fn sample_bases} to generate the bases data frame first. See {.code ?calc_smoothness}")
  }

  # construct gp
  cli::cli_inform("Fitting a GP model ...")
  if (verbose) {silent <- FALSE} else {silent <- TRUE}
  gp_params <- list(y = basis_df[["index"]], locs = basis_df[["dist"]],
                    X = as.matrix(rep(1,nrow(basis_df))),
                    start_parms = start_params, covfun_name = "matern_isotropic",
                    silent = silent,
                    other_gp_params
  )
  fit <- do.call("fit_model", gp_params)

  cov_params <- suppressMessages(tibble::as_tibble_row(fit$covparms, .name_repair = "unique"))
  colnames(cov_params) <- c("variance", "range", "smoothness", "nugget", "convergence")
  cov_params <-  cov_params |> dplyr::mutate(convergence = fit$conv)

  # return
  res <- tibble::as_tibble(cov_params)
  attr(res, "basis_df") <- basis_df
  attr(res, "fit_res") <- fit
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

  dim <- attr(x, "basis_df")$basis[[1]] |> dim()
  line <- glue::glue(nrow(attr(x, "basis_df")),
                     " [", dim[1], " x ", dim[2], "]")
  c("PP index" = attr(attr(x, "basis_df"), "idx"), "No. of bases" = line)
}

#' @rdname optim
#' @export
calc_squintability <- function(basis_df, method = c("ks", "nls"), scale = TRUE,
                               bin_width = 0.005, other_params = NULL){

  if (!inherits(basis_df, "basis_df")){
    cli::cli_abort("Please use {.fn sample_bases} to generate the bases data frame first. See {.code ?calc_smoothness}")
  }

  cli::cli_inform("Apply bin width of 0.005...")
  if (!is.na(bin_width)){
    dist_bin <- ceiling(basis_df[["dist"]] / bin_width) * bin_width
    basis_new <- basis_df |>
      dplyr::bind_cols(dist_bin = dist_bin) |>
      dplyr::group_by(dist_bin) |>
      dplyr::summarise(index = mean(index))
  } else{
    basis_new <- basis_df |> dplyr::mutate(dist_bin = dist)
  }

  if (scale){
    basis_new <- basis_new |>
      dplyr::mutate( index = (index - min(index))/(max(index) - min(index)))
  }

  fit_params <- list(basis_new, other_params)
  res <- switch(method,
                ks = do.call("fit_ks", fit_params),
                nls = do.call("fit_nls", fit_params)
                )


  if (method == "nls"){
    max_dist <- basis_df |> dplyr::pull(dist) |> max()

    res$res <- res$res  |>
      dplyr::bind_cols(max_dist = max_dist) |>
      dplyr::mutate(
        dd = (1/(1 + exp(-theta3 * theta2)) - 1/(1 + exp(theta3 * (max_dist)))),
        squint = abs((theta1 - theta4)/dd  * theta2 * theta3)) |>
      dplyr::select(-dd, -max_dist)
  }


  attr(res$res, "basis_df") <- basis_df
  attr(res$res, "fit_res") <- res$fit
  attr(res$res, "method") <- method

  class(res$res) <- c("squintability_res", class(res$res))
  return(res$res)

}

#' @rdname optim
#' @export
print.squintability_res <- function(x, width = NULL, ...){
  writeLines(format(x, width = width, ...))
}

#' @rdname optim
#' @export
tbl_sum.squintability_res <- function(x){

  dim <- attr(x, "basis_df")$basis[[1]] |> dim()
  basis_df <- attr(x, "basis_df")
  line <- glue::glue(nrow(attr(x, "basis_df")),
                     " [", dim[1], " x ", dim[2], "]")
  if (length(attr(basis_df, "step_size")) != 0) {
    line <- glue::glue(length(unique(basis_df$id)), " -> ", nrow(basis_df))
  } else{
    line <- glue::glue(nrow(x))
  }
  c("PP index" = attr(attr(x, "basis_df"), "idx"),
    "No. of bases" = line, method = attr(x, "method"))
}

#' @export
#' @rdname optim
fit_ks <- function(basis_df, idx, other_params = NULL){

  ks_params <- list(basis_df[["dist_bin"]], basis_df[["index"]], other_params)
  fit <- do.call("ksmooth", ks_params)
  ks_dt <- tibble::tibble(x = fit$x, y = fit$y) |>
    dplyr::mutate(dy = c(NA, -diff(y) / diff(x)))
  largest <- ks_dt |> dplyr::filter(dy == max(dy, na.rm = TRUE))
  list(res = tibble::tibble(idx = idx, max_x = largest$x, max_d = largest$dy,
                            squint = max_d * max_x),
       fit_res = NULL)

}

#' @export
#' @rdname optim
fit_nls <- function(basis_df, other_params = NULL){

  ff <- function(x, theta2, theta3){ 1 / (1 + exp(theta3 * (x - theta2)))}

  ff_ratio <- function(x, theta2, theta3){
    (ff(x, theta2, theta3) - ff(max(x), theta2, theta3))/
      (ff(0, theta2, theta3) - ff(max(x), theta2, theta3))
  }

  nls_prms <- list(
    formula = index ~ (theta1 - theta4) * ff_ratio(dist_bin, theta2, theta3) + theta4, data = basis_df) |>
    append(other_params)

  if (!"start" %in% names(other_params)){
    nls_prms <- c(nls_prms, list(start = list(theta1 = 1, theta2 = 1, theta3 = 50, theta4 = 0.01)))
  }

  fit <-  do.call("nls", nls_prms)
  theta_params <- stats::coef(fit)
  list(res = tibble::as_tibble_row(theta_params),
       fit = fit)

}

globalVariables(c("dist", "dist_bin", "dist", "y", "x", "dy",
                  "max_d", "max_x", "aa", "measure", "index", "theta1",
                  "theta2", "theta3", "theta4", "dd"))
