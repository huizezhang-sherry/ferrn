#' Bind the theoretical best record
#'
#' The theoretical best basis is usually known for a simulated problem.
#' Augment this information into the data object allows for evaluating the performance of optimisation against the theory.
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param matrix The theoretical basis to bind
#' @param index The index function used to calculate index value
#' @param raw_data The original data, used to compute the index value for the theoretical best basis
#' @examples
#' best <- matrix(c(0, 1, 0, 0, 0), nrow = 5)
#' tail(holes_1d_better %>% bind_theoretical(best, tourr::holes(), raw_data = boa5), 1)
#' @family bind
#' @export
bind_theoretical <- function(dt, matrix, index, raw_data) {
  num_row <- nrow(dt$basis[[1]])
  num_col <- ncol(dt$basis[[1]])

  if (!tourr::is_orthonormal(matrix)) {
    stop("The theoretical best basis needs to be orthonormal!")
  }

  theo <- tibble::tibble(
    basis = list(matrix),
    index_val = index(as.matrix(raw_data) %*% matrix),
    tries = NA,
    info = as.factor("theoretical"),
    loop = NA,
    method = NA,
    alpha = NA,
    id = 0
  )


  dt %>% dplyr::bind_rows(theo)
}

#' Bind random bases in the projection bases space
#'
#' Given the orthonormality constraint, the projection bases live in a high dimensional hollow sphere.
#' Generating random points on the sphere is useful to perceive the data object in the high dimensional space.
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param n Number of random points to generate in each dimension
#' @examples
#' bind_random(holes_1d_better) %>% tail(5)
#' @family bind
#' @export
bind_random <- function(dt, n = 500) {
  p <- nrow(dt$basis[[1]]) * ncol(dt$basis[[1]])
  ncol <- nrow(dt$basis[[1]])

  fix_matrix <- function(dt) {
    dt <- t(as.matrix(dt, nrow = p, ncol = ncol))
    rownames(dt) <- NULL
    dt
  }

  set.seed(1)
  n_geozoo <- p * n
  suppressWarnings(sphere_basis <- geozoo::sphere.hollow(p, n_geozoo)$points %>%
    tibble::as_tibble() %>%
    dplyr::nest_by(id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(basis = purrr::map(.data$data, fix_matrix)) %>%
    dplyr::select(.data$basis))

  sphere_points <- sphere_basis %>%
    dplyr::mutate(
      index_val = NA,
      tries = NA,
      info = as.factor("randomly_generated"),
      loop = NA,
      method = as.factor("randomly_generated"),
      alpha = NA,
      id = 0,
    )

  dt %>% dplyr::bind_rows(sphere_points)
}

#' Bind random bases in the projection bases space as a matrix
#'
#' @param basis A basis matrix returned by \code{get_basis_matrix()}
#' @param n Number of random points to generate in each dimension
#' @param front if the random matrix should be bound before or after the data basis
#' @examples
#' data <- get_basis_matrix(holes_1d_geo)
#' bind_random_matrix(data) %>% tail(5)
#' @return matrix
#' @family bind
#' @export
bind_random_matrix <- function(basis, n = 500, front = FALSE) {
  p <- ncol(basis)
  n_geozoo <- p * n
  set.seed(1)
  sphere_basis <- geozoo::sphere.hollow(p, n_geozoo)$points
  colnames(sphere_basis) <- colnames(basis)


  if (front) {
    out <- sphere_basis %>% rbind(basis)
  } else {
    out <- basis %>% rbind(sphere_basis)
  }

  return(out)
}
