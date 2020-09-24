#' bind external data to a given data object
#'
#'These functions allows you to bind the theoretical best basis and randomly generated basis on a hollow sphere to the data object
#'
#' In a simulation setting, the theoretical best basis is known for a given problem.
#'
#' Given the orthonormality constraint, the projection bases live in a high dimensional hollow sphere.
#' Generating random points on the sphere is useful to preceive the data object in the high dimensional space.
#'
#' @param dt the data object being binded to
#' @param matrix the theoretical basis to bind
#' @param index the index function used to calculate index value
#' @examples
#' best <- matrix(c(0, 1, 0, 0, 0), nrow = 5)
#' holes_1d_better %>% bind_theoretical(best, tourr::holes()) %>% tail()
#' dt <- bind_rows(holes_1d_better, holes_1d_geo)
#' dt %>%  bind_theoretical(best, tourr::holes()) %>% tail()
#' @export
#' @rdname bind_theoretical
bind_theoretical <- function(dt, matrix, index){

  num_row <- nrow(dt$basis[[1]])
  num_col <- ncol(dt$basis[[1]])

  if (ncol(matrix) != num_col | nrow(matrix) != num_row){
    stop("theoretical best matrix need to be of the same dimension as the data object!")
  }

  if (!tourr::is_orthonormal(matrix)){
    stop("The theoretical best basis needs to be orthonormal!")
  }

  method_index <- !is.na(unique(dt$method))
  method <- unique(dt$method)[method_index]

  theo <- tibble::tibble(basis = list(matrix),
                         index_val = index(matrix),
                         tries = NA,
                         info = "theoretical",
                         loop = NA,
                         method = method,
                         alpha = NA,
                         id = max(dt$id) + 1)


  dt %>% dplyr::bind_rows(theo)

}



#' @export
#' @rdname bind_theoretical
bind_random <- function(dt, ...){
  p <- nrow(dt$basis[[1]])
  ncol <- nrow(dt$basis[[1]])

  fix_matrix <- function(dt){
    dt <- t(as.matrix(dt, nrow = p, ncol = ncol))
    rownames(dt) <- NULL
    dt
  }

  sphere_basis <- geozoo::sphere.hollow(p, ...)$points %>%
    as_tibble() %>%
    dplyr::nest_by(id = dplyr::row_number()) %>%
    ungroup() %>%
    mutate(basis = purrr::map(data, fix_matrix)) %>%
    dplyr::select(basis)

  sphere_points <- sphere_basis %>%
    dplyr::mutate(index_val = NA,
                  tries = NA,
                  info = "randomly_generated",
                  loop = NA,
                  method = "randomly_generated",
                  alpha = NA,
                  id = max(dt$id) + 1,
    )

  dt %>% dplyr::bind_rows(sphere_points)

}
