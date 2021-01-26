#' Extract the record with the largest index value
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param group The grouping variable, useful when there are multiple algorithms in the data object
#' @param ... other argument passed to \code{compute_pca()}
#' @examples
#' dplyr::bind_rows(holes_1d_better, holes_1d_geo) %>% get_best(group = method)
#' @family get functions
#' @export
get_best <- function(dt, group = NULL, ...) {

  group <- dplyr::enexpr(group)

  res <- dt %>%
    dplyr::filter(!!sym("info") == "interpolation") %>%
    dplyr::group_by(!!group) %>%
    dplyr::filter(.data$index_val == max(.data$index_val)) %>%
    dplyr::distinct(.data$index_val, .keep_all = TRUE)

  res
}


#' Extract the starting records
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @examples
#' holes_1d_better %>% get_start()
#' @family get functions
#' @export
get_start <- function(dt) {
  dt %>%
    dplyr::filter(!!sym("id") == 1)
}

#' Extract interpolated records
#'
#' @param  dt A data object from the running the optimisation algorithm in guided tour
#' @param group The grouping variable, useful when there are multiple algorithms in the data object
#' @examples
#' holes_1d_better %>%
#'   get_interp() %>%
#'   head()
#' get_interp(dplyr::bind_rows(holes_1d_better, holes_1d_geo), group = method) %>% head()
#' @family get functions
#' @export
get_interp <- function(dt, group = NULL) {
  group <- dplyr::enexpr(group)
  dt %>%
    dplyr::filter(!!sym("info") == "interpolation") %>%
    dplyr::group_by(!!group) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::ungroup()
}

#' Extract the end point at each interpolation
#'
#' @param  dt A data object from the running the optimisation algorithm in guided tour
#' @param group The grouping variable, useful when there are multiple algorithms in the data object
#' @examples
#' holes_1d_better %>% get_interp_last()
#' get_interp_last(dplyr::bind_rows(holes_1d_better, holes_1d_geo), group = method)
#' @family get functions
#' @export
get_interp_last <- function(dt, group = NULL) {

  group <- dplyr::enexpr(group)

  if (!all(c("tries", "loop") %in% colnames(dt))) {
    stop("The data object must have variables tries and loop")
  }

  dt %>%
    get_interp(group = !!group) %>%
    dplyr::group_by(.data$tries, !!group) %>%
    dplyr::filter(.data$loop == max(.data$loop)) %>%
    dplyr::ungroup()
}

#' Extract the anchor points on the geodesic path
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param group The grouping variable, useful when there are multiple algorithms in the data object
#' @examples
#' holes_1d_better %>% get_anchor()
#' holes_1d_geo %>% get_anchor()
#' @family get functions
#' @export
get_anchor <- function(dt, group = NULL) {

  group <- dplyr::enexpr(group)

  dt %>%
    dplyr::filter(.data$info %in% c("new_basis", "best_line_search")) %>%
    dplyr::group_by(!!group) %>%
    dplyr::mutate(id = dplyr::row_number())
}


#' Extract search points during the optimisation
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @examples
#' holes_1d_better %>% get_search()
#' holes_1d_geo %>% get_search()
#' @family get functions
#' @export
get_search <- function(dt) {
  dt %>%
    dplyr::filter(stringr::str_detect(!!sym("info"), "search"))
}

#' Extract directional search points during the optimisation
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param ratio a buffer value to allow directional search points being distinguishable from the anchor points
#' @param ... other argument passed to \code{compute_pca()}
#' @examples
#' holes_1d_geo %>% compute_pca() %>% purrr::pluck("aug") %>% get_dir_search_transformed()
#' @family get functions
#' @export
get_dir_search_transformed <- function(dt, ratio = 3, ...){

  # check only valid for search_geodesic or pseudo-derivative
  if (!"PC1" %in% colnames(dt)){
    message("get_dir_search_transformed() needs to be used on data projected by compute_pca()")
    return(NULL)
  }
  dt <- dt %>% dplyr::filter(.data$method %in% c("pseudo_derivative", "search_geodesic"))

  if (nrow(dt) == 0){
    message("get_dir_search_transformed() is only applicable to geodesic search/ pseudo deriavtive")
    NULL
  }

  # compute the anchor points
  anchor <- dt %>% get_anchor() %>%
    dplyr::rename(anchor_x = .data$PC1, anchor_y = .data$PC2) %>%
    dplyr::select(.data$tries, .data$loop, .data$anchor_x, .data$anchor_y) %>%
    dplyr::mutate(anchor_x = dplyr::lag(.data$anchor_x, default = NA),
                  anchor_y = dplyr::lag(.data$anchor_y, default = NA))

  # compute the buffer
  dir_search <- dt %>% dplyr::filter(.data$info %in% c("direction_search", "best_direction_search"))

  dir_search %>%
    dplyr::left_join(anchor, by = c("tries", "loop")) %>%
    dplyr::mutate(trans_x = ifelse(.data$PC1 - .data$anchor_x < 0,
                                   .data$PC1 - abs(.data$PC1 - .data$anchor_x) * ratio,
                                   .data$PC1 + abs(.data$PC1 - .data$anchor_x) * ratio),
                  trans_y = ifelse(.data$PC2 - .data$anchor_y < 0,
                                   .data$PC2 - abs(.data$PC2 - .data$anchor_y) * ratio,
                                   .data$PC2 + abs(.data$PC2 - .data$anchor_y) * ratio))
}

#' Estimate the radius of the background circle based on the randomly generated points
#'
#' The space of projected bases is a circle when reduced to 2D. A radius is estimated using
#' the largest distance from the bases in the data object to the center point.
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param ... other argument passed to \code{compute_pca()}
#' @importFrom rlang .data
#' @family get functions
#' @export
get_space_param <- function(dt,...) {

    basis <- dt[1, ] %>% dplyr::pull(basis)
    n_row = nrow(basis[[1]])
    n_col = ncol(basis[[1]])

    dt <- dt %>%
      dplyr::select(-dplyr::contains("PC")) %>%
      dplyr::filter(.data$info != "randomly_generated") %>%
      dplyr::add_row(basis = list(matrix(rep(0, n_row * n_col), nrow = n_row, ncol = n_col)),
                     info = "origin") %>%
      compute_pca(...) %>%
      purrr::pluck("aug")

  center <- dt %>% dplyr::filter(.data$info == "origin") %>%
    dplyr::rename(x0 = .data$PC1, y0 = .data$PC2)

  x0 <- center$x0
  y0 <- center$y0

  r <- dt %>%
    dplyr::mutate(dist = sqrt((.data$PC1 - x0)^2 + (.data$PC2 - y0)^2)) %>%
    dplyr::filter(.data$dist == max(.data$dist)) %>%
    dplyr::pull(.data$dist)

  tibble::tibble(x0 = x0, y0 = y0, r = r)
}


#' Extract the theoretical best basis, if applicable
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @examples
#' best <- matrix(c(0, 1, 0, 0, 0), nrow = 5)
#' holes_1d_better %>% bind_theoretical(best, tourr::holes(), raw_data = boa5) %>% get_theo()
#' @family get functions
#' @export
get_theo <- function(dt) {
  theo <- dt %>% dplyr::filter(.data$info == "theoretical")

  if ("PC1" %in% colnames(theo)){
    out <- theo %>%
      dplyr::select(.data$PC1, .data$PC2)
  } else{
    out <- theo
  }

  out
}

#' Extract the end point of the interpolation and the target point in the iteration when an interruption happens
#'
#' The optimiser can find better basis on the interpolation path, an interruption is
#' implemented to stop further interpolation from the highest point to the target point.
#' This discrepancy is highlighted in the PCA plot. You should not use geodesic search on this function.
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param group The grouping variable, useful when there are multiple algorithms in the data object
#' @param precision The precision for the interruption
#' @param ... other argument passed to \code{compute_pca()}
#' @examples
#'holes_1d_better %>% get_interrupt()
#'holes_1d_geo %>% get_interrupt()
#' @family get functions
#' @export
get_interrupt <- function(dt, group = NULL, precision = 0.01, ...) {

  group <- dplyr::enexpr(group)
  if (any(unique(dt$method) %in% c("simulated_annealing", "search_better", "search_better_random"))){

    dt <- dt %>% dplyr::filter(dt$method %in% c("simulated_annealing", "search_better", "search_better_random"))

    anchor <- dt %>% get_anchor()
    interp_last <- dt %>% get_interp_last(group = !!group)

    interp_anchor <- dplyr::bind_rows(anchor, interp_last)

    problem_tries <- interp_anchor %>%
      dplyr::group_by(!!group) %>%
      dplyr::select(.data$info, .data$index_val, .data$tries) %>%
      tidyr::pivot_wider(names_from = .data$info, values_from = .data$index_val) %>%
      dplyr::mutate(match = ifelse(abs(round(.data$new_basis, 3) - round(.data$interpolation, 3)) > precision, TRUE, FALSE)) %>%
      dplyr::filter(match) %>%
      dplyr::mutate(id = paste0(!!group, .data$tries))

    interp_anchor %>%
      dplyr::mutate(id = paste0(!!group, .data$tries)) %>%
      dplyr::filter(.data$id %in% problem_tries$id)
  } else{
    message("interrupt is only implemented in simulated annealing methods")
    return(NULL)
  }

}

#' Extract the end point of the interpolation on the iteration where interruption happens
#'
#' The optimiser can find better basis on the interpolation path, an interruption is
#' implemented to stop further interpolation from the highest point to the target point.
#' This discrepancy is highlighted in the PCA plot. You should not use geodesic search on this function.
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param group The grouping variable, useful when there are multiple algorithms in the data object
#' @param precision The precision for the interruption
#' @param ... other argument passed to \code{compute_pca()}
#' @examples
#'holes_1d_better %>% get_interrupt_finish()
#'holes_1d_geo %>% get_interrupt_finish()
#' @family get functions
#' @export
get_interrupt_finish <- function(dt, group = NULL, precision = 0.01, ...){

  group <- dplyr::enexpr(group)

  if (any(unique(dt$method) %in% c("simulated_annealing", "search_better", "search_better_random"))){
  dt <- dt %>% dplyr::filter(dt$method %in% c("simulated_annealing", "search_better", "search_better_random")) %>%
    dplyr::group_by(!!group)
  anchor <- dt %>% get_anchor(group = !!group)
  interp_last <- dt %>% get_interp_last(group = !!group)

  interp_anchor <- dplyr::bind_rows(anchor, interp_last)

  problem_tries <- interp_anchor %>%
    dplyr::group_by(!!group) %>%
    dplyr::select(.data$info, .data$index_val, .data$tries) %>%
    tidyr::pivot_wider(names_from = .data$info, values_from = .data$index_val) %>%
    dplyr::mutate(match = ifelse(abs(round(.data$new_basis, 3) - round(.data$interpolation, 3)) > precision, TRUE, FALSE)) %>%
    dplyr::filter(match)%>%
    dplyr::mutate(id = paste0(!!group, .data$tries))

  interp_last %>%
    dplyr::mutate(id = paste0(!!group, .data$tries)) %>%
    dplyr::filter(.data$id %in% problem_tries$id)

  } else{
    message("interrupt is only implemented in simulated annealing methods")
    return(NULL)
  }
}



#' Extract the count in each iteration
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param iter The variable used to be counted by
#' @param group The grouping variable, useful when there are multiple algorithms in the data object
#' @param ... other argument passed to \code{compute_pca()}
#' @examples
#' get_search_count(holes_1d_better)
#' get_search_count(dplyr::bind_rows(holes_1d_better, holes_1d_geo), group = method)
#' @family get functions
#' @export
get_search_count <- function(dt, iter = NULL, group = NULL, ...) {
  group <- dplyr::enexpr(group)
  iter <- dplyr::enexpr(iter)

  dt_search <- dt %>%
    get_search() %>%
    dplyr::group_by(!!iter)

  if (!is.null(group)) dt_search <- dt_search %>% dplyr::group_by(!!iter, !!group)

  dt_count <- dt_search %>%
    dplyr::summarise(n = dplyr::n())

  if (!is.null(group)) dt_count <- dt_count %>% dplyr::arrange(!!group)

  dt_count
}


#' Extract all the bases as a matrix
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @examples
#' head(get_basis_matrix(holes_1d_better), 5)
#' @family get functions
#' @export
get_basis_matrix <- function(dt) {
  if (!"basis" %in% colnames(dt)) {
    stop("The dataset needs to have a basis column")
  }

  num_col <- ncol(dt$basis[[1]])
  num_row <- nrow(dt$basis[[1]])

  basis <- purrr::flatten_dbl(dt$basis) %>% matrix(ncol = num_row * num_col, byrow = TRUE)
  colnames(basis) <- paste0("V", 1:(num_row * num_col))
  basis
}

