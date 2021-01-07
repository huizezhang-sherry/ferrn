#' Extract the record with the largest index value
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param group The grouping variable, useful when there are multiple algorithms in the data object
#' @examples
#' dplyr::bind_rows(holes_1d_better, holes_1d_geo) %>% get_best(group = method)
#' @family get functions
#' @export
get_best <- function(dt, group = NULL) {
  group <- rlang::enexpr(group)
  var <- rlang::enexprs(var)

  res <- dt %>%
    dplyr::filter(!!sym("info") == "interpolation") %>%
    dplyr::group_by(!!group) %>%
    dplyr::filter(index_val == max(index_val)) %>%
    dplyr::distinct(index_val, .keep_all = TRUE) %>%
    dplyr::bind_rows(dt %>% dplyr::filter(!!sym("info") == "theoretical"))

  # if (!is.null(var)){
  #   if(!(!!var %in% colnames(dt))){
  #     stop("var needs to be one of the variables in the data object!")
  #   }
  #   res <- res %>% dplyr::select(!!var)
  # }

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
  group <- enexpr(group)
  dt %>%
    filter(!!sym("info") == "interpolation") %>%
    group_by(!!group) %>%
    mutate(id = dplyr::row_number())
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
  group <- enexpr(group)

  if (!all(c("tries", "loop") %in% colnames(dt))) {
    stop("The data object must have variables tries and loop")
  }

  dt %>%
    get_interp() %>%
    group_by(.data$tries, !!group) %>%
    filter(.data$loop == max(.data$loop)) %>%
    ungroup()
}

#' Extract the anchor points on the geodesic path
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @examples
#' holes_1d_better %>% get_anchor()
#' holes_1d_geo %>% get_anchor()
#' @family get functions
#' @export
get_anchor <- function(dt) {
  dt %>%
    filter(.data$info %in% c("new_basis", "best_line_search"))
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
    mutate(select = stringr::str_detect(!!sym("info"), "search")) %>%
    filter(.data$select) %>%
    dplyr::select(-.data$select)
}

#' Extract the center point of the random circle from the starting points
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param pca Boolean, if \code{compute_pca()} should be performed on the data
#' @param ... other argument passed to \code{compute_pca()}
#' @examples
#' dplyr::bind_rows(holes_1d_better, holes_1d_geo) %>% get_center(pca = TRUE, group = method)
#' @family get functions
#' @export
get_center <- function(dt, pca = FALSE, ...) {

  if (pca){
    dt <- dt %>% compute_pca(...)
    dt <- dt$aug
  }
  start <- dt %>% get_start()

  start %>%
    mutate(PC1 = sum(PC1) / nrow(start), PC2 = sum(PC2) / nrow(start)) %>%
    filter(row_number() == 1) %>%
    dplyr::select(PC1, PC2) %>%
    dplyr::rename(x0 = PC1, y0 = PC2)
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
  dt %>% filter(info == "theoretical")
}

#' Extract the end point of the interpolation and the target point in the iteration when an interruption happens
#'
#' The optimiser can find better basis on the interpolation path, an interruption is
#' implemented to stop further interpolation from the highest point to the target point.
#' This discrepancy is highlighted in the PCA plot. You should not use geodesic search on this function.
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @examples
#'holes_1d_better %>% get_interrupt()
#'holes_1d_geo %>% get_interrupt()
#' @family get functions
#' @export
get_interrupt <- function(dt) {

  if (any(unique(dt$method) %in% c("simulated_annealing", "search_better", "search_better_random"))){

    dt <- dt %>% filter(dt$method %in% c("simulated_annealing", "search_better", "search_better_random"))

    anchor <- dt %>% get_anchor()
    interp_last <- dt %>% get_interp_last()

    interp_anchor <- dplyr::bind_rows(anchor, interp_last)

    problem_tries <- interp_anchor %>%
      dplyr::select(info, index_val, tries) %>%
      tidyr::pivot_wider(names_from = info, values_from = index_val) %>%
      mutate(match = ifelse(abs(round(.data$new_basis, 3) - round(.data$interpolation, 3)) > 0.01, TRUE, FALSE)) %>%
      filter(match) %>%
      pull(tries)

    interp_anchor %>%
      dplyr::arrange(tries) %>%
      filter(tries %in% problem_tries)
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
#' @examples
#' get_search_count(holes_1d_better)
#' get_search_count(dplyr::bind_rows(holes_1d_better, holes_1d_geo), group = method)
#' @family get functions
#' @export
get_search_count <- function(dt, iter = tries, group = NULL) {
  group <- enexpr(group)
  iter <- enexpr(iter)

  dt_search <- dt %>%
    get_search() %>%
    group_by(!!iter)

  if (!is.null(group)) dt_search <- dt_search %>% group_by(!!iter, !!group)

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


globalVariables(c("id"))
