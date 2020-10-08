#' Relevel the info column with proper order
#'
#' Relevel the \code{info} column in the data object to be the step in the optimisation algorithms.
#'
#' For \code{search_geodesic},
#'\itemize{
#'\item{new_basis < direction_search < best_direction_search < best_line_search}}
#'
#' For \code{search_better} and \code{search_better_random},
#'
#'\itemize{
#'\item{random_search < new_basis < interpolation}}
#'
#' @param dt The data object to relevel
#' @param order A vector that allows you to customise the order of info
#' @examples
#' holes_1d_geo %>% relevel_geo()
#' holes_1d_better %>% relevel_better()
#'
#' @export
#' @rdname relevel
relevel_geo <- function(dt, order = c("new_basis", "direction_search", "best_direction_search",
                                      "best_line_search", "interpolation")){

  if (!all(order %in% unique(dt$info))){
    stop("All the order level must present in the data!")
  }
  method <- unique(dt$method)

  if (!"info" %in% colnames(dt)){
    stop("the data object needs to have an info column!")
  } else if(method != "search_geodesic"){
    stop("use relevel_geo only when the searching method is search_geodesic!")
  }

  dt %>% mutate(info = forcats::fct_relevel(.data$info, order))

}

#' @export
#' @rdname relevel
relevel_better <- function(dt, order =  c("random_search","new_basis", "interpolation")){

  method_index <- !is.na(unique(dt$method))
  method <- unique(dt$method)[method_index]

  # checking before relevel
  if (!"info" %in% colnames(dt)){
    stop("the data object needs to have an info column!")
  }

  if(!method %in% c("search_better", "search_better_random")){
    stop("use relevel_better if searching method is search_better or search_better_random!")
  }

  if (!all(order %in% unique(dt$info))){
    stop("All the order level must present in the data!")
  }

  dt %>% mutate(info = forcats::fct_relevel(.data$info, order))

}
