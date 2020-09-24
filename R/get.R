#'@title get the basis/ index value
#'@param dt a data object from the \href{https://cran.r-project.org/web/packages/tourr}{tourr} package
#'@param group the grouping variable, useful when there are multiple trial of tours in dt
#'@param var the variable to select if not returning everything
#'@examples
#'\dontrun{gholes_1d_better %>% get_start()}
#'\dontrun{bind_rows(holes_1d_better, holes_1d_geo) %>% get_best(group = method, var = c(basis, index_val))}
#'@return a tibble with the start/best observation(s)
#'@export
#'@rdname get_best
get_best <- function(dt, group = NULL, var = NULL){

  group <- rlang::enexpr(group)
  var <- rlang::enexprs(var)

  res <- dt %>%
    dplyr::filter(!!sym("info") == "interpolation") %>%
    dplyr::group_by(!!group) %>%
    dplyr::filter(!!sym("id") == max(id))

  # if (!is.null(var)){
  #   if(!(!!var %in% colnames(dt))){
  #     stop("var needs to be one of the variables in the data object!")
  #   }
  #   res <- res %>% dplyr::select(!!var)
  # }

  res
}

#'@export
#'@rdname get_best
get_start <- function(dt){

  dt %>%
    dplyr::filter(!!sym("id") == 1)
}


#' @title bind the theoretical basis to a given data object
#' @param dt the data object being binded to
#' @param matrix the theoretical basis to bind
#' @param index the index function used to calculate index value
#' @examples
#' best <- matrix(c(0, 1, 0, 0, 0), nrow = 5)
#' holes_1d_better %>% bind_theoretical(best, tourr::holes()) %>% tail()
#' dt <- bind_rows(holes_1d_better, holes_1d_geo)
#' dt %>%  bind_theoretical(best, tourr::holes()) %>% tail()
#' @export
bind_theoretical <- function(dt, matrix, index){

  num_row <- nrow(dt$basis[[1]])
  num_col <- ncol(dt$basis[[1]])

  if (ncol(matrix) != num_col | nrow(matrix) != num_row){
    stop("theoretical best matrix need to be of the same dimension as the data object!")
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
