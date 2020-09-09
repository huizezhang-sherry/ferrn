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

  if (!is.null(var)){
    if(!(!!var %in% colnames(dt))){
      stop("var needs to be one of the variables in the data object!")
    }
    res <- res %>% dplyr::select(!!var)
  }

  res
}

#'@export
#'@rdname get_best
get_start <- function(dt){

  dt %>%
    dplyr::filter(!!sym("id") == 1)
}
