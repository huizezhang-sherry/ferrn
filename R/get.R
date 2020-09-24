#' get the basis/ index value
#'
#'This set of functions allows you to grab the best basis found by the optimisation in guided tour
#'as well as the bases on the interpolation path.
#'
#'@param dt A data object from the running the optimisation algorithm in guided tour
#'@param group The grouping variable, useful when there are multiple algorithms in the data object
#'@examples
#'holes_1d_better %>% get_start()
#'holes_1d_better %>% get_interp()
#'bind_rows(holes_1d_better, holes_1d_geo) %>% get_best(group = method)
#'@export
#'@rdname get_best
get_best <- function(dt, group = NULL){

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


#' @export
#' @rdname get_best
get_interp <- function(dt, group = NULL){

  dt %>%
    filter(!!sym("info") == "interpolation") %>%
    group_by(!!enexpr(group)) %>%
    mutate(id = dplyr::row_number())
}
