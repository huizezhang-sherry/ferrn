#' get the basis/ index value
#'
#'This set of functions allows you to grab the best basis found by the optimisation in guided tour
#'as well as the bases on the interpolation path.
#'
#'@param dt A data object from the running the optimisation algorithm in guided tour
#'@param group The grouping variable, useful when there are multiple algorithms in the data object
#'@param iter The variable used to be counted by
#'@param basis A matrix produced by \code{get_basis_matrix()}
#'@examples
#'holes_1d_better %>% get_start()
#'holes_1d_better %>% get_interp()
#'dplyr::bind_rows(holes_1d_better, holes_1d_geo) %>% get_best(group = method)
#'@export
#'@rdname get_best
get_best <- function(dt, group = NULL){

  group <- rlang::enexpr(group)
  var <- rlang::enexprs(var)

  res <- dt %>%
    dplyr::filter(!!sym("info") == "interpolation") %>%
    dplyr::group_by(!!group) %>%
    dplyr::filter(index_val == max(index_val)) %>%
    distinct(index_val, .keep_all = TRUE)

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

  group <- enexpr(group)
  dt %>%
    filter(!!sym("info") == "interpolation") %>%
    group_by(!!group) %>%
    mutate(id = dplyr::row_number())
}


#' @export
#' @rdname get_best
get_search_count <- function(dt, iter = tries, group = NULL){
  group <- enexpr(group)
  iter <- enexpr(iter)

  dt_search <- dt %>%
    filter(!!sym("info") != "interpolation") %>%
    group_by(!!iter)

  if (!is.null(group)) dt_search <- dt_search %>% group_by(!!iter, !!group)

  dt_count <- dt_search %>%
    dplyr::summarise(n = dplyr::n())

  if (!is.null(group)) dt_count <- dt_count %>% dplyr::arrange(!!group)

  dt_count
}

#' @export
#'@rdname get_best
get_basis_matrix <- function(dt){

  if (!"basis" %in% colnames(dt)){
    stop("The dataset needs to have a basis column")
  }

  num_col <- ncol(dt$basis[[1]])
  num_row <- nrow(dt$basis[[1]])

  basis <- purrr::flatten_dbl(dt$basis)  %>% matrix(ncol = num_row * num_col, byrow = TRUE)
  colnames(basis) <- paste0("V", 1: (num_row * num_col))
  basis
}

globalVariables(c("id"))
