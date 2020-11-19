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
    group_by(tries, !!group) %>%
    filter(loop == max(loop)) %>%
    ungroup()
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
    filter(!!sym("info") != "interpolation") %>%
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
