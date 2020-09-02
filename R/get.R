#'@title get the basis/ index value
#'@param dt a data object from the \href{https://cran.r-project.org/web/packages/tourr}{tourr} package
#'@examples
#'\dontrun{get_best(holes_1d_better)$basis}
#'\dontrun{get_best(holes_1d_better)$index_val}
#'@return a list with the best index value along with its basis
#'@export
#'@rdname get_best
get_best <- function(dt){

  obj <- dt %>%
    dplyr::filter(info == "interpolation") %>%
    tail(1)

  list(basis = obj %>% dplyr::pull(basis) %>% .[[1]],
       index_val = obj %>% dplyr::pull(index_val))
}
