#'@title explore projected basis in 2D
#'@param glb_obj a global object resulted from the \href{https://cran.r-project.org/web/packages/tourr}{tourr} package
#'@param animate a boolean value indicating whether to animate the projected basis across id
#'@examples
#'\dontrun{explore_proj_pca(x2_geodesic)}
#'@return a ggplot object plotting the 2D projection of the basis
#'@import ggplot2
#'@importFrom rlang sym "!!"
#'@export
explore_proj_pca <- function(glb_obj, animate = FALSE){
  #browser()

  animate_id <- rlang::sym("animate_id")
  info <- rlang::sym("info")
  tries <- rlang::sym("tries")
  loop <- rlang::sym("loop")
  PC1 <- rlang::sym("PC1")
  PC2 <- rlang::sym("PC2")
  method <- rlang::sym("method")


  num_col <- ncol(glb_obj$basis[[1]])
  num_row <- nrow(glb_obj$basis[[1]])

  basis <- glb_obj$basis %>% unlist() %>% matrix(ncol = 5, byrow = TRUE)

  loadings <- basis %>%
    stats::prcomp() %>%
    stats::predict() %>%
    tibble::as_tibble()

  combined <- tibble::as_tibble(basis) %>%
    dplyr::bind_cols(loadings) %>%
    dplyr::bind_cols(glb_obj %>% dplyr::select(-basis)) %>%
    dplyr::group_by(!!tries, !!loop, !!info) %>%
    dplyr::mutate(animate_id = dplyr::group_indices()) %>%
    dplyr::ungroup()


  # re-arrange the info level - maybe this could be done in the tourr
  if (combined$method[[1]] == "geodesic"){
    combined <- combined %>%
      dplyr::mutate(info =  forcats::fct_relevel(!!info,
                                                 c("start", "direction_search",
                                                   "best_direction_search",
                                                   "line_search",
                                                   "best_line_search")))
  }else{
    combined <- combined %>%
      dplyr::mutate(info = forcats::fct_relevel(!!info,
                                                c("start","random_search",
                                                  "new_basis", "interpolation")))
  }


  p <- combined %>%
    ggplot(aes(x = !!PC1, y = !!PC2, col = !!info)) +
    geom_point() +
    theme(aspect.ratio = 1)

  if (animate){
    p <- p +
      gganimate::transition_time(!!animate_id) +
      gganimate::shadow_mark()
  }

  p
}

