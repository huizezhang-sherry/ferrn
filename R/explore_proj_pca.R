#'@title explore projected basis in 2D
#'@param glb_obj a global object resulted from the \href{https://cran.r-project.org/web/packages/tourr}{tourr} package
#'@param animate a boolean value indicating whether to animate the projected basis across id
#'@examples
#'\dontrun{holes_1d_geo %>% explore_proj_pca()}
#'\dontrun{bind_rows(holes_1d_geo, holes_1d_geo_polish) %>% explore_proj_pca(col = method)}
#'@return a ggplot object plotting the 2D projection of the basis
#'@import ggplot2
#'@importFrom rlang sym "!!"
#'@export
#'@rdname explore_proj_pca
compute_pca <- function(glb_obj) {

  info <- rlang::sym("info")
  tries <- rlang::sym("tries")
  loop <- rlang::sym("loop")

  num_col <- ncol(glb_obj$basis[[1]])
  num_row <- nrow(glb_obj$basis[[1]])

  if (num_col != 1){
    stop("Ferrn only supports PCA for 1D projection")
  }

  basis <- purrr::flatten_dbl(glb_obj$basis)  %>% matrix(ncol = num_row * num_col, byrow = TRUE)

  pca <- basis %>%
    stats::prcomp()

  combined <- tibble::as_tibble(basis) %>%
    dplyr::bind_cols(pca %>% stats::predict() %>% tibble::as_tibble()) %>%
    dplyr::bind_cols(glb_obj %>% dplyr::select(-basis)) %>%
    dplyr::group_by(!!tries, !!loop, !!info) %>%
    dplyr::mutate(animate_id = dplyr::group_indices()) %>%
    dplyr::ungroup()

  return(list(pca_summary = pca, combined = combined ))


  # if(num_col == 2){
  #   v1 <- stats::prcomp(basis[,1:num_row]) %>%
  #     stats::predict() %>%
  #     tibble::as_tibble() %>%
  #     dplyr::select(PC1)
  #
  #   v2 <- stats::prcomp(basis[,(num_row + 1):(2*num_row)]) %>%
  #     stats::predict() %>%
  #     tibble::as_tibble() %>%
  #     dplyr::select(PC1) %>% rename(PC2 = PC1)
  #
  #   combined <- tibble::as_tibble(basis) %>%
  #     dplyr::bind_cols(v1) %>%
  #     dplyr::bind_cols(v2) %>%
  #     dplyr::bind_cols(glb_obj %>% dplyr::select(-basis)) %>%
  #     dplyr::group_by(!!tries, !!loop, !!info) %>%
  #     dplyr::mutate(animate_id = dplyr::group_indices()) %>%
  #     dplyr::ungroup()
  #

}

#'@export
#'@rdname explore_proj_pca
explore_proj_pca <- function(glb_obj, col = info, animate = FALSE){
  #browser()

  animate_id <- rlang::sym("animate_id")
  PC1 <- rlang::sym("PC1")
  PC2 <- rlang::sym("PC2")
  col <- rlang::enexpr(col)

  #re-arrange the info level - maybe this could be done in the tourr
  # if (combined$method[[1]] == "search_geodesic"){
  #   combined <- combined %>%
  #     dplyr::mutate(info =  forcats::fct_relevel(!!info,
  #                                                c("start", "direction_search",
  #                                                  "best_direction_search",
  #                                                  "line_search",
  #                                                  "best_line_search")))
  # }else{
  #   combined <- combined %>%
  #     dplyr::mutate(info = forcats::fct_relevel(!!info,
  #                                               c("start","random_search",
  #                                                 "new_basis", "interpolation")))
  # }

  pca_obj <- compute_pca(glb_obj)

  p <- pca_obj$combined %>%
    ggplot(aes(x = !!PC1, y = !!PC2)) +
    geom_point(aes(col = !!col)) +
    #stat_density(aes(fill = after_stat(nlevel)), geom = "polygon", alpha = 0.5) +
    theme(aspect.ratio = 1)

  if (animate){
    p <- p +
      gganimate::transition_time(!!animate_id) +
      gganimate::shadow_mark()
  }

  p
}

