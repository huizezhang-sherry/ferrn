#'@title explore projected basis in 2D
#'@param glb_obj a global object resulted from the \href{https://cran.r-project.org/web/packages/tourr}{tourr} package
#'@param animate a boolean value indicating whether to animate the projected basis across id
#'@examples
#'\dontrun{explore_proj_pca(x2_geodesic)}
#'@return a ggplot object plotting the 2D projection of the basis
#'@import ggplot2
#'@importFrom rlang sym "!!"
#'@export
explore_proj_pca <- function(glb_obj, col = col, animate = FALSE){
  #browser()

  animate_id <- rlang::sym("animate_id")
  info <- rlang::sym("info")
  tries <- rlang::sym("tries")
  loop <- rlang::sym("loop")
  PC1 <- rlang::sym("PC1")
  PC2 <- rlang::sym("PC2")
  method <- rlang::sym("method")
  col <- rlang::enexpr(col)


  num_col <- ncol(glb_obj$basis[[1]])
  num_row <- nrow(glb_obj$basis[[1]])

  basis <- purrr::flatten_dbl(glb_obj$basis)  %>% matrix(ncol = num_row * num_col, byrow = TRUE)

  if (num_col == 1){
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

  } else if(num_col == 2){
    v1 <- stats::prcomp(basis[,1:num_row]) %>%
      stats::predict() %>%
      tibble::as_tibble() %>%
      dplyr::select(PC1)

    v2 <- stats::prcomp(basis[,(num_row + 1):(2*num_row)]) %>%
      stats::predict() %>%
      tibble::as_tibble() %>%
      dplyr::select(PC1) %>% rename(PC2 = PC1)

    combined <- tibble::as_tibble(basis) %>%
      dplyr::bind_cols(v1) %>%
      dplyr::bind_cols(v2) %>%
      dplyr::bind_cols(glb_obj %>% dplyr::select(-basis)) %>%
      dplyr::group_by(!!tries, !!loop, !!info) %>%
      dplyr::mutate(animate_id = dplyr::group_indices()) %>%
      dplyr::ungroup()



  }else{
    message("projection can be done only on 1d or 2d!")
    stop()
  }




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


  p <- combined %>%
    ggplot(aes(x = PC1, y = PC2)) +
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

