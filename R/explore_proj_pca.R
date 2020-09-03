#'@title explore projected basis in 2D
#'@param glb_obj a global object resulted from the \href{https://cran.r-project.org/web/packages/tourr}{tourr} package
#'@param animate a boolean value indicating whether to animate the projected basis across id
#'@examples
#'\dontrun{holes_1d_geo %>% explore_proj_pca()}
#'\dontrun{bind_rows(holes_1d_geo, holes_1d_geo_polish) %>% explore_proj_pca(col = method)}
#'@return a ggplot object plotting the 2D projection of the basis
#'@import ggplot2
#'@importFrom rlang sym "!!"
#'@importFrom dplyr bind_cols group_by mutate ungroup
#'@export
#'@rdname explore_proj_pca
compute_pca <- function(glb_obj) {

  info <- sym("info")
  tries <- sym("tries")
  loop <- sym("loop")

  num_col <- ncol(glb_obj$basis[[1]])
  num_row <- nrow(glb_obj$basis[[1]])

  basis <- purrr::flatten_dbl(glb_obj$basis)  %>% matrix(ncol = num_row * num_col, byrow = TRUE)

  if (num_col == 1){
    pca <- basis %>% stats::prcomp(scale. = TRUE)

    combined <- tibble::as_tibble(basis) %>%
      bind_cols(pca %>% stats::predict() %>% tibble::as_tibble()) %>%
      bind_cols(glb_obj %>% dplyr::select(-basis)) %>%
      group_by(!!tries, !!loop, !!info) %>%
      mutate(animate_id = dplyr::group_indices()) %>%
      ungroup()

  } else if(num_col ==2){
    message("Ferrn will perform PCA separately on each dimension")
    pca1 <- stats::prcomp(basis[,1:num_row], scale. = TRUE)
    pca2 <- stats::prcomp(basis[,(num_row + 1):(2*num_row)], scale. = TRUE)
    pca <- list(pca1, pca2)

    v1 <- pca1 %>% stats::predict() %>% tibble::as_tibble()
    v2 <- pca2 %>% stats::predict() %>% tibble::as_tibble()
    colnames(v2)[1:num_row] <- c(paste0("PC", seq(num_row + 1, 2*num_row)))

    combined <- tibble::as_tibble(basis) %>%
      bind_cols(v1) %>%
      bind_cols(v2) %>%
      bind_cols(glb_obj %>% dplyr::select(-basis)) %>%
      group_by(!!tries, !!loop, !!info) %>%
      mutate(animate_id = dplyr::group_indices()) %>%
      ungroup()

  } else{
    stop("ferrn can only handle 1d or 2d bases!")
  }

  return(list(pca_summary = pca, combined = combined))



}

#'@export
#'@rdname explore_proj_pca
explore_proj_pca <- function(glb_obj, col = info, size = 1, alpha = 1, animate = FALSE){

  animate_id <- rlang::sym("animate_id")
  col <- rlang::enexpr(col)

  pca_obj <- compute_pca(glb_obj)

  if (pca_obj$combined$method[2] == "search_geodesic"){
    dt <- pca_obj$combined %>%
      dplyr::mutate(info =  forcats::fct_relevel(info,
                                                 c("start", "direction_search",
                                                   "best_direction_search",
                                                   "line_search",
                                                   "best_line_search")))
  }else{
    dt <- pca_obj$combined
  }


  p <- dt %>%
    ggplot(aes(x = PC1, y = PC2), size = size, alpha = alpha) +
    geom_point(aes(col = !!col)) +
    geom_point(data = dt %>% dplyr::filter(info == "start"), col = scales::hue_pal()(6)[1]) +
    geom_point(data = dt %>% dplyr::filter(info == "best_direction_search"), col = scales::hue_pal()(6)[3]) +
    geom_point(data = dt %>% dplyr::filter(info == "best_line_search"), col = scales::hue_pal()(6)[5]) +

    #stat_density(aes(fill = after_stat(nlevel)), geom = "polygon", alpha = 0.5) +
    theme(aspect.ratio = 1)

  if (animate){
    p <- p +
      gganimate::transition_time(!!animate_id) +
      gganimate::shadow_mark()
  }

  p
}

