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
compute_pca <- function(dt) {

  info <- sym("info")
  tries <- sym("tries")
  loop <- sym("loop")

  num_col <- ncol(dt$basis[[1]])
  num_row <- nrow(dt$basis[[1]])

  basis <- purrr::flatten_dbl(dt$basis)  %>% matrix(ncol = num_row * num_col, byrow = TRUE)

  if (num_col == 1){
    pca <- basis %>% stats::prcomp(scale. = TRUE)

    aug <- tibble::as_tibble(basis) %>%
      bind_cols(pca %>% stats::predict() %>% tibble::as_tibble()) %>%
      bind_cols(dt %>% dplyr::select(-basis)) %>%
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

    aug <- tibble::as_tibble(basis) %>%
      bind_cols(v1) %>%
      bind_cols(v2) %>%
      bind_cols(dt %>% dplyr::select(-basis)) %>%
      group_by(!!tries, !!loop, !!info) %>%
      mutate(animate_id = dplyr::group_indices()) %>%
      ungroup()

  } else{
    stop("ferrn can only handle 1d or 2d bases!")
  }

  return(list(pca_summary = pca, aug = aug))

}

#'@export
#'@importFrom dplyr filter
#'@rdname explore_proj_pca
explore_proj_pca <- function(dt, col = info, size = 10, alpha = 1, facet = NULL,  animate = FALSE){

  animate_id <- sym("animate_id")
  col <- rlang::enexpr(col)
  facet <- rlang::enexpr(facet)

  if ("PC1" %in% colnames(dt)){
    pca_obj <- dt
  }else{
    pca_obj <- compute_pca(dt)$aug
  }


  if (pca_obj$method[2] == "search_geodesic"){
    pca_obj <- pca_obj %>%
      mutate(info =  forcats::fct_relevel(info,
                                                 c("direction_search",
                                                   "best_direction_search",
                                                   "line_search",
                                                   "best_line_search")))
  } else if (pca_obj$method[2] == "search_better"){
   pca_boj <- pca_obj %>%
     mutate(info == forcats::fct_relevel(info, c("new_basis", "interpolation",
                                                 "random_search")))
  }

  start <- pca_obj %>% group_by(!!facet) %>% dplyr::filter(!!sym("id") == 1)
  last <- pca_obj  %>% dplyr::filter(!!sym("info") == "interpolation") %>%  group_by(!!facet) %>%
    filter(!!sym("id") == max(id))


  p <- pca_obj %>%
    ggplot(aes(x = !!sym("PC1"), y = !!sym("PC2"), col = !!col), alpha = alpha) +
    geom_point() +
    geom_point(data = start, size = size) +
    geom_point(data = last, size = size) +
    scale_color_botanical(palette = "banksia") +
    #stat_density(aes(fill = after_stat(nlevel)), geom = "polygon", alpha = 0.5) +
    theme(aspect.ratio = 1, legend.position = "bottom") +
    facet_wrap(vars(!!facet))

  if ("theory" %in% pca_obj$info){
    p <-  p +
      geom_point(data = pca_obj %>% filter(!!sym("info") == "theory"), size = size)
  }

  if (animate){
    p <- p +
      gganimate::transition_time(!!animate_id) +
      gganimate::shadow_mark()
  }

  p
}

