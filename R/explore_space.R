#' Plotting the data object in the space reduced by PCA
#'
#' The set of functions returns a primary ggplot object
#' that plots the data object in a space reduced by PCA.
#' \code{compute_pca()} computes the PCA and \code{explore_space_pca()} does the plotting.`
#'@examples
#'
# explore_space_tour(dplyr::bind_rows(holes_1d_better, holes_1d_geo), color = method)
#'@param dt A data object to plot
#'@param random Boolean, if the random data from the high dimensional sphere should be bounded
#'@param pca Boolean, if principal components needs to be pre-computed
#'@param color A variable from the object that the diagnostic plot should be colored by
#'@param animate Boolean, if the plot should be animated
#'@param pal Color palette for \code{explore_space_tour()}
#'@import ggplot2
#'@importFrom rlang sym "!!"
#'@importFrom dplyr bind_cols group_by mutate ungroup
#'@importFrom tibble as_tibble
#'@export
#'@rdname explore_space
compute_pca <- function(dt, random = TRUE) {

  #browser()

  info <- sym("info"); tries <- sym("tries"); loop <- sym("loop")

  num_col <- ncol(dt$basis[[1]])
  num_row <- nrow(dt$basis[[1]])

#
#   if (random) dt <- dt %>% bind_random(n = 1000)
#   basis <- get_basis_matrix(dt) %>% abs()

  basis <- get_basis_matrix(dt) %>% abs() %>% bind_random_matrix(n = 1000)


  # Compute PCA
  if (num_col == 1){
    pca <- basis %>% stats::prcomp(scale. = TRUE)

    aug <- dt %>% bind_random(n = 1000) %>%
      bind_cols(-pca$x %>% as_tibble(.name_repair = "minimal")) %>%
      group_by(!!tries,  !!info) %>%
      mutate(animate_id = dplyr::cur_group_id()) %>%
      ungroup()

  } else if(num_col == 2){
    message("Ferrn will perform PCA separately on each dimension")
    pca1 <- stats::prcomp(basis[,1:num_row], scale. = TRUE)
    pca2 <- stats::prcomp(basis[,(num_row + 1):(2*num_row)], scale. = TRUE)
    pca <- list(pca1, pca2)

    v1 <- -pca1$x %>% as_tibble(.name_repair = "minimal")
    v2 <- -pca2$x %>% as_tibble(.name_repair = "minimal")
    colnames(v2)[1:num_row] <- c(paste0("PC", seq(num_row + 1, 2*num_row)))

    aug <- dt %>% bind_random(n = 1000) %>%
      bind_cols(v1) %>%
      bind_cols(v2) %>%
      group_by(!!tries, !!info) %>%
      mutate(animate_id = dplyr::cur_group_id()) %>%
      ungroup()

  } else{
    stop("ferrn can only handle 1d or 2d bases!")
  }

  return(list(pca_summary = pca, aug = aug))
}

#'@param pca A data object after performing PCA
#'@param color The color of the point
#'@param animate Whether the plot should be animated
#'@param ... Other arguments passed into \code{explore_space_pca}
#'@examples
#'best <- matrix(c(0, 1, 0, 0, 0), nrow = 5)
#'with_theo <- bind_theoretical(holes_1d_better, best, tourr::holes())
#'pca <- compute_pca(with_theo)$aug
#'explore_space_pca(pca)
#'@import ggplot2
#'@importFrom dplyr filter
#'@export
#'@rdname explore_space
explore_space_pca <- function(dt, pca = TRUE, color = info, animate = FALSE, ...){
  col <- rlang::enexpr(color)

  if (pca){
    dt <- compute_pca(dt, ...)$aug
  }

  p <- dt %>%
    ggplot(aes(x = PC1, y = PC2)) +
    geom_point(data = dt %>% dplyr::filter(info != "randomly_generated"), aes(col = !!col)) +
    geom_point(data = dt %>% dplyr::filter(info == "randomly_generated"), color = "grey") +
    theme(aspect.ratio = 1)

  if ("theoretical" %in% dt$info){
    p <- p +
      geom_point(data = dt %>% filter(info == "theoretical"), aes(col = !!col), size = 10) +
      geom_point(data = get_start(dt), aes(col = !!col), size = 5)
  }

  if (animate){
    p <- p + theme(legend.position = "none") +
      gganimate::transition_states(id) +
      gganimate::shadow_mark()
  }

  p

}

#' @export
#' @rdname explore_space
explore_space_tour <- function(dt, color = info, pal = botanical_palettes$banksia, ...){

  color <- rlang::enexpr(color)
  basis <- get_basis_matrix(dt) %>% bind_random_matrix() %>% flip_sign_matrix()


  n_rand <- nrow(basis) - nrow(dt)
  col <- c(pal[as.factor(dt %>% dplyr::pull(!!color))], rep("#D3D3D3", n_rand))

  tourr::animate_xy(basis, col = col)


}
globalVariables(c("PC1", "PC2", "info"))
