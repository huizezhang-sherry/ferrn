#' Explore the parameter space
#'
#' The set of functions returns a primary ggplot object
#' that plots the data object in a space reduced by PCA.
#' \code{compute_pca()} computes the PCA and \code{explore_space_pca()} does the plotting.`
#'@examples
# dplyr::bind_rows(holes_1d_geo, holes_1d_better) %>%
#   bind_theoretical(matrix(c(0, 1, 0, 0, 0), nrow = 5),
#                    index = tourr::holes(), raw_data = boa5) %>%
#   explore_space_pca(group = method)  +
#   scale_color_botanical(palette = "cherry") +
#   ggplot2::theme(legend.position = "bottom")
#'explore_space_tour(dplyr::bind_rows(holes_1d_better, holes_1d_geo), color = method)
#'@param dt A data object to plot
#'@param random Boolean, if the random data from the high dimensional sphere should be bounded
#'@param color A variable from the object that the diagnostic plot should be colored by
#'@param animate Boolean, if the plot should be animated
#'@param pal Color palette for \code{explore_space_tour()}
#'@param pca Boolean, if \code{compute_pca()} should be performed on the data
#'@param animate Whether the plot should be animated
#'@param group The grouping variable, useful when there are multiple algorithms in the data object
#'@param ... Other arguments passed into \code{explore_space_pca}
#'@import ggplot2
#'@importFrom dplyr filter bind_cols group_by mutate ungroup  sym enexpr
#'@importFrom rlang "!!"
#'@importFrom tibble as_tibble
#'@export
#'@rdname explore_space
compute_pca <- function(dt, group = NULL, random = TRUE, ...) {

  #browser()

  group <- enexpr(group)
  info <- sym("info"); tries <- sym("tries"); loop <- sym("loop")

  num_col <- ncol(dt$basis[[1]])
  num_row <- nrow(dt$basis[[1]])

  if (!is.null(group)){

    group_name <- dt %>% get_best(group = !!group) %>% pull(!!group)
    num_method <- group_name %>% length()
    max_bases <- dt %>% get_best(group = !!group) %>% pull(basis)
    max_id <- max_bases %>% vapply(function(x) abs(x) %>% which.max(), numeric(1))
    extract <- function(matrix, pos) matrix[pos %% nrow(matrix), (pos %/% nrow(matrix)) + 1]
    max_sign <- mapply(extract, max_bases, max_id) %>% sign()
    group_to_flip <- group_name[max_sign < 0]


    if (length(group_to_flip) == 0){
      basis <- dt %>% get_basis_matrix() %>% bind_random_matrix(n = 1000)
    }else{
      basis1 <- dt %>% filter(!!group %in% group_to_flip) %>% get_basis_matrix() %>% -.
      basis <- basis1 %>%
        rbind(dt %>% filter(!(!!group) %in% group_to_flip) %>% get_basis_matrix()) %>%
        bind_random_matrix(n = 1000, ...)
    }
  }else{
    basis <- dt %>% get_basis_matrix() %>% bind_random_matrix(n = 1000)
  }

  # Compute PCA
  if (num_col == 1){
    pca <- basis %>% stats::prcomp(scale. = TRUE)

    aug <- dt %>% bind_random(n = 1000, ...) %>%
      bind_cols(pca$x %>% as_tibble(.name_repair = "minimal")) %>%
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


#'@export
#'@rdname explore_space
explore_space_pca <- function(dt, pca = TRUE, group = NULL, color = NULL,
                              animate = FALSE, ...){
  #browser()
  color <- group <- enexpr(group)

  if (pca){
    dt <- compute_pca(dt, group = !!group, ...)$aug
  }

  p <- dt %>%
    ggplot(aes(x = PC1, y = PC2)) +
    geom_point(data = dt %>% dplyr::filter(info != "randomly_generated"), aes(col = !!color)) +
    geom_point(data = dt %>% dplyr::filter(info == "randomly_generated"), color = "grey") +
    theme(aspect.ratio = 1)

  if ("theoretical" %in% dt$info){
    p <- p +
      geom_point(data = dt %>% filter(info == "theoretical"), aes(col = !!color), size = 10) +
      geom_point(data = get_start(dt), aes(col = !!color), size = 5)
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
  basis <- get_basis_matrix(dt) %>% bind_random_matrix()


  n_rand <- nrow(basis) - nrow(dt)
  col <- c(pal[as.factor(dt %>% dplyr::pull(!!color))], rep("#D3D3D3", n_rand))

  tourr::animate_xy(basis, col = col)


}
globalVariables(c("PC1", "PC2", "info"))
