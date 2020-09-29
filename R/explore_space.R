#' Plotting the data object in the space reduced by PCA
#'
#' The set of functions returns a primary ggplot object
#' that plots the data object in a space reduced by PCA.
#' \code{compute_pca()} computes the PCA and \code{explore_space_pca()} does the plotting.`
#'@example
#'
#'explore_space_tour(dplyr::bind_rows(holes_1d_better, holes_1d_geo), color = method)
#'
#'@param dt a data object to plot
#'@import ggplot2
#'@importFrom rlang sym "!!"
#'@importFrom dplyr bind_cols group_by mutate ungroup
#'@importFrom tibble as_tibble
#'@export
#'@rdname explore_space
compute_pca <- function(dt, random = TRUE) {

  info <- sym("info"); tries <- sym("tries"); loop <- sym("loop")

  num_col <- ncol(dt$basis[[1]])
  num_row <- nrow(dt$basis[[1]])


  if (random){
    dt <- dt %>% bind_random(n = 1000)
    basis <- get_basis_matrix(dt)
  }

  # Compute PCA
  if (num_col == 1){
    pca <- basis %>% stats::prcomp(scale. = TRUE)

    aug <- dt %>%
      bind_cols(pca %>% stats::predict() %>% as_tibble(.name_repair = "minimal")) %>%
      group_by(!!tries,  !!info) %>%
      mutate(animate_id = dplyr::cur_group_id()) %>%
      ungroup()

  } else if(num_col == 2){
    message("Ferrn will perform PCA separately on each dimension")
    pca1 <- stats::prcomp(basis[,1:num_row], scale. = TRUE)
    pca2 <- stats::prcomp(basis[,(num_row + 1):(2*num_row)], scale. = TRUE)
    pca <- list(pca1, pca2)

    v1 <- pca1 %>% stats::predict() %>% as_tibble(.name_repair = "minimal")
    v2 <- pca2 %>% stats::predict() %>% as_tibble(.name_repair = "minimal")
    colnames(v2)[1:num_row] <- c(paste0("PC", seq(num_row + 1, 2*num_row)))

    aug <- dt %>%
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

#'@param pca a data object after performing PCA
#'@param col the color of the point
#'@examples
#'best <- matrix(c(0, 1, 0, 0, 0), nrow = 5)
#'with_theo <- bind_theoretical(holes_1d_better, best, tourr::holes())
#'pca <- compute_pca(with_theo)$aug
#'explore_space_pca(pca)
#'@import ggplot2
#'@importFrom dplyr filter
#'@export
#'@rdname explore_space
explore_space_pca <- function(dt, pca = TRUE, col = info){
  col <- rlang::enexpr(col)

  if (pca){
    dt <- compute_pca(dt)$aug
  }

  p <- dt %>%
    ggplot(aes(x = PC1, y = PC2)) +
    geom_point(data = dt %>% dplyr::filter(info != "randomly_generated"), aes(col = !!col)) +
    geom_point(data = dt %>% dplyr::filter(info == "randomly_generated"), col = "grey") +
    theme(aspect.ratio = 1)

  if ("theoretical" %in% dt$info){
    p <- p +
      geom_point(data = dt %>% filter(info == "theoretical"), size = 10) +
      geom_point(data = get_start(dt), size = 5)
  }

  p

}

#' @export
#' @rdname explore_space
explore_space_tour <- function(dt, color, pal = pal, ...){

  color <- rlang::enexpr(color)
  basis <- get_basis_matrix(dt) %>% bind_random_matrix()


  n_rand <- nrow(basis) - nrow(dt)
  pal <- c("#D95F02","#7570B3", "#E7298A")
  col <- c(pal[as.factor(dt %>% dplyr::pull(!!color))], rep("#D3D3D3", n_rand))

  tourr::animate_xy(basis, col = col)


}



globalVariables(c("PC1", "PC2", "info"))
