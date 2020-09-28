#' Plotting the data object in the space reduced by PCA
#'
#' The set of functions returns a primary ggplot object
#' that plots the data object in a space reduced by PCA.
#' \code{compute_pca()} computes the PCA and \code{explore_space_pca()} does the plotting.`
#'
#'@param dt a data object to plot
#'@import ggplot2
#'@importFrom rlang sym "!!"
#'@importFrom dplyr bind_cols group_by mutate ungroup
#'@importFrom tibble as_tibble
#'@export
#'@rdname explore_space_pca
compute_pca <- function(dt) {

  info <- sym("info"); tries <- sym("tries"); loop <- sym("loop")

  num_col <- ncol(dt$basis[[1]])
  num_row <- nrow(dt$basis[[1]])

  basis <- purrr::flatten_dbl(dt$basis)  %>% matrix(ncol = num_row * num_col, byrow = TRUE)

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
#'@rdname explore_space_pca
explore_space_pca <- function(pca, col = info){
  col <- rlang::enexpr(col)

  if (!sum(stringr::str_detect(colnames(pca), "PC"))){
    stop("The data object needs to contain principal components!")
  }

  p <- pca %>%
    ggplot(aes(x = PC1, y = PC2, col = !!col)) +
    geom_point() +
    theme(aspect.ratio = 1)

  if ("theoretical" %in% pca$info){
    p <- p +
      geom_point(data = pca %>% filter(info == "theoretical"), size = 10) +
      geom_point(data = get_start(pca), size = 10)
  }

  p

}
globalVariables(c("PC1", "PC2", "info"))
