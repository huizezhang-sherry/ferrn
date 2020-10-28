#' Compute PCA for the projection bases
#'
#'@param dt A data object from the running the optimisation algorithm in guided tour
#'@param group The grouping variable, useful when there are multiple algorithms in the data object
#'@param random Boolean, if the random data from the high dimensional sphere should be bounded
#'@examples
#' dplyr::bind_rows(holes_1d_geo, holes_1d_better) %>% compute_pca(group = method)
#'@export
compute_pca <- function(dt, group = NULL, random = TRUE) {

  if (!"basis" %in% colnames(dt)){
    stop("You need to have a basis column that contains the projection basis!")
  }

  group <- enexpr(group)
  info <- sym("info"); tries <- sym("tries"); loop <- sym("loop")

  num_col <- ncol(dt$basis[[1]])
  num_row <- nrow(dt$basis[[1]])

  dt <- dt %>% mutate(row_num = row_number())

  if (!is.null(group)){

    group_name <- dt %>% get_best(group = !!group) %>% pull(!!group)
    num_method <- group_name %>% length()
    max_bases <- dt %>% get_best(group = !!group) %>% pull(basis)
    max_id <- max_bases %>% vapply(function(x) abs(x) %>% which.max(), numeric(1))
    extract <- function(matrix, pos) matrix[pos %% nrow(matrix), (pos %/% nrow(matrix)) + 1]
    max_sign <- mapply(extract, max_bases, max_id) %>% sign()
    group_to_flip <- group_name[max_sign < 0]


    if (length(group_to_flip) == 0){
      message("there's no flip of the sign")
      basis <- dt %>% get_basis_matrix() %>% bind_random_matrix(n = 1000)
    }else{
      message("signs in all the bases will be fliped in group ", group_to_flip, "\n")
      basis1 <- dt %>% filter(!!group %in% group_to_flip) %>% get_basis_matrix() %>% -.
      basis <- basis1 %>%
        rbind(dt %>% filter(!(!!group) %in% group_to_flip) %>% get_basis_matrix()) %>%
        bind_random_matrix(n = 1000)
    }
  }else{
    basis <- dt %>% get_basis_matrix() %>% bind_random_matrix(n = 1000)
  }

  # Compute PCA
  if (num_col == 1){
    pca <- basis %>% stats::prcomp(scale. = TRUE)

    if (exists("group_to_flip")){
      dt_flip <- dt %>% filter(!!group %in% group_to_flip) %>%
        dplyr::add_row(dt %>% filter(!(!!group) %in% group_to_flip))
    }else{
      dt_flip <-  dt
    }

    v <- suppressMessages(pca$x %>% as_tibble(.name_repair = "minimal"))

    aug <- dt_flip %>%
      bind_random(n = 1000) %>%
      bind_cols(v)

  } else if(num_col == 2){
    message("Ferrn will perform PCA separately on each dimension")
    pca1 <- stats::prcomp(basis[,1:num_row], scale. = TRUE)
    pca2 <- stats::prcomp(basis[,(num_row + 1):(2*num_row)], scale. = TRUE)
    pca <- list(pca1, pca2)

    v1 <- suppressMessages(-pca1$x %>% as_tibble(.name_repair = "minimal"))
    v2 <- suppressMessages(-pca2$x %>% as_tibble(.name_repair = "minimal"))
    colnames(v2)[1:num_row] <- c(paste0("PC", seq(num_row + 1, 2*num_row)))

    aug <- dt %>% bind_random(n = 1000) %>%
      bind_cols(v1) %>%
      bind_cols(v2)

  } else{
    stop("ferrn can only handle 1d or 2d bases!")
  }

  return(list(pca_summary = pca, aug = aug))
}

#' Plot the PCA projection of the projection bases space
#'
#' The set of functions returns a primary ggplot object
#' that plots the data object in a space reduced by PCA.
#' \code{compute_pca()} computes the PCA and \code{explore_space_pca()} does the plotting.`
#'@param dt A data object from the running the optimisation algorithm in guided tour
#'@param pca Boolean, if \code{compute_pca()} should be performed on the data
#'@param group The grouping variable, useful when there are multiple algorithms in the data object
#'@param color A variable from the object that the diagnostic plot should be colored by
#'@param animate Boolean, if the plot should be animated
#'@examples
#'dplyr::bind_rows(holes_1d_geo, holes_1d_better) %>%
#'  bind_theoretical(matrix(c(0, 1, 0, 0, 0), nrow = 5),
#'                   index = tourr::holes(), raw_data = boa5) %>%
#'  explore_space_pca(group = method)  +
#'  scale_color_botanical(palette = "cherry") +
#'  ggplot2::theme(legend.position = "bottom")
#'@import ggplot2
#'@importFrom dplyr filter bind_cols group_by mutate ungroup  sym enexpr pull row_number
#'@importFrom rlang "!!"
#'@importFrom tibble as_tibble
#'@family plot
#'@export
explore_space_pca <- function(dt, pca = TRUE, group = NULL, color = NULL,
                              animate = FALSE){
  #browser()
  color <- group <- enexpr(group)

  if (pca){
    dt <- compute_pca(dt, group = !!group)$aug
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

#' Plot the grand tour animation of the projection bases space
#'
#'@param dt A data object from the running the optimisation algorithm in guided tour
#'@param color A variable from the object that the diagnostic plot should be colored by
#'@param palette The color palette to use
#'@examples
#'explore_space_tour(dplyr::bind_rows(holes_1d_better, holes_1d_geo), color = method)
#' @family plot
#' @export
explore_space_tour <- function(dt, color = info, palette = botanical_palettes$banksia){

  color <- rlang::enexpr(color)
  basis <- get_basis_matrix(dt) %>% bind_random_matrix()
  n_rand <- nrow(basis) - nrow(dt)
  col <- c(palette[as.factor(dt %>% dplyr::pull(!!color))], rep("#D3D3D3", n_rand))

  tourr::animate_xy(basis, col = col)


}
globalVariables(c("PC1", "PC2", "info"))
