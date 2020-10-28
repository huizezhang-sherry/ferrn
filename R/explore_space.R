#' Flip the sign of a group of bases
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param group The grouping variable, useful when there are multiple algorithms in the data object
#' @examples
#' dplyr::bind_rows(holes_1d_geo, holes_1d_better) %>% flip_sign(group = method)
#' @export
flip_sign <- function(dt, group = NULL){
  group <- enexpr(group)

  if (!is.null(group)){
    group_name <- dt %>% get_best(group = !!group) %>% pull(!!group)
    num_method <- group_name %>% length()
    max_bases <- dt %>% get_best(group = !!group) %>% pull(basis)
    max_id <- max_bases %>% vapply(function(x) abs(x) %>% which.max(), numeric(1))
    extract <- function(matrix, pos) matrix[pos %% nrow(matrix), (pos %/% nrow(matrix)) + 1]
    max_sign <- mapply(extract, max_bases, max_id) %>% sign()
    group_to_flip <- group_name[max_sign < 0]
    group_to_flip <- group_to_flip[group_to_flip != "theoretical"]

    if (length(group_to_flip) == 0){
      message("there's no flip of the sign")
      basis <- dt %>% get_basis_matrix()
    }else{
      message("signs in all the bases will be fliped in group ", group_to_flip, "\n")
      basis1 <- dt %>% filter(!!group %in% group_to_flip & !!group != "theoretical") %>%
        get_basis_matrix() %>% -.
      basis <- basis1 %>%
        rbind(dt %>% filter(!(!!group) %in% group_to_flip | !!group == "theoretical") %>% get_basis_matrix())

      dt_obj <- dt %>% filter(!!group %in% group_to_flip & !!group != "theoretical") %>%
        dplyr::bind_rows(dt %>% filter(!(!!group) %in% group_to_flip | !!group == "theoretical"))
    }
  }else{
    basis <- basis
    dt_obj <- dt
  }

  return(list(basis = basis,
              flip = !is.null(group),
              dt = dt_obj))
}


#' Compute PCA for the projection bases
#'
#'@param dt A data object from the running the optimisation algorithm in guided tour
#'@param group The grouping variable, useful when there are multiple algorithms in the data object
#'@param random Boolean, if the random data from the high dimensional sphere should be bounded
#'@examples
#' dplyr::bind_rows(holes_1d_geo, holes_1d_better) %>% compute_pca(group = method)
#'@export
compute_pca <- function(dt, group = NULL, random = TRUE) {2

  if (!"basis" %in% colnames(dt)){
    stop("You need to have a basis column that contains the projection basis!")
  }

  group <- enexpr(group)
  info <- sym("info"); tries <- sym("tries"); loop <- sym("loop")

  num_col <- ncol(dt$basis[[1]])
  num_row <- nrow(dt$basis[[1]])

  dt <- dt %>% mutate(row_num = row_number())

  flip <- flip_sign(dt, group = !!group)
  basis <- flip$basis

  # Compute PCA
  if (num_col == 1){
    pca <- basis %>% bind_random_matrix() %>% stats::prcomp(scale. = TRUE)
    v <- suppressMessages(pca$x %>% as_tibble(.name_repair = "minimal"))
    if (flip$flip) dt_flip <- flip$dt else dt_flip <- dt
    aug <- dt_flip %>% bind_random() %>% bind_cols(v)

  } else if(num_col == 2){
    message("Ferrn will perform PCA separately on each dimension")
    basis_2d <- basis %>% bind_random_matrix()
    pca1 <- stats::prcomp(basis_2d[,1:num_row], scale. = TRUE)
    pca2 <- stats::prcomp(basis_2d[,(num_row + 1):(2*num_row)], scale. = TRUE)
    pca <- list(pca1, pca2)

    v1 <- suppressMessages(-pca1$x %>% as_tibble(.name_repair = "minimal"))
    v2 <- suppressMessages(-pca2$x %>% as_tibble(.name_repair = "minimal"))
    colnames(v2)[1:num_row] <- c(paste0("PC", seq(num_row + 1, 2*num_row)))

    if (flip$flip) dt_flip <- flip$dt else dt_flip <- dt
    aug <- dt_flip %>% bind_random() %>% bind_cols(v1) %>% bind_cols(v2)

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
#'  scale_color_botanical(palette = "cherry")
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
    geom_point(data = dt %>% dplyr::filter(info == "randomly_generated"), color = "grey") +
    geom_point(data = dt %>% dplyr::filter(info != "randomly_generated"), aes(col = !!color)) +
    theme_void() +
    theme(aspect.ratio = 1, legend.position = "bottom")

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
#'@param group The grouping variable, useful when there are multiple algorithms in the data object
#'@param theoretical Boolean, if the theoretical bases have been inlcuded in the data object
#'@param color A variable from the object that the diagnostic plot should be colored by
#'@param palette The color palette to use
#'@param ... Additional argument passed to \code{tourr::animate_xy()}
#'@examples
#'explore_space_tour(dplyr::bind_rows(holes_1d_better, holes_1d_geo), group = method)
#' @family plot
#' @export
explore_space_tour <- function(dt, group = NULL, theoretical = FALSE, color = method, palette = botanical_palettes$cherry, ...){
  group <- enexpr(group)
  color <- enexpr(color)

  # get start
  dt <- dt %>% dplyr::mutate(row_num = row_number())
  n_start <- get_start(dt) %>% pull(row_num)

  # see if any flip need to be done
  flip <- dt %>% flip_sign(group = !!group)
  basis <- flip$basis %>% bind_random_matrix()
  n_rand <- nrow(basis) - nrow(dt)

  col <- c(palette[as.factor(dt %>% dplyr::pull(!!color))],
           rep("#D3D3D3", n_rand))
  cex <- c(rep(3, nrow(dt)),
           rep(1, n_rand))
  cex[n_start] <- 5

  if (theoretical){
    col[nrow(dt)] <- palette[3]
    cex[nrow(dt)] <-  10
  }


  tourr::animate_xy(basis, col = col, cex = cex)


}
globalVariables(c("PC1", "PC2", "info"))
