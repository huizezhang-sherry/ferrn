#' A ggproto for drawing circle
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param cir_alpha an alpha value for the transparency of the circle
#' @param cir_fill the color of the circle filling
#' @param cir_color the color of the circle brim
#' @family draw functions
draw_circle <- function(dt, cir_alpha = 0.5, cir_fill = "grey92", cir_color = "white") {
  ggforce::geom_circle(
    data = dt,
    aes(x0 = .data$x0, y0 = .data$y0, r = .data$r),
    alpha = cir_alpha, fill = cir_fill, color = cir_color
  )
}

#' A ggproto for drawing the circle center
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param cent_size the center point size
#' @param cent_alpha an alpha value for the transparency of the center point
#' @param cent_color the color of the center
#' @family draw functions
draw_center <- function(dt, cent_size = 1, cent_alpha = 1, cent_color = "black") {
  geom_point(
    data = dt,
    aes(x = .data$x0, y = .data$y0),
    size = cent_size, alpha = cent_alpha, color = cent_color
  )
}

#' A ggproto for drawing points
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param pnt_size the point size
#' @param pnt_alpha an alpha value for the transparency of the point
#' @param pnt_color the color of the points
#' @family draw functions
draw_points <- function(dt, pnt_size = 1, pnt_alpha = 1, pnt_color = NULL) {
  color <- enexpr(pnt_color)

  geom_point(
    data = dt,
    aes(x = PC1, y = PC2, color = !!color),
    size = pnt_size, alpha = pnt_alpha
  )
}



#' A ggproto for drawing the path
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param path_size the size of the path
#' @param path_alpha an alpha value for the transparency of the path
#' @param path_color the color of the path
#' @param path_group a group variable for path connection
#' @param ... other argument passed to \code{draw_path()}
#' @family draw functions
draw_path <- function(dt, path_size = 1.5, path_alpha = NULL,
                      path_color = NULL, path_group = NULL,...) {
  alpha <- enexpr(path_alpha)
  group <- enexpr(path_group)
  color <- enexpr(path_color)

  geom_path(
    data = dt,
    aes(x = PC1, y = PC2, alpha = !!alpha, group = !!group, color = !!color),
    size = path_size, ...
  )
}

#' A ggproto for annotating the symmetry of the starting points
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param anno_color the color of the annotation
#' @param anno_lty the linetype of the annotation
#' @param anno_alpha an alpha value for the transparency of the annotation
#' @family draw functions
draw_anno <- function(dt, anno_color = "black", anno_lty = "dashed", anno_alpha = 0.1) {
  geom_line(
    data = dt,
    aes(x = PC1, y = PC2), group = 1,
    color = anno_color, linetype = anno_lty, alpha = anno_alpha
  )
}

#' A ggproto for drawing the theoretical basis, if applicable
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param theo_label the symbol used for labelling the theoretical basis
#' @param theo_size the size of the label
#' @family draw functions
draw_theo <- function(dt, theo_label = "*", theo_size = 25) {
  geom_text(
    data = dt,
    aes(x = PC1, y = PC2),
    label = theo_label, size = theo_size
  )
}
#' Estimate the radius of the background circle based on the randomly generated points
#'
#' The space of projected bases is a circle when reduced to 2D. A radius is estimated using
#' the largest distance from the bases in the data object to the center point.
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @importFrom rlang .data
#' @family draw functions
estimate_circle <- function(dt) {
  center <- dt %>% get_center()
  x0 <- center$x0
  y0 <- center$y0

  r <- dt %>%
    dplyr::mutate(dist = sqrt((PC1 - x0)^2 + (PC2 - y0)^2)) %>%
    dplyr::filter(.data$dist == max(.data$dist)) %>%
    pull(.data$dist)

  tibble::tibble(x0 = x0, y0 = y0, r = r)
}
