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
add_space <- function(dt, cir_alpha = 0.5, cir_fill = "grey92", cir_color = "white") {
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
add_center <- function(dt, cent_size = 1, cent_alpha = 1, cent_color = "black") {
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
add_start <- function(dt, start_size = 5, start_alpha = 1, start_color = NULL) {
  color <- enexpr(start_color)

  geom_point(
    data = dt,
    aes(x = PC1, y = PC2, color = !!color),
    size = start_size, alpha = start_alpha
  )
}

add_anchor <- function(dt, anchor_size = 3, anchor_alpha = 1, anchor_color = NULL) {
  color <- enexpr(anchor_color)

  geom_point(
    data = dt,
    aes(x = PC1, y = PC2, color = !!color),
    size = anchor_size, alpha = anchor_alpha
  )
}

add_search <- function(dt, search_size = 0.5, search_alpha = 1, search_color = NULL) {
  color <- enexpr(search_color)

  geom_point(
    data = dt,
    aes(x = PC1, y = PC2, color = !!color),
    size = search_size, alpha = search_alpha
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
add_interp <- function(dt, interp_size = 1.5, interp_alpha = NULL,
                      interp_color = NULL, interp_group = NULL,...) {
  alpha <- enexpr(interp_alpha)
  group <- enexpr(interp_group)
  color <- enexpr(interp_color)

  geom_path(
    data = dt,
    aes(x = PC1, y = PC2, alpha = !!alpha, group = !!group, color = !!color),
    size = interp_size, ...
  )
}

add_interrupt <- function(dt, interrupt_size = 1.5, interrupt_alpha = NULL,
                     interrupt_color = NULL, interrupt_group = NULL, interrupt_linetype = "dashed", ...) {
  alpha <- enexpr(interrupt_alpha)
  group <- enexpr(interrupt_group)
  color <- enexpr(interrupt_color)

  geom_path(
    data = dt,
    aes(x = PC1, y = PC2, alpha = !!alpha, group = !!group, color = !!color),
    size = interrupt_size, linetype = interrupt_linetype, ...
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
add_anno <- function(dt, anno_color = "black", anno_lty = "dashed", anno_alpha = 0.1) {
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
add_theo <- function(dt, theo_label = "*", theo_size = 25) {
  geom_text(
    data = dt,
    aes(x = PC1, y = PC2),
    label = theo_label, size = theo_size
  )
}

# add_theo <- function(...){
#
#   draw_theo(dt = get_theo(dt), ...)
# }

