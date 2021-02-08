#' A ggproto for drawing circle
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param space_alpha numeric; the alpha of the basis space
#' @param space_fill character; the colour of the space filling
#' @param space_color character; the colour of the space brim
#' @param cent_size  numeric; the size of the centre point
#' @param cent_alpha numeric; an alpha of the centre point
#' @param cent_color character; the colour of the centre point
#' @param ... other aesthetics inherent from \code{explore_space_pca()}
#' @examples
#' library(ggplot2)
#' space <- tibble::tibble(x0 = 0, y0 = 0, r = 5)
#' ggplot() +
#'   add_space(space) +
#'   theme_void() +
#'   theme(aspect.ratio = 1)
#' @family draw functions
#' @return a wrapper for drawing the space in \code{explore_space_pca()}
#' @export
add_space <- function(dt, space_alpha = 0.5, space_fill = "grey92", space_color = "white",
                      cent_size = 1, cent_alpha = 1, cent_color = "black", ...) {
  list(
    ggplot2::geom_point(
      data = dt,
      ggplot2::aes(x = .data$x0, y = .data$y0),
      size = cent_size, alpha = cent_alpha, color = cent_color
    ),
    ggforce::geom_circle(
      data = dt,
      ggplot2::aes(x0 = .data$x0, y0 = .data$y0, r = .data$r),
      alpha = space_alpha, fill = space_fill, color = space_color
    )
  )
}

#' A ggproto for drawing start points
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param start_size numeric; the size of start point
#' @param start_alpha numeric; the alpha of start point
#' @param start_color the variable to be coloured by
#' @param ... other aesthetics inherent from \code{explore_space_pca()}
#' @return a wrapper for drawing start points in \code{explore_space_pca()}
#' @examples
#' library(ggplot2)
#' # construct the space and start df for plotting
#' space <- tibble::tibble(x0 = 0, y0 = 0, r = 5)
#' start <- holes_1d_geo %>%
#'   compute_pca() %>%
#'   purrr::pluck("aug") %>%
#'   clean_method() %>%
#'   get_start()
#' ggplot() +
#'   add_space(dt = space) +
#'   add_start(dt = start, start_color = info) +
#'   theme_void() +
#'   theme(aspect.ratio = 1)
#' @family draw functions
#' @export
add_start <- function(dt, start_size = 5, start_alpha = 1, start_color = NULL, ...) {
  ggplot2::geom_point(
    data = dt,
    ggplot2::aes(x = .data$PC1, y = .data$PC2, color = {{ start_color }}),
    size = start_size, alpha = start_alpha
  )
}


#' A ggproto for drawing start points
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param end_size numeric; the size of the end point
#' @param end_alpha numeric; the alpha of the end point
#' @param end_color the variable to be coloured by
#' @param ... other aesthetics inherent from \code{explore_space_pca()}
#' @return a wrapper for drawing end points in \code{explore_space_pca()}
#' @family draw functions
#' @export
add_end <- function(dt, end_size = 5, end_alpha = 1, end_color = NULL, ...) {
  ggplot2::geom_point(
    data = dt,
    ggplot2::aes(x = .data$PC1, y = .data$PC2, color = {{ end_color }}),
    size = end_size, alpha = end_alpha
  )
}

#' A ggproto for drawing anchor points
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param anchor_size numeric; the size of the anchor points
#' @param anchor_alpha numeric; the alpha of the anchor points
#' @param anchor_color the variable to be coloured by
#' @param ... other aesthetics inherent from \code{explore_space_pca()}
#' @return a wrapper for drawing anchor points in \code{explore_space_pca()}
#' @family draw functions
#' @export
add_anchor <- function(dt, anchor_size = 3, anchor_alpha = 0.5, anchor_color = NULL, ...) {
  ggplot2::geom_point(
    data = dt,
    ggplot2::aes(x = .data$PC1, y = .data$PC2, color = {{ anchor_color }}),
    size = anchor_size, alpha = anchor_alpha
  )
}

#' A ggproto for drawing search points
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param search_size numeric; the size of the search points
#' @param search_alpha numeric; the alpha of the anchor points
#' @param search_color the variable to be coloured by
#' @param ... other aesthetics inherent from \code{explore_space_pca()}
#' @return a wrapper for drawing search points in \code{explore_space_pca()}
#' @family draw functions
#' @export
add_search <- function(dt, search_size = 0.5, search_alpha = 0.5, search_color = NULL, ...) {
  ggplot2::geom_point(
    data = dt,
    ggplot2::aes(x = .data$PC1, y = .data$PC2, color = {{ search_color }}),
    size = search_size, alpha = search_alpha
  )
}

#' A ggproto for drawing directional search points
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param dir_size numeric; the size of the directional search points in pseudo derivative search
#' @param dir_alpha numeric; the alpha of the directional search points in pseudo derivative search
#' @param dir_color the variable to be coloured by
#' @param ... other aesthetics inherent from \code{explore_space_pca()}
#' @return a wrapper for drawing directional search points (used in pseudo derivative search) with buffer in \code{explore_space_pca()}
#' @family draw functions
#' @export
add_dir_search <- function(dt, dir_size = 0.5, dir_alpha = 0.5, dir_color = NULL, ...) {
  ggplot2::geom_point(
    data = dt,
    ggplot2::aes(x = .data$PC1, y = .data$PC2, color = {{ dir_color }}),
    size = dir_size, alpha = dir_alpha
  )
}


#' A ggproto for drawing finish points
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param interp_last_size numeric; the size of the last interpolation points in each iteration
#' @param interp_last_alpha numeric; the alpha of the last interpolation points in each iteration
#' @param interp_last_color the variable to be coloured by
#' @param ... other aesthetics inherent from \code{explore_space_pca()}
#' @return a wrapper for drawing the last interpolation points of each iteration in \code{explore_space_pca()}
#' @family draw functions
#' @export
add_interp_last <- function(dt, interp_last_size = 3, interp_last_alpha = 1, interp_last_color = NULL, ...) {
  ggplot2::geom_point(
    data = dt,
    ggplot2::aes(x = .data$PC1, y = .data$PC2, color = {{ interp_last_color }}),
    size = interp_last_size, alpha = interp_last_alpha
  )
}

#' A ggproto for drawing interpolation path
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param interp_size numeric; the size of the interpolation path
#' @param interp_alpha numeric; the alpha of the interpolation path
#' @param interp_color the variable to be coloured by
#' @param interp_group the variable to label different interpolation path
#' @param ... other aesthetics inherent from \code{explore_space_pca()}
#' @return a wrapper for drawing the interpolation points in \code{explore_space_pca()}
#' @family draw functions
#' @export
add_interp <- function(dt, interp_size = 1.5, interp_alpha = NULL,
                       interp_color = NULL, interp_group = NULL, ...) {
  ggplot2::geom_path(
    data = dt,
    ggplot2::aes(
      x = .data$PC1, y = .data$PC2,
      alpha = {{ interp_alpha }}, group = {{ interp_group }}, color = {{ interp_color }}
    ),
    size = interp_size
  )
}

#' A ggproto for annotating the interrupted path
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param interrupt_size numeric; the size of the interruption path
#' @param interrupt_alpha numeric; the alpha of the interruption path
#' @param interrupt_color the variable to be coloured by
#' @param interrupt_group the variable to label different interruption
#' @param interrupt_linetype character; the linetype to annotate the interruption
#' @param ... other aesthetics inherent from \code{explore_space_pca()}
#' @return a wrapper for annotating the interruption in \code{explore_space_pca()}
#' @family draw functions
#' @export
add_interrupt <- function(dt, interrupt_size = 0.5, interrupt_alpha = NULL,
                          interrupt_color = NULL, interrupt_group = NULL, interrupt_linetype = "dashed", ...) {
  ggplot2::geom_path(
    data = dt,
    ggplot2::aes(
      x = .data$PC1, y = .data$PC2,
      alpha = {{ interrupt_alpha }}, group = {{ interrupt_group }}, color = {{ interrupt_color }}
    ),
    size = interrupt_size, linetype = interrupt_linetype
  )
}



#' A ggproto for annotating the symmetry of the starting points
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param anno_color character; the colour of the annotation line
#' @param anno_lty character; the linetype of the annotation line
#' @param anno_alpha numeric; the alpha of the annotation line
#' @param ... other aesthetics inherent from \code{explore_space_pca()}
#' @return a wrapper for annotating the symmetry of start points in \code{explore_space_pca()}
#' @family draw functions
#' @export
add_anno <- function(dt, anno_color = "black", anno_lty = "dashed", anno_alpha = 0.1, ...) {
  ggplot2::geom_line(
    data = dt,
    ggplot2::aes(x = .data$PC1, y = .data$PC2), group = 1,
    color = anno_color, linetype = anno_lty, alpha = anno_alpha
  )
}

#' A ggproto for drawing the theoretical basis, if applicable
#'
#' This is  a wrapper function used by \code{explore_space_pca()} and
#' should be be called directly by the user
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param theo_label character; a symbol to label the theoretical point
#' @param theo_size numeric; the size of the theoretical point
#' @param theo_alpha numeric; the alpha of the theoretical point
#' @param ... other aesthetics inherent from \code{explore_space_pca()}
#' @return a wrapper for drawing theoretical points in \code{explore_space_pca()}
#' @family draw functions
#' @export
add_theo <- function(dt, theo_label = "*", theo_size = 25, theo_alpha = 0.8, ...) {
  ggplot2::geom_text(
    data = dt,
    ggplot2::aes(x = .data$PC1, y = .data$PC2),
    label = theo_label, size = theo_size, alpha = theo_alpha
  )
}
