#' Plot the grand tour animation of the bases space in high dimension

#' @rdname explore_space_tour
#' @family main plot functions
#' @return
#' \describe{
#'   \item{\code{explore_space_tour()}}{an animation of the search path in the high-dimensional sphere}
#'   \item{\code{prep_space_tour()}}{a list containing various components needed for producing the animation}
#' }
#' @export
explore_space_tour <- function(..., axes = "bottomleft") {
  prep <- prep_space_tour(...)

  tourr::animate_xy(prep$basis,
                    col = prep$col, cex = prep$cex, pch = prep$pch,
                    edges = prep$edges, edges.col = prep$edges_col,
                    axes = axes
  )
}


#' @param dt a data object collected by the projection pursuit guided tour optimisation in \code{tourr}
#' @param group the variable to label different runs of the optimiser(s)
#' @param flip logical; if the sign flipping need to be performed
#' @param n_random numeric; the number of random basis to generate
#' @param color the variable to be coloured by
#' @param rand_size numeric; the size of random points
#' @param rand_color character; the color hex code for random points
#' @param point_size numeric; the size of points searched by the optimiser(s)
#' @param end_size numeric; the size of end points
#' @param theo_size numeric; the size of theoretical point(s)
#' @param theo_shape numeric; the shape symbol in the basic plot
#' @param theo_color character; the color of theoretical point(s)
#' @param palette the colour palette to be used
#' @param axes see [tourr::animate_xy()]
#' @param ... other argument passed to \code{tourr::animate_xy()} and \code{prep_space_tour()}
#' @examples
#' explore_space_tour(dplyr::bind_rows(holes_1d_better, holes_1d_geo),
#'   group = method, palette = botanical_palettes$fern[c(1, 6)]
#' )
#' @rdname explore_space_tour
#' @export
prep_space_tour <- function(dt, group = NULL, flip = FALSE, n_random = 2000,
                            color = NULL, rand_size = 1, rand_color = "#D3D3D3",
                            point_size = 1.5, end_size = 5, theo_size = 3,
                            theo_shape = 17, theo_color = "black",
                            palette = botanical_palettes$fern, ...) {
  if (rlang::quo_is_null(dplyr::enquo(color))) {
    message("map method to color")
    color <- dplyr::sym("method")
  }

  # get start
  dt <- dt %>%
    dplyr::mutate(row_num = dplyr::row_number()) %>%
    clean_method()

  if (flip){
    flip <- dt %>% flip_sign(group = {{ group }})
    basis <- flip$basis %>% bind_random_matrix(front = TRUE)
  } else{
    flip = list(dt = dt)
    basis <- dt %>%
      get_basis_matrix() %>%
      bind_random_matrix(n = n_random, front = TRUE)
  }

  n_rand <- nrow(basis) - nrow(dt)
  n_end <- get_best(flip$dt, group = {{ group }}) %>% dplyr::pull(.data$row_num) + n_rand

  edges_dt <- flip$dt %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::filter(.data$info == "interpolation") %>%
    dplyr::group_by(.data$method) %>%
    dplyr::mutate(id2 = dplyr::lead(.data$id, default = NA)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$id2))

  edges <- edges_dt %>%
    dplyr::select(.data$id, .data$id2) %>%
    dplyr::mutate(id = .data$id + n_rand, id2 = .data$id2 + n_rand) %>%
    as.matrix()

  edges_col <- palette[as.factor(edges_dt %>% dplyr::pull({{ color }}))]

  col <- c(
    rep(rand_color, n_rand),
    palette[as.factor(dt %>% dplyr::pull({{ color }}))]
  )
  cex <- c(
    rep(rand_size, n_rand),
    rep(point_size, nrow(dt))
  )
  cex[n_end] <- end_size

  pch <- rep(20, nrow(basis))

  if ("theoretical" %in% dt$info) {
    theo_row_num <- dt %>%
      dplyr::filter(.data$info == "theoretical") %>%
      dplyr::pull(.data$row_num)

    col[theo_row_num + n_rand] <- theo_color
    cex[theo_row_num + n_rand] <- theo_size
    pch[theo_row_num + n_rand] <- theo_shape
  }

  return(list(
    basis = basis,
    col = col,
    cex = cex,
    pch = pch,
    edges = edges,
    edges_col = edges_col
  ))
}
