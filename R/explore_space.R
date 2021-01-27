#' Flip the sign of a group of bases
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param group The grouping variable, useful when there are multiple algorithms in the data object
#' @param ... other arguments passed
#' @examples
#' dplyr::bind_rows(holes_1d_geo, holes_1d_better) %>%
#'   flip_sign(group = method) %>%
#'   str(max = 1)
#' @export
flip_sign <- function(dt, group = NULL, ...) {
  if (!rlang::quo_is_null(dplyr::enquo(group))) {
    group_name <- dt %>%
      get_best(group = {{ group }}) %>%
      dplyr::pull({{ group }})
    num_method <- group_name %>% length()
    max_bases <- dt %>%
      get_best(group = {{ group }}) %>%
      dplyr::pull(basis)
    max_id <- max_bases %>% vapply(function(x) abs(x) %>% which.max(), numeric(1))
    extract <- function(matrix, pos) matrix[(pos - 1) %% nrow(matrix) + 1, ((pos - 1) %/% nrow(matrix)) + 1]
    max_sign <- mapply(extract, max_bases, max_id) %>% sign()
    group_to_flip <- group_name[max_sign < 0]
    group_to_flip <- group_to_flip[group_to_flip != "theoretical"]

    if (length(group_to_flip) == 0) {
      message("there's no flip of the sign")
      basis <- dt %>% get_basis_matrix()
      dt_obj <- dt
    } else {
      message(paste("signs in all the bases will be flipped in group", group_to_flip, "\n"))
      basis <- dt %>%
        dplyr::mutate(basis = ifelse({{ group }} %in% group_to_flip & {{ group }} != "theoretical",
          purrr::map(basis, ~ -.x), basis
        )) %>%
        get_basis_matrix()
      dt_obj <- dt
    }
  } else {
    basis <- dt %>% get_basis_matrix()
    dt_obj <- dt
  }

  return(list(
    basis = basis,
    flip = !rlang::quo_is_null(dplyr::enquo(group)),
    dt = dt_obj
  ))
}


#' Compute PCA for the projection bases
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param group The grouping variable, useful when there are multiple algorithms in the data object
#' @param random Boolean, if the random data from the high dimensional sphere should be bounded
#' @param flip boolean, if the sign flipping need to be computed
#' @param ... other arguments passed
#' @examples
#' dplyr::bind_rows(holes_1d_geo, holes_1d_better) %>% compute_pca(group = method)
#' @export
compute_pca <- function(dt, group = NULL, random = TRUE, flip = TRUE, ...) {
  if (!"basis" %in% colnames(dt)) {
    stop("You need to have a basis column that contains the projection basis!")
  }

  num_col <- ncol(dt$basis[[1]])
  num_row <- nrow(dt$basis[[1]])

  group <- dplyr::enexpr(group)
  dt <- dt %>% dplyr::mutate(row_num = dplyr::row_number())

  if (flip) {
    flip <- flip_sign(dt, group = {{ group }})
    basis <- flip$basis
  } else {
    flip <- list(
      basis = dt %>% get_basis_matrix(),
      flip = FALSE
    )
    basis <- flip$basis
  }

  # Compute PCA
  if (num_col == 1) {
    pca <- basis %>%
      bind_random_matrix() %>%
      stats::prcomp(scale. = TRUE)
    v <- suppressMessages(pca$x %>% tibble::as_tibble(.name_repair = "minimal"))
    if (flip$flip) dt_flip <- flip$dt else dt_flip <- dt
    aug <- dt_flip %>%
      bind_random() %>%
      dplyr::bind_cols(v)

    aug <- aug %>% clean_method()
  } else if (num_col == 2) {
    message("Ferrn will perform PCA separately on each dimension")
    basis_2d <- basis %>% bind_random_matrix()
    pca1 <- stats::prcomp(basis_2d[, 1:num_row], scale. = TRUE)
    pca2 <- stats::prcomp(basis_2d[, (num_row + 1):(2 * num_row)], scale. = TRUE)
    pca <- list(pca1, pca2)

    v1 <- suppressMessages(-pca1$x %>% tibble::as_tibble(.name_repair = "minimal"))
    v2 <- suppressMessages(-pca2$x %>% tibble::as_tibble(.name_repair = "minimal"))
    colnames(v2)[1:num_row] <- c(paste0("PC", seq(num_row + 1, 2 * num_row)))

    if (flip$flip) dt_flip <- flip$dt else dt_flip <- dt
    aug <- dt_flip %>%
      bind_random() %>%
      dplyr::bind_cols(v1) %>%
      dplyr::bind_cols(v2)

    aug <- aug %>% clean_method()
  } else {
    stop("ferrn can only handle 1d or 2d bases!")
  }

  return(list(pca_summary = pca, aug = aug))
}




#' Plot the PCA projection of the projection bases space
#'
#' The set of functions returns a primary ggplot object
#' that plots the data object in a space reduced by PCA.
#' \code{compute_pca()} computes the PCA and \code{explore_space_pca()} does the plotting.`
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param details if all the compoenents of the optimisers should be shown. With \code{details = FALSE} only the start, end and interpolation path will be shown
#' @param pca Boolean, if \code{compute_pca()} should be performed on the data
#' @param group The grouping variable, useful when there are multiple algorithms in the data object
#' @param color A variable from the object that the diagnostic plot should be colored by
#' @param ... different argument passed to \code{add_*()}
#' @param animate Boolean, if the plot should be animated
#' @examples
#' dplyr::bind_rows(holes_1d_geo, holes_1d_better) %>%
#'   bind_theoretical(matrix(c(0, 1, 0, 0, 0), nrow = 5),
#'     index = tourr::holes(), raw_data = boa5
#'   ) %>%
#'   explore_space_pca(group = method, details = TRUE) +
#'   scale_color_discrete_botanical(palette = "cherry")
#' @family plot
#' @export
explore_space_pca <- function(dt, details = FALSE, pca = TRUE, group = NULL, color = NULL,
                              ..., animate = FALSE) {
  if (rlang::quo_is_null(dplyr::enquo(color))) color <- dplyr::enexpr(group)

  if (pca) dt <- compute_pca(dt, group = {{ group }}, ...) %>% purrr::pluck("aug")

  p <- ggplot2::ggplot() +
    # set up
    add_space(dt = get_space_param(dt), ...) +
    # add points
    add_start(dt = get_start(dt), start_color = {{ color }}, ...) +
    add_end(dt = get_best(dt, group = {{ group }}, ...), end_color = {{ color }}, ...) +
    # add path
    add_interp(
      dt = get_interp(dt, group = {{ group }}),
      interp_alpha = .data[["id"]], interp_color = {{ color }}, interp_group = {{ group }}, ...
    ) +
    # theme
    ggplot2::scale_alpha_continuous(range = c(0.3, 1), guide = "none") +
    ggplot2::theme_void() +
    ggplot2::theme(aspect.ratio = 1, legend.position = "bottom", legend.title = ggplot2::element_blank())

  if (details) {
    p <- p + add_anchor(dt = get_anchor(dt), anchor_color = {{ color }}, ...) +
      add_search(dt = get_search(dt), search_color = {{ color }}, ...) +
      add_dir_search(dt = get_dir_search_transformed(dt, ...), dir_color = {{ color }}, ...) +
      add_finish(dt = get_interrupt_finish(dt, group = {{ group }}, ...), finish_color = {{ color }}, ...) +
      # add annotation
      add_interrupt(
        dt = get_interrupt(dt, group = {{ group }}, ...),
        interrupt_color = {{ color }}, interrupt_group = interaction(.data[["tries"]], {{ group }}), ...
      ) +
      add_anno(dt = get_start(dt), ...)
  }

  if (animate) {
    p <- ggplot2::ggplot() +
      # set up
      add_space(dt = get_space_param(dt), ...) +
      # add points
      add_start(dt = get_start(dt) %>% dplyr::select(-.data[["id"]]), start_color = {{ color }}, ...) +
      # add path
      add_interp(
        dt = get_interp(dt, group = {{ group }}),
        interp_alpha = .data[["id"]], interp_color = {{ color }}, interp_group = {{ group }}, ...
      ) +
      # theme
      ggplot2::scale_alpha_continuous(range = c(0.3, 1), guide = "none") +
      ggplot2::theme_void() +
      ggplot2::theme(aspect.ratio = 1, legend.position = "bottom", legend.title = ggplot2::element_blank()) +
      gganimate::transition_reveal(along = .data[["id"]])
  }

  if ("theoretical" %in% dt$info) {
    p <- p +
      add_theo(dt = get_theo(dt), ...)
  }


  p
}

#' Plot the grand tour animation of the projection bases space
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param group The grouping variable, useful when there are multiple algorithms in the data object
#' @param color A variable from the object that the diagnostic plot should be colored by
#' @param rand_size random point size
#' @param point_size other point size
#' @param end_size the size of the ending point
#' @param theo_size theoretical point size
#' @param theo_shape the numerical value of basic plot shape
#' @param palette The color palette to use
#' @param ... Additional argument passed to \code{tourr::animate_xy()}
#' @examples
#' \dontrun{
#' explore_space_tour(dplyr::bind_rows(holes_1d_better, holes_1d_geo),
#'   group = method, palette = botanical_palettes$fern[c(1, 6)]
#' )
#' }
#' @family plot
#' @rdname explore_space_tour
#' @export
prep_space_tour <- function(dt, group = NULL,
                            color = NULL, rand_size = 1, point_size = 1.5, end_size = 5,
                            theo_size = 3, theo_shape = 17,
                            palette = botanical_palettes$fern, ...) {
  if (rlang::quo_is_null(dplyr::enquo(color))) {
    message("map method to color")
    color <- dplyr::sym("method")
  }

  # get start
  dt <- dt %>%
    dplyr::mutate(row_num = dplyr::row_number()) %>%
    clean_method()

  # see if any flip need to be done
  flip <- dt %>% flip_sign(group = {{ group }})
  basis <- flip$basis %>% bind_random_matrix(front = TRUE)
  n_rand <- nrow(basis) - nrow(dt)
  n_end <- get_best(flip$dt, group = {{ group }}) %>% dplyr::pull(.data$row_num) + n_rand

  edges_dt <- flip$dt %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::filter(.data$info == "interpolation") %>%
    dplyr::group_by(.data$method) %>%
    dplyr::mutate(id2 = dplyr::lead(.data$id, defualt = NA)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$id2))

  edges <- edges_dt %>%
    dplyr::select(.data$id, .data$id2) %>%
    dplyr::mutate(id = .data$id + n_rand, id2 = .data$id2 + n_rand) %>%
    as.matrix()

  edges_col <- palette[as.factor(edges_dt %>% dplyr::pull({{ color }}))]

  col <- c(
    rep("#D3D3D3", n_rand),
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

    col[theo_row_num + n_rand] <- "black"
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

#' @rdname explore_space_tour
#' @export
explore_space_tour <- function(...) {
  prep <- prep_space_tour(...)

  tourr::animate_xy(prep$basis,
    col = prep$col, cex = prep$cex, pch = prep$pch,
    edges = prep$edges, edges.col = prep$edges_col,
    axes = "bottomleft"
  )
}
