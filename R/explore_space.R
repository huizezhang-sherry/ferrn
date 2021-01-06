#' Flip the sign of a group of bases
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param group The grouping variable, useful when there are multiple algorithms in the data object
#' @examples
#' dplyr::bind_rows(holes_1d_geo, holes_1d_better) %>%
#'   flip_sign(group = method) %>%
#'   str(max = 1)
#' @export
flip_sign <- function(dt, group = NULL) {
  group <- enexpr(group)

  if (!is.null(group)) {
    group_name <- dt %>%
      get_best(group = !!group) %>%
      pull(!!group)
    num_method <- group_name %>% length()
    max_bases <- dt %>%
      get_best(group = !!group) %>%
      pull(basis)
    max_id <- max_bases %>% vapply(function(x) abs(x) %>% which.max(), numeric(1))
    extract <- function(matrix, pos) matrix[pos %% nrow(matrix), (pos %/% nrow(matrix)) + 1]
    max_sign <- mapply(extract, max_bases, max_id) %>% sign()
    group_to_flip <- group_name[max_sign < 0]
    group_to_flip <- group_to_flip[group_to_flip != "theoretical"]

    if (length(group_to_flip) == 0) {
      message("there's no flip of the sign")
      basis <- dt %>% get_basis_matrix()
      dt_obj <- dt
    } else {
      message("signs in all the bases will be flipped in group ", group_to_flip, "\n")
      basis1 <- dt %>%
        filter(!!group %in% group_to_flip & !!group != "theoretical") %>%
        get_basis_matrix() %>%
        -.
      basis <- basis1 %>%
        rbind(dt %>% filter(!(!!group) %in% group_to_flip | !!group == "theoretical") %>% get_basis_matrix())

      dt_obj <- dt %>%
        filter(!!group %in% group_to_flip & !!group != "theoretical") %>%
        dplyr::bind_rows(dt %>% filter(!(!!group) %in% group_to_flip | !!group == "theoretical"))
    }
  } else {
    basis <- dt %>% get_basis_matrix()
    dt_obj <- dt
  }

  return(list(
    basis = basis,
    flip = !is.null(group),
    dt = dt_obj
  ))
}


#' Compute PCA for the projection bases
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param group The grouping variable, useful when there are multiple algorithms in the data object
#' @param random Boolean, if the random data from the high dimensional sphere should be bounded
#' @examples
#' dplyr::bind_rows(holes_1d_geo, holes_1d_better) %>% compute_pca(group = method)
#' @export
compute_pca <- function(dt, group = NULL, random = TRUE) {
  if (!"basis" %in% colnames(dt)) {
    stop("You need to have a basis column that contains the projection basis!")
  }

  group <- enexpr(group)
  info <- sym("info")
  tries <- sym("tries")
  loop <- sym("loop")

  num_col <- ncol(dt$basis[[1]])
  num_row <- nrow(dt$basis[[1]])

  dt <- dt %>% mutate(row_num = row_number())

  flip <- flip_sign(dt, group = !!group)
  basis <- flip$basis

  # Compute PCA
  if (num_col == 1) {
    pca <- basis %>%
      bind_random_matrix() %>%
      stats::prcomp(scale. = TRUE)
    v <- suppressMessages(pca$x %>% as_tibble(.name_repair = "minimal"))
    if (flip$flip) dt_flip <- flip$dt else dt_flip <- dt
    aug <- dt_flip %>%
      bind_random() %>%
      bind_cols(v)
  } else if (num_col == 2) {
    message("Ferrn will perform PCA separately on each dimension")
    basis_2d <- basis %>% bind_random_matrix()
    pca1 <- stats::prcomp(basis_2d[, 1:num_row], scale. = TRUE)
    pca2 <- stats::prcomp(basis_2d[, (num_row + 1):(2 * num_row)], scale. = TRUE)
    pca <- list(pca1, pca2)

    v1 <- suppressMessages(-pca1$x %>% as_tibble(.name_repair = "minimal"))
    v2 <- suppressMessages(-pca2$x %>% as_tibble(.name_repair = "minimal"))
    colnames(v2)[1:num_row] <- c(paste0("PC", seq(num_row + 1, 2 * num_row)))

    if (flip$flip) dt_flip <- flip$dt else dt_flip <- dt
    aug <- dt_flip %>%
      bind_random() %>%
      bind_cols(v1) %>%
      bind_cols(v2)
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
#' @param pca Boolean, if \code{compute_pca()} should be performed on the data
#' @param group The grouping variable, useful when there are multiple algorithms in the data object
#' @param color A variable from the object that the diagnostic plot should be colored by
#' @param animate Boolean, if the plot should be animated
#' @param theo_size the size of theoretical point
#' @param rand_size the size of random point
#' @examples
#' dplyr::bind_rows(holes_1d_geo, holes_1d_better) %>%
#'   bind_theoretical(matrix(c(0, 1, 0, 0, 0), nrow = 5),
#'     index = tourr::holes(), raw_data = boa5
#'   ) %>%
#'   explore_space_pca(group = method) +
#'   scale_color_botanical(palette = "cherry")
#' @import ggplot2
#' @importFrom dplyr filter bind_cols group_by mutate ungroup  sym enexpr pull row_number
#' @importFrom rlang "!!"
#' @importFrom tibble as_tibble
#' @importFrom stringr str_detect
#' @family plot
#' @export
explore_space_pca <- function(dt, pca = TRUE, group = NULL, color = NULL,
                              cir_alpha = 0.5, cir_fill = "grey92", cir_color = "white",
                              cent_size = 1, cent_alpha = 1, cent_color = "black",
                              start_size = 5, start_alpha = 1,
                              anchor_size = 3, anchor_alpha = 1,
                              search_size = 0.5, search_alpha = 0.5,
                              interp_size = 1.5, interrupt_size = 0.5,
                              anno_color = "black", anno_lty = "dashed", anno_alpha = 0.1,
                              theo_size = 25, theo_label = "*", animate = FALSE) {

  group <- enexpr(group)
  if (is.null(group)) color <- enexpr(color) else color <- group

  args <- rlang::fn_fmls()

  get_arg <- function(args, prefix) args[names(args) %>% stringr::str_detect(prefix)]
  fix_prefix <- function(args, new_prefix) paste0(new_prefix, stringr::str_extract(names(args), "\\_(.*)"))

  tidy_arg <- function(arg, prefix, new_prefix) {
    obj <- get_arg(arg, prefix)
    names(obj) <- obj %>% fix_prefix(new_prefix)
    return(obj)
  }

  circle <- get_arg(args, "cir")
  center <- get_arg(args, "cent")
  start <- tidy_arg(args, "start", "pnt")
  anchor <- tidy_arg(args, "anchor", "pnt")
  search <- tidy_arg(args, "search", "pnt")
  interp <- tidy_arg(args, "interp", "path")
  interrupt <- tidy_arg(args, "interrupt", "path")
  anno <- get_arg(args, "anno")
  theo <- get_arg(args, "theo")

  if (pca) {
    dt <- compute_pca(dt, group = !!group)$aug
  }

  dt <- dt %>% clean_method()

  p <- dt %>%
    ggplot() +
    # set up
    do.call(draw_circle, c(list(dt = estimate_circle(dt)), circle)) +
    do.call(draw_center, c(list(dt = get_center(dt)), center)) +
    # add points
    do.call(draw_points, c(list(dt = get_start(dt), pnt_color = color), start)) +
    do.call(draw_points, c(list(dt = get_anchor(dt), pnt_color = color), anchor)) +
    do.call(draw_points, c(list(dt = get_search(dt), pnt_color = color), search)) +
    # add path
    do.call(draw_path, c(
      list(
        dt = get_interp(dt, group = !!group),
        path_color = color, path_group = group, path_alpha = sym("id")
      ),
      interp
    )) +
    do.call(draw_path, c(
      list(
        dt = get_interrupt(dt),
        path_color = color, path_group = sym("tries"), linetype = "dashed"
      ),
      interrupt
    )) +
    scale_alpha_continuous(range = c(0.3, 1), guide = "none") +
    # add annotation
    do.call(draw_anno, c(list(dt = get_start(dt)), anno)) +
    # theme
    theme_void() +
    theme(aspect.ratio = 1, legend.position = "bottom", legend.title = element_blank())

  if ("theoretical" %in% dt$info) {
    p <- p + do.call(draw_theo, c(list(dt = get_theo(dt)), theo))
  }

  if (animate) {
    p <- p + theme(legend.position = "none") +
      gganimate::transition_states(id) +
      gganimate::shadow_mark()
  }

  p
}

#' Plot the grand tour animation of the projection bases space
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param group The grouping variable, useful when there are multiple algorithms in the data object
#' @param theoretical Boolean, if the theoretical bases have been inlcuded in the data object
#' @param color A variable from the object that the diagnostic plot should be colored by
#' @param palette The color palette to use
#' @param ... Additional argument passed to \code{tourr::animate_xy()}
#' @examples
#' \dontrun{
#' explore_space_tour(dplyr::bind_rows(holes_1d_better, holes_1d_geo), group = method)
#' }
#' @family plot
#' @rdname explore_space_tour
#' @export
prep_space_tour <- function(dt, group = NULL, theoretical = FALSE,
                            color = method, rand_size = 9, point_size = 1.5, theo_size = 10,
                            palette = botanical_palettes$cherry, ...) {
  group <- enexpr(group)
  color <- enexpr(color)

  # get start
  dt <- dt %>% dplyr::mutate(row_num = row_number())
  n_start <- get_start(dt) %>% pull(row_num)

  # see if any flip need to be done
  flip <- dt %>% flip_sign(group = !!group)
  basis <- flip$basis %>% bind_random_matrix(front = TRUE)
  n_rand <- nrow(basis) - nrow(dt)

  edges_dt <- flip$dt %>%
    mutate(id = row_number()) %>%
    filter(info == "interpolation") %>%
    group_by(method) %>%
    mutate(id2 = dplyr::lead(id, defualt = NA)) %>%
    ungroup() %>%
    filter(!is.na(id2))

  edges <- edges_dt %>%
    dplyr::select(id, id2) %>%
    mutate(id = id + n_rand, id2 = id2 + n_rand) %>%
    as.matrix()

  edges_col <- palette[as.factor(edges_dt %>% dplyr::pull(!!color))]

  col <- c(
    rep("#D3D3D3", n_rand),
    palette[as.factor(dt %>% dplyr::pull(!!color))]
  )
  cex <- c(
    rep(rand_size, n_rand),
    rep(point_size, nrow(dt))
  )
  cex[n_start] <- 5

  if (theoretical) {
    col[nrow(dt)] <- "black"
    cex[nrow(dt)] <- theo_size
  }

  return(list(
    basis = basis,
    col = col,
    cex = cex,
    edges = edges,
    edges_col = edges_col
  ))
}
globalVariables(c("PC1", "PC2", "info"))


#' @rdname explore_space_tour
#' @export
explore_space_tour <- function(...) {
  prep <- prep_space_tour(...)

  tourr::animate_xy(prep$basis, col = prep$col, cex = prep$cex, edges = prep$edges, edges.col = prep$edges_col)
}
