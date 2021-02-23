#' Plot the trace the search progression
#'
#' Trace the index value of search/ interpolation points in guided tour optimisation
#'
#' @param dt a data object collected by the projection pursuit guided tour optimisation in \code{tourr}
#' @param iter the variable to be plotted on the x-axis
#' @param color the variable to be coloured by
#' @param group the variable to label different runs of the optimiser(s)
#' @param cutoff numeric; if the number of interpolating points is smaller than \code{cutoff}, all the interpolation points will be plotted as dots
#' @param target_size numeric; the size of target points in the interpolation
#' @param interp_size numeric; the size of interpolation points
#' @param accuracy_x numeric; If the difference of two neighbour x-labels is smaller than \code{accuracy_x}, only one of them will be displayed. Used for better axis label
#' @param accuracy_y numeric; the precision of y-axis label
#' @examples
#' # Compare the trace of interpolated points in two algorithms
#' holes_1d_better %>%
#'   explore_trace_interp(interp_size = 2) +
#'   scale_color_continuous_botanical(palette = "fern")
#' @family main plot functions
#' @return a ggplot object for diagnosing how the index value progresses during the interpolation
#' @export
#' @rdname explore_trace
explore_trace_interp <- function(dt, iter = NULL, color = NULL, group = NULL, cutoff = 50,
                                 target_size = 3, interp_size = 1,
                                 accuracy_x = 5, accuracy_y = 0.01) {

  # check there is a column called info, there is a value called interpolation
  # check other variables as well

  if (rlang::quo_is_null(dplyr::enquo(iter))) {
    message("map id to the x-axis")
    iter <- dplyr::sym("id")
  }

  if (rlang::quo_is_null(dplyr::enquo(color))) {

    if (!rlang::quo_is_null(dplyr::enquo(group))) {
      message("map the group variable to color")
      color <- dplyr::enexpr(group)
    }

    message("map tries to color")
    color <- dplyr::sym("tries")
  }

  if ("search_geodesic" %in% dt$method) dt <- dt %>% clean_method()

  dt_interp <- get_interp(dt, group = {{ group }})
  interp_last <- dplyr::bind_rows(get_start(dt), get_interp_last(dt, group = {{ group }}))
  tick_x <- format_label(interp_last %>% dplyr::pull({{ iter }}), accuracy = accuracy_x)
  tick_y <- format_label(interp_last$index_val, accuracy = accuracy_y)

  if (rlang::quo_is_null(dplyr::enquo(group))) {
    a <- dt_interp %>%
      dplyr::summarise(
        row = dplyr::n(),
        diff = max({{ iter }}) - min({{ iter }}) + 1
      )
  } else {
    a <- dt_interp %>%
      dplyr::group_by(group = {{ group }}) %>%
      dplyr::summarise(
        row = dplyr::n(),
        diff = max({{ iter }}) - min({{ iter }}) + 1
      )
  }

  if (!all(a$row == a$diff, TRUE)) {
    stop("there is gap(s) in the variable iter!")
  }

  p <- dt_interp %>%
    ggplot2::ggplot(ggplot2::aes(x = {{ iter }}, y = .data$index_val, group = {{ group }})) +
    ggplot2::geom_line() +
    ggplot2::geom_point(data = interp_last, ggplot2::aes(col = {{ color }}), size = target_size) +
    ggplot2::geom_vline(data = interp_last, ggplot2::aes(xintercept = {{ iter }}), lty = "dashed", alpha = 0.3) +
    ggplot2::scale_x_continuous(breaks = tick_x) +
    ggplot2::scale_y_continuous(breaks = tick_y, labels = scales::label_number(accuracy = accuracy_y)) +
    theme_fern() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylab("Index value") +
    ggplot2::xlab("Time")

  # show intermediate points if not too many
  if (nrow(dt_interp) < cutoff) {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(x = {{ iter }}, y = .data$index_val, col = {{ color }}),
      size = interp_size
    )
  }

  p
}

#' Plot the count in each iteration
#'
#' @param dt a data object collected by the projection pursuit guided tour optimisation in \code{tourr}
#' @param iter the variable to be plotted on the x-axis
#' @param color the variable to be coloured by
#' @param cutoff numeric; if the number of searches in one iteration is smaller than \code{cutoff}, a point geom, rather than boxplot geom, will be used.
#' @param extend_lower a numeric for extending the y-axis to display text labels
#' @param ... arguments passed into geom_label_repel() for displaying text labels
#' @examples
#' # Summary plots for search points in two algorithms
#' library(patchwork)
#' library(dplyr)
#' library(ggplot2)
#' p1 <- holes_1d_better %>% explore_trace_search() +
#'   scale_color_continuous_botanical(palette = "fern")
#' p2 <- holes_2d_better_max_tries %>% explore_trace_search() +
#'   scale_color_continuous_botanical(palette = "daisy")
#' p1 / p2
#' @family main plot functions
#' @return a ggplot object for diagnosing how many points the optimiser(s) have searched
#' @export
explore_trace_search <- function(dt, iter = NULL, color = NULL, cutoff = 15, extend_lower = 0.95, ...) {

  if (rlang::quo_is_null(dplyr::enquo(iter))) {
    message("map tries to the x-axis")
    iter <- dplyr::sym("tries")
  }

  if (rlang::quo_is_null(dplyr::enquo(color))) {
    message("map tries to color")
    color <- dplyr::sym("tries")
  }


  search <- dt %>%
    dplyr::filter(.data$info != "interpolation")
  search_count <- get_search_count(search, iter = {{ iter }})


  largest <- dt %>%
    dplyr::pull({{ iter }}) %>%
    max()
  lowest_index_val <- min(dt$index_val)

  # filter data plotted with boxplot geom
  box_id <- search_count %>%
    dplyr::filter(.data$n >= cutoff) %>%
    dplyr::pull({{ iter }})
  search_box <- search %>%
    dplyr::filter({{ iter }} %in% box_id)

  # filter data plotted with point geom
  point_id <- search_count %>%
    dplyr::filter(.data$n < cutoff) %>%
    dplyr::pull({{ iter }})
  search_point <- search %>%
    dplyr::filter({{ iter }} %in% point_id)

  # filter the target points
  search_target <- search %>%
    dplyr::group_by({{ iter }}) %>%
    dplyr::filter(.data$index_val == max(.data$index_val))

  label_1 <- search_count %>% dplyr::filter({{ iter }} != largest)
  label_2 <- search_count %>% dplyr::filter({{ iter }} == largest)


  p <- search %>%
    ggplot2::ggplot(ggplot2::aes(x = {{ iter }}, y = .data$index_val, col = {{ color }})) +
    # point summary
    ggplot2::geom_point(data = search_point) +
    # boxplot summary
    ggplot2::geom_boxplot(data = search_box, ggplot2::aes(group = {{ iter }})) +
    ggplot2::geom_boxplot(
      data = search_box %>% dplyr::filter({{ iter }} == largest),
      color = "grey"
    ) +
    # target points
    ggplot2::geom_point(data = search_target %>% dplyr::filter({{ iter }} != largest), ggplot2::aes(group = 1), size = 3) +
    ggplot2::geom_line(data = search_target %>% dplyr::filter({{ iter }} != largest), ggplot2::aes(group = 1)) +
    ggplot2::geom_point(
      data = search_target %>% dplyr::filter({{ iter }} == largest),
      col = "grey", size = 3
    ) +
    # numeric summary box
    ggrepel::geom_label_repel(
      data = label_1,
      ggplot2::aes(y = 0.99 * lowest_index_val, label = .data[["n"]]), direction = "y", nudge_y = -0.1, ...
    ) +
    ggrepel::geom_label_repel(
      data = label_2,
      ggplot2::aes(y = 0.99 * lowest_index_val, label = .data[["n"]]),
      col = "grey", direction = "y", ...
    ) +
    # scale, lab and theme
    ggplot2::scale_x_continuous(breaks = seq(1, largest, 1)) +
    ggplot2::ylim(extend_lower * lowest_index_val, get_best(dt) %>% dplyr::pull(.data$index_val)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylab("Index value") +
    ggplot2::xlab("Iteration number")
  p
}
