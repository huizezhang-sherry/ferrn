#' Plot the trace the search progression
#'
#' Trace the index value of search/ interpolation points in guided tour optimisation
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param iter The iterator on the x-axis
#' @param color Colored by a particular varaible
#' @param cutoff If there are less than cutoff number of points on the interpolation path, all the points will be marked
#' @param accuracy The accuracy for display index value, recommand either 1e-2 or 12-3
#' @examples
#' # Compare the trace of interpolated points in two algorithms
#' holes_1d_better %>%
#'   explore_trace_interp() +
#'   scale_color_botanical(palette = "fern", discrete = FALSE)
#' @import ggplot2
#' @importFrom rlang sym "!!"
#' @family plot
#' @export
#' @rdname explore_trace
explore_trace_interp <- function(dt, iter = id, color = tries, cutoff = 50, accuracy = 1e-2) {

  # check there is a column called info, there is a value called interpolation
  # check other variables as well
  iter <- enexpr(iter)
  col <- enexpr(color)

  dt_interp <- get_interp(dt)
  interp_last <- dplyr::bind_rows(get_start(dt), get_interp_last(dt))
  tick_x <- interp_last %>% dplyr::pull(!!iter)
  tick_y <- interp_last$index_val

  a <- dt_interp %>%
    dplyr::summarise(
      row = dplyr::n(),
      diff = max(!!iter) - min(!!iter) + 1
    )

  if (!all(a$row == a$diff, TRUE)) {
    stop("there is gap(s) in the variable iter!")
  }

  p <- dt_interp %>%
    ggplot(aes(x = !!iter, y = index_val)) +
    geom_line() +
    geom_point(data = interp_last, aes(col = !!col), size = 3) +
    geom_vline(data = interp_last, aes(xintercept = !!iter), lty = "dashed", alpha = 0.3) +
    scale_x_continuous(breaks = tick_x) +
    scale_y_continuous(breaks = tick_y, labels = scales::label_number(accuracy =accuracy)) +
    theme_fern() +
    theme(legend.position = "none") +
    ylab("Index value") +
    xlab("Time")


  if (nrow(dt_interp) < cutoff){
    p <- p + geom_point(
      aes(x = !!iter, y = index_val, col = !!col)
    )
  }

  p
}

#' Plot the count in each iteration
#'
#' @param dt A data object from the running the optimisation algorithm in guided tour
#' @param iter The iterator on the x-axis
#' @param color Colored by a particular varaible
#' @param cutoff The cutoff number of observations for switching between point geom to boxplot geom in \code{explore_trace_search()}
#' @examples
#' # Summary plots for search points in two algorithms
#' library(patchwork)
#' library(dplyr)
#' library(ggplot2)
#' p1 <- holes_1d_better %>% explore_trace_search() +
#'   scale_color_botanical(palette = "fern") +
#'   xlim(c(1, 9))
#' p2 <- holes_2d_better_max_tries %>% explore_trace_search() +
#'   scale_color_botanical(palette = "daisy")
#' p1 / p2
#' @family plot
#' @export
explore_trace_search <- function(dt, iter = tries, color = tries, cutoff = 15) {
  iter <- enexpr(iter)
  col <- enexpr(color)

  search <- dt %>%
    filter(info != "interpolation")
  search_count <- get_search_count(search)


  largest <- max(eval(rlang::expr(`$`(dt, !!iter))))
  lowest_index_val <- min(dt$index_val)

  # filter data plotted with boxplot geom
  box_id <- search_count %>%
    filter(n >= cutoff)
  search_box <- search %>%
    filter(!!iter %in% `$`(box_id, !!iter))

  # filter data plotted with point geom
  point_id <- search_count %>%
    filter(n < cutoff)
  search_point <- search %>%
    filter(!!iter %in% `$`(point_id, !!iter))

  # filter the target points
  search_target <- search %>%
    group_by(!!iter) %>%
    filter(index_val == max(index_val))


  p <- search %>%
    ggplot(aes(x = !!iter, y = index_val, col = as.factor(!!col))) +
    # point summary
    geom_point(data = search_point) +
    # boxplot summary
    geom_boxplot(data = search_box) +
    geom_boxplot(
      data = search_box %>% filter(!!iter == largest),
      color = "grey"
    ) +
    # target points
    geom_point(data = search_target %>% filter(!!iter != largest), aes(group = 1), size = 3) +
    geom_line(data = search_target %>% filter(!!iter != largest), aes(group = 1)) +
    geom_point(
      data = search_target %>% filter(!!iter == largest),
      col = "grey", size = 3
    ) +
    # numeric summary box
    geom_label(data = search_count, aes(y = 0.99 * lowest_index_val, label = n)) +
    geom_label(
      data = search_count %>% filter(!!iter == largest),
      aes(y = 0.99 * lowest_index_val, label = n),
      col = "grey"
    ) +
    # scale, lab and theme
    scale_x_continuous(breaks = seq(1, largest, 1)) +
    theme(legend.position = "none") +
    ylab("Index value") +
    xlab("Iteration number")
  p
}
globalVariables(c("id", "tries", "n", "index_val"))
