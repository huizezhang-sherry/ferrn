#' Plot the trace the search progression
#'
#' Trace the index value of search/ interpolation points in guided tour optimisation
#'
#'@param dt A data object from the running the optimisation algorithm in guided tour
#'@param iter The iterator on the x-axis
#'@param color Colored by a particular varaible
#'@param group The grouping variable, useful when there are multiple algorithms in the data object to plot
#'@examples
#'# Compare the trace of interpolated points in two algorithms
#'dplyr::bind_rows(holes_1d_better, holes_1d_geo) %>% explore_trace_interp(group = method) +  scale_color_botanical(palette = "fern")
#'@import ggplot2
#'@importFrom rlang sym "!!"
#'@family plot
#'@export
#'@rdname explore_trace
explore_trace_interp <- function(dt, iter = id,  color = tries, group = NULL){

  # check there is a column called info, there is a value called interpolation
  # check other variables as well
  group <- rlang::enexpr(group)
  iter <- rlang::enexpr(iter)
  col <- rlang::enexpr(color)


  dt_interp <- get_interp(dt, group = !!group)

  a <- dt_interp %>%
    group_by(!!group) %>%
    dplyr::summarise(row = dplyr::n(), diff = max(!!iter) - min(!!iter) + 1)

  if(!all(a$row == a$diff, TRUE)){
    stop("there is gap(s) in the variable iter!")
  }


  p <- dt_interp %>%
    ggplot(aes(x = !!iter, y = !!sym("index_val"), col = as.factor(!!col)))  +
    geom_line() +
    geom_point() +
    ylab("index value") +
    theme(legend.position = "none")

  if (!is.null(group)){
    p <- p + facet_wrap(vars(!!group), labeller = "label_both", ncol = 1)
  }
  p
}

#' Plot the count in each iteration
#'
#'@param dt A data object from the running the optimisation algorithm in guided tour
#'@param iter The iterator on the x-axis
#'@param color Colored by a particular varaible
#'@param group The grouping variable, useful when there are multiple algorithms in the data object to plot
#'@param cutoff The cutoff number of observations for switching between point geom to boxplot geom in \code{explore_trace_search()}
#'@examples
#'# Summary plots for search points in two algorithms
#'proj_1D <- holes_1d_better %>% dplyr::mutate(proj = "1D")
#'proj_2D <- holes_2d_better_max_tries %>% dplyr::mutate(proj = "2D")
#'dplyr::bind_rows(proj_1D, proj_2D) %>% explore_trace_search(group = proj) +
#'  scale_color_botanical(palette = "daisy")
#'@family plot
#'@export
explore_trace_search <- function(dt, iter = tries, color = tries, cutoff = 15, group = NULL){

  iter <- enexpr(iter)
  group <- enexpr(group)
  col <- enexpr(color)

  search <- dt %>%
    dplyr::filter(info != "interpolation")
  search_count <- search %>%
    get_search_count(group = !!group)

  largest <- max(eval(rlang::expr(`$`(dt, !!iter))))
  lowest_index_val <- min(dt$index_val)

  box_id <- search_count %>%
    filter(n >= cutoff)
  search_box <- search %>%
    filter(!!iter %in% `$`(box_id, !!iter))
  if (!is.null(group))
    search_box <- search_box %>%
    filter(!!group %in% `$`(box_id, !!group))

  point_id <- search_count %>%
    filter(n < cutoff)
  search_point <- search %>%
    filter(!!iter %in% `$`(point_id, !!iter))
  if (!is.null(group))
    search_point <- search_point %>%
    filter(!!group %in% `$`(point_id, !!group))

  if (!is.null(group)) {
    search_line <- search %>%
      dplyr::group_by(!!iter, !!group) %>%
      dplyr::filter(index_val == max(index_val))
  } else{
    search_line <- search %>%
      dplyr::group_by(!!iter) %>%
      dplyr::filter(index_val == max(index_val))
  }

  p <- search %>%
    ggplot(aes(x = !!iter, y = index_val, col = as.factor(!!col))) +
    geom_boxplot(data = search_box) +
    geom_point(data = search_point) +
    geom_line(data = search_line, aes(group = 1)) +
    geom_label(data = search_count, aes(y = 0.99*lowest_index_val, label = n)) +
    scale_x_continuous(breaks = seq(1, largest, 1)) +
    theme(legend.position = "none") +
    ylab("index value") +
    xlab("Iteration number")

  if(!is.null(group)){
    p <- p + facet_wrap(vars(!!group), labeller = "label_both", ncol = 1) +
      theme(legend.position = "none")
  }

  p

}
globalVariables(c("id", "tries", "n", "index_val"))

