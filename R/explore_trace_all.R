#'@title explore trace plot
#'@param glb_obj a global object resulted from the \href{https://cran.r-project.org/web/packages/tourr}{tourr} package
#'@param magnify a boolean value indicating whether to magnify the index value for the last \code{tries}
#'@details
#' * `explore_trace_all()`: plots all the index value from the global object as a dot plot.
#'
#' * `explore_trace_interp()`: plots a line plot for the index value of the interpolated basis.
#'
#'@examples
#'\dontrun{
#' explore_trace_all(x2_object, 7)
#' explore_trace_interp(x2_object)
#'}
#'@return a ggplot object plotting the trace
#'@import ggplot2
#'@importFrom rlang sym "!!"
#'@export
#'@rdname explore_trace_all
explore_trace_all <- function(glb_obj, magnify = FALSE){

  info <- rlang::sym("info")
  id <- rlang::sym("id")
  index_val <- rlang::sym("index_val")
  tries <- rlang::sym("tries")
  loop <- rlang::sym("loop")

  p <- glb_obj %>%
    ggplot(aes(x = !!id, y = !!index_val, col = !!info)) +
    geom_point()

  if (magnify){

    tries_to_magnify <- glb_obj %>%
      dplyr::filter(loop == max(!!loop, na.rm = TRUE)) %>%
      dplyr::pull(tries) %>% unique()

    frac <- glb_obj %>%
      dplyr::filter(tries == tries_to_magnify, info != "line_search")

    p_frac <- frac %>%
      ggplot(aes(x = !!id, y = !!index_val, col = !!info)) +
      geom_point() +
      xlim(1, max(frac$id)) +
      scale_color_manual(values = scales::hue_pal()(6)[c(2, 3, 5)]) +
      theme(legend.position = "none")

    p <- p_frac/p +
      patchwork::plot_layout(heights = c(1, 2), guides = "collect")

  }

  p

}



#'@export
#'@rdname explore_trace_all
explore_trace_interp <- function(glb_obj){

  # check there is a column called info, there is a value called interpolation
  # check other variables as well
  info <- rlang::sym("info")
  id <- rlang::sym("id")
  index_val <- rlang::sym("index_val")

  interp <- glb_obj %>%
    dplyr::filter(!!info == "interpolation") %>%
    dplyr::mutate(id = dplyr::row_number())

  p <- interp %>%
    ggplot(aes(x = !!id, y = !!index_val))  +
    geom_line()

  p
}
