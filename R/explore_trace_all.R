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
explore_trace_all <- function(glb_obj, col = info, magnify = FALSE){
  #browser()

  info <- rlang::sym("info")
  id <- rlang::sym("id")
  index_val <- rlang::sym("index_val")
  tries <- rlang::sym("tries")
  loop <- rlang::sym("loop")
  col <- rlang::ensym(col)

  glb_obj <- glb_obj %>%
    dplyr:: mutate(id = dplyr::row_number()-1)

  # end_id <- glb_obj %>%
  #   dplyr::filter(info == "interpolation" | id == max(id)) %>%
  #   dplyr::group_by(tries) %>%
  #   dplyr::filter(id == min(id)) %>%
  #   dplyr::pull(id)
  #
  # begin_df <- glb_obj %>%
  #   dplyr::filter(info != "interpolation") %>%
  #   dplyr::group_by(tries) %>%
  #   dplyr::filter(id == min(id))
  #
  # bg <- tibble(xmin = begin_df %>% pull(id), xmax = end_id, alpha = begin_df$alpha)

  p <- glb_obj %>%
    ggplot() +
    geom_point(aes(x = !!id, y = !!index_val, col = !!col))
    # geom_rect(data = bg, aes(xmin = xmin, xmax = xmax,
    #                           ymin = -Inf, ymax = Inf,
    #                           fill = alpha), alpha = 0.3) +
    #scale_fill_manual(values = c("grey" = "black","white" = "white")) +
    #theme_minimal()



  # if (magnify){
  #
  #   if (glb_obj$method %>% tail(1) != "search_geodesic"){
  #     message("magnify is only applicable for geodesic search!")
  #   }else{
  #     tries_to_magnify <- glb_obj %>%
  #       dplyr::filter(loop == max(!!loop, na.rm = TRUE)) %>%
  #       dplyr::pull(tries) %>% unique()
  #
  #     frac <- glb_obj %>%
  #       dplyr::filter(tries == tries_to_magnify, info != "line_search")
  #
  #     num_level <- length(levels(as.factor(glb_obj$info)))
  #     level_selected <- which(levels(as.factor(glb_obj$info)) %in% levels(as.factor(frac$info)))
  #
  #     p_frac <- frac %>%
  #       ggplot(aes(x = !!id, y = !!index_val, col = !!col)) +
  #       geom_point() +
  #       xlim(1, max(frac$id)) +
  #       scale_color_manual(values = scales::hue_pal()(num_level)[level_selected]) +
  #       theme(legend.position = "none")
  #
  #     p <- p_frac/p +
  #       patchwork::plot_layout(heights = c(1, 2), guides = "collect")
  #   }
  #
  # }

  p

}



#'@export
#'@rdname explore_trace_all
explore_trace_interp <- function(dt, iter = id,  col = tries, facet = NULL){

  # check there is a column called info, there is a value called interpolation
  # check other variables as well
  info <- rlang::sym("info")
  index_val <- rlang::sym("index_val")
  method <- rlang::sym("method")
  facet <- rlang::enexpr(facet)

  interp <- dt %>%
    dplyr::filter((!!info == "interpolation" & !!method != "search_polish") | (!!info == "polish_best" & !!method == "search_polish")) %>%
    group_by(!!facet) %>%
    dplyr::mutate(id = dplyr::row_number()-1)

  # bg <- interp %>%
  #   dplyr::group_by(tries) %>%
  #   dplyr::filter(id == min(id) | id == max(id))
  #   #tidyr::pivot_wider(c(),names_from = id, values_from = id)

  p <- interp %>%
    ggplot(aes(x = !!rlang::enexpr(iter), y = !!index_val,
               group = 1, col = as.factor(!!rlang::enexpr(col))))  +
    geom_line() +
    geom_point() +
    facet_wrap(vars(!!facet), labeller = "label_both") +
    scale_color_botanical(palette = "banksia")

  p
}

#'@export
#'@rdname explore_trace_all
explore_trace_search <- function(glb_obj, iter = tries, cutoff = 15){
  #browser()

  dt <- glb_obj %>%
    dplyr::filter(info != "interpolation") %>%
    dplyr::mutate(id = dplyr::row_number())

  dt_count <- dt %>%
    group_by(!!rlang::enexpr(iter)) %>%
    summarise(n = n())

  largest <- eval(rlang::expr(`$`(dt, !!rlang::enexpr(iter))))
  lowest_index_val <- min(glb_obj$index_val)

  p <- dt %>%
    ggplot(aes(x = !!rlang::enexpr(iter), y = index_val, col = as.factor(!!rlang::enexpr(iter)))) +
    geom_boxplot(data = dt %>% dplyr::filter(tries %in% which(dt_count$n >= cutoff))) +
    geom_point(data = dt %>% dplyr::filter(tries %in% which(dt_count$n < cutoff))) +
    geom_line(data = dt %>% dplyr::group_by(!!rlang::ensym(iter)) %>% dplyr::filter(index_val == max(index_val)),
              aes(group = 1)) +
    geom_label(data = dt_count, aes(y = 0.99*lowest_index_val, label = n)) +
    scale_x_continuous(breaks = seq(1, max(largest), 1)) +
    theme(legend.position = "none")

  p

}


explore_trace_parameter <- function(glb_obj, var, iter = tries){
  #browser()

  dt <- glb_obj %>%
    dplyr::filter(info != "interpolation") %>%
    dplyr::mutate(id = dplyr::row_number())

  p <- dt %>%
    ggplot(aes(x =!!rlang::enexpr(iter), y = !!rlang::enexpr(var))) +
    geom_jitter(aes(col = as.factor(!!rlang::enexpr(iter)))) +
    geom_line(data = dt %>% dplyr::group_by(!!rlang::enexpr(iter)) %>% dplyr::filter(index_val == max(index_val)),
              aes(group = 1))  +
    theme(legend.position = "none")

  p
}
