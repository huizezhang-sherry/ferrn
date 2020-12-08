explore_platter <- function(dt, group = NULL, color = NULL){
  browser()

  group <- enexpr(group)
  if (is.null(group)) color <- enexpr(color) else color <- group

  p_pca <- explore_space_pca(dt, group = !!group) +
    scale_color_botanical(palette = "cherry")

  # p_trace <-  purrr::map(
  #   as.list(dplyr::distinct(dt_trace, !!group) %>% pull(!!group)),
  #   ~dt %>%
  #     filter(!!group == .x) %>%
  #     explore_trace_interp(color = !!color) +
  #     scale_color_botanical(palette = "cherry") +
  #     ggtitle(.x)
  # )


  p_trace <- dt %>%
    explore_trace_interp(group = !!group, color = !!group) +
    facet_wrap(vars(!!group), ncol = 1) +
    scale_color_botanical(palette = "cherry")


  (p_pca + p_trace)


}
