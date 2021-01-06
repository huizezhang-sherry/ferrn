#' @export
draw_circle <- function(dt, cir_alpha = 0.5, cir_fill = "grey92", cir_color = "white") {
  ggforce::geom_circle(
    data = dt,
    aes(x0 = x0, y0 = y0, r = r),
    alpha = cir_alpha, fill = cir_fill, color = cir_color
  )
}

#' @export
draw_points <- function(dt, pnt_size = 1, pnt_alpha = 1, pnt_color = NULL) {
  color <- enexpr(pnt_color)

  geom_point(
    data = dt,
    aes(x = PC1, y = PC2, color = !!color),
    size = pnt_size, alpha = pnt_alpha
  )
}

#' @export
draw_center <- function(dt, cent_size = 1, cent_alpha = 1, cent_color = "black") {
  geom_point(
    data = dt,
    aes(x = PC1, y = PC2),
    size = cent_size, alpha = cent_alpha, color = cent_color
  )
}


#' @export
draw_path <- function(dt, path_alpha = NULL, path_group = NULL,
                      path_color = NULL, path_size = 1.5, ...) {
  alpha <- enexpr(path_alpha)
  group <- enexpr(path_group)
  color <- enexpr(path_color)

  geom_path(
    data = dt,
    aes(x = PC1, y = PC2, alpha = !!alpha, group = !!group, color = !!color),
    size = path_size, ...
  )
}


#' @export
draw_anno <- function(dt, anno_color = "black", anno_lty = "dashed", anno_alpha = 0.1) {
  geom_line(
    data = dt,
    aes(x = PC1, y = PC2), group = 1,
    color = anno_color, linetype = anno_lty, alpha = anno_alpha
  )
}

#' @export
draw_theo <- function(dt, theo_label = "*", theo_size = 25) {
  geom_text(
    data = dt,
    aes(x = PC1, y = PC2),
    label = theo_label, size = theo_size
  )
}


#' @export
estimate_circle <- function(dt) {
  center <- dt %>% get_center()
  x0 <- center %>% pull(PC1)
  y0 <- center %>% pull(PC2)

  r <- dt %>%
    dplyr::mutate(dist = sqrt((PC1 - x0)^2 + (PC2 - y0)^2)) %>%
    dplyr::filter(dist == max(dist)) %>%
    pull(dist)

  tibble::tibble(x0 = x0, y0 = y0, r = r)
}
