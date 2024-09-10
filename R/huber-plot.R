#' Create Huber plot with ggplot2
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_path
#' @param index a function, the projection pursuit index function, see examples
#' @rdname huber
#' @export
geom_huber <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", ..., index = NULL,
                       show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHuber,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(index = list(index),  ...)
  )
}

GeomHuber <- ggplot2::ggproto(
  "GeomHuber",
  ggplot2::Geom,
  setup_data = function(data, params) {
    huber_data_setup(data, params)
  },

  draw_panel = function(data, panel_params, coord, lineend = "butt", ...) {

    data_circle <- data |>
      dplyr::filter(type == "circle") |>
      dplyr::mutate(linetype = "dashed")
    data_huber <- data |> dplyr::filter(type == "huber")

    # https://github.com/tidyverse/ggplot2/blob/HEAD/R/geom-abline.R
    ranges <- coord$backtransform_range(panel_params)
    if (coord$clip == "on" && coord$is_linear()) {
      # Ensure the line extends well outside the panel to avoid visible line
      # ending for thick lines
      ranges$x <- ranges$x + c(-1, 1) * diff(ranges$x)
    }

    data_best <- data
    data_best$x    <- ranges$x[1]
    data_best$xend <- ranges$x[2]
    data_best$y    <- ranges$x[1] * unique(data$slope) # intercept is always 0
    data_best$yend <- ranges$x[2] * unique(data$slope)
    data_best$linetype <- "solid"

    grid::gList(
      ggplot2::GeomSegment$draw_panel(data_best, panel_params, coord,
                                      lineend = lineend),
      ggplot2::GeomPath$draw_panel(data_circle, panel_params, coord, ...),
      ggplot2::GeomPath$draw_panel(data_huber, panel_params, coord, ...)
    )

  },


  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    colour = "black", linewidth = 0.5, linetype = "solid", alpha = 1,
    index = NULL
  )
)

huber_data_setup <- function(data, params){
  index_f <- params$index[[1]]
  res <- tibble::tibble(i = 0:360, theta = pi/180 * i) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      proj_data = list(as.matrix(cos(theta) * data$x + sin(theta) * data$y)),
      index = index_f(proj_data),
      PANEL = 1, group = -1, alpha = params$alpha) |>
    dplyr::ungroup()

  res1 <- res |>
    dplyr::mutate(
      range = round(max(index) - min(index), 5),
      idx_scaled = (index - min(index))/range * 2 + 3,
      x = idx_scaled * cos(theta),
      y = idx_scaled * sin(theta),
      type = "huber")

  res2 <- res |> dplyr::mutate(
      x = 4 * cos(theta),
      y = 4 * sin(theta),
      type = "circle")

  res <- dplyr::bind_rows(res1, res2)
  sel_idx <- which(res$index[1:360] > signif(max(res$index), 6) - 1e-06)
  theta_best <- pi/180 * (sel_idx - 1)
  res <- res |> dplyr::mutate(slope = sin(theta_best)/cos(theta_best))
  return(res)

}

#' @rdname huber
#' @export
theme_huber <- function(...) {
  ggplot2::theme_bw(...) %+replace%
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      complete = TRUE
    )
}
globalVariables(c("i", "theta", "proj_data", "idx_scaled"))
