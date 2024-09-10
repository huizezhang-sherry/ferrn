#' Create Huber plot with ggplot2
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_path
#' @param index a function, the projection pursuit index function, see examples
#' @rdname huber
#' @export
#' @examples
#' if (require(ash, quietly = TRUE)) {
#'   library(ggplot2)
#'   library(tourr)
#'   data(randu)
#'   randu_std <- as.data.frame(apply(randu, 2, function(x) (x-mean(x))/sd(x)))
#'   randu_std$yz <- sqrt(35)/6*randu_std$y-randu_std$z/6
#'   randu_df <- randu_std[c(1,4)]
#'   randu_huber <- prep_huber(randu_df, index = norm_bin(nr = nrow(randu_df)))
#'
#'   ggplot() +
#'     geom_huber(data = randu_huber$idx_df, aes(x = x, y = y)) +
#'     geom_point(data = randu_df, aes(x = x, y = yz)) +
#'     geom_abline(slope = randu_huber$slope, intercept = 0) +
#'     theme_huber() +
#'     coord_fixed()
#'
#'   ggplot(randu_huber$proj_df, aes(x = x)) +
#'     geom_histogram(breaks = seq(-2.2, 2.4, 0.12)) +
#'     xlab("") + ylab("") +
#'     theme_bw() +
#'     theme(axis.text.y = element_blank())
#'}
geom_huber <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", ...,
                       show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHuber,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
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

    grid::gList(
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

huber_data_setup <- function(data, param){
  theta <- pi/180 * (0:(nrow(data) - 1))
  res1 <- data |> dplyr::mutate(type = "huber")
  res2 <- data |> dplyr::mutate(
      x = 4 * cos(theta),
      y = 4 * sin(theta),
      type = "circle")
  res <- dplyr::bind_rows(res1, res2)
  return(res)

}

#' @export
#' @rdname huber
prep_huber <- function(data, index){
  data <- as.matrix(data)
  index_f <- index
  res <- tibble::tibble(i = 0:360, theta = pi/180 * i) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      proj_data = list(as.matrix(cos(theta) * data[,1] + sin(theta) * data[,2])),
      index = index_f(proj_data)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      range = round(max(index) - min(index), 5),
      idx_scaled = (index - min(index))/range * 2 + 3,
      x = idx_scaled * cos(theta),
      y = idx_scaled * sin(theta))

  sel_idx <- which(res$index[1:360] > signif(max(res$index), 6) - 1e-06)
  theta_best <- pi/180 * (sel_idx - 1)
  slope <-  sin(theta_best)/cos(theta_best)
  proj_df <- tibble::tibble(x = cos(theta_best) * data[, 1] + sin(theta_best) * data[, 2])
  return(list(idx_df = res, proj_df = proj_df, slope = slope))

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
