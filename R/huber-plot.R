#' Create Huber plots with ggplot2
#'
#' The Huber plot presents the projection pursuit index values of 2D data in each 1D
#' projection in polar coordinates, corresponding to each projection direction.
#' It offers a simpler illustration of more complex projection from
#' high-dimensional data to lower dimensions in projection pursuit. The
#' function \code{prep_huber()} calculates each component required for the Huber plot
#' (see details), which can then be supplied to various geom layers in ggplot2.
#'
#' @details the \code{prep_huber()} function calculates components required for
#' making the Huber plots. It returns a list including three elements:
#'\describe{
#'      \item{the \code{idx_df} data frame: }{the x/y coordinates of the index value, in polar
#' coordinates. Used for plotting the index value at each projection direction,
#' with the reference circle.}
#'      \item{the \code{proj_df} data frame: }{the best 1D projection. Used for plotting
#' the 1D projection in histogram.}
#'      \item{the \code{slope} value: }{the slope to plot in the Huber plot to indicate the
#' direction of the best 1D projection.}
#'    }
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_path
#' @param index a function, the projection pursuit index function, see examples
#' @rdname huber
#' @export
#' @examples
#' library(ggplot2)
#' library(tourr)
#' library(ash)
#' data(randu)
#' randu_std <- as.data.frame(apply(randu, 2, function(x) (x-mean(x))/sd(x)))
#' randu_std$yz <- sqrt(35)/6*randu_std$y-randu_std$z/6
#' randu_df <- randu_std[c(1,4)]
#' # randu_huber <- prep_huber(randu_df, index = norm_bin(nr = nrow(randu_df)))
#'
#' ggplot()  +
#'   geom_huber(data = randu_df, aes(x = x, y = yz),
#'              index_fun = norm_bin(nr = nrow(randu_df))) +
#'   coord_fixed() +
#'   theme_huber()
#'
#' # ggplot(randu_huber$proj_df, aes(x = x)) +
#' #   geom_histogram(breaks = seq(-2.2, 2.4, 0.12)) +
#' #   xlab("") + ylab("") +
#' #   theme_bw() +
#' #   theme(axis.text.y = element_blank())
geom_huber <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", ..., index_fun,
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  #browser()
  index_fun <-  match.fun(index_fun)
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = "huber",
    geom = GeomHuber,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, index_fun = index_fun, ...)
  )
}

StatHuber <- ggplot2::ggproto(
  "StatHuber",
  ggplot2::Stat,
  compute_group = function(data, scales, index_fun) {
    prep_huber(data, index_fun)
  },

  required_aes = c("x", "y")
)

stat_huber <- function(mapping = NULL, data = NULL, geom = "path",
                       position = "identity", ..., index_fun,
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatHuber,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, index_fun = index_fun, ...)
  )
}


GeomHuber <- ggplot2::ggproto(
  "GeomHuber",
  ggplot2::Geom,

  draw_panel = function(data, panel_params, coord, index_fun, lineend = "butt", ...) {
    data_circle <- data |>
      dplyr::filter(type == "circle") |>
      dplyr::mutate(linetype = "dashed")
    data_huber <- data |> dplyr::filter(type == "huber")
    data_orig <- data |> dplyr::filter(type == "original")
    data_abline <- data_circle[1, ] |> dplyr::mutate(intercept = 0)

    grid::gList(
      ggplot2::GeomPath$draw_panel(data_circle, panel_params, coord),
      ggplot2::GeomPath$draw_panel(data_huber, panel_params, coord),
      ggplot2::GeomPoint$draw_panel(data_orig, panel_params, coord),
      ggplot2::GeomAbline$draw_panel(data_abline, panel_params, coord)
    )

  },


  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    colour = "black", linewidth = 0.5, linetype = "solid", alpha = 1,
    index = NULL, fill = NA, pch = 19, size = 1,
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
  proj_df <- tibble::tibble(x = cos(theta_best) * data[, 1] + sin(theta_best) * data[, 2])

  res2 <- dplyr::bind_rows(res |> dplyr::select(x, y)) |>
    dplyr::mutate(slope = sin(theta_best)/cos(theta_best))

  theta <- pi/180 * (0:(nrow(res2) - 1))
  res1 <- res2 |> dplyr::mutate(type = "huber", linetype = "solid", group = 1)
  res2 <- res2 |> dplyr::mutate(
    x = 4 * cos(theta),
    y = 4 * sin(theta),
    type = "circle", linetype = "dashed", group = 2)
  orig_df <- data |> dplyr::select(x, y) |>
    dplyr::mutate(type = "original", group = 3, linetype = "solid")
  res <- dplyr::bind_rows(res1, res2, orig_df)

  res
  #return(list(idx_df = res, proj_df = proj_df, slope = slope))

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
