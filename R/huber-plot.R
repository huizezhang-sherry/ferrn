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
#' @param index.fun,index_fun the projection pursuit index function, see examples
#' @param ref.circle.color,ref.circle.colour,ref.circle.linetype,ref.circle.linewidth Default aesthetics for the reference circle
#' @param idx.max.color,idx.max.colour,idx.max.linetype,idx.max.linewidth Default aesthetics for the line indicating the best projection direction
#' @param idx.profile.color,idx.profile.colour,idx.profile.linetype,idx.profile.linewidth Default aesthetics for the index profile line
#' @param proj.points.color,proj.points.colour,proj.points.stroke,proj.points.alpha,proj.points.size,proj.points.shape Default aesthetics for the projected data points
#' @rdname huber
#' @export
#' @examples
#' library(ggplot2)
#' library(tourr)
#' library(ash)
#' data(randu)
#' # simplify the randu data into 2D for illustration
#' randu_std <- as.data.frame(apply(randu, 2, function(x) (x-mean(x))/sd(x)))
#' randu_std$yz <- sqrt(35)/6*randu_std$y-randu_std$z/6
#' randu_df <- randu_std[c(1,4)]
#'
#' # main example: Huber plot with geom_huber()
#' randu_df |>
#'   ggplot()  +
#'   geom_huber(aes(x = x, y = yz), index.fun = norm_bin(nr = nrow(randu_df))) +
#'   coord_fixed() +
#'   theme_huber()
#'
#' # compute the best projection data for histogram
#' randu_huber_best <- prep_huber_best_proj(
#'   randu_df, index_fun = norm_bin(nr = nrow(randu_df))
#'   )
#' randu_huber_best |>
#'   ggplot() +
#'   geom_histogram(aes(x = x), breaks = seq(-2.2, 2.4, 0.12)) +
#'   xlab("") + ylab("") +
#'   theme_bw() +
#'   theme(axis.text.y = element_blank())
StatHuber <- ggplot2::ggproto("StatHuber", ggplot2::Stat,
                              compute_group = function(data, scales, index.fun) {
                                #browser()
                                prep_huber_ggplot(data, index.fun)
                              },
                              required_aes = c("x", "y")
)

#' @export
#' @rdname huber
stat_huber <- function(mapping = NULL, data = NULL, geom = "path",
                       position = "identity", ..., index.fun,
                       na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    stat = StatHuber,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, index.fun = index.fun, ...)
  )
}

#' @export
#' @rdname huber
geom_huber <- function(mapping = NULL,
                       data = NULL,
                       stat = "identity",
                       position = "identity",
                       index.fun,
                       ref.circle.color = NULL,
                       ref.circle.colour = NULL,
                       ref.circle.linetype = "dashed",
                       ref.circle.linewidth = NULL,

                       idx.max.color = NULL,
                       idx.max.colour = NULL,
                       idx.max.linetype = "dashed",
                       idx.max.linewidth = NULL,

                       idx.profile.color = NULL,
                       idx.profile.colour = NULL,
                       idx.profile.linetype = "solid",
                       idx.profile.linewidth = NULL,

                       proj.points.color = NULL,
                       proj.points.colour = NULL,
                       proj.points.stroke = NULL,
                       proj.points.alpha = NULL,
                       proj.points.size = NULL,
                       proj.points.shape = NULL,


                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       ...) {
  index.fun <-  match.fun(index.fun)

  ref_circle_gp <- list(
    colour = ref.circle.color %||% ref.circle.colour,
    linetype = ref.circle.linetype,
    linewidth = ref.circle.linewidth
  )

  idx_max_gp <- list(
    colour = idx.max.color %||% idx.max.colour,
    linetype = idx.max.linetype,
    linewidth = idx.max.linewidth
  )

  idx_profile_gp <- list(
    colour = idx.profile.color %||% idx.profile.colour,
    linetype = idx.profile.linetype,
    linewidth = idx.profile.linewidth
  )

  proj_points_gp <- list(
    colour = proj.points.color %||% proj.points.colour,
    stroke = proj.points.stroke,
    alpha = proj.points.alpha,
    size = proj.points.size,
    shape = proj.points.shape

  )

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = "huber",
    geom = GeomHuber,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      index.fun = index.fun,
      ref_circle_gp = ref_circle_gp,
      idx_max_gp = idx_max_gp,
      idx_profile_gp = idx_profile_gp,
      proj_points_gp = proj_points_gp,
      na.rm = na.rm,
      ...)
  )
}

#' @export
#' @rdname huber
GeomHuber <- ggplot2::ggproto(
  "GeomHuber",
  ggplot2::Geom,

  draw_panel = function(data, panel_params, coord, index.fun,
                        lineend = "butt", ref_circle_gp = NULL,
                        idx_max_gp = NULL, idx_profile_gp = NULL,
                        proj_points_gp = NULL) {

    data_ref_circle <- data[data$type == "circle",]
    data_ref_circle$colour <- ref_circle_gp$colour %||% data_ref_circle$colour[1]
    data_ref_circle$linetype <- ref_circle_gp$linetype %||% data_ref_circle$linetype[1]
    data_ref_circle$linewidth <- ref_circle_gp$linewidth %||% data_ref_circle$linewidth[1]


    data_idx_profile <- data[data$type == "idx_profile",]
    data_idx_profile$colour <- idx_profile_gp$colour %||% data_idx_profile$colour[1]
    data_idx_profile$linetype <- idx_profile_gp$linetype %||% data_idx_profile$linetype[1]
    data_idx_profile$linewidth <- idx_profile_gp$linewidth %||% data_idx_profile$linewidth[1]

    data_idx_max <- data_idx_profile[1, ]
    data_idx_max$intercept <- 0
    data_idx_max$colour <- idx_max_gp$colour %||% data_idx_max$colour[1]
    data_idx_max$linetype <- idx_max_gp$linetype %||% data_idx_max$linetype[1]
    data_idx_max$linewidth <- idx_max_gp$linewidth %||% data_idx_max$linewidth[1]

    data_proj_points <- data[data$type == "original",]
    data_proj_points$colour <- proj_points_gp$colour %||% data_proj_points$colour[1]
    data_proj_points$shape <- proj_points_gp$shape %||% data_proj_points$shape[1]
    data_proj_points$stroke <- proj_points_gp$stroke %||% data_proj_points$stroke[1]
    data_proj_points$alpha <- proj_points_gp$alpha %||% data_proj_points$alpha[1]
    data_proj_points$size <- proj_points_gp$size %||% data_proj_points$size[1]


    grid::gList(
      ggplot2::GeomPath$draw_panel(data_ref_circle, panel_params, coord),
      ggplot2::GeomPath$draw_panel(data_idx_profile, panel_params, coord),
      ggplot2::GeomPoint$draw_panel(data_proj_points, panel_params, coord),
      ggplot2::GeomAbline$draw_panel(data_idx_max, panel_params, coord)
    )

  },

  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(
    colour = from_theme(colour %||% scales::col_mix(ink, paper, 0.2)),
    fill = from_theme(fill %||% paper),
    size = from_theme(pointsize),
    linewidth = 0.5, linetype = "dashed", alpha = 1,
    weight = 1, shape = 19)
)


#' @export
#' @rdname huber
prep_huber_best_proj <- function(data, index_fun){
  prep_huber_workhorse(data, index_fun)$proj_df
}


prep_huber_workhorse <- function(data, index_fun){

  if(ncol(data) != 2){
    cli::cli_abort("The input data must be a data frame with two columns The huber plot is only implemented for 2D data.")
  }

  # internal function
  # calculate the projection pursuit index values at each projection direction: 0: 359
  res <- tibble::tibble(i = 0:360, theta = pi/180 * i) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      proj_data = list(as.matrix(cos(theta) * data[,1] + sin(theta) * data[,2])),
      index = index_fun(proj_data)) |>
    dplyr::ungroup()

  # find the best projection direction and calculate the projected data
  sel_idx <- which(res$index[0:359] > signif(max(res$index), 6) - 1e-06)
  theta_best <- pi/180 * (sel_idx - 1)
  proj_df <- tibble::tibble(x = cos(theta_best) * data[, 1] + sin(theta_best) * data[, 2])

  return(list(idx_df = res, proj_df = proj_df, data = tibble::as_tibble(data)))

}

prep_huber_ggplot <- function(data, index_fun){

  # internal function: the input data is
  # | x | y | PANEL | group |

  # this is the internal function to prepare the data for ggplot2 Huber plot
  # by combining all three components: original data, index profile, reference circle
  # the slope for the best projection is included as a column in `idx_profile_dt`
  prep_obj <- prep_huber_workhorse(data[1:2], index_fun)
  prep_res <- prep_obj$idx_df

  # get the original data
  orig_df <- prep_obj$data |> dplyr::mutate(type = "original")
  colnames(orig_df) <- c("x", "y", "type")

  # compute the idx_profile data
  idx_profile_dt <- prep_res |>
    dplyr::mutate(
      range = round(max(index) - min(index), 5),
      idx_scaled = (index - min(index))/range * 2 + 3,
      x = idx_scaled * cos(theta),
      y = idx_scaled * sin(theta))

  sel_idx <- which(prep_res$index > signif(max(prep_res$index), 6) - 1e-06)
  theta_best <- pi/180 * (sel_idx - 1)
  idx_profile_dt <- idx_profile_dt |> dplyr::select(x, y) |>
    dplyr::mutate(slope = sin(theta_best)/cos(theta_best), type = "idx_profile")

  # compute the reference circle data
  circle_df <- prep_res |>
    dplyr::mutate(x = 4 * cos(theta), y = 4 * sin(theta), type = "circle") |>
    dplyr::select(x, y, type)

  dplyr::bind_rows(orig_df, idx_profile_dt, circle_df)
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
globalVariables(c("i", "theta", "proj_data", "idx_scaled", "type"))
