#' Data objects collected during the projection pursuit optimisation
#'
#' Simulated data to demonstrate the usage of four diagnostic plots in the package,
#' users can create their own guided tour data objects and diagnose with the visualisation designed in this package.
#'
#' The prefix \code{holes_*} indicates the use of holes index in the guided tour.
#' The suffix \code{*_better/geo/jellyfish} indicates the optimiser used:
#'  \code{search_better}, \code{search_geodesic}, \code{search_jellyfish}.
#'
#' @examples
#'holes_1d_better %>%
#' explore_trace_interp(interp_size = 2) +
#'   scale_color_continuous_botanical(palette = "fern")
#' @rdname data
"holes_1d_geo"

#' @rdname data
"holes_1d_better"

#' @rdname data
"holes_1d_jellyfish"

#' @rdname data
"holes_2d_better"

#' @rdname data
"holes_2d_better_max_tries"
