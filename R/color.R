#' Available colours in the palettes
#' @title A customised colour palette based on Australian botanies
#' @rdname color
#' @export
botanical_palettes <- list(
  # quantitative
  daisy = c(
    "#252B53", # purple
    "#C493D9", # pink
    "#6E716A" # grey
  ),


  banksia = c(
    "#4B1E07", # edge red
    "#B46515", # mid red
    "#4E6D24"
  ),

  cherry = c(
    "#524340", # ochre
    "#B4B754", # green
    "#F3B422" # yellow
  ),

  # sequential
  fern = c("#A56C30", "#7D5F24", "#B89436", "#4D6F32", "#364F24", "#2C4409"),
  acacia = c("#A39224", "#CCB113", "#7C8C60", "#4A5529")
)

#' Colour interpolation
#'
#' @param palette Colour palette from the botanical_palette
#' @param reverse logical, if the colour should be reversed
#' @export
#' @return a function for interpolating colour in the botanical palette
#' @rdname color
botanical_pal <- function(palette = "fern", reverse = FALSE) {
  pal <- botanical_palettes[[palette]]

  if (reverse) {
    pal <- rev(pal)
  }
  return(grDevices::colorRampPalette(pal))
}

#' continuous scale colour function
#'
#' @param ... other arguments passed into scale_color_gradientn
#' @param palette the colour palette from the botanical_palette
#' @param reverse logical; if the colour should be reversed
#' @export
#' @return a wrapper for continuous scales in the botanical palette
#' @rdname scale
scale_color_continuous_botanical <- function(palette = "fern", reverse = FALSE, ...) {
    ggplot2::scale_color_gradientn(colors = botanical_pal(palette)(256), ...)
}

#' Discrete scale colour function
#'
#' @param palette the colour palette from the botanical_palette
#' @param reverse logical; if the colour should be reversed
#' @export
#' @return a wrapper for discrete scales in the botanical palette
#' @rdname scale
scale_color_discrete_botanical <- function(palette = "fern", reverse = FALSE, ...) {
  ggplot2::discrete_scale("color", "botanical", palette = botanical_pal(palette, reverse), ...)
}

#' continuous scale fill function
#'
#' @param ... other arguments passed into scale_color_gradientn
#' @param palette colour palette from the botanical_palette
#' @param reverse logical; if the colour should be reversed
#' @export
#' @return a wrapper for continuous fill in the botanical palette
#' @rdname scale
scale_fill_continuous_botanical <- function(palette = "fern", reverse = FALSE, ...) {
  ggplot2::scale_fill_gradientn(colors = botanical_pal(palette, ...)(256), ...)

}

#' discrete scale fill function
#' @param palette colour palette from the botanical_palette
#' @param reverse logical; if the colour should be reversed
#' @export
#' @return a wrapper for discrete fill in the botanical palette
#' @rdname scale
scale_fill_discrete_botanical <- function(palette = "fern", reverse = FALSE, ...) {
  ggplot2::discrete_scale("fill", "botanical", palette = botanical_pal(palette, reverse), ...)

}

#' A specific theme for trace plots
#' @importFrom ggplot2 %+replace%
#' @return a ggplot2 theme for \code{explore_trace_interp()}
#' @export
theme_fern <- function() {
  ggplot2::theme_bw() %+replace%
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(),
      panel.grid.minor = ggplot2::element_blank(),
    )
}

