#' Available colors in the palettes
#' @title A customised color palette based on australian botanicals
#' @family botan
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
    "#4E6D24" # leves
  ),

  cherry = c(
    "#524340", # orchre
    "#B4B754", # green
    "#F3B422" # yellow
  ),

  # sequential
  fern = c("#A56C30", "#7D5F24", "#B89436", "#4D6F32", "#364F24", "#2C4409"),
  acacia = c("#A39224", "#CCB113", "#7C8C60", "#4A5529")
)

#' Color interpolation
#'
#' @param palette Color palette from the botanical_palette
#' @param reverse logical, if the color should be reversed
#' @export
#' @family botan
botanical_pal <- function(palette = "fern", reverse = FALSE) {
  pal <- botanical_palettes[[palette]]

  if (reverse) {
    pal <- rev(pal)
  }
  return(grDevices::colorRampPalette(pal))
}

#' continous scale color function
#'
#' @param ... Arguments passed into scale_color_gradientn
#' @param palette Color palette from the botanical_palette
#' @param reverse logical, if the color should be reversed
#' @export
#' @family botan
scale_color_continuous_botanical <- function(..., palette = "fern", reverse = FALSE) {
    ggplot2::scale_color_gradientn(colors = botanical_pal(palette, ...)(256))
}

#' Discrete scale color function
#'
#' @param palette Color palette from the botanical_palette
#' @param reverse logical, if the color should be reversed
#' @export
#' @family botan
scale_color_discrete_botanical <- function(palette = "fern", reverse = FALSE) {
  ggplot2::discrete_scale("color", "botanical", palette = botanical_pal(palette, reverse))
}

#' continous scale fill function
#'
#' @param ... Arguments passed into scale_color_gradientn
#' @param palette Color palette from the botanical_palette
#' @param reverse logical, if the color should be reversed
#' @export
#' @family botan
scale_fill_continuous_botanical <- function(..., palette = "fern", reverse = FALSE) {
  ggplot2::scale_fill_gradientn(colors = botanical_pal(palette, ...)(256))

}

#' discrete scale fill function
#' @param palette Color palette from the botanical_palette
#' @param reverse logical, if the color should be reversed
#' @export
#' @family botan
scale_fill_discrete_botanical <- function(palette = "fern", reverse = FALSE) {
  ggplot2::discrete_scale("fill", "botanical", palette = botanical_pal(palette, reverse))

}
