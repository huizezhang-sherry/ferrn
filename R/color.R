#'@title A customised color palette based on australian botanicals
#'@export
#'@rdname botanical_color
botanical_palettes <- list(
  #quantitative
  daisy = c("#252B53", # purple
            "#C493D9", # pink
            #"#FDFAF6", # white
            "#F1C916", # yellow
            "#DF9A42",  # orange
            "#6E716A", # grey
            "#60812A" # green
  ),

  # sequential
  fern_brown = c("#50372B", # mature dark brown
           # "#654431", # mature dark brown
           # "#804C23", # young curl brown
           # "#895825", # mature light brown
           "#A66C22" # mature light brown
           # "#8F6B30", # young lighter brown
           # "#AF8A45" # young lighter brown
  ),

  fern_green = c(

           "#2D431A", # leaves darker green
           # "#385730", # mature green
           # "#557664", # mature green
           "#72A066" # leaves dark green
           # "#5A6929", # young stem green
           # "#A3C3BE", # leaves light green
           # "#A7BA3D", # young stem green
           # "#C2EE94" # young stem green

  ),

  fern = c("#A66C22",  # mature light brown
            "#72A066" # leaves dark green
            ),

  cherry = c("#524340",  #orchre
             "#B4B754",  # green
             "#F3B422", # yellow
             "#D5A04D" # yellow with shade
             #"#FDFAF6", # white


  ),

  # diverging
  acacia = c("#7F6219", # dark yellow
             "#FBCC0A" # yellow
             #"#FEF531" # bright yellow
             #"#51771A" # leaf
  ),

  banksia = c("#4B1E07", # edge red
              "#B46515", # mid red
              "#4E6D24" # leves
  )
)

#'@title Color interpolation for botanical palettes
#'@export
#'@rdname botanical_color
botanical_pal <- function(palette = "fern", reverse = FALSE){
  pal <- botanical_palettes[[palette]]

  if (reverse){
    pal <- rev(pal)
  }
  return(colorRampPalette(pal))
}


#'@title A scale function in compatible with ggplot2
#'@export
#'@rdname botanical_color
scale_color_botanical <- function(..., palette = "fern", discrete = TRUE, reverse = FALSE){

  if (discrete){
    discrete_scale("color", "botanical", palette = botanical_pal(palette, reverse))
  }else{
    scale_color_gradientn(colors = botanical_pal(palette)(256))
  }

}
