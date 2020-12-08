#' A specific theme for trace plots
#' @export
theme_fern <- function(){
  theme_bw() %+replace%
    theme(
      panel.grid.major = element_line(),
      panel.grid.minor = element_blank(),
    )
}
