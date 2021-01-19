#' A specific theme for trace plots
#' @importFrom ggplot2 %+replace%
#' @export
theme_fern <- function() {
  ggplot2::theme_bw() %+replace%
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(),
      panel.grid.minor = ggplot2::element_blank(),
    )
}

#' Better label formatter to avoid overlapping
#' @param labels a numerical vector of labels
#' @param accuracy the accuracy of the label
#' @examples
#' format_label(c(0.87, 0.87, 0.9, 0.93, 0.95), 0.01)
#' format_label(c(0.87, 0.87, 0.9, 0.93, 0.95, 0.96, 0.96), 0.01)
#' @export
format_label <- function(labels, accuracy) {
  too_close <- diff(labels) < accuracy
  idx <- which(too_close)
  out <- c(labels[1])
  for (i in 2:length(labels)) {
    if (labels[i] - out[length(out)] > accuracy) {
      out <- append(out, labels[i])
    } else {
      if (i > length(labels) / 2) {
        if (labels[i - 2] %in% out) {
          out <- out[-length(out)]
          out <- append(out, labels[i])
        }
      }

      if (i == length(labels)) {
        out <- out[-length(out)]
        out <- append(out, labels[i])
      }
    }
  }

  return(out)
}
