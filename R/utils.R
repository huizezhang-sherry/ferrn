#' Better label formatting to avoid overlapping
#' @param labels a numerical vector of labels
#' @param accuracy the accuracy of the label
#' @examples
#' format_label(c(0.87, 0.87, 0.9, 0.93, 0.95), 0.01)
#' format_label(c(0.87, 0.87, 0.9, 0.93, 0.95, 0.96, 0.96), 0.01)
#' @return a vector of adjusted labels
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


#' Clean method names
#' @param dt a data object
#' @export
#' @rdname relevel
#' @examples
#' head(clean_method(holes_1d_better), 5)
#' @return a tibble with method cleaned
clean_method <- function(dt) {
  dt %>%
    dplyr::mutate(method = dplyr::case_when(
      .data$method == "search_better" ~ "CRS",
      .data$method == "search_better_random" ~ "SA",
      .data$method == "search_geodesic" ~ "PD",
      .data$method == "search_polish" ~ "polish",
      TRUE ~ NA_character_
    ))
}
