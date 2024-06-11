#' Plot the projection from the optimisation data collected from projection pursuit
#' @param dt a data object collected by the projection pursuit guided tour optimisation in \code{tourr}
#' @param data the original data
#' @param cols additional columns to include in the plot
#' @return a ggplot object
#' @examples
#' holes_1d_jellyfish |> get_best() |> plot_projection(data = boa5)
#' @rdname projection
#' @export
plot_projection <- function(dt, data, cols = NULL){

  cols <- dplyr::syms(cols)
  proj_df <- compute_projection(dt, data, cols = cols)

  d <- ncol(proj_df) - 1 - length(cols)

  if (d == 2){
   p <- proj_df |>
      ggplot2::ggplot() +
      ggplot2::geom_point(ggplot2::aes(x = V1, y = V2), size = 0.5)
  } else if (d == 1) {
    p <- proj_df |>
      ggplot2::ggplot() +
      ggplot2::geom_density(ggplot2::aes(x = V1))
  }

  p + ggplot2::facet_wrap(ggplot2::vars(.id)) +
    ggplot2::theme_bw() +
    ggplot2::theme(aspect.ratio = 1,
                   axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   #axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())
}


#' @rdname projection
#' @export
compute_projection <- function(dt, data, cols = NULL){

  basis_d <- sapply(dt$basis, function(xx) dim(xx)[2], simplify = TRUE) |> unique()

  if (basis_d > 2) {
    cli::cli_abort("The basis dimension should be less than 2")
  }

  cols <- dplyr::syms(cols)

  suppressWarnings(
    dt |>
      dplyr::mutate(.id = dplyr::row_number()) |>
      dplyr::rowwise() |>
      dplyr::mutate(proj = list(tibble::as_tibble(as.matrix(data) %*% basis))) |>
      dplyr::select(.id, proj, !!!cols) |>
      tidyr::unnest(proj) |>
      dplyr::ungroup()
  )
}
globalVariables(c("V1", "V2", ".id", "proj"))
