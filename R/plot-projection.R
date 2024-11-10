#' Plot the projection from the optimisation data collected from projection pursuit
#' @param dt a data object collected by the projection pursuit guided tour optimisation in \code{tourr}
#' @param data the original data
#' @param id the grouping variable
#' @param cols additional columns to include in the plot
#' @param label logical, whether to label each panel by its index value
#' @param animate_along the variable to animate along
#' @param keep numeric, the proportion of the data to keep for animation
#'  (default is 0.2). Only used when `animate_along` is not NULL
#' @return a ggplot object
#' @examples
#' library(dplyr)
#' holes_2d_jellyfish |>
#'   filter(loop == 1, tries %in% seq(1, 50, 5)) |>
#'   plot_projection(data = boa6)
#' \dontrun{
#' library(dplyr)
#' # track the first jellyfish (loop == 1)
#' holes_2d_jellyfish |>
#'   filter(loop == 1) |>
#'   plot_projection(data = boa6, animate_along = tries, id = loop)
#' }
#'
#' @rdname projection
#' @export
plot_projection <- function(dt, data, id = NULL, cols = NULL, label = TRUE,
                            animate_along = NULL, keep = 0.2){
  cols <- dplyr::syms(cols)
  id <- dplyr::enquo(id)
  animate_along <- dplyr::enquo(animate_along)
  if (rlang::quo_is_null(id)){id <- dplyr::quo(.id)}

  proj_df <- compute_projection(dt, as.matrix(data), id = rlang::as_label(id), cols = cols)

  d <- ncol(proj_df) - ncol(dt) - 1 # the extra one for .id

  if (!rlang::quo_is_null(animate_along)){
    num <- floor(1/keep)
    proj_df <- proj_df |> dplyr::filter(dplyr::row_number() %% num == 0)
  }

  if (d == 2){
   p <- proj_df |>
     ggplot2::ggplot(ggplot2::aes(x = V1, y = V2)) +
     ggplot2::geom_point(size = 0.5)
  } else if (d == 1) {
    p <- proj_df |>
      ggplot2::ggplot(ggplot2::aes(x = V1)) +
      ggplot2::geom_density()
  }

  if (label){
    label_df <- proj_df |> dplyr::select(index_val, !!id) |> unique()
    p <- p +
      ggplot2::geom_label(data = label_df, x = 3.2, y = 4, size = 3,
                          ggplot2::aes(label = round(index_val, 3)))
  }


  res <- p +
    ggplot2::facet_wrap(ggplot2::vars(!!id)) +
    ggplot2::theme_bw() +
    ggplot2::theme(aspect.ratio = 1,
                   axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::xlim(-5, 5) + ggplot2::ylim(-5, 5)

  if (!rlang::quo_is_null(animate_along)){
    animate_along <- rlang::as_label(animate_along)
    res <- res +
      ggplot2::labs(title = 'Iteration: {as.integer(frame_time)}') +
      gganimate::transition_time(!!rlang::sym(animate_along))
  }

  return(res)
}


#' @rdname projection
#' @export
compute_projection <- function(dt, data, id = NULL, cols = NULL){
  basis_d <- sapply(dt$basis, function(xx) dim(xx)[2], simplify = TRUE) |> unique()

  if (basis_d > 2) {
    cli::cli_abort("The basis dimension should be less than 2")
  }

  cols <- dplyr::syms(cols)
  id <- dplyr::enquo(id)
  if (rlang::quo_is_missing(id)){id <- dplyr::quo(.id)}

  suppressWarnings(
    dt |>
      dplyr::mutate(.id = dplyr::row_number()) |>
      dplyr::rowwise() |>
      dplyr::mutate(proj = list(tibble::as_tibble(as.matrix(data) %*% basis))) |>

      tidyr::unnest(proj) |>
      dplyr::ungroup()
  )
}
globalVariables(c("V1", "V2", ".id", "proj", "index_val"))
