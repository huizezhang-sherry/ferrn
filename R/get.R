#' Functions to get components from the data collecting object
#' @param dt a data object collected by the projection pursuit guided tour optimisation in the \code{tourr} package
#' @param group the variable to label different runs of the optimiser(s)
#' @param iter the variable to be counted by
#' @param group the variable to label different runs of the optimiser(s)
#' @param precision numeric; if the index value of the last interpolating point and the anchor point differ by \code{precision}, an interruption is registered
#' @param ratio numeric; a buffer value to deviate directional search points from the anchor points
#' @param ... other arguments passed to \code{compute_pca()}
#' @importFrom rlang .data
#' @details
#' \code{get_best}: extract the best basis found by the optimiser(s)
#'
#' \code{get_start}: extract the start point of the optimisation
#'
#' \code{get_interp}: extract the interpolation points
#'
#' \code{get_interp_last}: extract the last point in each interpolation
#'
#' \code{get_anchor}: extract the anchor points on the geodesic path
#'
#' \code{get_search}: extract search points in the optimisation (for
#' \code{search_geodesic})
#'
#' \code{get_dir_search}: extract directional search points (for
#' \code{search_geodesic})
#'
#' \code{get_space_param}: estimate the radius of the background circle
#' based on the randomly generated points. The space of projected bases is a
#' circle when reduced to 2D. A radius is estimated using the largest distance
#' from the bases in the data object to the centre point.
#'
#' \code{get_theo}: extract the theoretical basis, if exist
#'
#' \code{get_interrupt}: extract the end point of the interpolation and the
#' target point in the iteration when an interruption happens. The optimiser
#' can find better basis on the interpolation path, an interruption is
#' implemented to stop further interpolation from the highest point to the
#' target point. This discrepancy is highlighted in the PCA plot.
#'
#' \code{get_search_count}: summarise the number of search points in each iteration
#'
#' \code{get_basis_matrix}: extract all the bases as a matrix
#'
#' @examples
#' get_search(holes_1d_geo)
#'
#' get_anchor(holes_1d_geo)
#'
#' get_start(holes_1d_better)
#'
#' get_interrupt(holes_1d_better)
#'
#' get_interp(holes_1d_better) %>% head()
#'
#' get_basis_matrix(holes_1d_better) %>% head()
#'
#' get_best(dplyr::bind_rows(holes_1d_better, holes_1d_geo), group = method)
#'
#' get_search_count(holes_1d_better)
#' get_search_count(dplyr::bind_rows(holes_1d_better, holes_1d_geo), group = method)
#'
#' get_interp_last(holes_1d_better)
#' get_interp_last(dplyr::bind_rows(holes_1d_better, holes_1d_geo), group = method)
#'
#' res <- holes_1d_geo %>% compute_pca() %>% purrr::pluck("aug")
#' get_dir_search(res)
#'
#' best <- matrix(c(0, 1, 0, 0, 0), nrow = 5)
#' holes_1d_better %>%
#'   bind_theoretical(best, tourr::holes(), raw_data = boa5) %>%
#'   get_theo()
#' @rdname get
#' @return a tibble object containing the best basis found by the optimiser(s)
#' @export
get_best <- function(dt, group = NULL) {
  group <- rlang::enexpr(group) |> as.list()
  if (length(group) != 1) {group <- group[-1]}


  search_methods <- unique(dt[["method"]])
  if (!"search_jellyfish" %in% search_methods) {
    dt <- dt %>% dplyr::filter(.data[["info"]] == "interpolation")
  }

  res <- dt %>%
    dplyr::group_by(!!!group) %>%
    dplyr::filter(.data$index_val == max(.data$index_val, na.rm = TRUE)) %>%
    dplyr::distinct(.data$index_val, .keep_all = TRUE) |>
    dplyr::ungroup()

  res
}

#' @rdname get
#' @export
get_start <- function(dt, group = NULL) {
  dt %>%
    dplyr::group_by({{group}}) %>%
    dplyr::filter(.data[["tries"]] == 1)
}

#' @rdname get
#' @export
get_interp <- function(dt, group = NULL) {

  search_methods <- unique(dt[["method"]])
  if (!"search_jellyfish" %in% search_methods) {
    dt <- dt %>% dplyr::filter(.data[["info"]] == "interpolation")
  } else{
    dt <- dt |> dplyr::filter(.data[["method"]] == "search_jellyfish")
  }

  dt %>%
    #dplyr::filter(grepl("search", dt[["method"]])) %>%
    dplyr::group_by({{ group }}) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::ungroup()
}

#' @rdname get
#' @export
get_interp_last <- function(dt, group = NULL) {
  group <- dplyr::enexpr(group)

  if (!all(c("tries", "loop") %in% colnames(dt))) {
    stop("The data object must have variables tries and loop")
  }

  dt %>%
    get_interp(group = {{ group }}) %>%
    dplyr::group_by(.data$tries, {{ group }}) %>%
    dplyr::filter(.data$loop == max(.data$loop)) %>%
    dplyr::ungroup()
}

#' @rdname get
#' @export
get_anchor <- function(dt, group = NULL) {
  dt %>%
    dplyr::filter(.data$info %in% c("new_basis", "best_line_search")) %>%
    dplyr::group_by({{ group }}) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::ungroup()
}


#' @rdname get
#' @export
get_search <- function(dt) {
  dt %>%
    dplyr::filter(stringr::str_detect(.data[["info"]], "search"))
}


#' @rdname get
#' @export
get_dir_search <- function(dt, ratio = 5, ...) {
  # check only valid for search_geodesic or pseudo-derivative
  if (!"PC1" %in% colnames(dt)) {
    message("get_dir_search() needs to be used on data projected by compute_pca()")
    return(NULL)
  }
  dt <- dt %>% dplyr::filter(.data$method %in% c("PD", "search_geodesic"))

  if (nrow(dt) == 0) return(NULL)

  # compute the anchor points
  anchor <- dt %>%
    get_anchor() %>%
    dplyr::group_by(.data$tries) %>%
    dplyr::filter(.data$loop == max(.data$loop)) %>%
    dplyr::rename(anchor_x = .data$PC1, anchor_y = .data$PC2) %>%
    dplyr::select(.data$tries, .data$anchor_x, .data$anchor_y) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(tries = dplyr::lead(.data$tries)) %>%
    dplyr::filter(!is.na(.data$tries))

  # compute the buffer
  dir_search <- dt %>% dplyr::filter(.data$info %in% c("direction_search", "best_direction_search"))

  dir_search %>%
    dplyr::left_join(anchor, by = c("tries")) %>%
    dplyr::mutate(
      PC1 = ifelse(.data$PC1 - .data$anchor_x < 0,
        .data$PC1 - abs(.data$PC1 - .data$anchor_x) * ratio,
        .data$PC1 + abs(.data$PC1 - .data$anchor_x) * ratio
      ),
      PC2 = ifelse(.data$PC2 - .data$anchor_y < 0,
        .data$PC2 - abs(.data$PC2 - .data$anchor_y) * ratio,
        .data$PC2 + abs(.data$PC2 - .data$anchor_y) * ratio
      )
    )
}


#' @rdname get
#' @export
get_space_param <- function(dt, ...) {
  basis <- dt[1, ] %>% dplyr::pull(basis)
  n_row <- nrow(basis[[1]])
  n_col <- ncol(basis[[1]])

  dt <- dt %>%
    dplyr::select(-dplyr::contains("PC")) %>%
    dplyr::filter(.data$info != "randomly_generated") %>%
    dplyr::add_row(
      basis = list(matrix(rep(0, n_row * n_col), nrow = n_row, ncol = n_col)),
      info = "origin"
    ) %>%
    compute_pca(...) %>%
    purrr::pluck("aug")

  centre <- dt %>%
    dplyr::filter(.data$info == "origin") %>%
    dplyr::rename(x0 = .data$PC1, y0 = .data$PC2)

  x0 <- centre$x0
  y0 <- centre$y0

  r <- dt %>%
    dplyr::mutate(dist = sqrt((.data$PC1 - x0)^2 + (.data$PC2 - y0)^2)) %>%
    dplyr::filter(.data$dist == max(.data$dist)) %>%
    dplyr::pull(.data$dist)

  tibble::tibble(x0 = x0, y0 = y0, r = r)
}


#' @rdname get
#' @export
get_theo <- function(dt) {
  theo <- dt %>% dplyr::filter(.data$info == "theoretical")

  if ("PC1" %in% colnames(theo)) {
    out <- theo %>%
      dplyr::select(.data$PC1, .data$PC2)
  } else {
    out <- theo
  }

  out
}


#' @rdname get
#' @export
get_interrupt <- function(dt, group = NULL, precision = 0.001) {

  if (any(unique(dt$method) %in% c("CRS","SA", "search_better", "search_better_random"))) {
    dt <- dt %>% dplyr::filter(dt$method %in% c("CRS", "SA" , "search_better", "search_better_random"))

    anchor <- dt %>% get_anchor()
    interp_last <- dt %>% get_interp_last(group = {{ group }})

    interp_anchor <- dplyr::bind_rows(anchor, interp_last)

    problem_tries <- interp_anchor %>%
      dplyr::group_by({{ group }}) %>%
      dplyr::select(.data$info, .data$index_val, .data$tries) %>%
      tidyr::pivot_wider(names_from = .data$info, values_from = .data$index_val) %>%
      dplyr::mutate(match = ifelse(abs(round(.data$new_basis, 3) - round(.data$interpolation, 3)) > precision, TRUE, FALSE)) %>%
      dplyr::filter(match) %>%
      dplyr::mutate(id = paste0({{ group }}, .data$tries)) %>%
      dplyr::ungroup()

    interp_anchor %>%
      dplyr::mutate(id = paste0({{ group }}, .data$tries)) %>%
      dplyr::filter(.data$id %in% problem_tries$id)
  } else {
    message("interrupt is only implemented for creeping random search (search_better) and simulated annealing (search_better_random)")
    return(NULL)
  }
}

#' @rdname get
#' @export
get_search_count <- function(dt, iter = NULL, group = NULL) {

  if (rlang::quo_is_null(dplyr::enquo(iter))) {
    message("map tries to the x-axis")
    iter <- dplyr::sym("tries")
  }

  dt_search <- dt %>%
    dplyr::filter(stringr::str_detect(.data[["info"]], "search|new_basis")) %>%
    dplyr::group_by({{ iter }})

  if (!rlang::quo_is_null(dplyr::enquo(group))) dt_search <- dt_search %>% dplyr::group_by({{ iter }}, {{ group }})

  dt_count <- dt_search %>%
    dplyr::summarise(n = dplyr::n())

  if (!rlang::quo_is_null(dplyr::enquo(group))) dt_count <- dt_count %>% dplyr::arrange({{ group }})

  dt_count
}

#' @rdname get
#' @export
get_basis_matrix <- function(dt) {
  if (!"basis" %in% colnames(dt)) {
    stop("The dataset needs to have a basis column")
  }

  num_col <- ncol(dt$basis[[1]])
  num_row <- nrow(dt$basis[[1]])

  basis <- purrr::flatten_dbl(dt$basis) %>% matrix(ncol = num_row * num_col, byrow = TRUE)
  colnames(basis) <- paste0("V", 1:(num_row * num_col))
  basis
}
