#' Simulated sine, pipe, and gaussian mixture
#'
#' Simulated sine and pipe data for calculating optimisation features.
#' Each dataset has 1000 observations and the last two columns contain the
#' intended structure with the rest being noise.
#' @rdname pipe-sine-boa
#' @examples
#' library(ggplot2)
#' library(tidyr)
#' library(dplyr)
#' boa %>%
#'   pivot_longer(cols = x1:x10, names_to = "var", values_to = "value") %>%
#'   mutate(var = forcats::fct_relevel(as.factor(var), paste0("x", 1:10))) %>%
#'   ggplot(aes(x = value)) +
#'   geom_density() +
#'   facet_wrap(vars(var))
#'
#' sine1000 |> ggplot(aes(x = V5, y = V6)) + geom_point() + theme(aspect.ratio = 1)
#' pipe1000_8d |> ggplot(aes(x = V5, y = V6)) + geom_point() + theme(aspect.ratio = 1)
#' pipe1000_8d |> ggplot(aes(x = V7, y = V8)) + geom_point() + theme(aspect.ratio = 1)
"sine1000"

#' @rdname pipe-sine-boa
"sine1000_8d"

#' @rdname pipe-sine-boa
"pipe1000"

#' @rdname pipe-sine-boa
"pipe1000_8d"

#' @rdname pipe-sine-boa
"pipe1000_10d"

#' @rdname pipe-sine-boa
"pipe1000_12d"

#' @rdname pipe-sine-boa
"boa"

#' @rdname pipe-sine-boa
"boa5"

#' @rdname pipe-sine-boa
"boa6"
