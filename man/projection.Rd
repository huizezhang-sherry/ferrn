% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-projection.R
\name{plot_projection}
\alias{plot_projection}
\alias{compute_projection}
\title{Plot the projection from the optimisation data collected from projection pursuit}
\usage{
plot_projection(
  dt,
  data,
  id = NULL,
  cols = NULL,
  label = TRUE,
  animate_along = NULL,
  keep = 0.2
)

compute_projection(dt, data, id = NULL, cols = NULL)
}
\arguments{
\item{dt}{a data object collected by the projection pursuit guided tour optimisation in \code{tourr}}

\item{data}{the original data}

\item{id}{the grouping variable}

\item{cols}{additional columns to include in the plot}

\item{label}{logical, whether to label each panel by its index value}

\item{animate_along}{the variable to animate along}

\item{keep}{numeric, the proportion of the data to keep for animation
(default is 0.2). Only used when `animate_along` is not NULL}
}
\value{
a ggplot object
}
\description{
Plot the projection from the optimisation data collected from projection pursuit
}
\examples{
library(dplyr)
holes_2d_jellyfish |>
  filter(loop == 1, tries \%in\% seq(1, 50, 5)) |>
  plot_projection(data = boa6)
\dontrun{
library(dplyr)
# track the first jellyfish (loop == 1)
holes_2d_jellyfish |>
  filter(loop == 1) |>
  plot_projection(data = boa6, animate_along = tries, id = loop)
}

}
