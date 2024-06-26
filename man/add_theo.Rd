% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw.R
\name{add_theo}
\alias{add_theo}
\title{A ggproto for drawing the theoretical basis, if applicable}
\usage{
add_theo(
  dt,
  theo_label = "*",
  theo_size = 25,
  theo_alpha = 0.8,
  theo_color = "#000000",
  ...
)
}
\arguments{
\item{dt}{A data object from the running the optimisation algorithm in guided tour}

\item{theo_label}{character; a symbol to label the theoretical point}

\item{theo_size}{numeric; the size of the theoretical point}

\item{theo_alpha}{numeric; the alpha of the theoretical point}

\item{theo_color}{character; the colour of the theoretical point in hex}

\item{...}{other aesthetics inherent from \code{explore_space_pca()}}
}
\value{
a wrapper for drawing theoretical points in \code{explore_space_pca()}
}
\description{
This is  a wrapper function used by \code{explore_space_pca()} and
should be be called directly by the user
}
\seealso{
Other draw functions: 
\code{\link{add_anchor}()},
\code{\link{add_anno}()},
\code{\link{add_dir_search}()},
\code{\link{add_end}()},
\code{\link{add_interp}()},
\code{\link{add_interp_last}()},
\code{\link{add_interrupt}()},
\code{\link{add_search}()},
\code{\link{add_space}()},
\code{\link{add_start}()}
}
\concept{draw functions}
