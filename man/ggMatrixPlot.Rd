% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggMatrixPlot.R
\name{ggMatrixPlot}
\alias{ggMatrixPlot}
\alias{ggMatrixPlot.matrix}
\alias{ggMatrixPlot.list}
\alias{ggMatrixPlot.default}
\title{ggMatrixPlot}
\usage{
ggMatrixPlot(
  plotList = NULL,
  nr = NULL,
  nc = NULL,
  ...,
  leftLabels = NULL,
  topLabels = NULL,
  rightLabels = NULL,
  bottomLabels = NULL,
  removeXYlabels = c("xy", "x", "y", "none"),
  labelSize = 0.4 * graphOptions("fontsize"),
  labelPos = matrix(0.5, 4, 2),
  scaleXYlabels = c(0.9, 0.9),
  debug = FALSE
)

\method{ggMatrixPlot}{matrix}(
  plotList = NULL,
  nr = NULL,
  nc = NULL,
  layout = NULL,
  ...,
  leftLabels = NULL,
  topLabels = NULL,
  rightLabels = NULL,
  bottomLabels = NULL,
  removeXYlabels = c("xy", "x", "y", "none"),
  labelSize = 0.4 * graphOptions("fontsize"),
  labelPos = matrix(0.5, 4, 2),
  scaleXYlabels = c(0.9, 0.9),
  debug = FALSE
)

\method{ggMatrixPlot}{list}(
  plotList = NULL,
  nr = NULL,
  nc = NULL,
  layout = NULL,
  ...,
  leftLabels = NULL,
  topLabels = NULL,
  rightLabels = NULL,
  bottomLabels = NULL,
  removeXYlabels = c("xy", "x", "y", "none"),
  labelSize = 0.4 * graphOptions("fontsize"),
  labelPos = matrix(0.5, 4, 2),
  scaleXYlabels = c(0.9, 0.9),
  debug = FALSE
)

\method{ggMatrixPlot}{default}(
  plotList = NULL,
  nr = NULL,
  nc = NULL,
  layout = NULL,
  ...,
  leftLabels = NULL,
  topLabels = NULL,
  rightLabels = NULL,
  bottomLabels = NULL,
  removeXYlabels = c("xy", "x", "y", "none"),
  labelSize = 0.4 * graphOptions("fontsize"),
  labelPos = matrix(0.5, 4, 2),
  scaleXYlabels = c(0.9, 0.9),
  debug = FALSE
)
}
\arguments{
\item{plotList}{a list of ggplot2 objects}

\item{nr}{number of rows}

\item{nc}{number of columns}

\item{...}{ignored.}

\item{leftLabels}{labels left of the plots in plotList.}

\item{topLabels}{labels above the plots in plotList.}

\item{rightLabels}{labels right of the plots in plotList.}

\item{bottomLabels}{labels below the plots in plotList.}

\item{removeXYlabels}{Whether to remove the x and y axes titles.}

\item{labelSize}{two scalars for the magnification of the the x and y labels respectively.}

\item{labelPos}{relative position for the labels around the plots. The first column contains x-coordinates and the second y-coordinates. The first row is top, second right, third bottom, and fourth left (TRouBLe).}

\item{scaleXYlabels}{two scalars for the magnification of the the x and y labels respectively.}

\item{debug}{create an debug plot (see examples).}

\item{layout}{a matrix that specifies the position of each plot, akin to the layout for base plots.}
}
\description{
ggMatrixPlot
}
\details{
This function is intended to be called with a matrix as first argument, although other input is also supported.
}
\examples{
\dontrun{

library(ggplot2)
data("diamonds", package = "ggplot2")
vars  <- colnames(diamonds)[c(1, 5)]
nvars <- length(vars)

plotMatrix <- matrix(
  data = list(),
  nrow = nvars,
  ncol = nvars,
  dimnames = list(vars, vars)
)

for (i in seq_along(vars)) for (j in seq_along(vars)) {

  plotMatrix[[i, j]] <-
    ggplot(data = diamonds, aes_string(x = vars[i], y = vars[j])) +
    geom_point() +
    geom_rangeframe() +
    themeJaspRaw()

}

ggMatrixPlot(plotMatrix)

# gives an idea about how ggMatrixPlot works, you can add labels on all sides
# of the center (where the plots should be).
ggMatrixPlot(debug = TRUE)
}
}
