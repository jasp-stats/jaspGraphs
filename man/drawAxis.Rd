% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{drawAxis}
\alias{drawAxis}
\title{Deprecated: use \code{ggplot2::ggplot() + ggplot2::scale_x|y_continuous(...)} instead.}
\usage{
drawAxis(
  graph = NULL,
  xName = waiver(),
  yName = waiver(),
  breaks = waiver(),
  xBreaks = waiver(),
  yBreaks = waiver(),
  dat = NULL,
  xLabels = waiver(),
  yLabels = waiver(),
  xLimits = waiver(),
  yLimits = waiver(),
  force = FALSE,
  secondaryXaxis = waiver(),
  secondaryYaxis = waiver(),
  xTrans = "identity",
  yTrans = "identity"
)
}
\arguments{
\item{graph}{ggplot object.}

\item{xName}{name for x-axis.}

\item{yName}{name for y-axis.}

\item{breaks}{a list with $xBreaks and $yBreaks or waiver().}

\item{xBreaks}{x-axis breaks.}

\item{yBreaks}{y-axis breaks.}

\item{dat}{data.frame.}

\item{xLabels}{labels for x-axis.}

\item{yLabels}{labels for y-axis.}

\item{xLimits}{limits for x-axis.}

\item{yLimits}{limits for y-axis.}

\item{force}{force the axes to be present at the cost of drawing an invisible geom.}

\item{secondaryXaxis}{secondary x-axis.}

\item{secondaryYaxis}{secondary y-axis.}

\item{xTrans}{transformation function for the x-axis.}

\item{yTrans}{transformation function for the y-axis.}
}
\description{
Deprecated: use \code{ggplot2::ggplot() + ggplot2::scale_x|y_continuous(...)} instead.
}
