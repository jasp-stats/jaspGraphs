% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/themeApa.R
\name{themeApaRaw}
\alias{themeApaRaw}
\title{APA theme for ggplot2 objects based on \code{jtools::theme_apa}}
\usage{
themeApaRaw(
  legend.pos = getGraphOption("legend.position"),
  legend.use.title = FALSE,
  legend.font.size = getGraphOption("axis.title.cex"),
  font.size = getGraphOption("fontsize"),
  x.font.size = getGraphOption("fontsize"),
  y.font.size = getGraphOption("fontsize"),
  facet.title.size = getGraphOption("fontsize"),
  remove.y.gridlines = TRUE,
  remove.x.gridlines = TRUE,
  family = getGraphOption("family"),
  axisTickLength = getGraphOption("axisTickLength"),
  axisTickWidth = getGraphOption("axisTickWidth")
)
}
\arguments{
\item{legend.pos}{One of \code{"right"}, \code{"left"}, \code{"top"}, \code{"bottom"},
\code{"topleft"}, \code{"topright"}, \code{"topmiddle"}, \code{"bottomleft"},
\code{"bottomright"}, or \code{"bottommiddle"}.
Positions the legend, which will layer on top of any geoms, on the plane.}

\item{legend.use.title}{Logical. Specify whether to include a legend title.
Defaults to \code{FALSE}.}

\item{legend.font.size}{Integer indicating the font size of the labels in the
legend. Default and APA-recommended is 12, but if there are many labels it
may be necessary to choose a smaller size.}

\item{font.size}{General font size.}

\item{x.font.size}{Font size of x-axis label.}

\item{y.font.size}{Font size of x-axis label.}

\item{facet.title.size}{Font size of facet labels.}

\item{remove.y.gridlines}{Should the coordinate grid on the y-axis
(horizontal lines) be removed? Default is TRUE.}

\item{remove.x.gridlines}{Should the coordinate grid on the x-axis (vertical
lines) be removed? Default is TRUE.}

\item{family}{global font family.}

\item{axisTickLength}{length of axis ticks.}

\item{axisTickWidth}{width of axis ticks.}
}
\description{
Set a APA theme and additional options to a graph like
other themes, \code{graph + themeApaRaw()}.
}
\details{
This function is a modification of \code{\link[jtools:theme_apa]{jtools::theme_apa}}
that applies a theme to \code{ggplot2} figures with a
style that is roughly in line with APA guidelines. Users may need to
perform further operations for their specific use cases.

There are some things to keep in mind about APA style figures:
\itemize{
\item Main titles should be written in the word processor or typesetter
rather than on the plot image itself.
\item In some cases, users can forgo a legend in favor of describing the
figure in a caption (also written in the word processor/typesetter).
\item Legends are typically embedded on the coordinate plane of the figure
rather than next to it, as is default in \code{ggplot2}.
\item Use of color is generally discouraged since most of the applications
for which APA figures are needed involve eventual publication in non-color
print media.
\item There are no hard and fast rules on font size, though APA recommends
choosing between 8 and 14-point. Fonts in figures should be sans serif.
}

Because APA style calls for positioning legends on the plane itself, this
function includes options for choosing a position--top left, top right, bottom
left, bottom right--to place the legend. \code{ggplot2} provides no obvious
way to automatically choose a position that overlaps least with the geoms (the
plotted data), so users will need to choose one.

Facetting is supported, but APA guidelines are considerably less clear for
such situations.

This theme was created with inspiration from Rudolf Cardinal's
\href{http://egret.psychol.cam.ac.uk/statistics/R/graphs2.html}{code}, which
required updating for newer versions of \code{ggplot2} and adaptations for
APA style.
}
