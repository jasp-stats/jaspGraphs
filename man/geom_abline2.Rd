% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_abline2.R
\docType{data}
\name{geom_abline2}
\alias{geom_abline2}
\alias{GeomAbline2}
\title{Add an abline that does not exceed the extrema of the axes breaks.}
\usage{
geom_abline2(
  mapping = NULL,
  data = NULL,
  method = c("breaks", "ggplot2"),
  ...,
  slope,
  intercept,
  na.rm = FALSE,
  show.legend = NA
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{method}{Either "breaks" (default) to respect the extrema of the axes or "ggplot2" to obtain the ggplot2 behavior.}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}'s \code{params} argument. These
arguments broadly fall into one of 4 categories below. Notably, further
arguments to the \code{position} argument, or aesthetics that are required
can \emph{not} be passed through \code{...}. Unknown arguments that are not part
of the 4 categories below are ignored.
\itemize{
\item Static aesthetics that are not mapped to a scale, but are at a fixed
value and apply to the layer as a whole. For example, \code{colour = "red"}
or \code{linewidth = 3}. The geom's documentation has an \strong{Aesthetics}
section that lists the available options. The 'required' aesthetics
cannot be passed on to the \code{params}. Please note that while passing
unmapped aesthetics as vectors is technically possible, the order and
required length is not guaranteed to be parallel to the input data.
\item When constructing a layer using
a \verb{stat_*()} function, the \code{...} argument can be used to pass on
parameters to the \code{geom} part of the layer. An example of this is
\code{stat_density(geom = "area", outline.type = "both")}. The geom's
documentation lists which parameters it can accept.
\item Inversely, when constructing a layer using a
\verb{geom_*()} function, the \code{...} argument can be used to pass on parameters
to the \code{stat} part of the layer. An example of this is
\code{geom_area(stat = "density", adjust = 0.5)}. The stat's documentation
lists which parameters it can accept.
\item The \code{key_glyph} argument of \code{\link[ggplot2:layer]{layer()}} may also be passed on through
\code{...}. This can be one of the functions described as
\link[ggplot2:draw_key]{key glyphs}, to change the display of the layer in the legend.
}}

\item{slope}{controls the slope of the lines. If set, \code{data}, \code{mapping} and \code{show.legend} are overridden.}

\item{intercept}{controls the intercept of the lines. If set, \code{data}, \code{mapping} and \code{show.legend} are overridden.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}
}
\description{
The default abline of ggplot2 will exceeds the breaks and goes from one set of limits to the next.
This is a modified verions that respect one will not exceed the extrema of the x and y-breaks.
}
\examples{
library(ggplot2)
library(jaspGraphs)

dfRect <- data.frame(xmin = -10, ymin = -10, xmax = 10, ymax = 10)
basePlot <- ggplot(data = dfRect, aes(xmin = xmin, ymin = ymin, ymax = ymax, xmax = xmax)) +
  geom_rect(fill = NA, color = "black") +
  geom_rangeframe() +
  themeJaspRaw(legend.position = "right")

slopes <- seq(-5, 5, length.out = 15)
ints   <- seq(-2, 2, length.out = length(slopes))
basePlot +
  geom_abline2(intercept = ints - 2, slope = slopes, method = "breaks",  color = "green") +
  geom_abline2(intercept = ints + 2, slope = slopes, method = "ggplot2", color = "red") +
  ggtitle("specify 'method' manually")

dfAbline <- data.frame(
  intercept = -9:8,
  slope = 0.35,
  method = rep(c("ggplot2", "breaks"), 9)
)

basePlot +
  geom_abline2(data = dfAbline, mapping = aes(
    intercept = intercept, slope = slope, method = method, color = method
  ), show.legend = TRUE) +
  ggtitle("specify 'method' through aes")

basePlot + geom_abline2(data = dfAbline, mapping = aes(
    intercept = intercept, slope = slope, color = method
  )) +
  ggtitle("if method is not specified, 'breaks' is used as a default")

dfAbline <- data.frame(
  slope = c(Inf, Inf),
  intercept = 0,
  method = rep(c("ggplot2", "breaks"), 2)
)

basePlot +
  geom_abline2(data = dfAbline, mapping = aes(
    intercept = intercept, slope = slope, method = method, color = method
  ), show.legend = TRUE) +
  ggtitle("more reasonable behavior for infinite slopes")


# also works with discrete axes
df <- data.frame(
  x = letters[1:7],
  y = seq_along(letters[1:7])
)

ggplot(data = df, aes(x = x, y = y)) +
  geom_bar(stat = "identity", fill = NA, color = "grey") +
  geom_abline2(intercept = 0, slope = 0, size = 2, col = "blue") +
  geom_abline2(intercept = 0, slope = 1, size = 2, col = "red") +
  scale_y_continuous(breaks = 0:7) +
  geom_rangeframe() +
  themeJaspRaw()
}
\keyword{datasets}
