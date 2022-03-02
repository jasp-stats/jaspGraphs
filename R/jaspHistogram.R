#' @title jaspHistogram
#'
#' @param x, numeric, the data to show a histogram for
#' @param xName, string, the title on the x-axis
#' @param rugs, logical, should rugs be shown on the x-axis?
#' @param displayDensity, logical, should a density be superimposed on the plot?
#' @param binWidthType, string, type of bindwidth, matches the string values that can be passed to \code{\link[graphics]{hist}}.
#' @param numberOfBins, if binWidthType is set to "manual", this value determines the number of bins.
#' @param densityLineWidth, the line width of the superimposed density.
#' @example inst/examples/ex-jaspHistogram.R
#' @export
jaspHistogram <- function(
  x, xName,
  rugs = FALSE,
  displayDensity = FALSE,
  binWidthType = c("doane", "fd", "scott", "sturges", "manual"),
  numberOfBins = NA,
  densityLineWidth = 1) {

  binWidthType <- match.arg(binWidthType)
  x <- stats::na.omit(as.numeric(x))

  if (binWidthType == "doane") {

    # https://en.wikipedia.org/wiki/Histogram#Doane's_formula
    sigma.g1 <- sqrt((6*(length(x) - 2)) / ((length(x) + 1)*(length(x) + 3)))
    g1 <- mean(abs(x)^3)
    k <- 1 + log2(length(x)) + log2(1 + (g1 / sigma.g1))
    binWidthType <- k

  } else if (binWidthType == "fd" && grDevices::nclass.FD(x) > 10000) { # FD-method will produce extreme number of bins and crash ggplot, mention this in footnote

    binWidthType <- 10000

  } else if (binWidthType == "manual") {

    binWidthType <- numberOfBins

  }

  h <- graphics::hist(x, plot = FALSE, breaks = binWidthType)

  if (displayDensity) {
    dens <- stats::density(x)
    yhigh <- max(max(h[["density"]]), max(dens[["y"]]))
  } else {
    yhigh <- max(h[["counts"]])
  }

  xBreaks <- getPrettyAxisBreaks(c(x, h[["breaks"]]), min.n = 3)

  histogram <- densityLine <- NULL
  if (displayDensity) {

    yBreaks <- c(0, 1.05 * yhigh)
    yName   <- gettext("Density")
    yLabels <- NULL

    histogram <- ggplot2::geom_histogram(
      data    = data.frame(x = x),
      mapping = aes(x = x, y = .data$..density..),
      breaks  = h[["breaks"]],
      fill    = "grey",
      col     = "black",
      size    = .7
    )

    densityLine <- ggplot2::geom_line(
      data    = data.frame(x = dens[["x"]], y = dens[["y"]]),
      mapping = aes(x = .data$x, y = .data$y),
      lwd     = densityLineWidth,
      col     = "black"
    )
  } else {

    yBreaks <- getPrettyAxisBreaks(c(0, h[["counts"]]))
    yName   <- gettext("Counts")
    yLabels <- waiver()

    histogram <- ggplot2::geom_histogram(
      data     = data.frame(x = x),
      mapping  = ggplot2::aes(x = .data$x, y = .data$..count..),
      breaks   = h[["breaks"]],
      fill     = "grey",
      col      = "black",
      size     = .7
    )
  }

  rug <- NULL
  if (rugs)
    rug <- ggplot2::geom_rug(data = data.frame(x), mapping = ggplot2::aes(x = x), sides = "b")

  plot <-
    ggplot2::ggplot() +
    histogram +
    densityLine +
    rug +
    ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, labels = yLabels) +
    geom_rangeframe() +
    themeJaspRaw()

  if (displayDensity)
    plot <- plot + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())

  return(plot)
}
