#' @export
jaspHistogram <- function(
  x, variableName,
  rugs = FALSE,
  displayDensity = FALSE,
  binWidthType = c("doane", "fd", "scott", "sturges", "manual"),
  numberOfBins = NA,
  densityLineWidth = 1) {

  binWidthType <- match.arg(binWidthType)
  x <- na.omit(as.numeric(x))

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

  if (!displayDensity)
    yhigh <- max(h[["counts"]])
  else {
    dens <- density(x)
    yhigh <- max(max(h[["density"]]), max(dens[["y"]]))
  }

  xticks <- getPrettyAxisBreaks(c(x, h[["breaks"]]), min.n = 3)

  histogram <- densityLine <- NULL
  if (displayDensity) {
    histogram <-
      ggplot2::geom_histogram(
        data    = data.frame(x = x),
        mapping = aes(x = x, y = ..density..),
        breaks  = h[["breaks"]],
        fill    = "grey",
        col     = "black",
        size    = .7
      )
    densityLine <-
      ggplot2::geom_line(
        data    = data.frame(x = dens[["x"]], y = dens[["y"]]),
        mapping = aes(x = x, y = y),
        lwd     = densityLineWidth,
        col     = "black"
      )
  } else {
    histogram <-
      ggplot2::geom_histogram(
        data     = data.frame(x = x),
        mapping  = ggplot2::aes(x = x, y = ..count..),
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
    scale_x_continuous(name = variableName, breaks = xticks) +
    scale_y_continuous(name = if (displayDensity) gettext("Density") else gettext("Counts"), breaks = c(0,  1.05 * yhigh)) +
    geom_rangeframe() +
    themeJaspRaw()

  if (displayDensity)
    plot <- plot + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())

  return(plot)
}
