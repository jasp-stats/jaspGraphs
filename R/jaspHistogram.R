#' @title Histograms and Density plots for JASP
#'
#' @description A plot histogram with three components.
#' (1) A histogram, which can be tweaks with the same settings as \code{\link[graphics]{hist}}.
#' (2) A density line, which can optionally be shaded.
#' (3) rugs underneath the figure.
#' Each of these components can be enabled (or disabled) individually.
#' Colors are taken from \code{graphOptions("palette")}.
#'
#' @param x, numeric, the data to show a histogram for
#' @param xName, string, the title on the x-axis
#' @param groupingVariable, factor, show histograms and/ or densities for individual groups?
#' @param groupingVariableName, character, legend name of the grouping variable.
#' @param histogram, logical, should a histogram be shown?
#' @param histogramPosition, character, see argument \code{position} in \code{\link[ggplot2]{geom_histogram}}.
#' @param binWidthType, string, type of bindwidth, matches the string values that can be passed to \code{\link[graphics]{hist}}.
#' @param numberOfBins, if binWidthType is set to "manual", this value determines the number of bins.
#' @param rugs, logical, should rugs be shown on the x-axis?
#' @param rugsColor, logical, should the rugs be colored?
#' @param density, logical, should a density be superimposed on the plot?
#' @param densityColor, logical, should the density be colored?
#' @param densityShade, logical, should the area underneath the density be shaded?
#' @param densityShadeAlpha, numeric in \[0, 1\], transparancy for the shaded density.
#' @param densityLineWidth, positive number, the line width of the superimposed density.
#' @param hideYAxisLabels, logical, should the y-axis line be hidden? Defaults to \code{showDensity}.
#' @example inst/examples/ex-jaspHistogram.R
#' @export
jaspHistogram <- function(
  x, xName,
  groupingVariable = NULL,
  groupingVariableName,
  histogram = TRUE,
  histogramPosition = "dodge",
  binWidthType = c("doane", "fd", "scott", "sturges", "manual"),
  numberOfBins = NA,
  rugs = FALSE,
  rugsColor = FALSE,
  density = FALSE,
  densityColor = FALSE,
  densityShade = FALSE,
  densityShadeAlpha = 0.6,
  densityLineWidth = 1,
  hideYAxisLabels = density) {

  # validate input
  if (!is.vector(x, mode = "numeric"))
    stop2("`x` must be a numeric vector but has class ", paste(class(x), collapse = ", "))

  if (missing(xName))
    xName <- deparse1(substitute(x)) # identical to plot.default

  if (!is.character(xName))
    stop2("`xName` must be character but has class ", paste(class(xName), collapse = ", "), "!")

  if (!is.null(groupingVariable) && !is.factor(groupingVariable))
    stop2("`groupingVariable` must be a factor vector but has class ", paste(class(groupingVariable), collapse = ", "), "!")

  if (!is.null(groupingVariable) && missing(groupingVariableName))
    groupingVariableName <- deparse1(substitute(groupingVariable)) # identical to plot.default

  if (!missing(groupingVariableName) && !is.character(groupingVariableName))
    stop2("`groupingVariableName` must be character but has class ", paste(class(groupingVariableName), collapse = ", "), "!")


  hasGroupingVariable <- !is.null(groupingVariable)
  x <- stats::na.omit(as.numeric(x))

  binWidthType <- jaspHistogramBinWidth(x, binWidthType, numberOfBins)

  h <- graphics::hist(x, plot = FALSE, breaks = binWidthType)
  xBreaks <- getPrettyAxisBreaks(c(x, h[["breaks"]]), min.n = 3)

  histogramGeom <- scaleFill <- maxCounts <- maxDensity <- NULL
  if (histogram) {
    if (hasGroupingVariable) {

      dataHistogram <- data.frame(x = x, g = groupingVariable)
      scaleFill <- scale_JASPfill_discrete(name = groupingVariableName)
      aesHistogram <- if (density) {
        aes(x = .data$x, fill = .data$g, y =.data$..density..)
      } else {
        aes(x = .data$x, fill = .data$g, y =.data$..count..)
      }
      histogramGeom <- ggplot2::geom_histogram(
        data     = dataHistogram,
        mapping  = aesHistogram,
        breaks   = h[["breaks"]],
        col      = "black",
        size     = .7,
        position = histogramPosition
      )

      if (identical(histogramPosition, "stack") || inherits(histogramPosition, "PositionStack")) {
        # for a stacked figure we base maxCounts and maxDensity on the ungrouped data
        maxCounts  <- h[["counts"]]
        maxDensity <- h[["density"]]
      } else {
        # for each groupingvariable, bin by breaks and find the largest count
        temp <- do.call(rbind, tapply(x, groupingVariable, function(subset) {
          h <- graphics::hist(subset, plot = FALSE, breaks = binWidthType)
          c(counts = max(h[["counts"]]), density = max(h[["density"]]))
        }))
        maxCounts  <- max(temp[, "counts"])
        maxDensity <- max(temp[, "density"])
      }

    } else {
      dataHistogram <- data.frame(x = x)
      aesHistogram <- if (density) {
        aes(x = .data$x, y =.data$..density..)
      } else {
        aes(x = .data$x, y =.data$..count..)
      }
      histogramGeom <- ggplot2::geom_histogram(
        data    = dataHistogram,
        mapping = aesHistogram,
        breaks  = h[["breaks"]],
        fill    = "grey",
        col     = "black",
        size    = .7
      )
      scaleFill <-  NULL

      maxCounts  <- max(h[["counts"]])
      maxDensity <- max(h[["density"]])
    }
  }

  densityLineGeom <- densityShadedAreaGeom <- scaleColor <- NULL
  if (density) {

    yName <- gettext("Density")
    if (hasGroupingVariable && densityColor) {

      npoints <- 512
      dens <- tapply(x, groupingVariable, density, simplify = FALSE, n = npoints)
      densDf <- data.frame(
        x = c(vapply(dens, `[[`, "x", FUN.VALUE = numeric(npoints))),
        y = c(vapply(dens, `[[`, "y", FUN.VALUE = numeric(npoints))),
        g = rep(names(dens), each = npoints)
      )

      densityLineGeom <- ggplot2::geom_line(
        data      = densDf,
        mapping   = aes(x = .data$x, y = .data$y, group = .data$g, color = .data$g),
        linewidth = densityLineWidth,
      )

      scaleColor <- scale_JASPcolor_discrete(name = groupingVariableName)

      if (densityShade) {
        densityShadedAreaGeom <- ggplot2::geom_ribbon(
          data    = densDf,
          mapping = aes(x = .data$x, ymin = 0, ymax = .data$y, group = .data$g, fill = .data$g),
          alpha   = densityShadeAlpha
        )
        if (is.null(scaleFill))
          scaleFill <- scale_JASPfill_discrete(name = groupingVariableName)
      }

    } else {

      dens <- stats::density(x)
      densDf <- data.frame(x = dens[["x"]], y = dens[["y"]])

      densityLineGeom <- ggplot2::geom_line(
        data      = densDf,
        mapping   = aes(x = .data$x, y = .data$y),
        linewidth = densityLineWidth,
        col       = "black"
      )

      if (densityShade)
        densityShadedAreaGeom <- ggplot2::geom_ribbon(
          data    = densDf,
          mapping = aes(x = .data$x, ymin = 0, ymax = .data$y),
          alpha   = densityShadeAlpha
        )

    }

    yhigh <- max(maxDensity, max(densDf[["y"]]))
    yBreaks <- getPrettyAxisBreaks(c(0, 1.05 * yhigh))

  } else {

    yhigh   <- maxCounts
    yBreaks <- getPrettyAxisBreaks(c(0, yhigh))
    yName   <- gettext("Counts")

  }

  rugGeom <- NULL
  if (rugs) {
    if (rugsColor && hasGroupingVariable) {

      if (is.null(scaleColor))
        scaleColor <- scale_JASPcolor_discrete(name = groupingVariableName)

      rugGeom <- ggplot2::geom_rug(data = data.frame(x, g = groupingVariable), mapping = ggplot2::aes(x = .data$x, color = .data$g), sides = "b")
    } else {
      rugGeom <- ggplot2::geom_rug(data = data.frame(x),                       mapping = ggplot2::aes(x = .data$x),                  sides = "b")
    }
  }

  plot <-
    ggplot2::ggplot() +
    histogramGeom +
    densityShadedAreaGeom +
    densityLineGeom +
    rugGeom +
    ggplot2::scale_x_continuous(name = xName, breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = yName, breaks = yBreaks, limits = range(yBreaks)) +
    scaleFill +
    scaleColor +
    geom_rangeframe() +
    themeJaspRaw(legend.position = "right")

  if (hideYAxisLabels)
    plot <- plot + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

  return(plot)
}

#' @rdname jaspHistogram
#'
#' @return a numeric value for the number of bins or a string for the type of the bins.
#' @export
jaspHistogramBinWidth <- function(x, binWidthType = c("doane", "fd", "scott", "sturges", "manual"), numberOfBins = NA) {
  binWidthType <- match.arg(binWidthType)
  if (binWidthType == "doane") {

    # https://en.wikipedia.org/wiki/Histogram#Doane's_formula
    sigma.g1 <- sqrt((6*(length(x) - 2)) / ((length(x) + 1)*(length(x) + 3)))
    g1 <- mean(abs(x)^3)
    k <- 1 + log2(length(x)) + log2(1 + (g1 / sigma.g1))
    binWidthType <- k

  } else if (binWidthType == "fd" && grDevices::nclass.FD(x) > 10000) { # FD-method will produce extreme number of bins and crash ggplot, mention this in footnote

    warning2("The Freedman-Diaconis method would produce an extreme number of bins, setting the number of bins to 10,000.")
    binWidthType <- 10000

  } else if (binWidthType == "manual") {

    if (is.na(numberOfBins))
      stop2("numberOfBins argument must be specified when a binWidthType == 'manual'.")

    binWidthType <- numberOfBins

  } else {
    return(binWidthType)
  }
}
