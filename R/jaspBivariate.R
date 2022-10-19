#' @title Bivariate plots with optional confidence and prediction intervals.
#'
#' @description This plot consists of three layers:
#' \enumerate{
#'   \item The bivariate distribution.
#'   \item Smooth line through the data displayed using [ggplot2::geom_smooth].
#'   \item Prediction interval of y given x using [stats::predict.lm](assuming linear relationship), or prediction ellipse assuming bivariate normal distribution.
#' }
#' @param x Numeric vector of values on the x-axis.
#' @param y Numeric vector of values on the y-axis.
#' @param group Optional grouping variable.
#' @param xName Character; x-axis label. If left empty, the name of the \code{x} object is displayed. To remove the axis label, use \code{NULL}.
#' @param yName Character; y-axis label. If left empty, the name of the \code{y} object is displayed. To remove the axis label, use \code{NULL}.
#' @param type Character; How should the distribution of the data be displayed:
#' \describe{
#'    \item{"point"}{Using [geom_point].}
#'    \item{"hex"}{Using [ggplot2::geom_hex].}
#'    \item{"bin"}{Using [ggplot2::geom_bin2d].}
#'    \item{"contour"}{Using [ggplot2::geom_density2d].}
#'    \item{"density"}{Using [ggplot2::geom_density2d_filled].}
#' }
#' @param args A list of additional arguments passed to the geom function determined by \code{type} argument.
#' @param smooth Character; passed as \code{method} argument to [ggplot2::geom_smooth],
#' unless \code{smooth == "none"}, in which case the layer is not plotted.
#' @param smoothCi Logical; Should confidence interval around the smooth line be plotted?
#' Passed as \code{se} argument to [ggplot2::geom_smooth].
#' @param smoothCiLevel Numeric; Confidence level of the confidence interval around the smooth line.
#' Passed as \code{level} argument to [ggplot2::geom_smooth].
#' @param smoothArgs A list of additional arguments passed to [ggplot2::geom_smooth].
#' @param predict Character; Method for drawing the prediction interval:
#' \describe{
#'   \item{"none"}{Prediction interval is not displayed.}
#'   \item{"lm"}{Prediction interval is plotted, the confidence bands are calculated using [stats::predict.lm].}
#'   \item{"ellipse"}{Prediction ellipse is plotted using [ggplot2::stat_ellipse].}
#' }
#' @param predictLevel Numeric; Confidence level of the prediction interval.
#' @param predictArgs A list of additional arguments passed to the function that draws the prediction interval.
#' @param xBreaks Optional numeric vector that specifies the breaks along the x-axis.
#' @param yBreaks Optional numeric vector that specifies the breaks along the y-axis.
#' @export
jaspBivariate <- function(
    x, y, group = NULL, xName, yName,
    type               = c("point", "hex", "bin", "contour", "density"),
    args               = list(color = "black"),
    smooth             = c("none", "lm", "glm", "gam", "loess"),
    smoothCi           = FALSE,
    smoothCiLevel      = 0.95,
    smoothArgs         = list(),
    predict            = c("none", "lm", "ellipse"),
    predictCiLevel     = 0.95,
    predictArgs        = list(),
    xBreaks            = NULL,
    yBreaks            = NULL
) {

  type    <- match.arg(type)
  smooth  <- match.arg(smooth)
  predict <- match.arg(predict)

  if (is.null(group)) {
    df  <- data.frame(x = x, y = y)
    aes <- ggplot2::aes(x = x, y = y)
  } else {
    if(type != "point")
      stop("grouping variable is allowed only for type = 'point'.")

    df  <- data.frame(x = x, y = y, group = group)
    aes <- ggplot2::aes(x = x, y = y, group = group, fill = group, color = group)
  }

  if (missing(xName))
    xName <- deparse1(substitute(x)) # identical to plot.default

  if (missing(yName))
    yName <- deparse1(substitute(y)) # identical to plot.default


  baseGeom <- switch(
    type,
    point   = jaspGraphs::geom_point,
    hex     = ggplot2::geom_hex,
    bin     = ggplot2::geom_bin2d,
    contour = ggplot2::geom_density2d,
    density = ggplot2::geom_density2d_filled
  )
  baseLayer <- do.call(baseGeom, args)


  formula <- switch(
    smooth,
    gam = if(is.null(smoothArgs$formula)) { y ~ s(x, bs = "cs") } else { smoothArgs$formula },
          if(is.null(smoothArgs$formula)) { y ~ x }               else { smoothArgs$formula }
  )

  if (smooth != "none") {
    smoothArgs$method  <- smooth
    smoothArgs$se      <- smoothCi
    smoothArgs$level   <- smoothCiLevel
    smoothArgs$formula <- formula
    smoothLayer <- do.call(ggplot2::geom_smooth, smoothArgs)
  } else {
    smoothLayer <- NULL
  }


  if (predict == "lm") {
    fit <- lm(y~x, data = df)
    preds <- predict(fit, newdata = df, interval = "prediction", level = predictCiLevel)
    preds <- as.data.frame(preds)
    preds[["x"]] <- df[["x"]]
    predictArgs$data <- preds
    predictArgs$mapping <- ggplot2::aes(x = x, ymin = lwr, ymax = upr)
    predictLayer <- do.call(ggplot2::geom_ribbon, predictArgs)
  } else if (predict == "ellipse") {
    predictArgs$geom  <- "polygon"
    predictArgs$type  <- "t"
    predictArgs$level <- predictCiLevel
    predictLayer <- do.call(ggplot2::stat_ellipse, predictArgs)
  } else {
    predictLayer <- NULL
  }

  if (missing(xBreaks) || is.null(xBreaks))
    xBreaks <- getPrettyAxisBreaks(x)
  xRange <- range(c(x, xBreaks))
  xScale <- scale_x_continuous(breaks = xBreaks)

  if (missing(yBreaks) || is.null(yBreaks))
    yBreaks <- getPrettyAxisBreaks(y)
  yRange <- range(c(y, yBreaks))
  yScale <- scale_y_continuous(breaks = yBreaks)


  if (type == "point" && !is.null(group)) {
    scales <- list(
      scale_JASPfill_discrete(),
      scale_JASPcolor_discrete()
    )
  } else if (type %in% c("hex", "bin")) {
    scales <- scale_JASPfill_continuous()
  } else if (type == "density") {
    scales <- scale_JASPfill_discrete()
  } else {
    scales <- NULL
  }

  plot <- ggplot2::ggplot(data = df, mapping = aes) +
    baseLayer +
    smoothLayer +
    predictLayer +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe() +
    ggplot2::xlab(xName) +
    ggplot2::ylab(yName) +
    xScale +
    yScale +
    # this ensures that the axes do not get stretched outside of the data range
    # in case that the bounds of smoothLayer or predictLayer are outside of the region
    ggplot2::coord_cartesian(xlim = xRange, ylim = yRange) +
    scales

  return(plot)
}

#' @title Bivariate plots with marginal distributions along the axes.
#'
#' @description This plot consists of four elements:
#' \enumerate{
#'   \item The bivariate plot of \code{x} and \code{y} in the bottom-left panel displayed by [jaspBivariate].
#'   \item Marginal distributions along the diagonal displayed by [jaspHistogram]. The plot on the bottom-right has transposed axes.
#'   \item (Optional) custom plot on the top-right panel. See details.
#' }
#'
#' @param x Numeric vector of values on the x-axis.
#' @param y Numeric vector of values on the y-axis.
#' @param group Optional grouping variable.
#' @param xName Character; x-axis label. If left empty, the name of the \code{x} object is displayed. To remove the axis label, use \code{NULL}.
#' @param yName Character; y-axis label. If left empty, the name of the \code{y} object is displayed. To remove the axis label, use \code{NULL}.
#' @param margins Numeric vector; The proportions of the subplots relative to each other.
#' @param binWidthType See [jaspHistogram]. Used for determining consistent axes for the bivariate and marginal distribution plots.
#' @param numberOfBins See [jaspHistogram]. Used for determining consistent axes for the bivariate and marginal distribution plots.
#' @param histogramArgs An optional list of options passed to [jaspHistogram].
#' @param topRightPlotFunction An optional function that can be used to plotting something in the top-right panel. If \code{NULL} (default), an empty area is plotted.
#' @param topRightPlotArgs An optional list of options passed to \code{topRightPlotFunction}.
#' @param ... Additional options passed to [jaspBivariate].
#'
#' @export
jaspBivariateWithMargins <- function(
  x, y, group = NULL, xName, yName, margins = c(0.25, 0.75),
  binWidthType = c("doane", "fd", "scott", "sturges", "manual"), numberOfBins = NA,
  histogramArgs = list(density = TRUE),
  topRightPlotFunction = NULL,
  topRightPlotArgs = list(),
  ...
  ) {

  xBreaks <- getJaspHistogramBreaks(x = x, binWidthType = binWidthType, numberOfBins = numberOfBins)
  yBreaks <- getJaspHistogramBreaks(x = y, binWidthType = binWidthType, numberOfBins = numberOfBins)

  bottomLeft <- jaspBivariate(x = x, y = y, group = group, xName = xName, yName = yName, xBreaks = xBreaks, yBreaks = yBreaks, ...)

  histogramArgs[["binWidthType"]] <- binWidthType
  histogramArgs[["numberOfBins"]] <- numberOfBins

  topLeftArgs                      <- histogramArgs
  topLeftArgs[["x"]]               <- x
  topLeftArgs[["groupingVariable"]]<- group
  topLeftArgs[["groupingVariableName"]] <- " "
  topLeftArgs[["hideXAxisLabels"]] <- TRUE
  topLeftArgs[["hideYAxisLabels"]] <- TRUE
  topLeftArgs[["hideXAxisName"]]   <- TRUE
  topLeftArgs[["hideYAxisName"]]   <- TRUE
  topLeft <- do.call(jaspHistogram, topLeftArgs)

  bottomRightArgs                      <- histogramArgs
  bottomRightArgs[["x"]]               <- y
  bottomRightArgs[["groupingVariable"]]<- group
  bottomRightArgs[["groupingVariableName"]] <- " "
  bottomRightArgs[["hideXAxisLabels"]] <- TRUE
  bottomRightArgs[["hideYAxisLabels"]] <- TRUE
  bottomRightArgs[["hideXAxisName"]]   <- TRUE
  bottomRightArgs[["hideYAxisName"]]   <- TRUE
  bottomRight <- do.call(jaspHistogram, bottomRightArgs) +
    ggplot2::coord_flip()


  if (is.function(topRightPlotFunction) && is.list(topRightPlotArgs)) {
    topRightPlotArgs[["x"]]     <- x
    topRightPlotArgs[["y"]]     <- y
    topRight <- do.call(topRightPlotFunction, topRightPlotArgs)
  } else if (is.null(topRightPlotFunction)) {
    topRight <- patchwork::plot_spacer()
  }

  patchwork::wrap_plots(
    topLeft, topRight, bottomLeft, bottomRight,
    widths = rev(margins), heights = margins
  ) +
  patchwork::plot_layout(guides = "collect")
}
