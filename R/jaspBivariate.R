#' @title Bivariate plots with optional confidence and prediction intervals
# #' @encoding UTF-8
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
#' @param groupName Character; label of the grouping variable displayed as a legend title. If left empty, the name of the \code{group} object is displayed.
#' @param type Character; How should the distribution of the data be displayed:
#' \describe{
#'    \item{"point"}{Using [geom_point].}
#'    \item{"hex"}{Using [ggplot2::geom_hex].}
#'    \item{"bin"}{Using [ggplot2::geom_bin2d].}
#'    \item{"contour"}{Using [ggplot2::geom_density2d].}
#'    \item{"density"}{Using [ggplot2::geom_density2d_filled]. By default, regions below 10% of the maximum density are not filled.}
#' }
#' @param args A list of additional arguments passed to the geom function determined by \code{type} argument.
#' Contour lines are black unless a color is specified.
#' @param smooth Character; passed as \code{method} argument to [ggplot2::geom_smooth],
#' unless \code{smooth == "none"}, in which case the layer is not plotted.
#' @param smoothCi Logical; Should confidence interval around the smooth line be plotted?
#' Passed as \code{se} argument to [ggplot2::geom_smooth].
#' @param smoothCiLevel Numeric; Confidence level of the confidence interval around the smooth line.
#' Passed as \code{level} argument to [ggplot2::geom_smooth].
#' @param smoothArgs A list of additional arguments passed to [ggplot2::geom_smooth].
#' Without a grouping variable, the smooth line is black unless a color is specified.
#' @param predict Character; Method for drawing the prediction interval:
#' \describe{
#'   \item{"none"}{Prediction interval is not displayed.}
#'   \item{"lm"}{Prediction interval is plotted, the confidence bands are calculated using [stats::predict.lm].}
#'   \item{"ellipse"}{Prediction ellipse is plotted using [ggplot2::stat_ellipse].}
#' }
#' @param predictLevel Numeric; Confidence level of the prediction interval.
#' @param predictArgs A list of additional arguments passed to the function that draws the prediction interval.
#' @param xAxis,yAxis Optional shared axes, lists with elements \code{breaks} (axis breaks) and \code{limits} (axis limits).
#' Used by [jaspMatrixPlot] and [jaspBivariateWithMargins] so that all panels showing the same variable share the axis.
#' If \code{NULL}, the axis spans the pretty breaks of all layers, including smooth lines and prediction intervals.
#' If supplied, the axis is used as is; the parts of the layers outside the axis limits are clipped.
#' @param legendPosition Character; passed as \code{legend.position} to [themeJaspRaw].
#' @export
jaspBivariate <- function(
    x, y, group = NULL, xName, yName, groupName,
    type               = c("point", "hex", "bin", "contour", "density", "none"),
    args               = list(),
    smooth             = c("none", "lm", "glm", "gam", "loess"),
    smoothCi           = FALSE,
    smoothCiLevel      = 0.95,
    smoothArgs         = list(),
    predict            = c("none", "lm", "ellipse"),
    predictLevel       = 0.95,
    predictArgs        = .predictArgs(),
    xAxis              = NULL,
    yAxis              = NULL,
    legendPosition     = "none"
) {

  type    <- match.arg(type)
  smooth  <- match.arg(smooth)
  predict <- match.arg(predict)

  # deparse the names before x, y, and group are modified, otherwise substitute() returns the modified values
  if (missing(xName))
    xName <- deparse1(substitute(x)) # identical to plot.default

  if (missing(yName))
    yName <- deparse1(substitute(y)) # identical to plot.default

  if (!is.null(group) && missing(groupName))
    groupName <- deparse1(substitute(group))

  if (is.null(group)) {
    df  <- data.frame(x = x, y = y)
    aes <- ggplot2::aes(x = x, y = y)
  } else {
    if (type != "point" && type != "none")
      stop2("grouping variable is allowed only for type = 'point' or 'none'.")

    group <- factor(group)
    df  <- data.frame(x = x, y = y, group = group)
    aes <- ggplot2::aes(x = x, y = y, group = group, fill = group, color = group)
  }

  df <- stats::na.omit(df)
  x  <- df[["x"]]
  y  <- df[["y"]]

  baseGeom <- switch(
    type,
    point   = jaspGraphs::geom_point,
    hex     = ggplot2::geom_hex,
    bin     = ggplot2::geom_bin2d,
    contour = ggplot2::geom_density2d,
    density = ggplot2::geom_density2d_filled,
    none    = function(...) { return(NULL) }
  )
  if (type == "contour" && !.hasColorArg(args))
    args[["color"]] <- "black"

  # the lowest level of a filled density covers the whole panel, so it is not filled
  if (type == "density" && !any(c("breaks", "bins", "binwidth") %in% names(args))) {
    args[["contour_var"]] <- "ndensity"
    args[["breaks"]]      <- seq(0.1, 1, by = 0.1)
  }
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
    # with a grouping variable, the smooth lines are colored by group
    if (is.null(group) && !.hasColorArg(smoothArgs))
      smoothArgs$color <- "black"
    smoothLayer <- do.call(ggplot2::geom_smooth, smoothArgs)
  } else {
    smoothLayer <- NULL
  }


  if (predict == "lm") {
    fit <- stats::lm(y~x, data = df)
    preds <- stats::predict(fit, newdata = df, interval = "prediction", level = predictLevel)
    preds <- as.data.frame(preds)
    preds[["x"]] <- df[["x"]]
    predictArgs$data <- preds
    predictArgs$mapping <- ggplot2::aes(x = x, ymin = .data$lwr, ymax = .data$upr)
    predictLayer <- do.call(ggplot2::geom_ribbon, predictArgs)
  } else if (predict == "ellipse") {
    predictArgs$geom  <- "polygon"
    predictArgs$type  <- "t"
    predictArgs$level <- predictLevel
    predictLayer <- do.call(ggplot2::stat_ellipse, predictArgs)
  } else {
    predictLayer <- NULL
  }

  # without a shared axis, the JASP scales span all layers. A shared axis is used as is, and
  # oob_keep ensures that layers beyond it are clipped at the panel border instead of being dropped.
  xScale <- if (is.null(xAxis)) scale_x_continuous() else scale_x_continuous(breaks = xAxis[["breaks"]], limits = xAxis[["limits"]], oob = scales::oob_keep)
  yScale <- if (is.null(yAxis)) scale_y_continuous() else scale_y_continuous(breaks = yAxis[["breaks"]], limits = yAxis[["limits"]], oob = scales::oob_keep)


  if (type == "point" && !is.null(group)) {
    scales <- list(
      scale_JASPfill_discrete(name = groupName),
      scale_JASPcolor_discrete(name = groupName)
    )
  } else if (type %in% c("hex", "bin")) {
    scales <- scale_JASPfill_continuous()
  } else if (type == "density") {
    scales <- scale_JASPfill_discrete()
  } else {
    scales <- NULL
  }

  plot <- ggplot2::ggplot(data = df, mapping = aes) +
    smoothLayer +
    baseLayer +
    predictLayer +
    jaspGraphs::themeJaspRaw(legend.position = legendPosition) +
    jaspGraphs::geom_rangeframe() +
    ggplot2::xlab(xName) +
    ggplot2::ylab(yName) +
    xScale +
    yScale +
    scales

  return(plot)
}

.hasColorArg <- function(args) {
  any(c("color", "colour", "col") %in% names(args))
}

.predictArgs <- function(color = "black", linetype = 2, linewidth = 1, fill = NA, ...) {
  args <- list(...)
  args[["color"]]     <- color
  args[["linetype"]]  <- linetype
  args[["linewidth"]] <- linewidth
  args[["fill"]]       <- fill

  return(args)
}

#' @title Bivariate plots with marginal distributions along the axes
#'
#' @description This plot consists of four elements:
#' \enumerate{
#'   \item The bivariate plot of \code{x} and \code{y} in the bottom-left panel displayed by [jaspBivariate].
#'   \item Marginal distributions along the diagonal displayed by [jaspMarginal]. The plot on the bottom-right has transposed axes.
#'   \item (Optional) custom plot on the top-right panel. See \code{topRightPlotFunction}.
#' }
#'
#' @param x Numeric vector of values on the x-axis.
#' @param y Numeric vector of values on the y-axis.
#' @param group Optional grouping variable. Coerced to a factor.
#' @param xName Character; x-axis label. If left empty, the name of the \code{x} object is displayed. To remove the axis label, use \code{NULL}.
#' @param yName Character; y-axis label. If left empty, the name of the \code{y} object is displayed. To remove the axis label, use \code{NULL}.
#' @param groupName Character; label of the grouping variable displayed as a legend title. If left empty, the name of the \code{group} object is displayed.
#' @param margins Numeric vector; The proportions of the subplots relative to each other.
#' @param xMarginalArgs List, options for the marginal plot above. Defaults to the default values of [jaspMarginal].
#' @param yMarginalArgs List, options for the marginal plot to the right. Defaults to the default values of [jaspMarginal].
#' @param topRightPlotFunction An optional function that can be used to plotting something in the top-right panel.
#' It receives \code{x} and \code{y} (with incomplete cases removed) in addition to \code{topRightPlotArgs}.
#' If \code{NULL} (default), the legend or an empty area is plotted.
#' @param topRightPlotArgs An optional list of options passed to \code{topRightPlotFunction}.
#' @param legendPosition Either "topRight" or any values that is accepted by \code{\link[ggplot2]{theme}} for `legend.position`. If set to "topRight" then `topRightPlotFunction` cannot be used.
#' @param ... Additional options passed to [jaspBivariate].
#'
#' @export
jaspBivariateWithMargins <- function(
  x, y, group = NULL, xName, yName, groupName, margins = c(1/6, 5/6),
  xMarginalArgs = .marginalArgs(),
  yMarginalArgs = .marginalArgs(),
  topRightPlotFunction = NULL,
  topRightPlotArgs = list(),
  legendPosition = "topRight",
  ...
  ) {

  if (!is.null(group) && missing(groupName)) {
    groupName <- deparse1(substitute(group))
  } else if (missing(groupName)) {
    groupName <- ""
  }

  if (missing(xName))
    xName <- deparse1(substitute(x)) # identical to plot.default

  if (missing(yName))
    yName <- deparse1(substitute(y)) # identical to plot.default

  if (!is.null(topRightPlotFunction) && !is.function(topRightPlotFunction))
    stop2("`topRightPlotFunction` must be a function or NULL.")

  if (!is.list(topRightPlotArgs))
    stop2("`topRightPlotArgs` must be a list.")

  if (is.function(topRightPlotFunction) && identical(legendPosition, "topRight"))
    stop2(r"{`legendPosition = "topRight"` cannot be used in conjunction with `topRightPlotFunction`.}")

  if (is.null(group)) {
    df <- data.frame(x = x, y = y)
  } else {
    df <- data.frame(x = x, y = y, group = factor(group))
  }
  df <- stats::na.omit(df)
  dfGroup <- if (is.null(group)) list(NULL) else list(df[["group"]])

  xAxis <- jaspSharedAxis(x = df[["x"]], breaks = xMarginalArgs[["breaks"]] %||% "sturges")
  yAxis <- jaspSharedAxis(x = df[["y"]], breaks = yMarginalArgs[["breaks"]] %||% "sturges")

  makeBottomLeft <- function(xAxis, yAxis) {
    jaspBivariate(x = df[["x"]], y = df[["y"]], group = dfGroup[[1L]], xName = xName, yName = yName, groupName = groupName, xAxis = xAxis, yAxis = yAxis, ...)
  }
  bottomLeft <- makeBottomLeft(xAxis, yAxis)

  # the axes also cover the smooth lines and prediction intervals, and the marginals follow
  ranges   <- jaspPanelRanges(bottomLeft)
  newXAxis <- jaspExtendAxis(xAxis, ranges[["x"]])
  newYAxis <- jaspExtendAxis(yAxis, ranges[["y"]])
  if (!identical(newXAxis, xAxis) || !identical(newYAxis, yAxis)) {
    xAxis      <- newXAxis
    yAxis      <- newYAxis
    bottomLeft <- makeBottomLeft(xAxis, yAxis)
  }

  xMarginalArgs[["x"]]          <- df[["x"]]
  xMarginalArgs["group"]        <- dfGroup
  xMarginalArgs["xName"]        <- list(NULL)
  xMarginalArgs["yName"]        <- list(NULL)
  xMarginalArgs["groupName"]    <- list(groupName)
  xMarginalArgs[["xAxis"]]      <- xAxis
  xMarginalArgs[["axisLabels"]] <- "none"
  xMarginalArgs[["sides"]]      <- ""

  topLeft <- do.call(jaspMarginal, xMarginalArgs)


  yMarginalArgs[["x"]]          <- df[["y"]]
  yMarginalArgs["group"]        <- dfGroup
  yMarginalArgs["xName"]        <- list(NULL)
  yMarginalArgs["yName"]        <- list(NULL)
  yMarginalArgs["groupName"]    <- list(groupName)
  # the scale of the flipped marginal still belongs to x, so it gets the shared axis of y
  yMarginalArgs[["xAxis"]]      <- yAxis
  yMarginalArgs[["axisLabels"]] <- "none"
  yMarginalArgs[["sides"]]      <- ""

  bottomRight <- do.call(jaspMarginal, yMarginalArgs) +
    ggplot2::coord_flip()


  if (is.function(topRightPlotFunction)) {
    topRightPlotArgs[["x"]] <- df[["x"]]
    topRightPlotArgs[["y"]] <- df[["y"]]
    topRight <- do.call(topRightPlotFunction, topRightPlotArgs)
  } else {
    topRight <- if (identical(legendPosition, "topRight")) patchwork::guide_area() else patchwork::plot_spacer()
  }

  extraLegend <- if (identical(legendPosition, "topRight")) NULL else theme(legend.position = legendPosition)

  patchwork::wrap_plots(
    topLeft, topRight, bottomLeft, bottomRight,
    widths = rev(margins), heights = margins
  ) +
  patchwork::plot_layout(guides = "collect") & extraLegend
}
