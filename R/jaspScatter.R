#' @title Scatter plots with optional confidence and prediction intervals.
#'
#' @description This plot consists of three layers:
#' \enumerate{
#'   \item The distribution of the data displayed as [geom_point] or as a [ggplot2::geom_hex].
#'   \item Smooth line through the data displayed using [ggplot2::geom_smooth].
#'   \item Prediction interval of y given x using [stats::predict.lm](assuming linear relationship), or prediction ellipse assuming bivariate normal distribution.
#' }
#' @param x Numeric vector of values on the x-axis.
#' @param y Numeric vector of values on the y-axis.
#' @param group Optional grouping variable.
#' @param xName Character; x-axis label.
#' @param yName Character; y-axis label.
#' @param type Character; How should the distribution of the data be displayed:
#' \describe{
#'    \item{"point"}{Using [geom_point].}
#'    \item{"hex"}{Using [ggplot2::geom_hex].}
#'    \item{"contour"}{Using [ggplot2::geom_contour].}
#' }
#' @param bins,binwidth Arguments passed to [ggplot2::geom_hex].
#' @param palette Argument passed to [JASPcolors]. Palette to use for drawing [ggplot2::geom_hex] and [ggplot2::geom_contour].
#' @param fill Argument passed to [geom_point].
#' @param alpha Argument passed to [geom_point] or [ggplot2::geom_hex].
#' @param smooth Character; passed as \code{method} argument to [ggplot2::geom_smooth],
#' unless \code{smooth == "none"}, in which case the layer si not plotted.
#' @param smoothCi Logical; Should confidence interval around the smooth line be plotted?
#' Passed as \code{se} argument to [ggplot2::geom_smooth].
#' @param smoothCiLevel Numeric; Confidence level of the confidence interval around the smooth line.
#' Passed as \code{level} argument to [ggplot2::geom_smooth].
#' @param smoothColor Color of the smooth line.
#' @param predict Character; Method for drawing the prediction interval:
#' \describe{
#'   \item{"none"}{Prediction interval is not displayed.}
#'   \item{"lm"}{Prediction interval is plotted, the confidence bands are calculated using [stats::predict.lm].}
#'   \item{"ellipse"}{Prediction ellipse is plotted, assuming bi-variate normal model.}
#' }
#' @param predictCiLevel Numeric; Confidence level of the prediction interval.
#' @param predictColor Color of the prediction interval.
#' @param suppressAxesLabels Logical; should axis labels be suppressed.
#' @export
jaspScatter <- function(
    x, y, group = NULL, xName = NULL, yName = NULL,
    type               = c("point", "hex", "bin", "density", "contour"),
    args               = list(),
    smooth             = c("none", "lm", "glm", "gam", "loess"),
    smoothCi           = FALSE,
    smoothCiLevel      = 0.95,
    smoothArgs         = list(),
    predict            = c("none", "lm", "ellipse"),
    predictCiLevel     = 0.95,
    predictArgs        = list(),
    suppressAxesLabels = FALSE
) {

  if (is.null(group)) {
    df  <- data.frame(x = x, y = y)
    aes <- ggplot2::aes(x = x, y = y)
  } else {
    df  <- data.frame(x = x, y = y, group = group)
    aes <- ggplot2::aes(x = x, y = y, group = group, fill = group, color = group)
  }

  type    <- match.arg(type)
  smooth  <- match.arg(smooth)
  predict <- match.arg(predict)


  baseGeom <- switch(
    type,
    point   = jaspGraphs::geom_point,
    hex     = ggplot2::geom_hex,
    bin     = ggplot2::geom_bin2d,
    density = ggplot2::geom_density2d,
    contour = ggplot2::geom_density2d_filled
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

  plot <- ggplot2::ggplot(data = df, mapping = aes) +
    predictLayer +
    smoothLayer +
    baseLayer +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe() +
    ggplot2::xlab(xName) +
    ggplot2::ylab(yName)# +
    # ggplot2::scale_fill_gradientn(limits = 0:1, colors = JASPcolors(palette = palette))

  return(plot)
}


jaspScatterWithMargins <- function(
  x, y, group = NULL, xName = NULL, yName = NULL
  ) {

  bottomLeft <- jaspScatter(x = x, y = y)
  topLeft <- jaspHistogram(x = x)
  bottomRight <- jaspHistogram(x = y) + ggplot2::coord_flip()
  topRight <- patchwork::plot_spacer()

  patchwork::wrap_plots(
    topLeft, topRight, bottomLeft, bottomRight
  )
}
