#' @title Make a Q-Q plot

#' @param residuals Numeric vector, the residuals of the analysis.
#'
#' @param lower Numeric vector, lower confidence interval of each residual. If NULL, no error bars are drawn.
#' @param upper Numeric vector, lower confidence interval of each residual. If NULL, no error bars are drawn.
#' @param abline Logical, should an abline be drawn that best fits the points?
#' @param ablineOrigin  Logical, should an abline be drawn through the origin?
#' @param ablineColor String, color of the abline.
#' @param identicalAxes Logical, should the axes have the same range?
#' @param na.rm Logical, should NA's be removed from residuals?
#' @param xName String, name for the x-axis.
#' @param yName String, name for the y-axis.
#'
#' @details This function is equivalent to \code{qqnorm(residuals); qqline(residuals)}, but uses \code{ggplot2} and allows for confidence bands.
#'
#' @examples
#' x <- rnorm(100)
#' jaspGraphs::plotQQnorm(x)
#' # add a confidence interval for each point
#' lower <- x - .1
#' upper <- x + .1
#' jaspGraphs::plotQQnorm(x, lower, upper)
#'
#' @export
plotQQnorm <- function(residuals, lower = NULL, upper = NULL, abline = TRUE, ablineOrigin = FALSE, ablineColor = "red", identicalAxes = FALSE, na.rm = TRUE,
                       ciLevel = NULL, fillColor = "steelblue", ciAlpha = 0.25, ciLineColor = "black", xName = gettext("Theoretical quantiles",domain="R-jaspGraphs"), yName = gettext("Observed quantiles",domain="R-jaspGraphs")) {

  n <- length(residuals)
  hasErrorbars <- !is.null(lower) && !is.null(upper)

  df <- data.frame(
    y = residuals,
    x = stats::qnorm(stats::ppoints(n))[order(order(residuals))]
  )

  ciLayer <- NULL

  if (hasErrorbars) {
    df$ymin <- lower
    df$ymax <- upper
  } else if (is.numeric(ciLevel)) {
    # Computation from jaspDistributions, based on:
    # Stirling, W. D. (1982). Enhancements to aid interpretation of probability plots. Journal of the Royal Statistical Society: Series D (The Statistician), 31(3), 211-220.
    # Quesenberry, C. P., & Hales, C. (1980). Concentration bands for uniformity plots. Journal of Statistical Computation and Simulation, 11(1), 41-53.
    i     <- seq_len(n)
    alpha <- 1 - ciLevel

    pLower <- stats::qbeta(alpha/2, i, n - i + 1)
    pUpper <- stats::qbeta(1 - alpha/2, i, n - i + 1)

    zLower <- stats::qnorm(pLower)
    zUpper <- stats::qnorm(pUpper)

    df$ymin <- int + slope * zLower[order(order(residuals))]
    df$ymax <- int + slope * zUpper[order(order(residuals))]

    ciLayer <- ggplot2::geom_ribbon(
      data = df,
      mapping = ggplot2::aes(x = x, ymin = ymin, ymax = ymax),
      fill = fillColor, alpha = ciAlpha, color = ciLineColor,
      inherit.aes = FALSE
    )
  }

  if (isTRUE(na.rm)) {
    df <- df[!(is.na(residuals)), ]
  }

  # determine axes breaks
  if (identicalAxes) {
    xBreaks <- yBreaks <- getPrettyAxisBreaks(unlist(df))
  } else {
    xBreaks <- getPrettyAxisBreaks(df$x)
    yBreaks <- getPrettyAxisBreaks(c(df$y, df$ymin, df$ymax))
  }

  # from stats::qqline
  xvals <- stats::quantile(df$y, c(0.25, 0.75), names = FALSE)
  yvals <- stats::qnorm(c(0.25, 0.75))
  slope <- diff(xvals) / diff(yvals)
  int <- xvals[1L] - slope * yvals[1L]

  # initial guess for line range
  xvals <- range(xBreaks)
  yvals <- int + slope * xvals

  # TODO: if any y-values exceed the axes boundaries, recalculate line segment
  # if (yvals[1L] < yBreaks[1]) {
  # }
  # if (yvals[2L] > yBreaks[2L]) {
  # }

  # construct plot
  aes <- ggplot2::aes
  dfLine <- data.frame(x = xvals, y = yvals)
  g <- ggplot2::ggplot(data = df, aes(x = .data$x, y = .data$y))

  if (abline && ablineOrigin) {
    g <- g + ggplot2::geom_line(data = data.frame(x = c(min(xvals), max(xvals)), y = c(min(xvals), max(xvals))),
                                  mapping = ggplot2::aes(x = .data$x, y = .data$y),
                                  col = ablineColor,
                                  size = 1)
  } else if (abline) {
    g <- g + ggplot2::geom_line(mapping = aes(x = .data$x, y = .data$y), data = dfLine, inherit.aes = FALSE, color = ablineColor)
  }

  if (hasErrorbars)
    g <- g + ggplot2::geom_errorbar(aes(ymin = .data$ymin, ymax = .data$ymax))

  g <- g +
    ciLayer +
    geom_point() +
    scale_x_continuous(name = xName, breaks = xBreaks, limits = range(xBreaks)) +
    scale_y_continuous(name = yName, breaks = yBreaks, limits = range(yBreaks))

  return(jaspGraphs::themeJasp(g))

}
