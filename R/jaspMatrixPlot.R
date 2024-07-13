#' Matrix plot
#'
#' @description Plot that consists of \code{ncol{data}} by \code{ncol{data}} plots,
#' where subplot on position \eqn{(i, j)} plots \code{data[, c(i, j)]}.
#' The plot can display three different types of plots:
#' \describe{
#'   \item{\code{diagonal}}{Where \code{i == j}.}
#'   \item{\code{topRight}}{Where \code{i < j}.}
#'   \item{\code{bottomLeft}}{Where \code{i > j}.}
#' }
#'
#' @param data Data frame of data to plot.
#' @param diagonalPlotFunction A function that draws the plots on the diagonal. Must accept arguments \code{x} (numeric), \code{xName} (character).
#' @param diagonalPlotArgs A list of additional arguments to pass to \code{diagonalPlotFunction}.
#' @param topRightPlotFunction A function that draws the plots in the upper triangle. Must accept arguments \code{x} (numeric), \code{xName} (character).
#' @param topRightPlotArgs A list of additional arguments to pass to \code{topRightPlotFunction}.
#' @param bottomLeftPlotFunction A function that draws the plots in the lower triangle. Must accept arguments \code{x} (numeric), \code{xName} (character).
#' @param bottomLeftPlotArgs A list of additional arguments to pass to \code{bottomLeftPlotFunction}.
#' @param overwriteDiagonalAxes,overwriteTopRightAxes,overwriteBottomLeftAxes Which axes should be overwritten such that they have a common range. Possible options:
#' \describe{
#'   \item{\code{"none"}}{No axes are overwritten, hence the plots get their own scales given by \code{diagonal}, \code{topRight}, and \code{bottomLeft} functions, respectively.}
#'   \item{\code{"both"}}{Both axes are overwritten. The plots inherit scales by setting their \code{breaks} determined by [getPrettyAxisBreaks], and the plotting region is set by [ggplot2::coord_cartesian] with \code{limits} set to \code{range(breaks)}. Further, the name of the axis is set to \code{NULL}.}
#'   \item{\code{"x"}}{x-axis gets overwritten (see option \code{"both"}), y-axis does not (see option \code{"none"}).}
#'   \item{\code{"y"}}{y-axis gets overwritten (see option \code{"both"}), x-axis does not (see option \code{"none"}).}
#' }
#' @param binWidthType See [jaspHistogram]. Used for determining consistent axes.
#' @param numberOfBins See [jaspHistogram]. Used for determining consistent axes.
#' @param axesLabels Optional character vector; provide column/row names of the matrix.
#' @export
jaspMatrixPlot <- function(
    data,
    diagonalPlotFunction       = jaspHistogram,
    diagonalPlotArgs           = list(),
    topRightPlotFunction       = jaspBivariate,
    topRightPlotArgs           = list(),
    bottomLeftPlotFunction     = NULL,
    bottomLeftPlotArgs         = list(),
    overwriteDiagonalAxes      = "x",
    overwriteTopRightAxes      = "both",
    overwriteBottomLeftAxes    = "both",
    binWidthType               = "doane",
    numberOfBins               = NULL,
    axesLabels
) {

  # validate input
  if (!is.data.frame(data) || nrow(data) == 0 || ncol(data) < 2)
    stop2("`data` must be a data frame")

  if(missing(axesLabels)) {
    axesLabels <- colnames(data)
  } else if(ncol(data) != length(axesLabels)) {
    stop2("`axesLabels` must be the same length as `ncol(data)`.")
  }

  isNumeric <- vapply(data, is.numeric, logical(1))
  data <- data[, isNumeric, drop = FALSE]
  axesLabels <- axesLabels[isNumeric]

  if (ncol(data) < 2)
    stop2("`data` must have more than 1 numeric column.")

  overwriteDiagonalAxes   <- match.arg(overwriteDiagonalAxes,   choices = c("none", "both", "x", "y"))
  overwriteTopRightAxes   <- match.arg(overwriteTopRightAxes,   choices = c("none", "both", "x", "y"))
  overwriteBottomLeftAxes <- match.arg(overwriteBottomLeftAxes, choices = c("none", "both", "x", "y"))

  titles    <- c(list(patchwork::plot_spacer()), lapply(axesLabels, .makeTitle))

  plots <- titles
  i <- length(plots) + 1
  for (row in seq_along(axesLabels)) {
    y       <- data[[row]]
    yName   <- axesLabels[[row]]
    yBreaks <- getJaspHistogramBreaks(x = y, binWidthType = binWidthType, numberOfBins = numberOfBins)

    plots[[i]] <- .makeTitle(yName, angle = 90)
    i <- i + 1

    for (col in seq_along(axesLabels)) {
      x       <- data[[col]]
      xName   <- axesLabels[[col]]
      xBreaks <- getJaspHistogramBreaks(x = x, binWidthType = binWidthType, numberOfBins = numberOfBins)

      if (row == col) { # diagonal
        if(is.function(diagonalPlotFunction)) {
          diagonalPlotArgs[["x"]]       <- x
          diagonalPlotArgs[["xName"]]   <- xName
          diagonalPlotArgs[["xBreaks"]] <- xBreaks
          plot <- .trySubPlot(diagonalPlotFunction, diagonalPlotArgs, overwriteDiagonalAxes)
        } else {
          plot <- patchwork::plot_spacer()
        }
      } else if(row < col) { # topRight
        if(is.function(topRightPlotFunction)) {
          topRightPlotArgs[["x"]]       <- x
          topRightPlotArgs[["y"]]       <- y
          topRightPlotArgs[["xName"]]   <- xName
          topRightPlotArgs[["yName"]]   <- yName
          topRightPlotArgs[["xBreaks"]] <- xBreaks
          topRightPlotArgs[["yBreaks"]] <- yBreaks
          plot <- .trySubPlot(topRightPlotFunction, topRightPlotArgs, overwriteTopRightAxes)
        } else {
          plot <- patchwork::plot_spacer()
        }
      } else { # bottomLeft
        if(is.function(bottomLeftPlotFunction)) {
          bottomLeftPlotArgs[["x"]]       <- x
          bottomLeftPlotArgs[["y"]]       <- y
          bottomLeftPlotArgs[["xName"]]   <- xName
          bottomLeftPlotArgs[["yName"]]   <- yName
          bottomLeftPlotArgs[["xBreaks"]] <- xBreaks
          bottomLeftPlotArgs[["yBreaks"]] <- yBreaks
          plot <- .trySubPlot(bottomLeftPlotFunction, bottomLeftPlotArgs, overwriteBottomLeftAxes)
        } else {
          plot <- patchwork::plot_spacer()
        }
      }
      # plots[[col, row]] <- plot
      plots[[i]] <- plot
      i <- i + 1
    }
  }

  margins <- c(1*length(axesLabels), rep(9, length(axesLabels)))

  out <- patchwork::wrap_plots(plots, ncol = ncol(data)+1, nrow = ncol(data)+1, byrow = TRUE, widths = margins, heights = margins)
  out <- out + patchwork::plot_layout(guides = "collect")
  return(out)
}

.makeTitle <- function(nm, angle = 0) {
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 1/2, y = 1/2, label = nm, angle = angle,
      size = 1.2 * getGraphOption("fontsize") / ggplot2::.pt
    ) +
    ggplot2::ylim(0:1) + ggplot2::xlim(0:1) +
    ggplot2::theme_void()
}

.makeErrorPlot <- function(e) {
  message <- as.character(e)
  message <- strsplit(message, ": ")[[1]]
  message <- paste(message[-1], collapse = "")
  message <- strwrap(message, width = 20, initial = gettext("Plotting not possible:\n"))
  message <- paste(message, collapse = "\n")

  res <- ggplot2::ggplot() +
    ggplot2::geom_label(
      data    = data.frame(x = 0.5, y = 0.5, label = message),
      mapping = ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      fill    = grDevices::adjustcolor("red", alpha.f = 0.5),
      size    = 0.7 * getGraphOption("fontsize") / ggplot2::.pt,
      hjust   = "center",
      vjust   = "center"
    ) +
    ggplot2::xlim(0:1) +
    ggplot2::ylim(0:1) +
    ggplot2::theme_void()

  return(res)
}


.trySubPlot <- function(fun, args, overwriteAxes) {
  res <- try(do.call(fun, args), silent = TRUE)

  if(inherits(res, "try-error"))
    return(.makeErrorPlot(res))

  if(overwriteAxes %in% c("both", "x"))
    res <- res + ggplot2::xlab(NULL)

  if(overwriteAxes %in% c("both", "y"))
    res <- res + ggplot2::ylab(NULL)

  return(res)
}
