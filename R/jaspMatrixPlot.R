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
#' @param diagonalPlotFunction A function that draws the plots on the diagonal.
#' Must accept arguments \code{x} (numeric), \code{xName} (character), and \code{xAxis} (the shared axis of \code{x}, see Details).
#' @param diagonalPlotArgs A list of additional arguments to pass to \code{diagonalPlotFunction}.
#' @param topRightPlotFunction A function that draws the plots in the upper triangle.
#' Must accept arguments \code{x}, \code{y} (numeric), \code{xName}, \code{yName} (character), and \code{xAxis}, \code{yAxis} (the shared axes of \code{x} and \code{y}, see Details).
#' @param topRightPlotArgs A list of additional arguments to pass to \code{topRightPlotFunction}.
#' @param bottomLeftPlotFunction A function that draws the plots in the lower triangle. Must accept the same arguments as \code{topRightPlotFunction}.
#' @param bottomLeftPlotArgs A list of additional arguments to pass to \code{bottomLeftPlotFunction}.
#' @param overwriteDiagonalAxes,overwriteTopRightAxes,overwriteBottomLeftAxes Which axis titles should be removed, because they are already shown by the row and column titles of the matrix. Possible options:
#' \describe{
#'   \item{\code{"none"}}{No axis titles are removed.}
#'   \item{\code{"both"}}{Both axis titles are removed.}
#'   \item{\code{"x"}}{Only the x-axis title is removed.}
#'   \item{\code{"y"}}{Only the y-axis title is removed.}
#' }
#' Common axes across a row or column are obtained by passing the same \code{xAxis} and \code{yAxis} to every panel.
#' @param breaks Method for computing the bin breaks of each variable, see \code{breaks} in [jaspMarginal].
#' @param axesLabels Optional character vector; provide column/row names of the matrix.
#'
#' @details The axis of each variable is computed once and passed as \code{xAxis} and \code{yAxis} to every panel,
#' so that all panels showing the same variable share an axis. An axis is a list with elements
#' \code{binBreaks} (the histogram bin breaks computed with \code{breaks}), \code{breaks} (the axis breaks),
#' and \code{limits} (the axis limits, which span the axis breaks).
#' The axes cover all layers of all panels: if a panel extends beyond the axes of its variables, e.g., because of a prediction interval,
#' these axes are extended and every panel showing these variables is drawn again with the extended axes.
#' With three or more variables, fewer axis breaks are used so that the tick labels of the narrow panels do not overlap.
#' @export
jaspMatrixPlot <- function(
    data,
    diagonalPlotFunction       = jaspMarginal,
    diagonalPlotArgs           = list(),
    topRightPlotFunction       = jaspBivariate,
    topRightPlotArgs           = list(),
    bottomLeftPlotFunction     = NULL,
    bottomLeftPlotArgs         = list(),
    overwriteDiagonalAxes      = "x",
    overwriteTopRightAxes      = "both",
    overwriteBottomLeftAxes    = "both",
    breaks                     = "sturges",
    axesLabels
) {

  # validate input
  if (!is.data.frame(data) || nrow(data) == 0)
    stop2("`data` must be a non-empty data frame.")

  if (ncol(data) < 2)
    stop2("`data` must have more than 1 column.")

  if(missing(axesLabels)) {
    axesLabels <- colnames(data)
  } else if(ncol(data) != length(axesLabels)) {
    stop2("`axesLabels` must be the same length as `ncol(data)`.")
  }

  # the labels are stored as column names, so that they are dropped together with the non-numeric columns
  colnames(data) <- axesLabels
  data <- data[, vapply(data, is.numeric, logical(1)), drop = FALSE]
  axesLabels <- colnames(data)

  if (ncol(data) < 2)
    stop2("`data` must have more than 1 numeric column.")

  overwriteDiagonalAxes   <- match.arg(overwriteDiagonalAxes,   choices = c("none", "both", "x", "y"))
  overwriteTopRightAxes   <- match.arg(overwriteTopRightAxes,   choices = c("none", "both", "x", "y"))
  overwriteBottomLeftAxes <- match.arg(overwriteBottomLeftAxes, choices = c("none", "both", "x", "y"))

  titles    <- c(list(patchwork::plot_spacer()), lapply(axesLabels, .makeTitle))

  # fewer breaks in narrow panels, otherwise the tick labels run together
  nBreaks <- if (ncol(data) <= 2) 5 else 3
  axes    <- lapply(data, function(v) try(jaspSharedAxis(x = v, breaks = breaks, n = nBreaks), silent = TRUE))

  # the panel in row `row` and column `col` with the current axes, NULL if there is no panel
  makePanel <- function(row, col) {
    if (row == col) {
      fun <- diagonalPlotFunction;   args <- diagonalPlotArgs;   overwriteAxes <- overwriteDiagonalAxes
    } else if (row < col) {
      fun <- topRightPlotFunction;   args <- topRightPlotArgs;   overwriteAxes <- overwriteTopRightAxes
    } else {
      fun <- bottomLeftPlotFunction; args <- bottomLeftPlotArgs; overwriteAxes <- overwriteBottomLeftAxes
    }

    if (!is.function(fun))
      return(NULL)

    args[["x"]]     <- data[[col]]
    args[["xName"]] <- axesLabels[[col]]
    args[["xAxis"]] <- axes[[col]]
    if (row != col) {
      args[["y"]]     <- data[[row]]
      args[["yName"]] <- axesLabels[[row]]
      args[["yAxis"]] <- axes[[row]]
    }
    return(.trySubPlot(fun, args, overwriteAxes))
  }

  k      <- ncol(data)
  panels <- matrix(list(), k, k)
  for (row in seq_len(k))
    for (col in seq_len(k))
      panels[row, col] <- list(makePanel(row, col))

  # the axes also cover the smooth lines and prediction intervals of every panel. The axis of
  # a variable is the x-axis of its column and the y-axis of its row, except on the diagonal.
  ranges <- vector("list", k)
  for (row in seq_len(k)) {
    for (col in seq_len(k)) {
      panel <- panels[[row, col]]
      if (is.null(panel) || !panel[["success"]])
        next

      panelRanges <- try(jaspPanelRanges(panel[["plot"]]), silent = TRUE)
      if (inherits(panelRanges, "try-error"))
        next

      ranges[[col]] <- c(ranges[[col]], panelRanges[["x"]])
      if (row != col)
        ranges[[row]] <- c(ranges[[row]], panelRanges[["y"]])
    }
  }

  oldAxes <- axes
  for (v in seq_len(k))
    if (!inherits(axes[[v]], "try-error"))
      axes[[v]] <- jaspExtendAxis(axes[[v]], ranges[[v]], n = nBreaks)

  changed <- !mapply(identical, axes, oldAxes)
  for (row in seq_len(k))
    for (col in seq_len(k))
      if (changed[row] || changed[col])
        panels[row, col] <- list(makePanel(row, col))

  plots <- titles
  i <- length(plots) + 1
  for (row in seq_len(k)) {
    plots[[i]] <- .makeTitle(axesLabels[[row]], angle = 90)
    i <- i + 1

    for (col in seq_len(k)) {
      panel      <- panels[[row, col]]
      plots[[i]] <- if (is.null(panel)) patchwork::plot_spacer() else panel[["plot"]]
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


# returns a list with the plot and whether it succeeded, a failed plot shows the error message
.trySubPlot <- function(fun, args, overwriteAxes) {
  # the shared axis of a variable could not be computed, e.g., because it has no finite values
  for (axis in args[c("xAxis", "yAxis")])
    if (inherits(axis, "try-error"))
      return(list(plot = .makeErrorPlot(axis), success = FALSE))

  res <- try(do.call(fun, args), silent = TRUE)

  if(inherits(res, "try-error"))
    return(list(plot = .makeErrorPlot(res), success = FALSE))

  if(overwriteAxes %in% c("both", "x"))
    res <- res + ggplot2::xlab(NULL)

  if(overwriteAxes %in% c("both", "y"))
    res <- res + ggplot2::ylab(NULL)

  return(list(plot = res, success = TRUE))
}
