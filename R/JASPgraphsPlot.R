#' @importFrom ggplot2 is.ggplot

jaspGraphsPlot <- R6::R6Class(
  classname = "jaspGraphsPlot",
  public = list(
    initialize = function(subplots, plotFunction = reDrawJaspGraphsPlot, ...) {

      if (!all(vapply(subplots, is.ggplot, TRUE)))
        stop("all subplots should be of class ggplot!")
      if (!is.function(plotFunction))
        stop("plotFunction should be a function!")
      plotArgs <- list(...)
      if (!length(names(plotArgs)) == length(plotArgs))
        stop("all arguments in ... should be named.")
      if (is.null(plotArgs[["names"]]) && identical(plotFunction, reDrawJaspGraphsPlot))
        plotArgs[["names"]] <- paste0("plot", seq_along(subplots))

      self$subplots     <- subplots
      self$plotFunction <- plotFunction
      self$plotArgs     <- plotArgs
    },
    plot = function(...) self$plotFunction(self$subplots, self$plotArgs, ...),
    plotFunction = NULL,
    plotArgs     = NULL,
    subplots     = NULL
  )
)

#' Methods for interacting with a jaspGraphsPlot
#'
#' @name jaspGraphsPlotMethods
#' @param x an object of class jaspGraphsPlot
#' @param field the name or index of a subplot
#' @param value the value that should be assigned
#'
#' @description These methods are mainly convenience functions that ensure things like seq_along work.
#'
#' @export
`[[.jaspGraphsPlot` <- function(x, field) x$subplots[[field]]

#' @export
#' @rdname jaspGraphsPlotMethods
`[[<-.jaspGraphsPlot` <- function(x, field, value) {
  x$subplots[[field]] <- value
  return(x)
}

#' @export
#' @rdname jaspGraphsPlotMethods
is.jaspGraphsPlot <- function(x) {
  inherits(x, "jaspGraphsPlot")
}

#' @export
#' @rdname jaspGraphsPlotMethods
length.jaspGraphsPlot <- function(x) {
  length(x$subplots)
}

#' @export
#' @rdname jaspGraphsPlotMethods
names.jaspGraphsPlot <- function(x) {
  names(x$subplots)
}

reDrawJaspGraphsPlot <- function(subplots, args, grob = FALSE, newpage = FALSE,
                                 decodeplotFun = get0("decodeplot"), ...) {
  # redraws plots from PlotPriorAndPosterior, PlotRobustnessSequential, and ggMatrixplot
  g <- gridExtra::arrangeGrob(
    grobs         = subplots,
    heights       = args[["heights"]],
    layout_matrix = args[["layout"]],
    widths        = args[["widths"]],
    names         = args[["names"]]
  )
  if (!is.null(decodeplotFun))
    g <- decodeplotFun(g)
  
  if (grob)
    return(g)
  else
    return(gridExtra::grid.arrange(g, ..., newpage = newpage))
}

reDrawAlignedPlot <- function(subplots, args, grob = FALSE, newpage = FALSE,
                              decodeplotFun = get0("decodeplot"), ...) {
  # redraws plots from JASPScatterPlot
  g <- makeGrobAlignedPlots(
    mainplot   = subplots[["mainPlot"]],
    abovePlot  = subplots[["topPlot"]],
    rightPlot  = subplots[["rightPlot"]],
    showLegend = args[["showLegend"]],
    size       = args[["size"]]
  )
  if (!is.null(decodeplotFun))
    g <- decodeplotFun(g)

  if (grob) {
    return(g)
  } else {
    if (newpage)
      grid::grid.newpage()
    return(grid::grid.draw(g, ...))
  }
}

currentDevIsSvg <- function() isTRUE(try(attr(grDevices::dev.cur(), "names") == "devSVG"))
