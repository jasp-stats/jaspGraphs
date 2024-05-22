#' @importFrom ggplot2 is.ggplot

# silences a note in the R CMD Check but is not strictly necessary
#'@importFrom R6 R6Class

jaspGraphsPlot <- R6::R6Class(
  classname = "jaspGraphsPlot",
  public = list(
    initialize = function(subplots, plotFunction = reDrawJaspGraphsPlot, ...) {

      if (!all(vapply(subplots, is.ggplot, TRUE)))
        stop2("all subplots should be of class ggplot!")
      if (!is.function(plotFunction))
        stop2("plotFunction should be a function!")
      plotArgs <- list(...)
      if (!length(names(plotArgs)) == length(plotArgs))
        stop2("all arguments in ... should be named.")
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

reDrawJaspGraphsPlot <- function(subplots, args, grob = FALSE, newpage = TRUE,
                                 decodeplotFun = getDecodeplotFun(), ...) {
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

reDrawAlignedPlot <- function(subplots, args, grob = FALSE, newpage = TRUE,
                              decodeplotFun = getDecodeplotFun(), ...) {
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

getDecodeplotFun <- function() {
  jaspBaseInstalled <- length(find.package("jaspBase", .libPaths(), quiet = TRUE, verbose = FALSE)) != 0L
  if (!jaspBaseInstalled)
    return(NULL)
  if (utils::packageVersion("jaspBase") < "0.16.4")
    return(get0("decodeplot"))
  else # no longer in the global environment
    return(get0("decodeplot", envir = asNamespace("jaspBase")))
}

currentDevIsSvg <- function() isTRUE(try(attr(grDevices::dev.cur(), "names") == "devSVG"))
