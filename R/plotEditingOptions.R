#' @title Get the editable options for a plot
#' @param plot a plot object
#' @param asJSON should the list be converted to JSON?
#'
#' @export
plotEditingOptions <- function(plot, asJSON = FALSE) {
  options <- tryCatch(
    expr = getPlotEditingOptions(plot),
    unsupportedFigureError = function(e) {
      plotEditingOptionsError(e[["message"]])
    },
    error = function(e) {
      plotEditingOptionsError(
        gettextf("Computing plotEditingOptions gave an error: %s",
                 .extractErrorMessage(e)),
        unexpected = TRUE
      )
    }
  )
  if (asJSON)
    return(rListToJson(options))
  else
    return(options)
}

getPlotEditingOptions <- function(graph) {
  UseMethod("getPlotEditingOptions", graph)
}

#' @exportS3Method
getPlotEditingOptions.gg <- function(graph) {
  # ensures    that loading an edited graph returns the final set of options
  if (!is.null(graph[["plot_env"]][[".____plotEditingOptions____"]][["oldOptions"]]))
    return(graph[["plot_env"]][[".____plotEditingOptions____"]][["oldOptions"]])
  return(getPlotEditingOptions.ggplot(graph))
}

#' @exportS3Method
getPlotEditingOptions.ggplot <- function(graph) {
  getPlotEditingOptions.ggplot_built(ggplot_build(graph))
}

#' @exportS3Method
getPlotEditingOptions.ggplot_built <- function(graph) {

  # TODO: test if graph can be edited at all!
  validateGraphType(graph)

  # only relevant for continuous scales?
  opts <- graph[["layout"]][["panel_params"]]
  axisTypes <- getAxisType(opts)

  currentAxis <- graph[["layout"]][["get_scales"]](1L)

  xSettings <- getAxisInfo(currentAxis[["x"]], opts, graph)
  ySettings <- getAxisInfo(currentAxis[["y"]], opts, graph)

  if (isCoordFlipped(graph[["layout"]][["coord"]])) {
    tmp <- xSettings
    xSettings <- ySettings
    ySettings <- tmp
  }

  coords <- graph[["data"]][[1]]

  out <- list(
    xAxis = list(
      type     = axisTypes[["x"]],
      settings = xSettings
    ), yAxis = list(
      type     = axisTypes[["y"]],
      settings = ySettings
    ),
    coords = coords,
    error = ErrorType$Success
  )

  return(out)
}

#' @exportS3Method
getPlotEditingOptions.qgraph <- function(graph) {
  plotEditingOptionsError(gettext("This plot cannot be edited because it was created with qgraph instead of ggplot."))
}

#' @exportS3Method
getPlotEditingOptions.jaspGraphsPlot <- function(graph) {
  plotEditingOptionsError(gettext("This plot cannot be edited because it consists of multiple smaller figures."))
}

#' @exportS3Method getPlotEditingOptions "function"
getPlotEditingOptions.function <- function(graph) {
  plotEditingOptionsError(gettext("This plot cannot be edited because it was created with base R instead of ggplot."))
}

#' @exportS3Method
getPlotEditingOptions.default <- function(graph) {
  plotEditingOptionsError(
    gettextf("cannot create plotEditingOptions for object of class: %s.", paste(class(graph), collapse = ",")),
    unexpected = TRUE
  )
}

rListToJson <- function(lst) {
  tryCatch(
    toJSON(lst),
    error = function(e) {
      toJSON(plotEditingOptionsError(
        gettextf("Converting plotEditingOptions to JSON gave an error: %s.",
                 .extractErrorMessage(e)),
        unexpected = TRUE
      ))
    }
  )
}

plotEditingOptionsError <- function(error, unexpected = FALSE) {
  if (unexpected) {
    list(
      reasonNotEditable = gettextf("Plot editing terminated unexpectedly. Fatal error in plotEditingOptions: %s To receive assistance with this problem, please report the message above at: https://jasp-stats.org/bug-reports", error),
      errorType = ErrorType$FatalError
    )
  } else {
    list(
      reasonNotEditable = error,
      errorType = ErrorType$ValidationError
    )
  }
}

validateGraphType <- function(graph) {

  # more to come!
  if (is.coordPolar(graph[["layout"]][["coord"]]))
    unsupportedFigureError(gettext("This plot cannot be edited because it uses polar coordinates (e.g., pie chart)."))

}

is.coordPolar <- function(x) inherits(x, "CoordPolar")

unsupportedFigureError <- function(message) {
  e <- structure(class = c("unsupportedFigureError", "error", "condition"),
                 list(message=message, call=sys.call(-1)))
  stop2(e)
}
