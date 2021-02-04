#' @title Get the editable options for a graph
#' @param graph a plot object
#' @param asJSON should the list be converted to JSON?
#'
#' @export
plotEditingOptions <- function(graph, asJSON = FALSE) {
  options <- tryCatch(
    expr = getPlotEditingOptions(graph),
    unsupportedFigureError = function(e) {
      plotEditingOptionsError(
        gettextf("This figure is a %s", e)
      )
    },
    error = function(e) {
      plotEditingOptionsError(
        gettextf("Computing plotEditingOptions gave an error: %s",
                 .extractErrorMessage(e)),
        unexpected = TRUE
      )
    }
  )
  return(rListToJson(options, asJSON))
}

getPlotEditingOptions <- function(graph) {
  UseMethod("getPlotEditingOptions", graph)
}

getPlotEditingOptions.gg <- function(graph) {
  # ensures    that loading an edited graph returns the final set of options
  if (!is.null(graph[["plot_env"]][[".____plotEditingOptions____"]][["oldOptions"]]))
    return(graph[["plot_env"]][[".____plotEditingOptions____"]][["oldOptions"]])
  return(getPlotEditingOptions.ggplot(graph))
}

getPlotEditingOptions.ggplot <- function(graph) {
  getPlotEditingOptions.ggplot_built(ggplot_build(graph))
}

getPlotEditingOptions.ggplot_built <- function(graph) {

  # TODO: test if graph can be edited at all!

  # only relevant for continuous scales?
  opts <- graph[["layout"]][["panel_params"]]
  axisTypes <- getAxisType(opts)

  currentAxis <- graph[["layout"]][["get_scales"]](1L)

  xSettings <- getAxisInfo(currentAxis[["x"]], opts, graph)
  ySettings <- getAxisInfo(currentAxis[["y"]], opts, graph)

  out <- list(
    xAxis = list(
      type     = axisTypes[["x"]],
      settings = xSettings
    ), yAxis = list(
      type     = axisTypes[["y"]],
      settings = ySettings
    )
  )

  return(out)
}

getPlotEditingOptions.qgraph <- function(graph) {
  plotEditingOptionsError(gettext("This figure was created with qgraph."))
}

getPlotEditingOptions.jaspGraphsPlot <- function(graph) {
  plotEditingOptionsError(gettext("This figure consists of multiple smaller figures."))
}

getPlotEditingOptions.default <- function(graph) {
  plotEditingOptionsError(
    gettextf("cannot create plotEditingOptions for object of class: %s.", paste(class(graph), collapse = ",")),
    unexpected = TRUE
  )
}

rListToJson <- function(lst, asJSON = FALSE) {
  if (!asJSON) return(lst)
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
  reason <- if (unexpected)
    gettextf("Plot editing terminated unexpectedly. Fatal error in plotEditingOptions: %s To receive assistance with this problem, please report the message above at: https://jasp-stats.org/bug-reports", error)
  else
    gettextf("This plot can not be edited because: %s", error)
  list(reasonNotEditable = reason)
}
