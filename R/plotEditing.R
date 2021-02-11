# TODO:
#
# - drop limits from modifyable options?
# - introduce expand as option?
# - breaks[1] >= limits[1] ? expand[1] = breaks[1] - limits[1]] : expands[1] <- 0
#
# better handling of non-standard ggplot scales
# for example, breaks may be NULL, waiver(), a numeric vector, or a function
#
#
# - testing with vdiffr. test if original plot is retrieved by applying original options after editing plot
# - probably use limits
# - rename "title" to "name"?

# for multiple panel plots, examine https://github.com/zeehio/facetscales to manipulate individual axes.


# TODO: copied from common, do this more elegantly!
fromJSON  <- function(x) jsonlite::fromJSON(x, TRUE, FALSE, FALSE)
toJSON    <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null = "null")
.extractErrorMessage <- function(error) {

  split <- base::strsplit(as.character(error), ":")[[1]]
  last <- split[[length(split)]]
  stringr::str_trim(last)
}



#' @importFrom ggplot2 layer_scales is.ggplot ggplot_build

`%|NW|%` <- function(a, b) if (!(is.null(a) || is.waive(a))) a else b
`%|W|%`  <- function(a, b) if (                !is.waive(a)) a else b

validateOptions <- function(newOptions, oldOptions) {

  if (!is.list(newOptions)) {
    stop("options should be an R list or a json string!")
  }

  if (newOptions[["xAxis"]][["type"]] != oldOptions[["xAxis"]][["type"]] ||
      newOptions[["yAxis"]][["type"]] != oldOptions[["yAxis"]][["type"]]) {
    stop("The axis type in the new options list does not match the graph!")
  }

}

optionsDiff <- function(new, old) {

  # do we really need this function? reassigning is not too bad...
  tempFun <- function(nm, new, old) !identical(new[[nm]], old[[nm]])

  new[["xAxis"]][["settings"]] <- new[["xAxis"]][["settings"]][
    vapply(names(new[["xAxis"]][["settings"]]), tempFun,
      FUN.VALUE = logical(1L),
      new = new[["xAxis"]][["settings"]], old = old[["xAxis"]][["settings"]],
      USE.NAMES = FALSE
    )
  ]

  new[["yAxis"]][["settings"]] <- new[["yAxis"]][["settings"]][
    vapply(names(new[["yAxis"]][["settings"]]), tempFun,
           FUN.VALUE = logical(1L),
           new = new[["yAxis"]][["settings"]], old = old[["yAxis"]][["settings"]],
           USE.NAMES = FALSE
    )
  ]

  return(new)
}

#' @title Edit a plot
#' @param graph a ggplot2 object
#' @param newOptions an options list
#'
#' @export
plotEditing <- function(graph, newOptions) {

  if (!is.ggplot(graph))
    stop("graph should be a ggplot2")

  if (isTRUE(newOptions[["resetPlot"]])) {
    if (hasOriginalEditingOptions(graph)) {
      return(Recall(graph, as.list(graph[["plot_env"]][[".____originalPlotEditingOptions____"]][["oldOptions"]])))
    } else {
      return(graph)
    }
  }

  ggbuild     <- ggplot_build(graph)
  # TODO: first check if plot was previously edited, if so use those options
  oldOptions  <- plotEditingOptions(ggbuild)
  validateOptions(newOptions, oldOptions)

  # CONSIDER: since a diff takes place here and some options may interact, logic should be moved outside of
  # internalUpdateAxis and into earlier functions. internalUpdateAxis then no longer needs to be an S3 method.
  # diffOptions <- optionsDiff(newOptions, oldOptions)

  # could be a loop over all scales from e.g., facetted plots
  currentAxis <- ggbuild[["layout"]][["get_scales"]](1L)

  graph <- graph + internalUpdateAxis(currentAxis[["x"]], newOptions[["xAxis"]][["settings"]])
  graph <- graph + internalUpdateAxis(currentAxis[["y"]], newOptions[["yAxis"]][["settings"]])

  # if (length(diffOptions[["xAxis"]][["settings"]]) > 0L)
  #   graph <- graph + internalUpdateAxis(currentAxis[["x"]], diffOptions[["xAxis"]][["settings"]])
  #
  # if (length(diffOptions[["yAxis"]][["settings"]]) > 0L)
  #   graph <- graph + internalUpdateAxis(currentAxis[["y"]], diffOptions[["yAxis"]][["settings"]])

  # 'remember' if an edited plot had options set to automatic or manual
  newOptions[["resetPlot"]] <- FALSE
  env <- list2env(list(oldOptions = newOptions), parent = emptyenv())
  if (!hasOriginalEditingOptions(graph))
    graph[["plot_env"]][[".____originalPlotEditingOptions____"]] <- env
  graph[["plot_env"]][[".____plotEditingOptions____"]] <- env

  return(graph)

}

hasOriginalEditingOptions <- function(graph) !is.null(graph[["plot_env"]][[".____originalPlotEditingOptions____"]])
