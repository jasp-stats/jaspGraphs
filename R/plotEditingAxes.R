# all axis types in ggplot
AxisTypes <- c(
  "ScaleContinuous",
  "ScaleContinuousDate",
  "ScaleContinuousDatetime",
  "ScaleDiscretePosition"
)

getAxisType <- function(opts) {

  # Could be more specific!
  return(c("x" = if (opts[[1L]][["x"]][["is_discrete"]]()) "ScaleDiscrete" else "ScaleContinuous",
           "y" = if (opts[[1L]][["y"]][["is_discrete"]]()) "ScaleDiscrete" else "ScaleContinuous"
  ))
  # UseMethod("getAxisType", x)
}

getAxisType.list <- function(x) {
  # for output of layer_scales(plot)
  scaleX <- match(class(x[[1L]]), AxisTypes)
  scaleY <- match(class(x[[2L]]), AxisTypes)
  scaleX <- scaleX[!is.na(scaleX)]
  scaleY <- scaleY[!is.na(scaleY)]
  scaleX <- AxisTypes[scaleX]
  scaleY <- AxisTypes[scaleY]
  return(c("x" = scaleX, "y" = scaleY))
}

getAxisType.ggplot_built <- function(x) {
  return(c(
    "x" = class(x[["layout"]][["panel_scales_x"]][[1L]])[[2L]],
    "y" = class(x[["layout"]][["panel_scales_y"]][[1L]])[[2L]]
  ))
}

getAxisType.ggplot <- function(x) {
  return(getAxisType.list(layer_scales(x, i = 1L, j = 1L)))
}

getAxisTitle <- function(x, xory) {
  if (xory == "x") {
    return(x[["layout"]][["panel_scales_x"]][[1L]][["name"]] %|NW|% x[["plot"]][["labels"]][["x"]])
  } else {
    return(x[["layout"]][["panel_scales_y"]][[1L]][["name"]] %|NW|% x[["plot"]][["labels"]][["y"]])
  }
}


evenly_spaced <- function(x) {
  by <- x[2L] - x[1L]
  return(all((x[-length(x)] - x[-1L] - by) <= .Machine[["double.eps"]]))
}

getAxisInfo <- function(x, opts, ggbuild) {
  UseMethod("getAxisInfo", x)
}

expand_default <- function(scale, discrete = c(0, 0.6, 0, 0.6), continuous = c(0.05, 0, 0.05, 0)) {
  # copy of ggplot2:::expand_default to please R CMD check about :::
  a <- scale$expand
  if (!is.waive(a))
    return(a)
  else if (scale$is_discrete())
    return(discrete)
  else
    return(continuous)
}

getAxisInfo.ScaleContinuousPosition <- function(x, opts, ggbuild) {

  xory <- x[["aesthetics"]][1L]

  opts2keep <- list(
    labels = opts[[1]][[xory]][["get_labels"]](),
    breaks = opts[[1]][[xory]][["get_breaks"]](),
    limits = opts[[1]][[xory]][["get_limits"]](),
    expand = x[["expand"]] %|W|% expand_default(x),
    title  = getAxisTitle(ggbuild, xory)
  )

  if (anyNA(opts2keep[["breaks"]])) {
    idx <- which(!is.na(opts2keep[["breaks"]]))
    opts2keep[["breaks"]] <- opts2keep[["breaks"]][idx]
    opts2keep[["labels"]] <- opts2keep[["labels"]][idx]
  }

  breaks <- opts2keep[["breaks"]]
  from <- breaks[1L]
  to   <- breaks[length(breaks)]
  by   <- (to - from) / (length(breaks) - 1)

  opts2keep[["range"]] <- c(from, to, by)
  opts2keep[["breaksType"]] <- if (all(seq(from, to, by) == breaks)) "range" else "manual"
  # opts2keep[["limitsType"]] <- if (all(range(breaks) == opts2keep[["limits"]])) "automatic" else "manual"
  opts2keep[["limitsType"]] <- if (is.null(x[["limits"]])) "data" else "manual"

  return(opts2keep)

}

getAxisInfo.ScaleDiscretePosition <- function(x, opts, ggbuild) {

  xory <- x[["aesthetics"]][1L]
  return(list(
    labels = x[["get_labels"]](),
    shown  = x[["get_limits"]](),
    title  = getAxisTitle(ggbuild, xory)
  ))

}

internalUpdateAxis <- function(currentAxis, newSettings) {
  if (!is.null(newSettings[["title"]]))
    currentAxis[["name"]] <- newSettings[["title"]]
  UseMethod("internalUpdateAxis", currentAxis)
}

internalUpdateAxis.ScaleContinuousPosition <- function(currentAxis, newSettings) {

  if (newSettings[["breaksType"]] == "range") {
    tmp <- newSettings[["range"]]
    currentAxis[["breaks"]] <- seq(tmp[1L], tmp[2L], tmp[3L])
    currentAxis[["labels"]] <- as.character(currentAxis[["breaks"]])
  } else {
    currentAxis[["breaks"]] <- sort(newSettings[["breaks"]])
    currentAxis[["labels"]] <- newSettings[["labels"]]
  }

  if (newSettings[["limitsType"]] == "data") {
    currentAxis[["limits"]] <- NULL
  } else if (newSettings[["limitsType"]] == "breaks") {
    currentAxis[["limits"]] <- range(currentAxis[["breaks"]])
  } else if (newSettings[["limitsType"]] == "manual") {
    currentAxis[["limits"]] <- newSettings[["limits"]]
  }
  # TODO: see if some plot element fall outside of the new limits, i.e., currentAxis[["range"]][["range"]] is wider than user limits

  if (!is.null(newSettings[["expand"]])) {
    currentAxis[["expand"]] <- newSettings[["expand"]]
  }

  return(currentAxis)
}

internalUpdateAxis.ScaleDiscretePosition <- function(currentAxis, newSettings) {

  # newSettings only contains not modified settings!
  if (!is.null(newSettings[["shown"]]))
    currentAxis[["limits"]] <- newSettings[["shown"]]

  if (!is.null(newSettings[["labels"]]))
    currentAxis[["labels"]] <- newSettings[["labels"]]

  return(currentAxis)
}
