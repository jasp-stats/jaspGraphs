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
  title <- if (xory == "x") {
    x[["layout"]][["panel_scales_x"]][[1L]][["name"]] %|W|% x[["plot"]][["labels"]][["x"]]
  } else {
    x[["layout"]][["panel_scales_y"]][[1L]][["name"]] %|W|% x[["plot"]][["labels"]][["y"]]
  }

  # TODO: this could be an S3 method that dispatches on the class of the title. We should get an idea of what all allowed classes are somehow
  if (is.null(title)) {
    titleType <- "NULL"
  } else if (is.call(title) || is.expression(title)) {
    titleType <- "expression"
    title     <- Reduce(paste, trimws(deparse(title)))
  } else if (is.character(title)) {
    titleType <- "character"
  } else {
    titleTypeWarning(title)
    titleType <- "character"
    # this conversion may fail...
    tmp <- try(as.character(title))
    if (!inherits(tmp, "try-error"))
      title <- tmp
  }

  if (titleType == "expression")
    title <- Reduce(paste, trimws(deparse(title)))

  return(list(
    title     = title,
    titleType = titleType
  ))
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
    expand = x[["expand"]] %|W|% expand_default(x)
  )

  opts2keep[c("title", "titleType")] <- getAxisTitle(ggbuild, xory)


  if (is.null(opts2keep[["breaks"]])) {

    opts2keep[["breaksType"]] <- "NULL"
    opts2keep[["range"]]      <- "NULL"
    # set reasonable defaults for this
    breaks <- getPrettyAxisBreaks(opts2keep[["limits"]])
    opts2keep[["breaks"]] <- breaks
    opts2keep[["labels"]] <- breaks

  } else {

    if (anyNA(opts2keep[["breaks"]])) {
      idx <- which(!is.na(opts2keep[["breaks"]]))
      opts2keep[["breaks"]] <- opts2keep[["breaks"]][idx]
      opts2keep[["labels"]] <- opts2keep[["labels"]][idx]
    }
    breaks <- opts2keep[["breaks"]]

  }

  from <- breaks[1L]
  to   <- breaks[length(breaks)]
  by   <- (to - from) / (length(breaks) - 1)

  opts2keep[["range"]] <- c(from, to, by)
  if (is.null(opts2keep[["breaksType"]])) { # only set this if breaks weren't NULL
    # this tryCatch is ugly, but seq can crash in many ways..
    opts2keep[["breaksType"]] <- tryCatch(
      if (all(seq(from, to, by) == breaks)) "range" else "manual",
      error = function(e) return("manual")
    )
  }

  opts2keep[["limitsType"]] <- if (is.null(x[["limits"]]))
    "data"
  else if (isTRUE(all.equal(range(breaks), x[["limits"]])))
    "breaks"
  else
    "manual"


  return(opts2keep)

}

getAxisInfo.ScaleDiscretePosition <- function(x, opts, ggbuild) {

  xory <- x[["aesthetics"]][1L]
  opts2keep <- list(
    labels = x[["get_labels"]](),
    shown  = x[["get_limits"]](),
    title  = getAxisTitle(ggbuild, xory)
  )
  opts2keep[c("title", "titleType")] <- getAxisTitle(ggbuild, xory)
  return(opts2keep)

}

internalUpdateAxis <- function(currentAxis, newSettings) {
  if (!is.null(newSettings[["title"]]))
    currentAxis[["name"]] <- internalUpdateTitle(newSettings[["titleType"]], newSettings[["title"]])
  UseMethod("internalUpdateAxis", currentAxis)
}

internalUpdateAxis.ScaleContinuousPosition <- function(currentAxis, newSettings) {

  if (newSettings[["breaksType"]] == "NULL") {
     currentAxis[["breaks"]] <- NULL
     currentAxis[["labels"]] <- NULL
     # shouldn't be possible, but will do bad things if it happens
     if (newSettings[["limitsType"]] == "breaks")
       newSettings[["limitsType"]] <- "manual"

  } else if (newSettings[["breaksType"]] == "range") {
    tmp <- newSettings[["range"]]
    # zapsmall avoids floating point artefacts (e.g., try as.character(seq(-0.6, 0.2, 0.2)))
    currentAxis[["breaks"]] <- zapsmall(seq(tmp[1L], tmp[2L], tmp[3L]))
    currentAxis[["labels"]] <- as.character(currentAxis[["breaks"]])
  } else {
    currentAxis[["breaks"]] <- sort(newSettings[["breaks"]])
    currentAxis[["labels"]] <- newSettings[["labels"]]
  }

  currentAxis[["limits"]] <- switch(newSettings[["limitsType"]],
    "data"   = NULL, # let ggplot2 figure it out
    "breaks" = range(currentAxis[["breaks"]]),
    "manual" = newSettings[["limits"]]
  )

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

internalUpdateTitle <- function(titleType, title) {
  return(switch(titleType,
    "NULL"       = NULL,
    "character"  = title,
    "expression" = parse(text = title),
    {
      titleTypeWarning(titleType)
      title
    }
  ))
}

titleTypeWarning <- function(title) {
  msg <- if (is.character(title))
    title
  else
    paste(class(title), collapse = ", ")
  warning(sprintf("Unknown title type: %s. I'm pretending it works as character and hope for the best...", msg))
}
