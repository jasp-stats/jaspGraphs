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

  if (isCoordFlipped(x[["layout"]][["coord"]]))
    xory <- if (xory == "x") "y" else "x"

  title <- if (xory == "x") {
    x[["layout"]][["panel_scales_x"]][[1L]][["name"]] %|W|% x[["plot"]][["labels"]][["x"]]
  } else {
    x[["layout"]][["panel_scales_y"]][[1L]][["name"]] %|W|% x[["plot"]][["labels"]][["y"]]
  }

  # TODO: this could be an S3 method that dispatches on the class of the title. We should get an idea of what all allowed classes are somehow
  if (is.null(title)) {
    titleType <- TitleType$Null
  } else if (is.call(title)) {
    titleType <- TitleType$Expression
    title     <- Reduce(paste, trimws(deparse(title)))
  } else if (is.expression(title)) {
    titleType <- TitleType$Expression
    title     <- as.character(title) #Reduce(paste, trimws(deparse(title)))
  } else if (is.character(title)) {
    titleType <- TitleType$Character
  } else {
    titleTypeWarning(title)
    titleType <- TitleType$Character
    # this conversion may fail...
    tmp <- try(as.character(title))
    if (!inherits(tmp, "try-error"))
      title <- tmp
  }

  return(list(
    title     = title,
    titleType = titleType
  ))
}


evenly_spaced <- function(x) {
  by <- x[2L] - x[1L]
  return(all((x[-length(x)] - x[-1L] - by) <= .Machine[["double.eps"]]))
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

getAxisInfo <- function(x, opts, ggbuild) {
  UseMethod("getAxisInfo", x)
}

getAxisInfo.ScaleContinuousPosition <- function(x, opts, ggbuild) {

  xory <- x[["aesthetics"]][1L]

  opts2keep <- list(
    labels = opts[[1]][[xory]][["get_labels"]](),
    breaks = opts[[1]][[xory]][["get_breaks"]](),
    limits = opts[[1]][[xory]][["get_limits"]](),
    # x[["expand"]] does not take coord_flip() into account
    expand = opts[[1]][[xory]][["scale"]][["expand"]] %|W|% expand_default(x)
  )

  opts2keep[c("title", "titleType")] <- getAxisTitle(ggbuild, xory)


  if (is.null(opts2keep[["breaks"]])) {

    opts2keep[["breaksType"]] <- BreaksType$Null
    opts2keep[["range"]]      <- "NULL" # TODO: fix this!
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
  by   <- if (length(breaks) > 1L) (to - from) / (length(breaks) - 1) else 0

  opts2keep[["range"]] <- c(from, to, by)
  if (is.null(opts2keep[["breaksType"]])) { # only set this if breaks weren't NULL
    # this tryCatch is ugly, but seq can crash in many ways..
    opts2keep[["breaksType"]] <- tryCatch({
      breaksValues <- seq(from, to, by)
      condition <- all(
        isTRUE(all.equal(seq(from, to, by), breaks)),
        opts2keep[["labels"]] == as.character(breaksValues)
      )
      if (condition) "range" else "manual"
    }, error = function(e) return("manual")
    )
  }

  opts2keep[["limitsType"]] <-
    if (is.null(x[["limits"]]))                                LimitsType$Data
    else if (isTRUE(all.equal(range(breaks), x[["limits"]])))  LimitsType$Breaks
    else                                                       LimitsType$Manual


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

  if (is.null(opts2keep[["breaksType"]]))

  return(opts2keep)

}

internalUpdateAxis <- function(currentAxis, newSettings) {
  if (!is.null(newSettings[["title"]]))
    currentAxis[["name"]] <- internalUpdateTitle(newSettings[["titleType"]], newSettings[["title"]])
  UseMethod("internalUpdateAxis", currentAxis)
}

internalUpdateAxis.ScaleContinuousPosition <- function(currentAxis, newSettings) {

  if (newSettings[["breaksType"]] == BreaksType$Null) {
     currentAxis[["breaks"]] <- NULL
     currentAxis[["labels"]] <- NULL
     # shouldn't be possible, but will do bad things if it happens
     if (newSettings[["limitsType"]] == LimitsType$Breaks)
       newSettings[["limitsType"]] <- LimitsType$Manual

  } else if (newSettings[["breaksType"]] == BreaksType$Range) {
    tmp <- newSettings[["range"]]
    # zapsmall avoids floating point artefacts (e.g., try as.character(seq(-0.6, 0.2, 0.2)))
    currentAxis[["breaks"]] <- zapsmall(seq(tmp[1L], tmp[2L], tmp[3L]))
    currentAxis[["labels"]] <- as.character(currentAxis[["breaks"]])
  } else {
    # currentAxis[["breaks"]] <- sort(newSettings[["breaks"]])
    currentAxis[["breaks"]] <- newSettings[["breaks"]]
    currentAxis[["labels"]] <- newSettings[["labels"]]
  }

  currentAxis[["limits"]] <- switchEnum(newSettings[["limitsType"]], LimitsType,
    Data   = NULL, # let ggplot2 figure it out
    Breaks = range(currentAxis[["breaks"]]),
    Manual = newSettings[["limits"]]
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
  return(switchEnum(titleType, TitleType,
    Null       = NULL,
    Character  = title,
    Expression = {
      expr <- try(parse(text = title))
      if (inherits(expr, "try-error")) {
        warning("input was not a valid R expression, reinterpreting as character")
        return(title)
      }
      return(expr)
    },
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

remapPositionOfFlippedPlot <- function(position) {
  # for a plot with ggplot2::coord_flip(), the scales are modified
  # in particular, given
  # scales <- ggbuild[["layout"]][["get_scales"]](1L)
  # instead of scales$x$position == "bottom"
  # we have scales$x$position == "left"
  switch(position,
    "top"  = "right",
    "right" = "top",
    "left" = "bottom",
    "bottom" = "left"
  )
}
