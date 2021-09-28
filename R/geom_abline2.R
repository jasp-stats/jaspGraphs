#' Add an abline that does not exceed the extrema of the axes breaks.
#'
#' @description
#' The default abline of ggplot2 will exceeds the breaks and goes from one set of limits to the next.
#' This is a modified verions that respect one will not exceed the extrema of the x and y-breaks.
#'
#' @inheritParams ggplot2::geom_abline
#' @param method Either "breaks" (default) to respect the extrema of the axes or "ggplot2" to obtain the ggplot2 behavior.
#'
#' @example inst/examples/ex-geom_abline2.R
#' @export
geom_abline2 <- function(mapping = NULL, data = NULL, method = c("breaks", "ggplot2"), ..., slope, intercept,
                          na.rm = FALSE, show.legend = NA) {

  method <- match.arg(method)
  if (is.null(mapping) && missing(slope) && missing(intercept)) {
    slope <- 1
    intercept <- 0
  }
  if (!missing(slope) || !missing(intercept)) {
    if (missing(slope))
      slope <- 1
    if (missing(intercept))
      intercept <- 0
    data <- data.frame(intercept = intercept, slope = slope, method = method)
    mapping <- ggplot2::aes(intercept = intercept, slope = slope, method = method)
    show.legend <- FALSE
  }
  layer(data = data, mapping = mapping, stat = ggplot2::StatIdentity,
        geom = GeomAbline2, position = ggplot2::PositionIdentity, show.legend = show.legend,
        inherit.aes = FALSE, params = list(na.rm = na.rm, ...))
}

#' @rdname geom_abline2
#' @usage NULL
#' @format NULL
#' @export
#' @export
GeomAbline2 <- ggplot2::ggproto(
  "abline2",
  ggplot2::GeomAbline,
  required_aes = c("slope", "intercept"),
  optional_aes = "method",

  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE, prepend = NULL) {

    if (is.null(data$method)) {
      idxBreaks <- seq_len(nrow(data))
      idxGGplot2 <- integer()
    } else {
      if (!all(data$method %in% c("breaks", "ggplot2")))
        stop("aestethic 'method' for geom_abline2 expects either 'breaks' or 'ggplot2' but got something else!")

      idxBreaks  <- which(data$method == "breaks")
      idxGGplot2 <- which(data$method == "ggplot2")
    }

    if (length(idxBreaks) > 0) {
      ranges <- list(
        x = getRange(panel_params$x$get_breaks()),
        y = getRange(panel_params$y$get_breaks())
      )

      data$x   [idxBreaks] <- ranges$x[1]
      data$xend[idxBreaks] <- ranges$x[2]

      data$y   [idxBreaks] <- ranges$x[1] * data$slope[idxBreaks] + data$intercept[idxBreaks]
      data$yend[idxBreaks] <- ranges$x[2] * data$slope[idxBreaks] + data$intercept[idxBreaks]

      idxInfinite <- idxBreaks[is.infinite(data$y[idxBreaks])]
      if (length(idxInfinite) > 0) {
        data$y[idxInfinite] <- ranges$x[1]
        data$x[idxInfinite] <- 0
      }

      idxInfinite <- idxBreaks[is.infinite(data$yend[idxBreaks])]
      if (length(idxInfinite) > 0) {
        data$yend[idxInfinite] <- ranges$x[2]
        data$xend[idxInfinite] <- 0
      }

      # for all y and yend, check if they exceed the lower bound or upper bound of ranges$y.
      # if they do we change y = ax + b to x = (y - b) / a
      idxNeedsFlip <- idxBreaks[data$y[idxBreaks] < ranges$y[1]]
      if (length(idxNeedsFlip) > 0) {
        data$y[idxNeedsFlip] <-  ranges$y[1]
        data$x[idxNeedsFlip] <- (ranges$y[1] - data$intercept[idxNeedsFlip]) / data$slope[idxNeedsFlip]
      }

      idxNeedsFlip <- idxBreaks[data$y[idxBreaks] > ranges$y[2]]
      if (length(idxNeedsFlip) > 0) {
        data$y[idxNeedsFlip] <-  ranges$y[2]
        data$x[idxNeedsFlip] <- (ranges$y[2] - data$intercept[idxNeedsFlip]) / data$slope[idxNeedsFlip]
      }

      idxNeedsFlip <- idxBreaks[data$yend[idxBreaks] < ranges$y[1]]
      if (length(idxNeedsFlip) > 0) {
        data$yend[idxNeedsFlip] <-  ranges$y[1]
        data$xend[idxNeedsFlip] <- (ranges$y[1] - data$intercept[idxNeedsFlip]) / data$slope[idxNeedsFlip]
      }

      idxNeedsFlip <- idxBreaks[data$yend[idxBreaks] > ranges$y[2]]
      if (length(idxNeedsFlip) > 0) {
        data$yend[idxNeedsFlip] <-  ranges$y[2]
        data$xend[idxNeedsFlip] <- (ranges$y[2] - data$intercept[idxNeedsFlip]) / data$slope[idxNeedsFlip]
      }

    }

    if (length(idxGGplot2) > 0) {

      ranges <- coord$backtransform_range(panel_params)
      if (coord$clip == "on" && coord$is_linear()) {
        ranges$x <- ranges$x + c(-1, 1) * diff(ranges$x)
      }

      data$x   [idxGGplot2] <- ranges$x[1]
      data$xend[idxGGplot2] <- ranges$x[2]
      data$y   [idxGGplot2] <- ranges$x[1] * data$slope[idxGGplot2] + data$intercept[idxGGplot2]
      data$yend[idxGGplot2] <- ranges$x[2] * data$slope[idxGGplot2] + data$intercept[idxGGplot2]

    }

    GeomSegment$draw_panel(unique(data), panel_params, coord)

  }
)

getRange <- function(x) {
  # consider making this an S3 method if there are more cases

  if (is.character(x)) {

    # discrete axis return a character vector with attibute "pos" that contains the numeric positions
    if (!is.null(attr(x, "pos")))
      return(range(attr(x, "pos"), na.rm = TRUE))

    warning("jaspGraphs::getRange got a character and didn't quite understand it!", domain = NA)

  }

  return(range(x, na.rm = TRUE))

}
