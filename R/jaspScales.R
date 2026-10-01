# NOTE: the limits argument of ggplot2::scale_*_continuous is broken...
# consider opening an issue for this.

#' @title Continuous axis scales
#' @param name see details
#' @param breaks see details
#' @param minor_breaks see details
#' @param n.breaks see details
#' @param labels see details
#' @param limits see details
#' @param expand see details
#' @param oob see details
#' @param na.value see details
#' @param trans see details
#' @param transform see details
#' @param guide see details
#' @param position see details
#' @param sec.axis see details
#' @details These functions are virtually identical to \code{\link[ggplot2]{scale_x_continuous}} and \code{\link[ggplot2]{scale_y_continuous}}
#' except that default values are different, these use a different function to determine the default
#' axis breaks.
#'
#' @rdname scale_x_continuous
#' @export
scale_x_continuous <- function(name = waiver(), breaks = getPrettyAxisBreaks, minor_breaks = waiver(),
                               n.breaks = NULL, labels = axesLabeller, limits = "JASP", expand = waiver(), oob = censor,
                               na.value = NA_real_, trans = "identity", transform = "identity", guide = waiver(), position = "bottom",
                               sec.axis = waiver()) {

  if (graphOptions("ggVersion") >= "4.0.0") {
    call <- rlang::caller_call()
    sc <- continuous_scale(
      get_ggplot_global()$x_aes,
      palette = identity, name = name, breaks = breaks, n.breaks = n.breaks,
      minor_breaks = minor_breaks, labels = labels, limits = if (identical(limits, "JASP")) NULL else limits,
      expand = expand, oob = oob, na.value = na.value, transform = transform,
      guide = guide, position = position, call = call,
      super = ScaleContinuousPositionJASP
    )
    sc <- set_sec_axis4_0_0(sec.axis, sc)

  } else if (graphOptions("ggVersion") >= "3.3.0") {

    sc <- continuous_scale(c("x", "xmin", "xmax", "xend", "xintercept",
                             "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper",
                             "x0"), "position_c", identity, name = name, breaks = breaks,
                           n.breaks = n.breaks, minor_breaks = minor_breaks, labels = labels,
                           limits = limits, expand = expand, oob = oob, na.value = na.value,
                           trans = trans, guide = guide, position = position, super = ScaleContinuousPosition)
    sc <- set_sec_axis(sec.axis, sc)

    if (identical(limits, "JASP"))
      sc$get_limits <- jaspLimits

  } else {

    sc <- continuous_scale(c("x", "xmin", "xmax", "xend", "xintercept",
                             "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper"),
                           "position_c", identity, name = name, breaks = breaks,
                           minor_breaks = minor_breaks, labels = labels, limits = limits,
                           expand = expand, oob = oob, na.value = na.value, trans = trans,
                           guide = "none", position = position, super = ScaleContinuousPosition)

    if (!is.waive(sec.axis)) {
      if (is.formula(sec.axis))
        sec.axis <- sec_axis(sec.axis)
      if (!is.sec_axis(sec.axis))
        stop2("Secondary axes must be specified using 'sec_axis()'")
      sc$secondary.axis <- sec.axis
    }

    if (identical(limits, "JASP"))
      sc$get_limits <- jaspLimits
  }

  sc
}


#' @rdname scale_x_continuous
#' @export
scale_y_continuous <- function(name = waiver(), breaks = getPrettyAxisBreaks, minor_breaks = waiver(),
                               n.breaks = NULL, labels = axesLabeller, limits = "JASP", expand = waiver(), oob = censor,
                               na.value = NA_real_, trans = "identity", transform = "identity", guide = waiver(), position = "left",
                               sec.axis = waiver()) {

  if (graphOptions("ggVersion") >= "4.0.0") {
    call <- rlang::caller_call()
    sc <- continuous_scale(
      get_ggplot_global()$y_aes,
      palette = identity, name = name, breaks = breaks, n.breaks = n.breaks,
      minor_breaks = minor_breaks, labels = labels, limits = if (identical(limits, "JASP")) NULL else limits,
      expand = expand, oob = oob, na.value = na.value, transform = transform,
      guide = guide, position = position, call = call,
      super = ScaleContinuousPositionJASP
    )
    sc <- set_sec_axis4_0_0(sec.axis, sc)

  } else if (graphOptions("ggVersion") >= "3.3.0") {

    sc <- continuous_scale(c("y", "ymin", "ymax", "yend", "yintercept",
                             "ymin_final", "ymax_final", "lower", "middle", "upper",
                             "y0"), "position_c", identity, name = name, breaks = breaks,
                           n.breaks = n.breaks, minor_breaks = minor_breaks, labels = labels,
                           limits = limits, expand = expand, oob = oob, na.value = na.value,
                           trans = trans, guide = guide, position = position, super = ScaleContinuousPosition)

    sc <- set_sec_axis(sec.axis, sc)

    if (identical(limits, "JASP"))
      sc$get_limits <- jaspLimits

  } else {
    sc <- continuous_scale(c("y", "ymin", "ymax", "yend", "yintercept",
                             "ymin_final", "ymax_final", "lower", "middle", "upper"),
                           "position_c", identity, name = name, breaks = breaks,
                           minor_breaks = minor_breaks, labels = labels, limits = limits,
                           expand = expand, oob = oob, na.value = na.value, trans = trans,
                           guide = "none", position = position, super = ScaleContinuousPosition)

    if (!is.waive(sec.axis)) {
      if (is.formula(sec.axis))
        sec.axis <- sec_axis(sec.axis)
      if (!is.sec_axis(sec.axis))
        stop2("Secondary axes must be specified using 'sec_axis()'")
      sc$secondary.axis <- sec.axis
    }

    if (identical(limits, "JASP"))
      sc$get_limits <- jaspLimits
  }

  sc
}


jaspLimits <- function(..., self = self) {
  # this function is basically identical to what is normally is in sc$get_limits,
  # except for everything inside if (identical(self$limits, "JASP"))

  if (self$is_empty()) {
    return(c(0, 1))
  }
  if (identical(self$limits, "JASP")) {
    # ensures that outer breakpoints are always included in plot
    range(getPrettyAxisBreaks(self$range$range))
  } else if (!is.null(self$limits)) {
    ifelse(!is.na(self$limits), self$limits, self$range$range)
  } else {
    self$range$range
  }
}

set_sec_axis <- function(sec.axis, scale) {
  # copied from ggplot2:::set_sec_axis
  # this function exists to please the R CMD check
  if (!is.waive(sec.axis)) {
    if (is.formula(sec.axis)) {
      sec.axis <- sec_axis(sec.axis)
    }
    if (!is.sec_axis(sec.axis)) {
      stop2("Secondary axes must be specified using 'sec_axis()'")
    }
    scale$secondary.axis <- sec.axis
  }
  return(scale)
}

set_sec_axis4_0_0 <- function(sec.axis, scale) {
  # copied from ggplot2:::set_sec_axis in ggplot2 4.0.0
  # this function exists to please the R CMD check
  is_sec_axis <- function(x) inherits(x, "AxisSecondary")

  if (!ggplot2::is_waiver(sec.axis)) {
    if (scale$is_discrete()) {
      if (!identical(.subset2(sec.axis, "trans"), identity)) {
        cli::cli_abort("Discrete secondary axes must have the {.fn identity} transformation.")
      }
    }
    if (rlang::is_formula(sec.axis)) sec.axis <- ggplot2::sec_axis(sec.axis)
    if (!is_sec_axis(sec.axis)) {
      cli::cli_abort("Secondary axes must be specified using {.fn sec_axis}.")
    }
    scale$secondary.axis <- sec.axis
  }
  return(scale)
}

get_ggplot_global <- function() {
  # this function exists to please the R CMD check
  utils::getFromNamespace("ggplot_global", "ggplot2")
}


# Custom scale prototype for ggplot2 >= 4.0.0
#
# Improvements over the default ScaleContinuousPosition:
# 1. get_limits: by default, the limits span the pretty breaks of the data, so
#    the outer breaks (and the axis line drawn by geom_rangeframe) are never cut off.
#    The same holds for the missing side of one-sided limits, e.g., c(NA, 10).
# 2. get_breaks: ggplot2 computes breaks from the expanded view range, which can
#    yield a coarser step whose outer breaks fall outside the limits. Instead,
#    breaks are derived from the data and the explicit limits, so they coincide
#    with the limits. When the coord sets the view range, e.g.,
#    coord_cartesian(xlim = ...), the standard ggplot2 behavior is used.
ScaleContinuousPositionJASP <- ggplot2::ggproto(
  "ScaleContinuousPositionJASP",
  ggplot2::ScaleContinuousPosition,

  get_limits = function(self) {
    if (self$is_empty()) return(c(0, 1))

    if (!is.null(self$limits)) {
      # Explicit limits, a missing side (e.g., limits = c(NA, 10)) spans the breaks
      ifelse(!is.na(self$limits), self$limits, range(jaspDataBreaks(self)))
    } else {
      # JASP default: span the breaks of the data. range() also sorts the
      # limits for order-reversing transforms (reverse, reciprocal).
      range(jaspDataBreaks(self))
    }
  },

  get_breaks = function(self, limits = self$get_limits()) {
    if (self$is_empty()) return(numeric())

    # Fixed breaks, or a view range set by the coord: standard ggplot2 behavior
    if (!is.function(self$breaks) || !jaspIsDefaultViewRange(self, limits))
      return(ggplot2::ggproto_parent(ggplot2::ScaleContinuousPosition, self)$get_breaks(limits))

    # Breaks of the data and explicit limits rather than of the expanded view range
    jaspDataBreaks(self, self$breaks)
  }
)

# Breaks of the trained data range, where the explicit sides of the limits replace
# those of the data range, on the transformed scale. These are computed on the
# original scale, unless that yields breaks outside the domain of the
# transformation (e.g., 0 for a log transformation). In that case, the breaks of
# the transformation are used instead (e.g., 1, 10, 100 for a log transformation),
# and if these do not cover the range either, breaks computed on the transformed scale.
jaspDataBreaks <- function(self, breaks = getPrettyAxisBreaks) {
  transformation <- self$get_transformation()
  range          <- self$range$range
  if (!is.null(self$limits))
    range <- sort(ifelse(!is.na(self$limits), self$limits, range))
  originalRange  <- transformation$inverse(range)

  # the breaks must be finite and cover the range, otherwise the axis is cut off
  tol     <- 1e-8 * max(1, abs(range))
  isValid <- function(x) length(x) > 0L && all(is.finite(x)) && min(x) <= range[1L] + tol && max(x) >= range[2L] - tol

  result <- suppressWarnings(transformation$transform(breaks(originalRange)))
  if (!isValid(result))
    result <- suppressWarnings(transformation$transform(transformation$breaks(originalRange)))
  if (!isValid(result))
    result <- breaks(range)
  result
}

# TRUE if viewRange consists of the limits of the scale plus at most the expansion
# of the scale, FALSE if the coord sets the view range (e.g., coord_cartesian(xlim = ...)).
jaspIsDefaultViewRange <- function(self, viewRange) {
  limits    <- sort(self$get_limits())
  viewRange <- sort(viewRange)

  expand <- if (ggplot2::is_waiver(self$expand)) ggplot2::expansion(mult = 0.05) else self$expand
  if (length(expand) == 2L)
    expand <- rep(expand, 2L)

  width    <- diff(limits)
  maxRange <- c(limits[1L] - width * expand[1L] - expand[2L],
                limits[2L] + width * expand[3L] + expand[4L])
  tol      <- 1e-8 * max(1, abs(maxRange))

  viewRange[1L] <= limits[1L]   + tol && viewRange[2L] >= limits[2L]   - tol &&
  viewRange[1L] >= maxRange[1L] - tol && viewRange[2L] <= maxRange[2L] + tol
}
