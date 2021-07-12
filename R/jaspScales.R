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
                               na.value = NA_real_, trans = "identity", guide = waiver(), position = "bottom",
                               sec.axis = waiver()) {

  if (graphOptions("ggVersion") >= "3.3.0") {

    sc <- continuous_scale(c("x", "xmin", "xmax", "xend", "xintercept",
                             "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper",
                             "x0"), "position_c", identity, name = name, breaks = breaks,
                           n.breaks = n.breaks, minor_breaks = minor_breaks, labels = labels,
                           limits = limits, expand = expand, oob = oob, na.value = na.value,
                           trans = trans, guide = guide, position = position, super = ScaleContinuousPosition)
    set_sec_axis(sec.axis, sc)

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
  }

  if (identical(limits, "JASP"))
    sc$get_limits <- jaspLimits

  sc
}

set_sec_axis <- function(sec.axis, scale) {
  # copied from ggplot2:::set_sec_axis
  # this function exists to please the R CMD check
  if (!is.waive(sec.axis)) {
    if (is.formula(sec.axis))
      sec.axis <- sec_axis(sec.axis)
    if (!is.sec_axis(sec.axis))
      stop2("Secondary axes must be specified using 'sec_axis()'")
    scale$secondary.axis <- sec.axis
  }
  return(scale)
}


#' @rdname scale_x_continuous
#' @export
scale_y_continuous <- function(name = waiver(), breaks = getPrettyAxisBreaks, minor_breaks = waiver(),
                               n.breaks = NULL, labels = axesLabeller, limits = "JASP", expand = waiver(), oob = censor,
                               na.value = NA_real_, trans = "identity", guide = waiver(), position = "left",
                               sec.axis = waiver()) {

  if (graphOptions("ggVersion") >= "3.3.0") {

    sc <- continuous_scale(c("y", "ymin", "ymax", "yend", "yintercept",
                             "ymin_final", "ymax_final", "lower", "middle", "upper",
                             "y0"), "position_c", identity, name = name, breaks = breaks,
                           n.breaks = n.breaks, minor_breaks = minor_breaks, labels = labels,
                           limits = limits, expand = expand, oob = oob, na.value = na.value,
                           trans = trans, guide = guide, position = position, super = ScaleContinuousPosition)

    set_sec_axis(sec.axis, sc)

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
  }

  if (identical(limits, "JASP"))
    sc$get_limits <- jaspLimits

  sc
}

jaspLimits <- function(..., self = self) {
  # this function is basically identical to what is normally is in sc$get_limits,
  # except for everything inside if (identical(self$limits, "JASP"))

  if (self$is_empty())
    return(c(0, 1))
  if (identical(self$limits, "JASP")) {
    # ensures that outer breakpoints are always included in plot
    range(getPrettyAxisBreaks(self$range$range))
  } else if (!is.null(self$limits)) {
    ifelse(!is.na(self$limits), self$limits, self$range$range)
  } else {
    self$range$range
  }

}

# The approach below is preferred but errors using ggplot2 3.2.1
#'
#' scale_x_continuous <- function(name = waiver(), breaks = getPrettyAxisBreaks, minor_breaks = waiver(),
#'                                labels = axesLabeller, limits = jaspLimits, expand = waiver(), oob = censor,
#'                                na.value = NA_real_, trans = "identity", position = "bottom",
#'                                sec.axis = waiver()) {
#'   return(ggplot2::scale_x_continuous(
#'     name         = name,
#'     breaks       = breaks,
#'     minor_breaks = minor_breaks,
#'     labels       = labels,
#'     limits       = limits,
#'     expand       = expand,
#'     oob          = oob,
#'     sec.axis     = sec.axis
#'   ))
#' }
#'
#'
#' scale_y_continuous <- function(name = waiver(), breaks = getPrettyAxisBreaks, minor_breaks = waiver(),
#'                                labels = axesLabeller, limits = jaspLimits, expand = waiver(), oob = censor,
#'                                na.value = NA_real_, trans = "identity", position = "left",
#'                                sec.axis = waiver()) {
#'     return(ggplot2::scale_y_continuous(
#'     name         = name,
#'     breaks       = breaks,
#'     minor_breaks = minor_breaks,
#'     labels       = labels,
#'     limits       = limits,
#'     expand       = expand,
#'     oob          = oob,
#'     sec.axis     = sec.axis
#'   ))
#' }
#'
#'
#' jaspLimits <- function(...) {
#'
#'   dots <- c(...)
#'   if (length(dots) != 2L)
#'     return(c(0, 1))
#'   else
#'     return(range(getPrettyAxisBreaks(dots)))
#'
#' }
