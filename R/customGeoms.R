
setDefaults <- function(lst, ...) {

  defaults <- list(...)
  nms2change <- setdiff(names(defaults), names(lst))
  lst[nms2change] <- defaults[nms2change]
  return(lst)

}

#' @title Custom geoms
#' @param mapping see details
#' @param data see details
#' @param stat see details
#' @param position see details
#' @param size JASP default
#' @param shape JASP default
#' @param colour JASP default
#' @param fill JASP default
#' @param alpha JASP default
#' @param stroke JASP default
#' @param linewidth JASP default
#' @param linetype JASP default
#' @param ... see details
#' @param na.rm see details
#' @param show.legend see details
#' @param inherit.aes see details
#' @details These functions are virtually identical to \code{\link[ggplot2]{geom_point}} and \code{\link[ggplot2]{geom_line}}
#' except that their underlying Geoms have different default values.
#'
#' @rdname geom_point
#' @export
geom_point <- function(
    # default values
    mapping = NULL, data = NULL, stat = "identity", position = "identity",
    # custom jasp defaults
    size = 3, shape = 21, colour = "black",
    fill = "grey", alpha = NA, stroke = 0.5,
    # other defaults
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  ggplot2::geom_point(
    mapping = mapping, data = data, stat = stat, position = position,
    size = size, shape = shape, colour = colour,
    fill = fill, alpha = alpha, stroke = stroke,
    ...,
    na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes
  )
}

#' @rdname geom_point
#' @export
geom_line <- function(
        # default values
    mapping = NULL, data = NULL, stat = "identity", position = "identity",
    # custom jasp defaults
    linewidth = 1.00, colour = "black", linetype = 1, alpha = NA,
    # other defaults
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  # ggplot2 3.4.0 renamed `size` to `linewidth`. Check if `size` was specified.
  # If so, rename it to `linewidth` and show a deprecation warning.

  if ("size" %in% ...names()) {
    lifecycle::deprecate_warn(
      "0.6.0",
      "jaspGraphs::geom_line(size)",
      "jaspGraphs::geom_line(linewidth)",
      details = "In ggplot2 version 3.4.0 the argument `size` was renamed to `linewidth`, likewise in jaspGraphs. For now, jaspGraphs automatically assigned `linewidth = size`. Please fix this in your code, as this will become an error in a future version of jaspGraphs."
    )

    # specifying both `size` and `linewidth` is an error
    if ("linewidth" %in% ...names())
      stop("`jaspGraphs::geom_line`: Cannot specify both size and linewidth!", domain = NA)

  }

  ggplot2::geom_line(
    mapping = mapping, data = data, stat = stat, position = position,
    linewidth = linewidth, colour = colour, linetype = linetype,
    alpha = alpha,
    ...,
    na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes
  )

}
