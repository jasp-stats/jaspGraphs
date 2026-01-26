
setDefaults <- function(lst, ...) {

  defaults <- list(...)
  nms2change <- setdiff(names(defaults), names(lst))
  lst[nms2change] <- defaults[nms2change]
  return(lst)

}

# NOTE: the name must be Geom<J>name<P>name because of ggplot2::make_constructor
GeomJaspPoint <- ggplot2::ggproto(
  "GeomJaspPoint",
  ggplot2::GeomPoint,
  default_aes = utils::modifyList(
    ggplot2::GeomPoint$default_aes,
    list(size = 3, colour = "black", fill = "grey", shape = 21)
  )
)

GeomJaspLine <- ggplot2::ggproto(
  "GeomJaspLine",
  ggplot2::GeomLine,
  default_aes = utils::modifyList(
    ggplot2::GeomLine$default_aes,
    list(linewidth = 1, colour = "black", linetype = 1, alpha = NA)
  )
)

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
geom_point <- ggplot2::make_constructor(GeomJaspPoint)


#' @rdname geom_point
#' @export
geom_line <- ggplot2::make_constructor(GeomJaspLine)
