
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

#' @title Custom geoms for JASP
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_line
#' @details These functions are virtually identical to \code{\link[ggplot2]{geom_point}} and \code{\link[ggplot2]{geom_line}}
#' except that their underlying Geoms have different default values.
#'
#' @rdname geom_point
#' @export
geom_point <- ggplot2::make_constructor(GeomJaspPoint)


#' @rdname geom_point
#' @export
geom_line <- ggplot2::make_constructor(GeomJaspLine)
