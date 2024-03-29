#' @rdname graphOptions
#' @export
.graphOptions <- list2env(list(
  legend.position = "auto",
  fontsize        = 17L,
  legend.cex      = 1.25,
  axis.title.cex  = 1.2,
  family          = NULL,
  legend.coordinates = list(left = .15,
                            mid = .5,
                            right = .8,
                            top = .75,
                            mid = .5,
                            bottom = .25),
  bty = list(type = "n", lwdX = .3, lwdY = .3),
  axisTickLength = grid::unit(x = .3, units = "cm"),
  axisTickWidth = .3,
  digits = list(axes = 3L, BF = 3L),
  ggVersion = utils::packageVersion("ggplot2"),
  palette = "colorblind",
  debug = TRUE
))

#' @rdname graphOptions
#' @export
getGraphOption <- function(name) return(get(name, envir = .graphOptions))

#' @rdname graphOptions
#' @export
setGraphOption <- function(name, value) {
  assign(name, value, envir = .graphOptions)
  return(invisible(value))
}

#' @rdname graphOptions
#' @title Global graph options
#' @param ... modify options using name = value.
#' @param name character string of the value to get or set.
#' @param value the value to change x into.
#'
#' @export
graphOptions <- function(...) {

  args <- list(...)
  if (length(args) == 1L && is.list(args[[1L]]))
    args <- args[[1L]]

  if (!(length(args) && is.null(names(args)))) {
    if (length(args)) {
      for (i in seq_along(args)) {
        setGraphOption(names(args)[[i]], args[[i]])
      }

      return(invisible(args))

    } else {

      return(as.list(.graphOptions))

    }
  }

  args <- unlist(args)
  out <- as.list(.graphOptions)[args]

  if (length(out) == 1)
    out <- out[[1]]

  return(out)
}

.onAttach <- function(libname, pkgname)
  assign(".graphOptions", .graphOptions, envir = as.environment("package:jaspGraphs"))


