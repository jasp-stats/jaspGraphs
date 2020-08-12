#' @method print jaspGraphs
#' @export
print.jaspGraphs <- function(x, ...) {

  if (ggplot2::is.ggplot(x)) {
    # do not call ggplot2:::print.ggplot() to please R CMD check
    NextMethod()
  } else if (inherits(x, c("gtable", "gTree", "grob", "gDesc"))) {
    gridExtra::grid.arrange(x, ...)
  } else if (length(class(x)) > 1L) {
    NextMethod()
  } else {
    stop(sprintf(
      "unsupported plot object of class: %s",
      paste0(class(x), sep = ", ")
    ))
  }
  return(invisible(TRUE))
}

#' @method plot jaspGraphs
#' @export
plot.jaspGraphs <- function(x, ...) print.jaspGraphs(x, ...)

#' @method print jaspGraphsPlot
#' @export
print.jaspGraphsPlot <- function(x, ...) {

  x$plot(...)
  return(invisible(TRUE))
}

#' @method plot jaspGraphsPlot
#' @export
plot.jaspGraphsPlot <- function(x, ...) print.jaspGraphsPlot(x, ...)

