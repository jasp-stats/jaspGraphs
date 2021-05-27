#' @title Compute axis breaks
#' @param x the object to compute axis breaks for
#'
#' @param ... if x is numeric, this is passed to pretty
#' @details this is just a wrapper for pretty.
#'
#' @export
getPrettyAxisBreaks <- function(x, ...) {
  force(x)
  UseMethod("getPrettyAxisBreaks", x)
}

#' @export
getPrettyAxisBreaks.numeric <- function(x, ...) {
  return(base::pretty(x, ...))
}

#' @export
getPrettyAxisBreaks.factor <- function(x, ...) {
  return(unique(x))
}

#' @export
getPrettyAxisBreaks.character <- function(x, ...) {
  return(unique(x))
}

#' @export
getPrettyAxisBreaks.default <- function(x, ...) {
  if (is.numeric(x))
    return(pretty(x, ...))
  else return(unique(x))
}
