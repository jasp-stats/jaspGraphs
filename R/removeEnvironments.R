#' @title removeEnvironments - remove environments from ggplot2 objects
#' @param x a plot object. Currently, only ggplot2 objects and jaspGraphsPlot are supported, other objects are returned unchanged
#' @param debugInfo Should debugInfo be printed? Possible values are 0, no info is shown; 1, some info is shown; or 2, a lot of info is shown.
#' @param ... internal.
#'
#' @details This function may breaks ggplot2 objects that rely on looking up values in environments.
#' see also https://github.com/tidyverse/ggplot2/issues/3994 and https://github.com/tidyverse/ggplot2/issues/4056
#'
#' @example inst/examples/ex-removeEnvironments.R
#' @returns an object of the same class as the input
#' @export
removeEnvironments <- function(x, debugInfo = 0,...) {
  UseMethod("removeEnvironments", x)
}

#' @export
removeEnvironments.ggplot <- function(x, debugInfo = 0, ...) {
  x$plot_env <- rlang::new_environment()
  return(removeEnvironmentsInner(x, debugInfo = debugInfo, ...))
}

#' @export
removeEnvironments.jaspGraphsPlot <- function(x, debugInfo = 0, ...) {

  for (i in seq_along(x)) {
    if (debugInfo >= 1) cat("subplot", i, "\n")
    x[[i]] <- removeEnvironmentsInner(x, debugInfo = debugInfo, ...)
  }
  return(x)
}

#' @export
removeEnvironments.default <- function(x, debugInfo = 0, ...) {
  if (debugInfo >= 1) cat("dispatched to removeEnvironments.default\n")
  return(x)
}

removeEnvironmentsInner <- function(
    x,
    exitEarlyClasses = c("unit", "element", "data.frame", "FacetNull"),
    replacementEnv = rlang::new_environment(parent = baseenv()),
    environmentsAlreadyVisited = rlang::new_environment(),
    debugInfo = 1,
    level = deparse1(substitute(x))) {

  if (inherits(x, exitEarlyClasses) || isNamespace(x))
    return(x)

  if (debugInfo >= 2) {
    cat("level:", level, "\n")
    cat("class:", paste(class(x), collapse = ", "), "\n")
  }

  if (inherits(x, "uneval")) { # returned by ggplot2::aes

    if (debugInfo >= 1) {
      cat("level:", level, "\n")
      cat("class:", paste(class(x), collapse = ", "), "\n")
      cat("replacing environment of subelements of x\n")
    }

    for (i in seq_along(x)) {
      environment(x[[i]]) <- replacementEnv
    }

  } else if (inherits(x, "gtable")) {

    # gtable has special indexing with [[ and integers so we use names
    for (i in setdiff(names(x), c("widths", "heights"))) {
      if (!is.null(x[[i]])) {
        level <- paste0(level, "$", i)
        x[[i]] <- Recall(x[[i]], exitEarlyClasses, replacementEnv, environmentsAlreadyVisited, debugInfo, level)
      }
    }

  } else if (is.list(x)) {

    if (!is.null(environment(x)) && !isNamespace(environment(x))) {
      if (debugInfo >= 1) {
        cat("level:", level, "\n")
        cat("class:", paste(class(x), collapse = ", "), "\n")
        cat("replacing environment(x)\n")
      }
      environment(x) <- replacementEnv
    }

    nms <- names(x)
    for (i in seq_along(x)) {
      if (!is.null(x[[i]])) {

        if (is.null(nms) || nms[i] == "") {
          newlevel <- paste0(level, "[[", i, "]]")
        } else {
          newlevel <- paste0(level, "$", nms[i])
        }
        x[[i]] <- Recall(x[[i]], exitEarlyClasses, replacementEnv, environmentsAlreadyVisited, debugInfo, newlevel)
      }
    }

  } else if (ggplot2::is.ggproto(x)) {

    if (inherits(x, "LayerInstance") && inherits(x$geom, "GeomCustomAnn")) {

      # cowplot saves unnecessary stuff
      super <- .subset2(x, "super")
      superEnv <- environment(super)
      superEnv$env$geom_params <- NULL
      superEnv$env$params <- NULL
      superEnv$env$geom <- NULL

    }

    hash <- rlang::hash(x)
    if (!exists(hash, envir = environmentsAlreadyVisited)) {

      environmentsAlreadyVisited[[hash]] <- NULL
      nms <- union(names(x), names(as.list(x)))
      for (i in nms) {
        if (!is.null(.subset2(x, i)) && !isNamespace(environment(.subset2(x, i)))) {
          newlevel <- paste0(level, "$", i)
          x[[i]] <- Recall(.subset2(x, i), exitEarlyClasses, replacementEnv, environmentsAlreadyVisited, debugInfo, newlevel)
        }
      }

    }

  } else if (is.environment(x) && !isNamespace(x) && !identical(x, .GlobalEnv)) {

    hash <- rlang::hash(x)
    if (!exists(hash, envir = environmentsAlreadyVisited)) {

      environmentsAlreadyVisited[[hash]] <- NULL
      for (i in names(x)) {
        newlevel <- paste0(level, "$", i)
        x[[i]] <- Recall(x[[i]], exitEarlyClasses, replacementEnv, environmentsAlreadyVisited, debugInfo, newlevel)
      }
    }

  } else if (is.function(x)) {

    hash <- rlang::hash(x)
    if (!exists(hash, envir = environmentsAlreadyVisited)) {

      environmentsAlreadyVisited[[hash]] <- NULL
      newlevel <- paste0("environment(", level, ")")
      environment(x) <- Recall(environment(x), exitEarlyClasses, replacementEnv, environmentsAlreadyVisited, debugInfo, newlevel)

    }

  } else if (!is.null(x) && !is.function(x) && !is.null(environment(x)) && !isNamespace(environment(x))) {
    if (debugInfo >= 1) {
      cat("level:", level, "\n")
      cat("class:", paste(class(x), collapse = ", "), "\n")
      cat("replacing environment(x)\n")
    }
    environment(x) <- replacementEnv
  } else if (mode(x) == "...") {
    # DOTSXP objects...
    if (debugInfo >= 1) {
      cat("level:", level, "\n")
      cat("class:", paste(class(x), collapse = ", "), "\n")
      cat("replacing DOTSXP with NULL\n")
    }
    return(NULL)
  }

  return(x)

}
