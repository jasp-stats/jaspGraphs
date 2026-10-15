#' @title Create a serializable plot recipe
#' @param fun A function defined and bound in a package namespace, or a string
#'   such as \code{"pkg:::functionName"} or \code{"pkg::functionName"}.
#'   Functions are stored as namespace references so their closure environments
#'   are not serialized. Same-package functions can be passed directly without
#'   a package qualifier. Local functions and primitive functions are not supported.
#' @param args Named list of arguments passed to \code{fun}.
#' @param editOptions Optional plot editing options to apply after materializing.
#' @return An object of class \code{jaspPlotRecipe} containing the function
#'   reference, arguments, and optional editing options.
#' @seealso \code{\link{materializeJaspPlotRecipe}}, \code{\link{isJaspPlotRecipe}},
#'   \code{\link{setJaspPlotRecipeEditOptions}}
#' @export
createJaspPlotRecipe <- function(fun, args = list(), editOptions = NULL) {

  if (is.function(fun)) {
    fun <- .functionToString(fun)
  } else if (!is.character(fun) || length(fun) != 1L || is.na(fun) || !nzchar(fun))
    stop("`fun` must be a single non-empty function reference string or a function from a package.", domain = NA)

  if (!is.list(args))
    stop("`args` must be a list.", domain = NA)
  assertJaspPlotRecipeValue(args)
  editOptions <- normalizeJaspPlotRecipeEditOptions(editOptions)
  assertJaspPlotRecipeValue(editOptions)

  structure(
    list(
      fun         = fun,
      args        = args,
      editOptions = editOptions
    ),
    class = "jaspPlotRecipe"
  )
}

.functionToString <- function(fun) {

  if (!is.function(fun) || is.primitive(fun)) {
    stop("`fun` must be a non-primitive function from a package namespace.", domain = NA)
  }

  ns <- environment(fun)
  if (is.null(ns) || !isNamespace(ns)) {
    stop("`fun` was a function, but is not defined in a package namespace.", domain = NA)
  }

  pkgName <- utils::packageName(ns)
  if (!nzchar(pkgName)) {
    stop("Could not determine the package namespace for `fun`.", domain = NA)
  }

  sameFunctionBinding <- function(name) {
    obj <- tryCatch(
      get(name, envir = ns, inherits = FALSE),
      error = function(e) NULL
    )

    is.function(obj) &&
      typeof(obj) == "closure" &&
      identical(obj, fun, ignore.environment = FALSE)
  }

  exports <- getNamespaceExports(pkgName)

  exportedMatches <- exports[vapply(exports, sameFunctionBinding, logical(1L))]
  if (length(exportedMatches) > 0L) {
    return(paste0(
      pkgName,
      "::",
      exportedMatches[[1L]]
    ))
  }

  allNames <- ls(ns, all.names = TRUE)
  internalNames <- setdiff(allNames, exports)

  internalMatches <- internalNames[vapply(internalNames, sameFunctionBinding, logical(1L))]
  if (length(internalMatches) > 0L) {
    return(paste0(
      pkgName,
      ":::",
      internalMatches[[1L]]
    ))
  }

  stop(
    "`fun` is defined in a package namespace, but no matching namespace binding was found.",
    domain = NA
  )
}

#' Test whether an object is a plot recipe
#'
#' @param x An object to test.
#' @return \code{TRUE} if \code{x} inherits from \code{jaspPlotRecipe}, otherwise
#'   \code{FALSE}.
#' @seealso \code{\link{createJaspPlotRecipe}}
#' @export
isJaspPlotRecipe <- function(x) {
  inherits(x, "jaspPlotRecipe")
}

assertJaspPlotRecipeValue <- function(x) {
  if (is.environment(x) || is.function(x))
    stop("Plot recipe arguments cannot contain environments or functions.", domain = NA)

  if (is.list(x))
    lapply(x, assertJaspPlotRecipeValue)

  attrs <- attributes(x)
  if (length(attrs) > 0L)
    lapply(attrs, assertJaspPlotRecipeValue)

  invisible(NULL)
}

resolveJaspPlotRecipeFunction <- function(fun) {
  if (is.function(fun))
    return(fun)

  if (!is.character(fun) || length(fun) != 1L || is.na(fun) || !nzchar(fun))
    stop("Invalid plot recipe function reference.", domain = NA)

  parts <- strsplit(fun, ":::", fixed = TRUE)[[1L]]
  if (length(parts) == 2L)
    return(utils::getFromNamespace(parts[[2L]], parts[[1L]]))

  parts <- strsplit(fun, "::", fixed = TRUE)[[1L]]
  if (length(parts) == 2L) {
    if (!requireNamespace(parts[[1L]], quietly = TRUE))
      stop(sprintf("Could not load namespace '%s' for plot recipe.", parts[[1L]]), domain = NA)
    return(getExportedValue(parts[[1L]], parts[[2L]]))
  }

  obj <- get(fun, mode = "function", inherits = TRUE)
  if (!is.function(obj))
    stop(sprintf("Plot recipe reference '%s' did not resolve to a function.", fun), domain = NA)

  obj
}

#' Materialize a plot recipe
#'
#' Resolve the stored function reference and call it with the recipe's arguments.
#' Optionally apply the stored plot editing options to the resulting plot.
#'
#' @param recipe A \code{jaspPlotRecipe} object, or an object to return unchanged.
#' @param applyEdits Logical, whether to apply the recipe's stored editing options.
#' @return The object returned by the stored function, with edits applied when
#'   requested. Objects that are not plot recipes are returned unchanged.
#' @seealso \code{\link{createJaspPlotRecipe}}, \code{\link{plotEditing}}
#' @export
materializeJaspPlotRecipe <- function(recipe, applyEdits = TRUE) {
  if (!isJaspPlotRecipe(recipe))
    return(recipe)

  assertJaspPlotRecipeValue(recipe[["args"]])
  fun  <- resolveJaspPlotRecipeFunction(recipe[["fun"]])
  plot <- do.call(fun, recipe[["args"]])

  if (applyEdits && !is.null(recipe[["editOptions"]]))
    plot <- plotEditing(plot, recipe[["editOptions"]])

  plot
}

normalizeJaspPlotRecipeEditOptions <- function(editOptions) {
  if (is.null(editOptions))
    return(NULL)

  if (!is.list(editOptions))
    stop("`editOptions` must be a list or NULL.", domain = NA)

  if (isTRUE(editOptions[["resetPlot"]]))
    return(NULL)

  editOptions[["resetPlot"]] <- FALSE
  editOptions
}

#' Set plot recipe editing options
#'
#' Store editing options in a recipe for use when the plot is materialized.
#' This function does not materialize the plot or validate the options against it.
#'
#' @param recipe A \code{jaspPlotRecipe} object.
#' @param editOptions A list of plot editing options, or \code{NULL} to clear them.
#'   A list containing \code{resetPlot = TRUE} also clears the stored options.
#' @return The recipe with its stored editing options updated.
#' @seealso \code{\link{createJaspPlotRecipe}}, \code{\link{materializeJaspPlotRecipe}},
#'   \code{\link{plotEditing}}
#' @export
setJaspPlotRecipeEditOptions <- function(recipe, editOptions) {
  if (!isJaspPlotRecipe(recipe))
    stop("`recipe` is not a jaspPlotRecipe.", domain = NA)

  editOptions <- normalizeJaspPlotRecipeEditOptions(editOptions)
  assertJaspPlotRecipeValue(editOptions)
  recipe[["editOptions"]] <- editOptions
  recipe
}
