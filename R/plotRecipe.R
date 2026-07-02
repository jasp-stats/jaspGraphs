#' @title Create a serializable plot recipe
#' @param fun Function reference. Prefer a string such as
#'   \code{"pkg:::functionName"} or \code{"pkg::functionName"} so the closure
#'   environment is not serialized with the recipe.
#' @param args Named list of arguments passed to \code{fun}.
#' @param editOptions Optional plot editing options to apply after materializing.
#' @export
createJaspPlotRecipe <- function(fun, args = list(), editOptions = NULL) {
  if (!is.character(fun) || length(fun) != 1L || !nzchar(fun))
    stop("`fun` must be a single non-empty function reference string.", domain = NA)

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

  if (!is.character(fun) || length(fun) != 1L || !nzchar(fun))
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

#' @export
setJaspPlotRecipeEditOptions <- function(recipe, editOptions) {
  if (!isJaspPlotRecipe(recipe))
    stop("`recipe` is not a jaspPlotRecipe.", domain = NA)

  editOptions <- normalizeJaspPlotRecipeEditOptions(editOptions)
  assertJaspPlotRecipeValue(editOptions)
  recipe[["editOptions"]] <- editOptions
  recipe
}
