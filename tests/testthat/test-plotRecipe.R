test_that("plot recipes materialize without retaining a ggplot", {
  recipe <- createJaspPlotRecipe(
    "jaspGraphs::plotQQnorm",
    list(residuals = stats::rnorm(20))
  )

  expect_true(isJaspPlotRecipe(recipe))
  expect_false(ggplot2::is_ggplot(recipe))
  expect_true(ggplot2::is_ggplot(materializeJaspPlotRecipe(recipe)))
})

test_that("plot recipes reject environment-bearing arguments", {
  expect_error(
    createJaspPlotRecipe("ggplot2::ggplot", list(data = new.env())),
    "cannot contain environments or functions"
  )
  expect_error(
    createJaspPlotRecipe("ggplot2::ggplot", list(mapping = y ~ x)),
    "cannot contain environments or functions"
  )
})

test_that("package functions are stored as serializable namespace references", {
  recipe <- createJaspPlotRecipe(plotQQnorm, list(residuals = seq(-2, 2, length.out = 20)))
  expect_identical(recipe$fun, "jaspGraphs::plotQQnorm")
  expect_identical(unserialize(serialize(recipe, NULL)), recipe)
  expect_true(ggplot2::is_ggplot(materializeJaspPlotRecipe(recipe)))

  # An internal function in the same module needs no package qualifier.
  internal <- createJaspPlotRecipe(normalizeJaspPlotRecipeEditOptions, list(editOptions = list()))
  expect_identical(internal$fun, "jaspGraphs:::normalizeJaspPlotRecipeEditOptions")
  expect_identical(materializeJaspPlotRecipe(internal), list(resetPlot = FALSE))

  external <- createJaspPlotRecipe(stats::median, list(x = c(1, 3, 5)))
  expect_identical(external$fun, "stats::median")
  expect_identical(materializeJaspPlotRecipe(external), 3)
})

test_that("non-syntactic namespace bindings round-trip", {
  recipe <- createJaspPlotRecipe(base::`%in%`, list(x = 1:3, table = 2:3))
  expect_identical(recipe$fun, "base::%in%")
  expect_identical(materializeJaspPlotRecipe(recipe), c(FALSE, TRUE, TRUE))
})

test_that("plot recipes reject functions without a namespace binding", {
  expect_error(createJaspPlotRecipe(function(x) x), "not defined in a package namespace")
  expect_error(createJaspPlotRecipe(sum), "non-primitive function")
  unbound <- function(x) x
  environment(unbound) <- asNamespace("jaspGraphs")
  expect_error(createJaspPlotRecipe(unbound), "no matching namespace binding")
  for (fun in list(NA_character_, "", character(), c("stats::median", "stats::mean")))
    expect_error(createJaspPlotRecipe(fun), "single non-empty function reference")
})

test_that("plot recipe edit options can be stored and reset", {
  recipe <- createJaspPlotRecipe(
    "jaspGraphs::plotQQnorm",
    list(residuals = stats::rnorm(20))
  )
  options <- plotEditingOptions(recipe)
  options$xAxis$settings$title <- "Edited x"

  edited <- plotEditing(recipe, options)
  expect_false(edited$editOptions$resetPlot)
  expect_identical(edited$editOptions[names(options)], options)
  expect_true(ggplot2::is_ggplot(materializeJaspPlotRecipe(edited)))

  reset <- plotEditing(edited, list(resetPlot = TRUE))
  expect_null(reset$editOptions)
})
