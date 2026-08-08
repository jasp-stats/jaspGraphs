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
