# panel parameters of a (sub)plot, the ranges are those of the visible panel
getPanelParams <- function(p) ggplot2::ggplot_build(p)$layout$panel_params[[1L]]

# position of the panel in row `row` and column `col` of a jaspMatrixPlot with k variables,
# the first row and column contain the titles
matrixPanelIndex <- function(row, col, k) row * (k + 1) + col + 1

set.seed(2026)
n     <- 100
group <- factor(sample(c("Control", "Treatment"), n, replace = TRUE))
x     <- rnorm(n, mean = c(Control = 0, Treatment = 1.5)[group])
y     <- 3 * x + rnorm(n, sd = 3)
z     <- rexp(n, rate = 0.7)

test_that("jaspBivariateWithMargins: marginals share the axes of the bivariate panel", {
  skip_if(utils::packageVersion("ggplot2") < "4.0.0")

  p <- jaspBivariateWithMargins(x, y, xName = "x", yName = "y")
  expect_s3_class(p, "patchwork")

  top    <- getPanelParams(p[[1L]])
  main   <- getPanelParams(p[[3L]])
  right  <- getPanelParams(p[[4L]]) # coord_flip, the vertical axis shows y

  expect_equal(top$x.range, main$x.range)
  expect_equal(right$y.range, main$y.range)
})

test_that("jaspBivariateWithMargins: incomplete cases and a character group", {
  skip_if(utils::packageVersion("ggplot2") < "4.0.0")

  xNA <- x
  yNA <- y
  xNA[c(3, 10)] <- NA
  yNA[20]       <- NA

  p <- jaspBivariateWithMargins(xNA, yNA, as.character(group), xName = "x", yName = "y", groupName = "group")
  expect_s3_class(p, "patchwork")
  expect_equal(nrow(ggplot2::layer_data(p[[3L]], 1L)), n - 3L)
  expect_equal(getPanelParams(p[[1L]])$x.range, getPanelParams(p[[3L]])$x.range)
  expect_equal(getPanelParams(p[[4L]])$y.range, getPanelParams(p[[3L]])$y.range)

  expect_error(jaspBivariateWithMargins(x, y, topRightPlotFunction = "not a function"), "must be a function")
  expect_error(jaspBivariateWithMargins(x, y, topRightPlotFunction = function(...) NULL, topRightPlotArgs = 1), "must be a list")
})

# TRUE if the limits of the scales of the panel cover all of its layers
coversAllLayers <- function(p) {
  b <- ggplot2::ggplot_build(p)
  covers <- function(scale) {
    limits <- scale$get_limits()
    range  <- scale$range$range
    range[1L] >= limits[1L] && range[2L] <= limits[2L]
  }
  covers(b$layout$panel_scales_x[[1L]]) && covers(b$layout$panel_scales_y[[1L]])
}

test_that("jaspBivariate: the axes extend to cover smooth and prediction layers", {
  skip_if(utils::packageVersion("ggplot2") < "4.0.0")

  dataAxis <- jaspGraphs:::jaspSharedAxis(y, breaks = NULL)

  for (predict in c("lm", "ellipse")) {
    p <- jaspBivariate(x, y, predict = predict, predictLevel = 0.999, smooth = "lm", smoothCi = TRUE)
    expect_true(coversAllLayers(p))
    expect_true(diff(range(getPanelParams(p)$y$get_breaks(), na.rm = TRUE)) > diff(dataAxis$limits))

    # a supplied axis is used as is, the prediction band beyond it is kept rather than replaced by NA
    p <- jaspBivariate(x, y, predict = predict, predictLevel = 0.999, yAxis = dataAxis)
    expect_equal(range(getPanelParams(p)$y$get_breaks(), na.rm = TRUE), dataAxis$limits)
    predictData <- ggplot2::layer_data(p, 2L)
    predictY    <- unlist(predictData[intersect(c("y", "ymin", "ymax"), names(predictData))])
    expect_false(anyNA(predictY))
    expect_true(min(predictY) < dataAxis$limits[1L] || max(predictY) > dataAxis$limits[2L])
  }
})

test_that("jaspBivariateWithMargins: the marginals follow axes that are extended by a prediction interval", {
  skip_if(utils::packageVersion("ggplot2") < "4.0.0")

  p <- jaspBivariateWithMargins(x, y, predict = "ellipse", predictLevel = 0.999)
  expect_true(coversAllLayers(p[[3L]]))
  expect_equal(getPanelParams(p[[1L]])$x.range, getPanelParams(p[[3L]])$x.range)
  expect_equal(getPanelParams(p[[4L]])$y.range, getPanelParams(p[[3L]])$y.range)

  # without the ellipse, the axes are those of the data
  pData <- jaspBivariateWithMargins(x, y)
  expect_true(diff(getPanelParams(p[[3L]])$x.range) > diff(getPanelParams(pData[[3L]])$x.range))
})

test_that("jaspBivariate: default line colors", {
  expect_equal(unique(ggplot2::layer_data(jaspBivariate(x, y, smooth = "lm"), 1L)$colour), "black")
  expect_equal(unique(ggplot2::layer_data(jaspBivariate(x, y, type = "contour"), 1L)$colour), "black")
  expect_equal(unique(ggplot2::layer_data(jaspBivariate(x, y, type = "contour", args = list(color = "red")), 1L)$colour), "red")

  # with a grouping variable, the smooth lines are colored by group
  grouped <- jaspBivariate(x, y, group, smooth = "lm")
  expect_length(unique(ggplot2::layer_data(grouped, 1L)$colour), nlevels(group))
})

test_that("jaspMarginal: shared axis", {
  skip_if(utils::packageVersion("ggplot2") < "4.0.0")

  axis <- jaspGraphs:::jaspSharedAxis(x, breaks = "doane")
  expect_equal(getPanelParams(jaspMarginal(x, xAxis = axis))$x$get_breaks(), axis$breaks)
  expect_equal(getPanelParams(jaspMarginal(x, xAxis = NULL, breaks = "doane"))$x$get_breaks(), axis$breaks)

  # without bin breaks, the bins are computed from `breaks`
  axisNoBins <- axis
  axisNoBins$binBreaks <- NULL
  p <- jaspMarginal(x, xAxis = axisNoBins, breaks = 3)
  expect_lte(nrow(ggplot2::layer_data(p, 1L)), 4L)
})

test_that("jaspMatrixPlot: panels share the axes of their row and column", {
  skip_if(utils::packageVersion("ggplot2") < "4.0.0")

  data <- data.frame(x = x, y = y, z = z, label = "a")
  k    <- 3L
  # the ellipses extend beyond the data, so they extend the axes of their row and column
  p    <- jaspMatrixPlot(data, bottomLeftPlotFunction = jaspBivariate, bottomLeftPlotArgs = list(predict = "ellipse", predictLevel = 0.999))
  expect_s3_class(p, "patchwork")
  expect_length(p, (k + 1L)^2)

  for (row in 2:k)
    for (col in seq_len(row - 1L))
      expect_true(coversAllLayers(p[[matrixPanelIndex(row, col, k)]]))

  params <- lapply(seq_len(k), function(row) lapply(seq_len(k), function(col) getPanelParams(p[[matrixPanelIndex(row, col, k)]])))

  for (col in seq_len(k))
    for (row in seq_len(k))
      expect_equal(params[[row]][[col]]$x.range, params[[1L]][[col]]$x.range)

  for (row in seq_len(k)) {
    offDiagonal <- setdiff(seq_len(k), row)
    for (col in offDiagonal)
      expect_equal(params[[row]][[col]]$y.range, params[[row]][[offDiagonal[1L]]]$y.range)
  }
})

test_that("jaspMatrixPlot: input validation and failing panels", {
  expect_error(jaspMatrixPlot(data.frame()), "non-empty data frame")
  expect_error(jaspMatrixPlot(data.frame(x = x)), "more than 1 column")
  expect_error(jaspMatrixPlot(data.frame(x = x, label = "a")), "more than 1 numeric column")
  expect_error(jaspMatrixPlot(data.frame(x = x, y = y), axesLabels = "x"), "same length")

  # the axis labels are dropped together with the non-numeric columns
  p <- jaspMatrixPlot(data.frame(label = "a", x = x, y = y), axesLabels = c("Label", "X", "Y"))
  expect_equal(ggplot2::layer_data(p[[2L]], 1L)$label, "X")

  # a variable without finite values gives error panels instead of an error
  p <- jaspMatrixPlot(data.frame(x = x, empty = NA_real_))
  expect_s3_class(p, "patchwork")
  expect_match(ggplot2::layer_data(p[[matrixPanelIndex(2L, 2L, 2L)]], 1L)$label, "Plotting not possible")
})

test_that("multipanel plots match snapshots", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if(utils::packageVersion("ggplot2") < "4.0.0")

  vdiffr::expect_doppelganger(
    "jaspBivariateWithMargins-default",
    jaspBivariateWithMargins(x, y, xName = "x", yName = "y", smooth = "lm", smoothCi = TRUE)
  )
  vdiffr::expect_doppelganger(
    "jaspBivariateWithMargins-group",
    jaspBivariateWithMargins(
      x, y, group, xName = "x", yName = "y", groupName = "group",
      xMarginalArgs = .marginalArgs(density = TRUE, histogram = FALSE),
      yMarginalArgs = .marginalArgs(density = TRUE, histogram = FALSE),
      predict = "ellipse"
    )
  )
  vdiffr::expect_doppelganger(
    "jaspMatrixPlot-3x3",
    jaspMatrixPlot(
      data.frame(x = x, y = y, z = z),
      diagonalPlotArgs       = list(densityOverlay = TRUE),
      bottomLeftPlotFunction = jaspBivariate,
      bottomLeftPlotArgs     = list(type = "contour", predict = "ellipse")
    )
  )
})
