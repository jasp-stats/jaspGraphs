test_that("jaspHistogram: default, grouped, manual bins, and density-shade variants", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  set.seed(1234)
  x <- rnorm(1e3) + rnorm(1e3, 1, .1) - rgamma(1e3, 10, 5)

  # Case A: default options (histogram only)
  p_default <- jaspHistogram(x, "random data", histogram = TRUE, density = FALSE, rugs = FALSE)
  testthat::expect_true(ggplot2::is_ggplot(p_default))
  vdiffr::expect_doppelganger("jaspHistogram-default", p_default)

  # Case B: grouped variant with density and rugs
  groupingVariable <- gl(3, 333, length = 1e3)
  p_grouped <- jaspHistogram(x, "random data", groupingVariable, "group name", histogram = TRUE, density = TRUE, rugs = TRUE, rugsColor = TRUE)
  testthat::expect_true(ggplot2::is_ggplot(p_grouped))
  vdiffr::expect_doppelganger("jaspHistogram-grouped-density-rugs", p_grouped)

  # Case C: manual bin width
  p_manual <- jaspHistogram(x, "random data", binWidthType = "manual", numberOfBins = 80)
  testthat::expect_true(ggplot2::is_ggplot(p_manual))
  vdiffr::expect_doppelganger("jaspHistogram-manual-bins", p_manual)

  # Case D: density-only with shading (using mtcars example)
  p_density <- jaspHistogram(mtcars$drat, "drat", factor(mtcars$cyl), "cyl", histogram = FALSE, hideYAxisLabels = FALSE, density = TRUE, densityColor = TRUE, densityShade = TRUE, densityShadeAlpha = 0.45)
  testthat::expect_true(ggplot2::is_ggplot(p_density))
  vdiffr::expect_doppelganger("jaspHistogram-density-shade", p_density)

})
