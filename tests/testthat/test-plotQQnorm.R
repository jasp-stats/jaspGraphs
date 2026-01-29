test_that("plotQQnorm: default, error bars, abline-origin/identical axes variants", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  set.seed(2026)
  x <- rnorm(100)

  # Case A: default QQ plot
  p1 <- plotQQnorm(x)
  testthat::expect_true(ggplot2::is.ggplot(p1))
  vdiffr::expect_doppelganger("plotQQnorm-default", p1)

  # Case B: with lower/upper error bars
  lower <- x - 0.1
  upper <- x + 0.1
  p2 <- plotQQnorm(x, lower = lower, upper = upper)
  testthat::expect_true(ggplot2::is.ggplot(p2))
  vdiffr::expect_doppelganger("plotQQnorm-errorbars", p2)

  # Case C: abline through origin and identical axes
  p3 <- plotQQnorm(x, abline = TRUE, ablineOrigin = TRUE, identicalAxes = TRUE)
  testthat::expect_true(ggplot2::is.ggplot(p3))
  vdiffr::expect_doppelganger("plotQQnorm-abline-origin-identicalaxes", p3)

})
