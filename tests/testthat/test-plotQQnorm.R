test_that("plotQQnorm: default, error bars, abline-origin/identical axes variants", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  set.seed(2026)
  x <- rnorm(100)

  # Case A: default QQ plot
  p1 <- plotQQnorm(x)
  testthat::expect_true(ggplot2::is_ggplot(p1))
  vdiffr::expect_doppelganger("plotQQnorm-default", p1)

  # Case B: with lower/upper error bars
  lower <- x - 0.1
  upper <- x + 0.1
  p2 <- plotQQnorm(x, lower = lower, upper = upper)
  testthat::expect_true(ggplot2::is_ggplot(p2))
  vdiffr::expect_doppelganger("plotQQnorm-errorbars", p2)

  # Case C: abline through origin and identical axes
  p3 <- plotQQnorm(x, abline = TRUE, ablineOrigin = TRUE, identicalAxes = TRUE)
  testthat::expect_true(ggplot2::is_ggplot(p3))
  vdiffr::expect_doppelganger("plotQQnorm-abline-origin-identicalaxes", p3)

})

test_that("plotQQnorm: missing values are dropped before the quantiles are computed (na.rm = TRUE)", {
  set.seed(2026)
  x <- rnorm(30)
  xNA <- x
  xNA[c(3, 17, 25)] <- NA
  xComplete <- xNA[!is.na(xNA)]

  # the plot of the incomplete data equals the plot of its complete cases (points and reference line)
  pNA <- plotQQnorm(xNA)
  pComplete <- plotQQnorm(xComplete)
  testthat::expect_true(ggplot2::is_ggplot(pNA))
  testthat::expect_equal(pNA$data, pComplete$data)
  testthat::expect_equal(pNA$layers[[1]]$data, pComplete$layers[[1]]$data)

  # confidence bands and error bars use the same complete cases
  testthat::expect_equal(plotQQnorm(xNA, ciLevel = 0.95)$data,
                         plotQQnorm(xComplete, ciLevel = 0.95)$data)
  testthat::expect_equal(plotQQnorm(xNA, lower = xNA - 0.1, upper = xNA + 0.1)$data,
                         plotQQnorm(xComplete, lower = xComplete - 0.1, upper = xComplete + 0.1)$data)
})
