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

test_that("plotQQnorm: confidence band surrounds the reference line that is drawn", {

  ribbonData <- function(p) ggplot2::layer_data(p, which(vapply(p$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1L))))

  # skewed data, where the quartile line (intercept -0.30, slope 0.43) is far from y = x
  set.seed(6)
  y <- as.numeric(scale(rlnorm(50, 0, 1)))
  n <- length(y)
  i <- seq_len(n)
  zLower <- stats::qnorm(stats::qbeta(0.025, i, n - i + 1))
  zUpper <- stats::qnorm(stats::qbeta(0.975, i, n - i + 1))

  # Case A: ablineOrigin draws y = x, so the band is centred on y = x
  band1 <- ribbonData(plotQQnorm(y, ablineOrigin = TRUE, identicalAxes = TRUE, ciLevel = 0.95))
  testthat::expect_equal(band1$ymin, zLower)
  testthat::expect_equal(band1$ymax, zUpper)

  # Case B: by default the quartile line is drawn, so the band follows the quartile line
  q     <- stats::quantile(y, c(0.25, 0.75), names = FALSE)
  slope <- diff(q) / diff(stats::qnorm(c(0.25, 0.75)))
  int   <- q[1L] - slope * stats::qnorm(0.25)
  band2 <- ribbonData(plotQQnorm(y, ciLevel = 0.95))
  testthat::expect_equal(band2$ymin, int + slope * zLower)
  testthat::expect_equal(band2$ymax, int + slope * zUpper)

})
