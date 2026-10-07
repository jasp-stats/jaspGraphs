# returns the limits of the x scale and the visible x breaks (both on the transformed scale)
getXScaleInfo <- function(p) {
  b      <- ggplot2::ggplot_build(p)
  breaks <- b$layout$panel_params[[1L]]$x$get_breaks()
  list(
    limits = b$layout$panel_scales_x[[1L]]$get_limits(),
    breaks = breaks[!is.na(breaks)]
  )
}

# the outer breaks coincide with the limits, so the axis is not cut off
expectOuterBreaksAtLimits <- function(info) {
  testthat::expect_true(all(is.finite(info$limits)))
  testthat::expect_equal(range(info$breaks), sort(info$limits))
}

# the pretty breaks of 0.2 - 6.8 are 0:7, but those of the expanded range -0.13 - 7.13 are 0, 2, 4, 6
dat  <- data.frame(x = c(0.2, 3, 6.8), y = c(1, 2, 3))
base <- ggplot2::ggplot(dat, ggplot2::aes(x, y)) + ggplot2::geom_point()

datLog  <- data.frame(x = c(2.5, 50, 800), y = c(1, 2, 3))
baseLog <- ggplot2::ggplot(datLog, ggplot2::aes(x, y)) + ggplot2::geom_point()

test_that("jaspScales: default limits span the pretty breaks of the data", {
  skip_if(utils::packageVersion("ggplot2") < "4.0.0")

  info <- getXScaleInfo(base + jaspGraphs::scale_x_continuous())
  expect_equal(info$limits, c(0, 7))
  expect_equal(info$breaks, 0:7)

  # the expansion of the scale or the coord does not affect the breaks
  info <- getXScaleInfo(base + jaspGraphs::scale_x_continuous(expand = ggplot2::expansion(mult = 0.2)))
  expect_equal(info$breaks, 0:7)

  info <- getXScaleInfo(base + jaspGraphs::scale_x_continuous() + ggplot2::coord_cartesian(expand = FALSE))
  expect_equal(info$breaks, 0:7)

  # fixed breaks are kept as is
  info <- getXScaleInfo(base + jaspGraphs::scale_x_continuous(breaks = c(2, 4)))
  expect_equal(info$limits, c(0, 7))
  expect_equal(info$breaks, c(2, 4))
})

test_that("jaspScales: explicit and one-sided limits", {
  skip_if(utils::packageVersion("ggplot2") < "4.0.0")

  info <- getXScaleInfo(base + jaspGraphs::scale_x_continuous(limits = c(0, 10)))
  expect_equal(info$limits, c(0, 10))
  expect_equal(info$breaks, seq(0, 10, 2))

  info <- getXScaleInfo(base + jaspGraphs::scale_x_continuous(limits = c(0, 7)))
  expect_equal(info$breaks, 0:7)

  # the missing side spans the pretty breaks of the data and the explicit side rather than the data
  info <- getXScaleInfo(base + jaspGraphs::scale_x_continuous(limits = c(NA, 10)))
  expect_equal(info$limits, c(0, 10))
  expectOuterBreaksAtLimits(info)

  info <- getXScaleInfo(base + jaspGraphs::scale_x_continuous(limits = c(-2, NA)))
  expect_equal(info$limits, c(-2, 8))
  expectOuterBreaksAtLimits(info)

  # the pretty breaks of -3 - 6.8 are -4, -2, ..., 8, so only the missing side can coincide with a break
  info <- getXScaleInfo(base + jaspGraphs::scale_x_continuous(limits = c(-3, NA)))
  expect_equal(info$limits, c(-3, 8))
  expect_equal(max(info$breaks), 8)
})

test_that("jaspScales: coord_cartesian limits give breaks for the visible range", {
  skip_if(utils::packageVersion("ggplot2") < "4.0.0")

  info <- getXScaleInfo(base + jaspGraphs::scale_x_continuous() + ggplot2::coord_cartesian(xlim = c(3, 3.5)))
  expect_equal(info$breaks, seq(3, 3.5, 0.1))

  info <- getXScaleInfo(base + jaspGraphs::scale_x_continuous() + ggplot2::coord_cartesian(xlim = c(0, 10)))
  expect_equal(info$breaks, seq(0, 10, 2))

  # coord limits equal to the scale limits are the same as no coord limits
  info <- getXScaleInfo(base + jaspGraphs::scale_x_continuous() + ggplot2::coord_cartesian(xlim = c(0, 7)))
  expect_equal(info$breaks, 0:7)
})

test_that("jaspScales: transformations give finite limits that span the breaks", {
  skip_if(utils::packageVersion("ggplot2") < "4.0.0")

  # pretty breaks on the original scale include 0, which is outside the domain of log10
  info <- getXScaleInfo(baseLog + jaspGraphs::scale_x_continuous(transform = "log10"))
  expect_equal(10^info$limits, c(1, 1000))
  expectOuterBreaksAtLimits(info)

  # breaks of the transformation do not cover the data, so pretty breaks on the transformed scale are used
  info <- getXScaleInfo(baseLog + jaspGraphs::scale_x_continuous(transform = "reciprocal"))
  expectOuterBreaksAtLimits(info)
  expect_true(info$limits[1L] <= 1 / 800 && info$limits[2L] >= 1 / 2.5)

  info <- getXScaleInfo(base + jaspGraphs::scale_x_continuous(transform = "reverse"))
  expect_equal(info$limits, c(-7, 0))
  expect_equal(sort(info$breaks), -(7:0))

  info <- getXScaleInfo(baseLog + jaspGraphs::scale_x_continuous(transform = "sqrt"))
  expectOuterBreaksAtLimits(info)
})
