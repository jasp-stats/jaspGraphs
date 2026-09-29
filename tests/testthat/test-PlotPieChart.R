test_that("plotPieChart: polar, non-polar, axis ticks and large group variants", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  # Case A: simple polar pie chart
  value <- c(25, 25, 50)
  ggroups <- letters[1:3]
  p1 <- plotPieChart(value, ggroups)
  testthat::expect_true(ggplot2::is_ggplot(p1))
  vdiffr::expect_doppelganger("plotPieChart-polar-default", p1)

  # Case B: polar with legend labels and no axis ticks
  ga <- letters[4:6]
  p2 <- plotPieChart(value, ggroups, ga, showAxisTicks = FALSE, legend.position = "none")
  testthat::expect_true(ggplot2::is_ggplot(p2))
  vdiffr::expect_doppelganger("plotPieChart-polar-no-ticks-no-legend", p2)

  # Case C: non-polar variant
  p3 <- plotPieChartCartesian(value, ggroups)
  testthat::expect_true(ggplot2::is_ggplot(p3))
  vdiffr::expect_doppelganger("plotPieChart-nonpolar-default", p3)

  # the test below does not replicate on macos
  testthat::skip_on_os("mac")
  # Case D: many small slices
  # set.seed(1)
  # value_many <- rpois(25, 10)
  # hardcoded output of rpois above after test failures on macos only
  value_many <- c(8L, 10L, 7L, 11L, 14L, 12L, 11L, 9L, 14L, 11L, 8L, 2L, 8L, 10L, 7L, 12L, 11L, 12L, 12L, 10L, 3L, 11L, 13L, 7L, 8L)
  groups_many <- as.character(seq_len(25))
  p4 <- plotPieChart(value_many, groups_many)
  testthat::expect_true(ggplot2::is_ggplot(p4))
  vdiffr::expect_doppelganger("plotPieChart-many-slices", p4)

})

test_that("plotPieChartCartesian: labels match slice boundaries, are rounded, and do not overlap", {
  skip_on_cran()

  # Case A: label at the end of the first (counterclockwise) slice reads 145 / 224 = 64.7
  p1 <- plotPieChartCartesian(c(145, 79) / 224 * 100, c("1", "2"))
  labs1 <- ggplot2::layer_data(p1, 2L)
  testthat::expect_equal(labs1$label, c("0 / 100", "64.7"))
  testthat::expect_equal(atan2(labs1$y[2L], labs1$x[2L]) %% (2 * pi), (pi / 2 + 2 * pi * 145 / 224) %% (2 * pi))

  # Case B: boundaries closer than 5 percentage points to the previous label (or to 0 / 100) are dropped
  value <- c(3, 1, 4, 5, 8, 10, 11, 6)
  p2 <- plotPieChartCartesian(value / sum(value) * 100, as.character(seq_along(value)))
  testthat::expect_equal(ggplot2::layer_data(p2, 2L)$label, c("0 / 100", "6.2", "16.7", "27.1", "43.8", "64.6", "87.5"))

})
