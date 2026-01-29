test_that("plotPieChart: polar, non-polar, axis ticks and large group variants", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  # Case A: simple polar pie chart
  value <- c(25, 25, 50)
  ggroups <- letters[1:3]
  p1 <- plotPieChart(value, ggroups)
  testthat::expect_true(ggplot2::is.ggplot(p1))
  vdiffr::expect_doppelganger("plotPieChart-polar-default", p1)

  # Case B: polar with legend labels and no axis ticks
  ga <- letters[4:6]
  p2 <- plotPieChart(value, ggroups, ga, showAxisTicks = FALSE, legend.position = "none")
  testthat::expect_true(ggplot2::is.ggplot(p2))
  vdiffr::expect_doppelganger("plotPieChart-polar-no-ticks-no-legend", p2)

  # Case C: non-polar variant
  p3 <- plotPieChartNonPolar(value, ggroups)
  testthat::expect_true(ggplot2::is.ggplot(p3))
  vdiffr::expect_doppelganger("plotPieChart-nonpolar-default", p3)

  # Case D: many small slices
  set.seed(1)
  value_many <- rpois(25, 10)
  groups_many <- as.character(seq_len(25))
  p4 <- plotPieChart(value_many, groups_many)
  testthat::expect_true(ggplot2::is.ggplot(p4))
  vdiffr::expect_doppelganger("plotPieChart-many-slices", p4)

})
