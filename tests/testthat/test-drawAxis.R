test_that("drawAxis: dat-derived breaks, force plot and secondary axes", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  dat <- data.frame(x = 1:10, y = seq(10, 100, by = 10))

  # basic axis derived from data
  p1 <- drawAxis(dat = dat)
  testthat::expect_true(ggplot2::is_ggplot(p1))
  vdiffr::expect_doppelganger("drawAxis-from-dat", p1)

  # force a graph even without data (adds invisible geom)
  p2 <- drawAxis(graph = NULL, xBreaks = c(0, 5, 10), yBreaks = c(0, 50, 100), force = TRUE)
  testthat::expect_true(ggplot2::is_ggplot(p2))
  vdiffr::expect_doppelganger("drawAxis-force-invisible-geom", p2)

  # secondary axis on x
  p3 <- drawAxis(dat = dat, secondaryXaxis = list(name = "secX", trans = ~ .))
  testthat::expect_true(ggplot2::is_ggplot(p3))
  vdiffr::expect_doppelganger("drawAxis-secondary-x", p3)

})
