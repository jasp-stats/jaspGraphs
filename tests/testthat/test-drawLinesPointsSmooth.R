test_that("drawLines, drawPoints, drawSmooth: basic rendering and group mapping", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  set.seed(123)
  dat1 <- data.frame(x = 1:10, y = seq(1, 10) + rnorm(10, 0, .1))

  p_lines <- drawLines(dat = dat1)
  testthat::expect_true(ggplot2::is_ggplot(p_lines))
  vdiffr::expect_doppelganger("drawLines-basic", p_lines)

  # grouped lines
  dat2 <- data.frame(x = rep(1:5, 2), y = rnorm(10), g = rep(c("A", "B"), each = 5))
  p_lines_g <- drawLines(dat = dat2)
  testthat::expect_true(ggplot2::is_ggplot(p_lines_g))
  vdiffr::expect_doppelganger("drawLines-grouped", p_lines_g)

  # drawPoints
  p_points <- drawPoints(dat = dat1)
  testthat::expect_true(ggplot2::is_ggplot(p_points))
  vdiffr::expect_doppelganger("drawPoints-basic", p_points)

  dat3 <- data.frame(x = rep(1:5, 2), y = rnorm(10), g = rep(c("A", "B"), each = 5))
  p_points_g <- drawPoints(dat = dat3)
  testthat::expect_true(ggplot2::is_ggplot(p_points_g))
  vdiffr::expect_doppelganger("drawPoints-grouped", p_points_g)

  # drawSmooth using dat and method lm (deterministic)
  dat_s <- data.frame(x = 1:20, y = 1:20 + rnorm(20, 0, 1))
  p_smooth_dat <- drawSmooth(dat = dat_s, method = "lm", se = FALSE, color = "blue")
  testthat::expect_true(ggplot2::is_ggplot(p_smooth_dat))
  vdiffr::expect_doppelganger("drawSmooth-dat-lm", p_smooth_dat)

  # drawSmooth using graph as base (extracts data from graph)
  base_g <- ggplot2::ggplot(dat_s, ggplot2::aes(x = x, y = y)) + ggplot2::geom_point()
  p_smooth_graph <- drawSmooth(graph = base_g, method = "lm", se = FALSE, color = "red")
  testthat::expect_true(ggplot2::is_ggplot(p_smooth_graph))
  vdiffr::expect_doppelganger("drawSmooth-graph-lm", p_smooth_graph)

})
