test_that("JASPScatterPlot produces expected plots", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  set.seed(42)
  x <- rnorm(100)
  y <- rgamma(100, 1, 1)

  p <- JASPScatterPlot(x, y, xName = "Normal", yName = "Gamma")
  testthat::expect_s3_class(p, "jaspGraphsPlot")

  # snapshot the main subplot (the scatter itself)
  vdiffr::expect_doppelganger("JASPScatterPlot-default-main", p$subplots[[1]])

  # grouped variant (uses mtcars from base datasets)
  p2 <- JASPScatterPlot(x = mtcars$mpg, y = mtcars$disp, group = mtcars$cyl)
  vdiffr::expect_doppelganger("JASPScatterPlot-grouped-main", p2$subplots[[1]])
})
