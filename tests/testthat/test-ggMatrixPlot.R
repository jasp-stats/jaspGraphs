test_that("ggMatrixPlot: 2x2 matrix, scaled labels, and debug variants", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  set.seed(10)
  data("diamonds", package = "ggplot2")
  vars  <- colnames(diamonds)[c(1, 5)]

  # build a small plot matrix (2x2)
  plotMatrix <- matrix(data = list(), nrow = 2, ncol = 2, dimnames = list(vars, vars))
  for (i in seq_along(vars)) for (j in seq_along(vars)) {
    samp <- head(diamonds, 200)
    plotMatrix[[i, j]] <- ggplot2::ggplot(data = samp, ggplot2::aes_string(x = vars[i], y = vars[j])) +
      ggplot2::geom_point() +
      geom_rangeframe() +
      themeJaspRaw()
  }

  p <- ggMatrixPlot(plotMatrix)
  testthat::expect_s3_class(p, "jaspGraphsPlot")
  grob_p <- p$plotFunction(p$subplots, args = p$plotArgs, grob = TRUE)
  vdiffr::expect_doppelganger("ggMatrixPlot-2x2-matrix", grob_p)

  # scaled labels variant
  # TODO: @don needs to fix
  # p2 <- ggMatrixPlot(plotMatrix, scaleXYlabels = c(1.3, 1.3))
  # grob_p2 <- p2$plotFunction(p2$subplots, args = p2$plotArgs, grob = TRUE)
  # vdiffr::expect_doppelganger("ggMatrixPlot-scaled-labels", grob_p2)

  # debug plot (constructed by the function itself)
  p_debug <- ggMatrixPlot(debug = TRUE, nr = 3, nc = 2)
  testthat::expect_s3_class(p_debug, "jaspGraphsPlot")
  grob_debug <- p_debug$plotFunction(p_debug$subplots, args = p_debug$plotArgs, grob = TRUE)
  vdiffr::expect_doppelganger("ggMatrixPlot-debug", grob_debug)

})
