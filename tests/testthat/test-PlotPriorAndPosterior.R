test_that("PlotPriorAndPosterior: basic line plot and BF+CRI+median variants", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  n <- 100
  x <- seq(-3, 3, length.out = n)
  dfLines <- data.frame(
    x = x,
    y = c(dnorm(x, 0, 1), dnorm(x, 1, .5)),
    g = rep(c("Prior", "Posterior"), each = n)
  )

  # Case A: simple lines only
  p <- PlotPriorAndPosterior(dfLines)
  testthat::expect_s3_class(p, "jaspGraphs")
  # when no BF/CRI/median the return is a ggplot-like object; snapshot it directly
  vdiffr::expect_doppelganger("PlotPriorAndPosterior-lines-only", p)

  # Case B: with points, BF, CRI and median -> composite with subplots
  dfPoints <- data.frame(
    x = c(0, 0),
    y = dnorm(0, c(0, 1), c(1, .5)),
    g = c("Prior", "Posterior")
  )
  BF10 <- dnorm(0, 1, .5) / dnorm(0, 0, 1)
  CRI <- qnorm(c(0.025, 0.975), 1, .5)
  median_val <- qnorm(0.5, 1, .5)

  p2 <- PlotPriorAndPosterior(dfLines, dfPoints, BF10, CRI = CRI, median = median_val)
  testthat::expect_s3_class(p2, "jaspGraphsPlot")

  # Snapshot the central main graph and the BF pizza subplot
  # main graph is stored under name 'mainGraph'
  main_plot <- p2$subplots[["mainGraph"]]
  vdiffr::expect_doppelganger("PlotPriorAndPosterior-composite-main", main_plot)

  if (!is.null(p2$subplots[["BFpizza"]])) {
    vdiffr::expect_doppelganger("PlotPriorAndPosterior-composite-BFpizza", p2$subplots[["BFpizza"]])
  }

  # Also snapshot the CI text panel if present
  if (!is.null(p2$subplots[["CItext"]])) {
    vdiffr::expect_doppelganger("PlotPriorAndPosterior-composite-CItext", p2$subplots[["CItext"]])
  }

})
