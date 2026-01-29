test_that("convertGgplotToPlotly: structural conversion to plotly object", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  p <- ggplot2::qplot(1:10, rnorm(10)) + themeJaspRaw()

  # confirm it converts and returns a plotly-like object
  res <- convertGgplotToPlotly(p, returnJSON = FALSE)
  testthat::expect_true(inherits(res, "plotly"))

  # basic structural checks: traces should be present and layout exists
  expect_true(length(res$x$data) >= 1)
  expect_true(!is.null(res$x$layout))

  # also ensure ggplotly yields similar structure (non-image test)
  res2 <- plotly::ggplotly(p)
  testthat::expect_true(inherits(res2, "plotly"))
  expect_true(length(res2$x$data) >= 1)
})
