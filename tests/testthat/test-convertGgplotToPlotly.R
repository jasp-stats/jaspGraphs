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

test_that("convertGgplotToPlotly: jaspMatrixPlot conversion preserves matrix layout", {
  skip_on_cran()
  skip_if_not_installed("plotly")

  vars <- c("disp", "hp")
  plotMatrix <- matrix(data = list(), nrow = 2, ncol = 2, dimnames = list(vars, vars))

  for (i in seq_along(vars)) for (j in seq_along(vars)) {
    plotMatrix[[i, j]] <- ggplot2::ggplot(mtcars, ggplot2::aes_string(x = vars[i], y = vars[j])) +
      ggplot2::geom_point() +
      geom_rangeframe() +
      themeJaspRaw()
  }

  p <- ggMatrixPlot(plotMatrix)
  res <- convertGgplotToPlotly(p, returnJSON = FALSE)

  testthat::expect_true(inherits(res, "plotly"))

  xaxes <- grep("^xaxis", names(res$x$layout), value = TRUE)
  yaxes <- grep("^yaxis", names(res$x$layout), value = TRUE)
  testthat::expect_equal(length(xaxes), prod(dim(p$plotArgs$layout)))
  testthat::expect_equal(length(yaxes), prod(dim(p$plotArgs$layout)))

  xWidths <- unique(round(vapply(xaxes, function(ax) diff(res$x$layout[[ax]]$domain), numeric(1L)), 2L))
  yHeights <- unique(round(vapply(yaxes, function(ax) diff(res$x$layout[[ax]]$domain), numeric(1L)), 2L))
  testthat::expect_gt(length(xWidths), 1L)
  testthat::expect_gt(length(yHeights), 1L)
  testthat::expect_true(length(res$x$layout$shapes) > 0L)

  annotations <- res$x$layout$annotations
  testthat::expect_false(is.null(annotations))
  testthat::expect_true(any(vapply(annotations, function(annotation) identical(annotation$textangle, -90), logical(1L))))

  p_shared <- ggMatrixPlot(plotMatrix, shareX = TRUE, shareY = TRUE)
  res_shared <- convertGgplotToPlotly(p_shared, returnJSON = FALSE)
  xaxes_shared <- grep("^xaxis", names(res_shared$x$layout), value = TRUE)
  yaxes_shared <- grep("^yaxis", names(res_shared$x$layout), value = TRUE)
  testthat::expect_lt(length(xaxes_shared), length(xaxes))
  testthat::expect_lt(length(yaxes_shared), length(yaxes))
})
