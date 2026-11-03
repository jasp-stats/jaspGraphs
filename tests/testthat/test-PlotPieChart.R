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

test_that("plotPieChart: compact rendered labels fit and do not overlap", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  cases <- list(
    list(value = c(145, 79), labels = c("64.7%", "35.3%")),
    list(value = c(50, 50), labels = c("50.0%", "50.0%")),
    list(value = c(100, 0), labels = c("100.0%", "0.0%")),
    list(value = 100, labels = "100.0%"),
    list(value = rep(1, 12), labels = rep("8.3%", 8))
  )
  for (case in cases) {
    value <- case$value
    p <- plotPieChart(value, letters[seq_along(value)], legendName = "Group")
    svg <- tempfile(fileext = ".svg")
    # Draw in a 320 x 320 pixel viewport at 96 dpi, irrespective of vdiffr's device size.
    vdiffr::write_svg(function() {
      grid::pushViewport(grid::viewport(
        x = 0, y = 1, just = c("left", "top"),
        width = grid::unit(320 / 96, "inches"), height = grid::unit(320 / 96, "inches")
      ))
      grid::grid.draw(ggplot2::ggplotGrob(p))
      grid::popViewport()
    }, svg)
    text <- grep("<text ", readLines(svg), value = TRUE, fixed = TRUE)
    unlink(svg)

    # Read actual text widths and positions from the rendered SVG, not the
    # approximate collision boxes used when constructing the plot.
    isLabel <- grepl("%</text>", text, fixed = TRUE)
    expect_equal(sub(".*>([^<]+)</text>", "\\1", text[isLabel]), case$labels)
    x <- as.numeric(sub(".* x='([^']+)'.*", "\\1", text))
    y <- as.numeric(sub(".* y='([^']+)'.*", "\\1", text))
    width <- as.numeric(sub(".* textLength='([^']+)px'.*", "\\1", text))
    height <- as.numeric(sub(".*font-size: ([0-9.]+)px.*", "\\1", text))
    anchor <- ifelse(grepl("text-anchor='end'", text, fixed = TRUE), 1,
                     ifelse(grepl("text-anchor='middle'", text, fixed = TRUE), 0.5, 0))
    xmin <- x - anchor * width
    xmax <- xmin + width
    expect_true(all(xmin[isLabel] >= 0 & xmax[isLabel] <= 320 / 96 * 72))
    expect_true(all(y[isLabel] - height[isLabel] >= 0 & y[isLabel] <= 320 / 96 * 72))

    # Include legend text in the collision check.
    pairs <- utils::combn(seq_along(text), 2L)
    overlap <- apply(pairs, 2L, function(i) {
      any(isLabel[i]) && xmin[i[1]] < xmax[i[2]] && xmax[i[1]] > xmin[i[2]] &&
        y[i[1]] - height[i[1]] < y[i[2]] && y[i[1]] > y[i[2]] - height[i[2]]
    })
    expect_false(any(overlap))
  }
})

test_that("plotPieChart: labelled pies keep consistent centred bounds", {
  values <- list(c(50, 50), c(25, 75), c(145, 79), c(100, 0), rep(1, 12))
  panels <- lapply(values, function(value) {
    p <- plotPieChart(value, letters[seq_along(value)], legendName = "Group")
    ggplot2::ggplot_build(p)$layout$panel_params[[1L]]
  })
  expect_equal(panels[[1L]]$x.range, -rev(panels[[1L]]$x.range))
  expect_equal(panels[[1L]]$y.range, -rev(panels[[1L]]$y.range))
  for (panel in panels[-1L]) {
    expect_equal(panel$x.range, panels[[1L]]$x.range)
    expect_equal(panel$y.range, panels[[1L]]$y.range)
  }
})

test_that("plotPieChart: percentage labels survive Plotly conversion", {
  p <- plotPieChart(c(145, 79), c("a", "b"), legendName = "Group")
  converted <- convertGgplotToPlotly(p, returnJSON = FALSE)
  expect_s3_class(converted, "plotly")
  text <- unlist(lapply(converted$x$data, function(trace) {
    if (isTRUE(grepl("text", trace$mode))) trace$text
  }))
  expect_equal(unname(text), c("64.7%", "35.3%"))
})

test_that("plotPieChart: polar coordinates remain available with a deprecation warning", {
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_warning(p <- plotPieChart(c(25, 25, 50), letters[1:3], polarAxis = TRUE), "deprecated")
  expect_s3_class(p$coordinates, "CoordPolar")
  expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
})

test_that("plotPieChartCartesian: labels show slice percentages, are rounded, and do not overlap", {
  skip_on_cran()

  # Case A: each slice is labelled with its own percentage, in the middle of the (counterclockwise) slice
  p1 <- plotPieChartCartesian(c(145, 79) / 224 * 100, c("1", "2"))
  labs1 <- ggplot2::layer_data(p1, 2L)
  testthat::expect_equal(labs1$label, c("64.7%", "35.3%"))
  testthat::expect_equal(atan2(labs1$y[1L], labs1$x[1L]) %% (2 * pi), (pi / 2 + pi * 145 / 224) %% (2 * pi))

  # Case B: labels of small neighbouring slices that would overlap a larger slice's label are dropped
  value <- c(1, 1, 3, 4, 10, 14, 10, 5)
  p2 <- plotPieChartCartesian(value / sum(value) * 100, as.character(seq_along(value)))
  testthat::expect_equal(ggplot2::layer_data(p2, 2L)$label, c("6.2%", "8.3%", "20.8%", "29.2%", "20.8%", "10.4%"))

})
