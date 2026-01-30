test_that("geom_abline2: methods, aes mapping, infinite slopes, discrete axes", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  dfRect <- data.frame(xmin = -10, ymin = -10, xmax = 10, ymax = 10)
  basePlot <- ggplot2::ggplot(data = dfRect, ggplot2::aes(xmin = xmin, ymin = ymin, ymax = ymax, xmax = xmax)) +
    ggplot2::geom_rect(fill = NA, color = "black") +
    geom_rangeframe() +
    themeJaspRaw(legend.position = "right")

  slopes <- seq(-5, 5, length.out = 15)
  ints   <- seq(-2, 2, length.out = length(slopes))

  p1 <- basePlot +
    geom_abline2(intercept = ints - 2, slope = slopes, method = "breaks",  color = "green") +
    geom_abline2(intercept = ints + 2, slope = slopes, method = "ggplot2", color = "red") +
    ggplot2::ggtitle("specify 'method' manually")

  vdiffr::expect_doppelganger("geom_abline2-manual", p1)

  dfAbline <- data.frame(
    intercept = -9:8,
    slope = 0.35,
    method = rep(c("ggplot2", "breaks"), 9)
  )

  p2 <- basePlot +
    geom_abline2(data = dfAbline, mapping = ggplot2::aes(
      intercept = intercept, slope = slope, method = method, color = method
    ), show.legend = TRUE) +
    ggplot2::ggtitle("specify 'method' through aes")

  vdiffr::expect_doppelganger("geom_abline2-aes", p2)

  p3 <- basePlot + geom_abline2(data = dfAbline, mapping = ggplot2::aes(
    intercept = intercept, slope = slope, color = method
  )) +
    ggplot2::ggtitle("if method is not specified, 'breaks' is used as a default")

  vdiffr::expect_doppelganger("geom_abline2-default-method", p3)

  dfInf <- data.frame(
    slope = c(Inf, Inf),
    intercept = 0,
    method = rep(c("ggplot2", "breaks"), 2)
  )

  p4 <- basePlot +
    geom_abline2(data = dfInf, mapping = ggplot2::aes(
      intercept = intercept, slope = slope, method = method, color = method
    ), show.legend = TRUE) +
    ggplot2::ggtitle("more reasonable behavior for infinite slopes")

  vdiffr::expect_doppelganger("geom_abline2-inf-slopes", p4)

  # discrete axes variant
  df <- data.frame(x = letters[1:7], y = seq_along(letters[1:7]))
  p5 <- ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_bar(stat = "identity", fill = NA, color = "grey") +
    geom_abline2(intercept = 0, slope = 0, size = 2, col = "blue") +
    geom_abline2(intercept = 0, slope = 1, size = 2, col = "red") +
    ggplot2::scale_y_continuous(breaks = 0:7) +
    geom_rangeframe() +
    themeJaspRaw()

  vdiffr::expect_doppelganger("geom_abline2-discrete", p5)

})
