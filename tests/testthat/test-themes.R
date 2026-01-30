test_that("themes: themeJasp, themeApaRaw, themePubrRaw, getEmptyTheme apply correctly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  # base plot
  set.seed(1)
  p_base <- ggplot2::ggplot(data = data.frame(x = 1:10, y = rnorm(10))) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = x, y = y))

  p_jasp <- p_base + jaspGraphs::geom_rangeframe(sides = "bl") + themeJaspRaw()
  testthat::expect_true(ggplot2::is_ggplot(p_jasp))
  vdiffr::expect_doppelganger("themeJasp-basic", p_jasp)

  p_apa <- p_base + themeApaRaw(legend.pos = "none")
  testthat::expect_true(ggplot2::is_ggplot(p_apa))
  vdiffr::expect_doppelganger("themeApaRaw-basic", p_apa)

  p_pubr <- p_base + themePubrRaw()
  testthat::expect_true(ggplot2::is_ggplot(p_pubr))
  vdiffr::expect_doppelganger("themePubrRaw-basic", p_pubr)

  # getEmptyTheme applied to empty plot
  p_empty <- ggplot2::ggplot() + getEmptyTheme()
  testthat::expect_true(ggplot2::is_ggplot(p_empty))
  vdiffr::expect_doppelganger("getEmptyTheme-empty-plot", p_empty)

})
