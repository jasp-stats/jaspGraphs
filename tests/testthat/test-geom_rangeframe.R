test_that("geom_rangeframe: default, sides, coord_flip and sec axis/top position variants", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  graph <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()

  p_default <- graph + geom_rangeframe() + themeJaspRaw()
  vdiffr::expect_doppelganger("geom_rangeframe-default", p_default)

  p_sides <- graph + geom_rangeframe(sides = "b") + themeJaspRaw()
  vdiffr::expect_doppelganger("geom_rangeframe-sides-bottom", p_sides)

  p_flip <- graph + coord_flip() + geom_rangeframe(sides = "b") + themeJaspRaw()
  vdiffr::expect_doppelganger("geom_rangeframe-coordflip-bottom", p_flip)

  p_sec <- graph +
    ggplot2::scale_x_continuous(sec.axis = ggplot2::sec_axis(~ log10(.))) +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ log10(.))) +
    geom_rangeframe(sides = "trbl") + themeJaspRaw()
  vdiffr::expect_doppelganger("geom_rangeframe-secaxis-all-sides", p_sec)

  # top position + panelInfo argument
  p_top <- graph + ggplot2::scale_x_continuous(position = "top") + geom_rangeframe(sides = "tl", panelInfo = list(t = "x.major")) + themeJaspRaw()
  vdiffr::expect_doppelganger("geom_rangeframe-top-position", p_top)

})
