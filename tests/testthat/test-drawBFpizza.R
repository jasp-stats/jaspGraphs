test_that("drawBFpizza & drawBFpizzaNonPolar: polar, labels and error handling", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  # simple polar pizza
  p1 <- drawBFpizza(c(1, 4))
  testthat::expect_true(ggplot2::is_ggplot(p1))
  vdiffr::expect_doppelganger("drawBFpizza-polar-simple", p1)

  # with labels
  p2 <- drawBFpizza(data.frame(y = c(1, 4)), labels = c("data | H0", "data | H1"))
  testthat::expect_true(ggplot2::is_ggplot(p2))
  vdiffr::expect_doppelganger("drawBFpizza-polar-with-labels", p2)

  # non-polar variant
  p3 <- drawBFpizzaNonPolar(c(1, 4), labels = c("data | H0", "data | H1"))
  testthat::expect_true(ggplot2::is_ggplot(p3))
  vdiffr::expect_doppelganger("drawBFpizza-nonpolar-with-labels", p3)

  # infinite cases - one infinite is allowed and converted
  p4 <- drawBFpizza(c(Inf, 1))
  testthat::expect_true(ggplot2::is_ggplot(p4))

  # both infinite -> error
  testthat::expect_error(drawBFpizza(c(Inf, Inf)))

})
