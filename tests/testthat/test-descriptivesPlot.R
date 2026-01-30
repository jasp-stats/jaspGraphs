test_that("descriptivesPlot: single, two-levels, grouped, and larger variants", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  # Case A: single value with horizontal line
  x <- "A"
  y <- -0.188
  ciLower <- y - .1
  ciUpper <- y + .1
  p1 <- descriptivesPlot(x, y, ciLower, ciUpper, horizontalLine = -.1)
  testthat::expect_true(ggplot2::is_ggplot(p1))
  vdiffr::expect_doppelganger("descriptivesPlot-single", p1)

  # Case B: two levels with names and horizontal line
  x <- c("Fast", "Slow")
  y <- c(15, 22)
  ciLower <- y - 3
  ciUpper <- y + 3
  p2 <- descriptivesPlot(x, y, ciLower, ciUpper, xName = "Condition", yName = "Response time", horizontalLine = 18.5)
  testthat::expect_true(ggplot2::is_ggplot(p2))
  vdiffr::expect_doppelganger("descriptivesPlot-two-levels", p2)

  # Case C: grouped data
  x <- c("0", "1", "0", "1")
  y <- c(15, 22, 16, 21)
  ciLower <- y - 3
  ciUpper <- y + 3
  group <- c("m", "m", "f", "f")
  p3 <- descriptivesPlot(x, y, ciLower, ciUpper, xName = "contBinom", group = group, groupName = "facGender", horizontalLine = 19)
  testthat::expect_true(ggplot2::is_ggplot(p3))
  vdiffr::expect_doppelganger("descriptivesPlot-grouped", p3)

  # Case D: larger randomized example (connectedPoints = FALSE, show breaks)
  set.seed(42)
  kx <- 5
  kg <- 20
  n  <- kx * kg
  x <- as.character(rep(1:kx, kg))
  y <- rnorm(n, 10 + 10 * as.numeric(x), 2)
  ciLower <- y - rnorm(n, 3 * as.numeric(x), 2)
  ciUpper <- y + rnorm(n, 3 * as.numeric(x), 2)
  group <- rep(seq_len(kg), each = kx)
  p4 <- descriptivesPlot(x, y, ciLower, ciUpper, xName = "Groups", yName = "Performance", group = group, groupName = "Legend title", breaksAtExtremaOnly = FALSE, connectedPoints = FALSE)
  testthat::expect_true(ggplot2::is_ggplot(p4))
  vdiffr::expect_doppelganger("descriptivesPlot-large", p4)

})
