context("Plot Editing")

expect_equal_opts <- function(newPlot, oldOpts, ...) {

  newOpts <- plotEditingOptions(newPlot)



  keep <- setdiff(names(oldOpts$xAxis$settings), c("range", "breaksType"))
  oldOpts$xAxis$settings <- oldOpts$xAxis$settings[keep]
  oldOpts$yAxis$settings <- oldOpts$yAxis$settings[keep]

  newOpts$xAxis$settings <- newOpts$xAxis$settings[keep]
  newOpts$yAxis$settings <- newOpts$yAxis$settings[keep]

  expect_equal(newOpts, oldOpts, ...)

}

data("mtcars")

test_that("manipulating continuous axes works", {

  g1a <- ggplot(mtcars, aes(x = mpg, y = disp)) + geom_line()

  opts1a <- plotEditingOptions(g1a)

  opts1b <- opts1a
  opts1b$xAxis$settings$breaks <- seq(10, 20, 2)
  opts1b$xAxis$settings$labels <- as.character(seq(10, 20, 2))
  opts1b$xAxis$settings$title  <- "HOOOOI"
  opts1b$xAxis$settings$breaksType <- "manual"
  opts1b$xAxis$settings$limitsType <- "breaks"
  # opts1b$xAxis$settings$expand[c(2, 4)] <- c(5, 10)

  g1b <- plotEditing(g1a, opts1b)
  expect_equal_opts(g1b, opts1b)

  opts1c <- opts1a
  opts1c$yAxis$settings$limitsType <- "data"
  opts1c$yAxis$settings$breaksType <- "manual"
  opts1c$yAxis$settings$breaks <- seq(50, 400, 50)
  opts1c$yAxis$settings$labels <- as.character(seq(50, 400, 50))

  g1c <- plotEditing(g1a, opts1c)
  expect_equal_opts(g1c, opts1c)

  opts1d <- opts1a
  opts1d$xAxis$settings$range <- c(10, 30, 10)
  opts1d$xAxis$settings$limitsType <- "manual"
  opts1d$xAxis$settings$limits <- c(10, 31)

  g1d <- plotEditing(g1a, opts1d)
  expect_equal_opts(g1d, opts1d)


})

test_that("manipulating discrete axes works", {

  g2a <- ggplot(mtcars, aes(x = mpg, y = factor(cyl))) + geom_point()

  opts2a <- plotEditingOptions(g2a)

  opts2b <- opts2a
  opts2b$xAxis$settings$breaks <- seq(10, 20, 2)
  opts2b$xAxis$settings$labels <- as.character(seq(10, 20, 2))
  opts2b$xAxis$settings$title  <- "HOOOOI"

  g2b <- plotEditing(g2a, opts2b)
  expect_equal(plotEditingOptions(g2b), opts2b)

  opts2c <- opts2a
  opts2c$yAxis$settings$shown  <- c("4", "6")
  opts2c$yAxis$settings$labels <- c("vier", "zes")
  opts2c$yAxis$settings$title <- "YOYOYO"

  g2c <- plotEditing(g2a, opts2c)
  expect_equal(plotEditingOptions(g2c), opts2c)

  opts2d <- opts2c
  opts2d$xAxis$settings$breaks <- seq(15, 25, 5)
  opts2d$xAxis$settings$labels <- as.character(seq(15, 25, 5))

  g2d <- plotEditing(g2a, opts2d)
  expect_equal(plotEditingOptions(g2d), opts2d)

  opts2e <- opts2a
  opts2e$yAxis$settings$shown  <- c("4", "5", "6", "8", "10")
  opts2e$yAxis$settings$labels <- c("vier", "vijf", "zes", "acht", "tien")
  opts2e$yAxis$settings$title <- "tellen"

  g2e <- plotEditing(g2a, opts2e)


})
