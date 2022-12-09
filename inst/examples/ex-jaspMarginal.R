set.seed(1)
x <- rnorm(250)
jaspMarginal(x)
jaspMarginal(x, breaks = 5)
jaspMarginal(x, breaks = 'doane')

jaspMarginal(x, density = TRUE, histogram = FALSE, rug = TRUE)


group <- as.factor(sample(letters[1:2], 500, TRUE, prob = c(0.3, 0.7)))
x     <- rnorm(500, mean = c(a = 0, b = 1)[group])

jaspMarginal(x, group, density = TRUE, rug = TRUE) # does not preserve marginal proportions
jaspMarginal(x, group, density = TRUE, rug = TRUE, type = "count") # preserves marginal proportions

# stacked groups
jaspMarginal(
  x, group, density = TRUE, type = "count",
  histogramArgs = .histogramArgs(position = ggplot2::position_stack()),
  densityArgs = .densityArgs(position = ggplot2::position_stack())
  )

jaspMarginal(x, group, densityOverlay = TRUE, type = "count", histogramArgs = .histogramArgs(position = ggplot2::position_stack()))
