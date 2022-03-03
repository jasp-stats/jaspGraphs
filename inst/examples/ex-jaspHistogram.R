set.seed(1234)

# some random data
x <- rnorm(1e3) + rnorm(1e3, 1, .1) - rgamma(1e3, 10, 5)

# individual plot components can be enabled or disabled
jaspHistogram(x, "random data", histogram = TRUE,  density = FALSE, rugs = FALSE) +
  ggplot2::ggtitle("default options")
jaspHistogram(x, "random data", histogram = TRUE,  density = TRUE,  rugs = FALSE)
jaspHistogram(x, "random data", histogram = TRUE,  density = TRUE,  rugs = TRUE)
jaspHistogram(x, "random data", histogram = TRUE,  density = FALSE, rugs = TRUE)
jaspHistogram(x, "random data", histogram = FALSE, density = FALSE, rugs = TRUE)
jaspHistogram(x, "random data", histogram = FALSE, density = FALSE, rugs = FALSE) +
  ggplot2::ggtitle("this is a bit boring though")

# histogram style can be tweaked
jaspHistogram(x, binWidthType = "sturges") + ggplot2::ggtitle("breaks: sturges")
jaspHistogram(x, binWidthType = "manual", numberOfBins = 250) +
  ggplot2::ggtitle("breaks: manual")

# split by group
groupingVariable <- gl(3, 333, length = 1e3)
groupingVariableName <- "group name"

jaspHistogram(x, "random data", groupingVariable, groupingVariableName)
jaspHistogram(x, "random data", groupingVariable, groupingVariableName,
              histogramPosition = "identity") # stacked
jaspHistogram(x, "random data", groupingVariable, groupingVariableName, density = TRUE,
              rugs = TRUE, rugsColor = TRUE)
jaspHistogram(x, "random data", groupingVariable, groupingVariableName, density = TRUE,
              rugs = TRUE, hideYAxisLabels = FALSE)
jaspHistogram(x, "random data", groupingVariable, groupingVariableName, density = TRUE,
              densityColor = TRUE, hideYAxisLabels = FALSE, densityLineWidth = 2)

# Can also be used to make fancy density plots
data("mtcars")
graphOptions(palette = "viridis")
jaspHistogram(mtcars$drat, "drat", factor(mtcars$cyl), "cyl",
              histogram = FALSE, hideYAxisLabels = FALSE,
              density = TRUE, densityColor = TRUE, densityShade = TRUE, densityShadeAlpha = 0.45
)
graphOptions(palette = "colorblind")
