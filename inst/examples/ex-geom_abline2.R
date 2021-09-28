library(ggplot2)
library(jaspGraphs)

dfRect <- data.frame(xmin = -10, ymin = -10, xmax = 10, ymax = 10)
basePlot <- ggplot(data = dfRect, aes(xmin = xmin, ymin = ymin, ymax = ymax, xmax = xmax)) +
  geom_rect(fill = NA, color = "black") +
  geom_rangeframe() +
  themeJaspRaw(legend.position = "right")

slopes <- seq(-5, 5, length.out = 15)
ints   <- seq(-2, 2, length.out = length(slopes))
basePlot +
  geom_abline2(intercept = ints - 2, slope = slopes, method = "breaks",  color = "green") +
  geom_abline2(intercept = ints + 2, slope = slopes, method = "ggplot2", color = "red") +
  ggtitle("specify 'method' manually")

dfAbline <- data.frame(
  intercept = -9:8,
  slope = 0.35,
  method = rep(c("ggplot2", "breaks"), 9)
)

basePlot +
  geom_abline2(data = dfAbline, mapping = aes(
    intercept = intercept, slope = slope, method = method, color = method
  ), show.legend = TRUE) +
  ggtitle("specify 'method' through aes")

basePlot + geom_abline2(data = dfAbline, mapping = aes(
    intercept = intercept, slope = slope, color = method
  )) +
  ggtitle("if method is not specified, 'breaks' is used as a default")

dfAbline <- data.frame(
  slope = c(Inf, Inf),
  intercept = 0,
  method = rep(c("ggplot2", "breaks"), 2)
)

basePlot +
  geom_abline2(data = dfAbline, mapping = aes(
    intercept = intercept, slope = slope, method = method, color = method
  ), show.legend = TRUE) +
  ggtitle("more reasonable behavior for infinite slopes")


# also works with discrete axes
df <- data.frame(
  x = letters[1:7],
  y = seq_along(letters[1:7])
)

ggplot(data = df, aes(x = x, y = y)) +
  geom_bar(stat = "identity", fill = NA, color = "grey") +
  geom_abline2(intercept = 0, slope = 0, size = 2, col = "blue") +
  geom_abline2(intercept = 0, slope = 1, size = 2, col = "red") +
  scale_y_continuous(breaks = 0:7) +
  geom_rangeframe() +
  themeJaspRaw()
