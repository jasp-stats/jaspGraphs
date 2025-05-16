library(jaspGraphs)
library(plotly)

plt <- jaspGraphs::jaspHistogram(rnorm(100))
plt_plotly <- ggplotly(plt)

plt_plotly


plt <- jaspGraphs::JASPScatterPlot(rnorm(100), rnorm(100))
plt$subplots[[2]]


ggplotly.jaspGraphsPlot


plt_plotly <- ggplotly(plt)

plt_plotly2 <- plotly::subplot(
  plotly::subplot(plt$subplots[[2]], plt$subplots[[1]], nrows = 2),
  plotly::subplot(ggplot2::ggplot() + ggplot2::theme_void(), plt$subplots[[3]], nrows = 2),
  nrows = 1
)

plt_plotly

htmlwidgets::saveWidget(plt_plotly2, file = "myplot.html")
