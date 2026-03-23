# plotly has 2 S3 methods for converting geoms to plotly traces: geom2trace and to_basic.
# to_basic handles the raw data and does possibly additional data manipulation
# geom2trace maps the data to a plotly trace
# often, we only need to adjust to_basic so that it changes the class
# to an existing ggplot object and then and then everything works fine.

# geom2trace.abline2 <- function(data, params, p) {
#   class(data) <- c("GeomHline", "GeomPath", class(data))
#   plotly::geom2trace(data, params, p)
# }

to_basic.abline2 <- function(data, prestats_data, layout, params, p, ...) {
  class(data) <- c("GeomAbline", class(data))
  plotly::to_basic(data, prestats_data, layout, params, p, ...)
}


# handled by geom2trace.alignedtext
# geom2trace.alignedtext <- function(data, params, p) {
#   class(data) <- c("GeomText", class(data))
#   plotly::geom2trace(data, params, p)
# }

to_basic.alignedtext <- function(data, prestats_data, layout, params, p, ...) {
  # if we "fix" the class here, could we avoid a custom geom2trace?
  class(data) <- c("GeomText", class(data))
  data$label <- paste(data$label1, data$label2)
  return(data) # do not call plotly::to_basic because GeomText has no specific s3 method
}

# plotly does not support geom_label (https://github.com/plotly/plotly.R/issues/2425).
# geom_label layers are extracted before ggplotly() in convertGgplotToPlotly and
# converted to plotly annotations (which support bgcolor, bordercolor, and textangle).
# See maybeRemoveGeomLabelLayers() in convertGgplotToPlotly.R


geom2trace.GeomBoxplotCustomOverride <- function(data, params, p) {

  # a custom wrapper for the geom2trace method for boxplots, to handle the case where ggplot2 receives precomputed box stats
  # this looks like a genuine bug in plotly, so we should probably try to upstream it as well

  # marker styling must inherit from GeomPoint$default_aes
  # https://github.com/hadley/ggplot2/blob/ab42c2ca81458b0cf78e3ba47ed5db21f4d0fc30/NEWS#L73-L7
  point_defaults <- ggplot2::GeomPoint$use_defaults(NULL)
  hide_outliers <- isFALSE(params$outliers) || isTRUE(is.na(params$outlier_gp$shape))

  has_box_stats <- all(c("lower", "middle", "upper", "ymin", "ymax") %in% names(data))
  has_raw_y <- "y" %in% names(data)

  # If ggplot2 receives precomputed box stats (e.g., stat = "identity"),
  # plotly needs q1/median/q3/lowerfence/upperfence rather than raw y values.
  if (has_box_stats && !has_raw_y) {
      return(plotly:::compact(list(
          x = data[["x"]],
          hoverinfo = "y",
          key = data[["key"]],
          customdata = data[["customdata"]],
          frame = data[["frame"]],
          ids = data[["ids"]],
          type = "box",
          q1 = data[["lower"]],
          median = data[["middle"]],
          q3 = data[["upper"]],
          lowerfence = data[["ymin"]],
          upperfence = data[["ymax"]],
          fillcolor = plotly:::toRGB(
            plotly:::aes2plotly(data, params, "fill"),
            plotly:::aes2plotly(data, params, "alpha")
          ),
          boxpoints = if (hide_outliers) FALSE,
          marker = list(
              opacity = point_defaults$alpha,
              outliercolor = plotly:::toRGB(point_defaults$colour),
              line = list(
                  width = plotly:::mm2pixels(point_defaults$stroke),
                  color = plotly:::toRGB(point_defaults$colour)
              ),
              size = plotly:::mm2pixels(point_defaults$size)
          ),
          line = list(
              color = plotly:::aes2plotly(data, params, "colour"),
              width = plotly:::aes2plotly(data, params, plotly:::linewidth_or_size(ggplot2::GeomBoxplot))
          )
      )))
  }

  # Default behavior for regular geom_boxplot input (x + y) remains unchanged.
  plotly:::compact(list(
      x = data[["x"]],
      y = data[["y"]],
      hoverinfo = "y",
      key = data[["key"]],
      customdata = data[["customdata"]],
      frame = data[["frame"]],
      ids = data[["ids"]],
      type = "box",
      fillcolor = plotly:::toRGB(
        plotly:::aes2plotly(data, params, "fill"),
        plotly:::aes2plotly(data, params, "alpha")
      ),
      boxpoints = if (hide_outliers) FALSE,
      # markers/points
      marker = list(
          opacity = point_defaults$alpha,
          outliercolor = plotly:::toRGB(point_defaults$colour),
          line = list(
              width = plotly:::mm2pixels(point_defaults$stroke),
              color = plotly:::toRGB(point_defaults$colour)
          ),
          size = plotly:::mm2pixels(point_defaults$size)
      ),
      line = list(
          color = plotly:::aes2plotly(data, params, "colour"),
          width = plotly:::aes2plotly(data, params, plotly:::linewidth_or_size(ggplot2::GeomBoxplot))
      )
  ))
}
