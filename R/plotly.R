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
