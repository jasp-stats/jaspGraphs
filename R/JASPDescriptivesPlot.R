#' Descriptive plot of categorical variable (x-axis) vs. a continuous variable (y-axis)
#'
#' @param x character vector indicating the groups shown on the x-axis.
#' @param y numeric vector indicating the y-values.
#' @param ciLower numeric vector with the lower bound of the confidence interval.
#' @param ciUpper numeric vector with the upper bound of the confidence interval.
#' @param xName character, name to use as the x-axis title.
#' @param yName character, name to use as the y-axis title.
#' @param group character vector indicating group membership, which is used for shape and fill color
#' @param groupName character, name to show as the legend title.
#' @param horizontalLine numeric, draw a horizontal asymptote at this y-value.
#' @param horizontalLineLineType line type of the horizontal asymptote.
#' @param position a value for the position argument of e.g., geom_line.
#' @param lineSize a value for the size argument of geom_line.
#' @param pointSize a value for the size argument of geom_point.
#' @param errorbarWidth a value for the width argument of geom_errorbar.
#' @param noXLevelNames Logical, should the level names on the x-axis be hidden?
#' @param breaksAtExtremaOnly Logical, Should only the outermost y-axis breaks be shown?
#' @param connectedPoints Logical, Should the dots be connected with lines?
#' @param legendPosition Indicate the legend position (passed to theme).
#'
#' @return a ggplot2 object.
#' @export
#'
#' @example inst/examples/ex-JASPDescriptivesPlot.R
descriptivesPlot <- function(x, y, ciLower = NULL, ciUpper = NULL,
                             xName = NULL, yName = NULL,
                             group = NULL, groupName = NULL,
                             horizontalLine = NULL, horizontalLineLineType = "dashed",
                             position = ggplot2::position_dodge(.2),
                             lineSize = .7, pointSize = 4,
                             errorbarWidth = .2,
                             noXLevelNames = length(x) == 1L,
                             breaksAtExtremaOnly = TRUE,
                             connectedPoints = TRUE,
                             legendPosition = if (is.null(group)) "none" else "right"
                             ) {

  hasGrouping <- !is.null(group)

  mapping <- aes(x = .data$x, y = .data$y, group = .data$group, fill = .data$group, shape = .data$group, color = .data$group)
  if (!is.null(ciLower) && !is.null(ciUpper)) {
    mapping$ymin <- ggplot2::quo(.data$ymin)
    mapping$ymax <- ggplot2::quo(.data$ymax)
  }

  df <- data.frame(
    x     = x,
    y     = y,
    ymin  = ciLower,
    ymax  = ciUpper,
    group = factor(if (hasGrouping) group else rep(1, length(x)))
  )

  yBreaks <- getPrettyAxisBreaks(c(df$y, df$ymax, df$ymin, horizontalLine))
  yLimits <- range(yBreaks)

  if (breaksAtExtremaOnly)
    yBreaks <- sort(c(yLimits, horizontalLine))

  horizontalAsymptoteLine <- NULL
  if (!is.null(horizontalLine))
    horizontalAsymptoteLine <- ggplot2::geom_hline(yintercept = horizontalLine, linetype = horizontalLineLineType)


  # This is not very pretty, but it's what JASP does...
  nValues <- max(1, length(unique(group)))

  fillValues <-
    if      (nValues == 1L) "black"
    else if (nValues <= 10) rep(c("white","black"), length.out = nValues)
    else                    c(rep(c("white", "black"), 5), rep("grey", nValues - 10))

  shapeValues <- c(rep(c(21:25), each=2), 21:25, 7:14, 33:112)[seq_len(nValues)]
  colorValues <- rep("black", nValues)

  guideLegend <- ggplot2::guide_legend(nrow = min(10, nValues), title = groupName, keywidth = 0.1, keyheight = 0.3, default.unit = "inch")

  scale_fill  <- ggplot2::scale_fill_manual( values = fillValues,  guide = guideLegend)
  scale_shape <- ggplot2::scale_shape_manual(values = shapeValues, guide = guideLegend)
  scale_color <- ggplot2::scale_color_manual(values = colorValues, guide = guideLegend)

  # length(x) > 1L avoids a warning whenever there is only one value and no lines can be drawn (e.g., the one-sample t-ttest)
  geomLine <- NULL
  if (length(x) > 1L && connectedPoints)
    geomLine <- ggplot2::geom_line(position = position, linewidth = lineSize)

  p <- ggplot(df, mapping = mapping) +
    ggplot2::geom_errorbar(color = "black", width = errorbarWidth, position = position) +
    geomLine +
    ggplot2::geom_point(position = position, size = pointSize) +
    horizontalAsymptoteLine +
    xlab(xName) +
    scale_y_continuous(name = yName, breaks = yBreaks, limits = yLimits) +
    scale_fill + scale_shape + scale_color +
    geom_rangeframe() +
    themeJaspRaw(legend.position = legendPosition)

  if (noXLevelNames)
    p <- p + theme(axis.text.x =  element_blank(),
                   axis.ticks.x = element_blank())

  return(p)
}
