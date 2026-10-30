#' Make a Pie Chart
#'
#' @param value The value for each group.
#' @param group Character of factor that indicates which value belongs to which group.
#' @param legendName Character, title of the legend.
#' @param legendLabels Character vector with names for the legend.
#' @param legendColors Character vector with colors.
#' @param showAxisText Logical, should each slice be labelled with its percentage (for \code{polarAxis = TRUE}: should the axis text be shown)?
#' @param asPercentages Logical, should value be transformed to percentages? Recommended to be true.
#' @param showAxisTicks Logical, should the ticks on the polar coordinates be shown?
#' @param palette Character vector with palette name. If this option is set then legendColors is ignored. If palette is NULL then legendColors are used.
#' @param polarAxis Logical, should the pie chart be drawn in polar coordinates? Setting this to TRUE is deprecated because polar coordinates are incompatible with interactive graphs.
#' @param ... Arguments to be passed to \code{\link{themeJasp}}.
#'
#' @return a ggplot object.
#' @export
#'
#' @example inst/examples/ex-PlotPieChart.R
#'
#' @importFrom ggplot2 ggplot geom_bar coord_polar scale_fill_manual labs element_text element_line element_blank
plotPieChart <- function(value, group,
                         legendName = deparse(substitute(group)),
                         legendLabels = if (is.factor(group)) levels(group) else unique(group),
                         legendColors = NULL,
                         showAxisText = TRUE, showAxisTicks = showAxisText, asPercentages = TRUE,
                         palette = getGraphOption("palette"),
                         polarAxis = FALSE,
                         ...) {

  if (!is.numeric(value))
    stop2("value should be numeric!")
  if (!(is.character(group) || is.factor(group)))
    stop2("group should be a character or factor!")
  if (length(value) != length(group))
    stop2("value and group should have the same length!")

  if (asPercentages)
    value <- value / sum(value) * 100

  nUnique <- length(unique(group))
  if (length(legendColors) == 1L && legendColors %in% names(jaspGraphs_data)) {
    legendColors <- jaspGraphs_data[[legendColors]][["colors"]]
    legendColors <- scales::gradient_n_pal(legendColors)(seq(0, 1, length.out = nUnique))
  }


  if (polarAxis) {

    lifecycle::deprecate_warn(
      when = "0.20.0",
      what = "plotPieChart(polarAxis = 'must be FALSE')",
      details = "Use `polarAxis = FALSE` for a pie chart compatible with interactive graphs."
    )

    g <- plotPieChartPolar(
      value = value,
      group = group,
      legendName = legendName,
      legendLabels = legendLabels,
      legendColors = legendColors,
      showAxisText = showAxisText,
      showAxisTicks = showAxisTicks,
      asPercentages = asPercentages,
      palette = palette,
      ...
    )

    extraTheme <- theme(
      axis.text.x = if (showAxisText) element_text() else element_blank(),
      axis.ticks  = element_blank(),
    )

    # change the default arguments for themeJasp
    dots <- list(...)
    dots <- setDefaults(dots, legend.position = "right", bty = "o")

    g <- do.call(themeJasp, c(list(graph = g), dots)) + extraTheme

  } else {

    g <- plotPieChartCartesian(
      value = value,
      group = group,
      legendName = legendName,
      legendLabels = legendLabels,
      legendColors = legendColors,
      showAxisText = showAxisText,
      showAxisTicks = showAxisTicks,
      asPercentages = asPercentages,
      palette = palette,
      ...
    )

    extraTheme <- theme(
      axis.text    = element_blank(),
      axis.text.x  = element_blank(),
      axis.text.y  = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks   = element_blank(),
    )

    # change the default arguments for themeJaspRaw
    dots <- list(...)
    dots <- setDefaults(dots, legend.position = "right")

    g <- g +
      do.call(themeJaspRaw, dots) +
      extraTheme
  }

  return(g)

}

plotPieChartPolar <- function(value, group,
                         legendName = deparse(substitute(group)),
                         legendLabels = if (is.factor(group)) levels(group) else unique(group),
                         legendColors = NULL,
                         showAxisText = TRUE, showAxisTicks = showAxisText, asPercentages = TRUE,
                         palette = getGraphOption("palette"),
                         ...) {

  df <- data.frame(
    y = value,
    g = factor(group)
  )

  scale_fill <- NULL
  if (!is.null(palette)) {
    scale_fill <- scale_JASPfill_discrete(palette = palette, name = legendName,
                                          breaks = legendLabels)
  } else if (!is.null(legendColors)) {
    scale_fill <- scale_fill_manual(values = legendColors, name = legendName, breaks = legendLabels)
  }

  g <- ggplot(df, aes(x = "", y = value, fill = group)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    scale_fill +
    labs(x = "", y = "")

  return(g)

}

plotPieChartCartesian <- function(value, group,
                                 legendName = deparse(substitute(group)),
                                 legendLabels = if (is.factor(group)) levels(group) else unique(group),
                                 legendColors = NULL,
                                 showAxisText = TRUE, showAxisTicks = showAxisText, asPercentages = TRUE,
                                 palette = getGraphOption("palette"),
                                 ...) {

  df <- data.frame(
    y = value,
    g = factor(group)
  )

  scale_fill <- NULL
  if (!is.null(palette)) {
    scale_fill <- scale_JASPfill_discrete(palette = palette, name = legendName,
                                          breaks = legendLabels)
  } else if (!is.null(legendColors)) {
    scale_fill <- scale_fill_manual(values = legendColors, name = legendName, breaks = legendLabels)
  }

  cum <- unname(cumsum(df$y)) / 100
  fromPerc <- unname(cbind(c(0, cum[-length(cum)]), cum))
  fromTheta <- 2 * pi * fromPerc

  offset <- pi / 2
  fromTheta <- fromTheta + offset
  n <- 256
  r <- c(0, rep(1, n), 0) # radius
  dfP <- do.call(rbind, lapply(1:nrow(fromTheta), function(i) {
    t <- c(pi / 2, seq(fromTheta[i, 1], fromTheta[i, 2], length.out = n), fromTheta[i, 2])
    x <- r * cos(t)
    y <- r * sin(t)
    data.frame(x = x, y = y, grp = df$g[i])
  }))
  dfP

  g <- ggplot2::ggplot(dfP, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$grp, color = .data$grp)) +
    ggplot2::geom_polygon(show.legend = TRUE, color = "black") +
    scale_fill +
    # the axes are hidden, so the jaspGraphs position scales would only add padding around the pie
    ggplot2::scale_x_continuous() + ggplot2::scale_y_continuous() +
    getEmptyTheme() +
    # slice labels may extend a little beyond the panel when the plot is smaller than the label boxes assume
    ggplot2::coord_fixed(clip = "off")

  if (showAxisText) {

    r <- 1.05
    t <- rowMeans(fromTheta)

    # percentage of each slice, placed just outside the middle of the slice
    slicePerc <- 100 * (fromPerc[, 2] - fromPerc[, 1])
    labels    <- sprintf("%.1f%%", slicePerc)

    dfTxt <- data.frame(
      x = r * cos(t),
      y = r * sin(t),
      l = labels,
      # anchor the text on the side facing the pie so labels grow outwards
      h = (1 - cos(t)) / 2,
      v = (1 - sin(t)) / 2
    )

    # approximate label boxes (padded by one character) in data units, assuming a pie radius of ~75pt as in a default-sized JASP plot
    fontsize   <- getGraphOption("fontsize")
    ptPerUnit  <- 75
    textWidth  <- 0.6 * (nchar(labels) + 1) * fontsize / ptPerUnit
    textHeight <- fontsize / ptPerUnit
    xmin <- dfTxt$x - dfTxt$h * textWidth
    xmax <- xmin + textWidth
    ymin <- dfTxt$y - dfTxt$v * textHeight
    ymax <- ymin + textHeight

    # drop labels that would overlap, labelling the largest slices first
    keep <- logical(length(labels))
    for (i in order(slicePerc, decreasing = TRUE)) {
      if (!any(keep & xmin < xmax[i] & xmax > xmin[i] & ymin < ymax[i] & ymax > ymin[i]))
        keep[i] <- TRUE
    }

    g <- g + ggplot2::geom_text(data = dfTxt[keep, ], aes(x = .data$x, y = .data$y, label = .data$l, hjust = .data$h, vjust = .data$v),
                           size = fontsize / ggplot2::.pt, inherit.aes = FALSE) +
      ggplot2::expand_limits(x = c(xmin[keep], xmax[keep]), y = c(ymin[keep], ymax[keep]))

  }

  return(g)

}

