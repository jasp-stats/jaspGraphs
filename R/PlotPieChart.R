#' Make a Pie Chart
#'
#' @param value The value for each group.
#' @param group Character of factor that indicates which value belongs to which group.
#' @param legendName Character, title of the legend.
#' @param legendLabels Character vector with names for the legend.
#' @param legendColors Character vector with colors.
#' @param showAxisText Logical, should the axis text be shown?
#' @param asPercentages Logical, should value be transformed to percentages? Recommended to be true.
#' @param showAxisTicks Logical, should the ticks on the polar coordinates be shown?
#' @param palette Character vector with palette name. If this option is set then legendColors is ignored. If palette is NULL then legendColors are used.
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
                         ...) {

  if (!is.numeric(value))
    stop2("value should be numeric!")
  if (!(is.character(group) || is.factor(group)))
    stop2("group should be a character or factor!")
  if (length(value) != length(group))
    stop2("value and group should have the same length!")

  # change the default arguments for themeJasp
  dots <- list(...)
  dots <- setDefaults(dots,
                      legend.position = "right",
                      bty = "o")

  if (asPercentages)
    value <- value / sum(value) * 100

  nUnique <- length(unique(group))
  if (length(legendColors) == 1L && legendColors %in% names(jaspGraphs_data)) {
    legendColors <- jaspGraphs_data[[legendColors]][["colors"]]
    legendColors <- scales::gradient_n_pal(legendColors)(seq(0, 1, length.out = nUnique))
  }

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

  graph <- ggplot(df, aes(x = "", y=value, fill=group)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    scale_fill +
    labs(x = "", y = "")

  return(do.call(themeJasp, c(list(graph = graph), dots)) + theme(
    axis.text.x = if (showAxisText) element_text() else element_blank(),
    axis.ticks  = element_blank()
  ))

}

#' Make a pie chart without coord_polar
#' @rdname plotPieChart
#' @export
plotPieChartNonPolar <- function(value, group,
                                 legendName = deparse(substitute(group)),
                                 legendLabels = if (is.factor(group)) levels(group) else unique(group),
                                 legendColors = NULL,
                                 showAxisText = TRUE, showAxisTicks = showAxisText, asPercentages = TRUE,
                                 palette = getGraphOption("palette"),
                                 ...) {

  if (!is.numeric(value))
    stop2("value should be numeric!")
  if (!(is.character(group) || is.factor(group)))
    stop2("group should be a character or factor!")
  if (length(value) != length(group))
    stop2("value and group should have the same length!")

  # change the default arguments for themeJasp
  dots <- list(...)
  dots <- setDefaults(dots,
                      legend.position = "right",
                      bty = "o")

  if (asPercentages)
    value <- value / sum(value) * 100

  nUnique <- length(unique(group))
  if (length(legendColors) == 1L && legendColors %in% names(jaspGraphs_data)) {
    legendColors <- jaspGraphs_data[[legendColors]][["colors"]]
    legendColors <- scales::gradient_n_pal(legendColors)(seq(0, 1, length.out = nUnique))
  }

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
    getEmptyTheme() +
    ggplot2::coord_fixed()

  if (showAxisText) {

    r <- 1.25
    t <- fromTheta[, 1] #- 2 * offset

    # labels <- cum * 100
    labels <- cumsum(rev(df$y))
    labels <- c("0 / 100", rev(labels[-length(labels)]))

    dfTxt <- data.frame(
      x = r * cos(t),
      y = r * sin(t),
      l = labels
    )
    g <- g + ggplot2::geom_text(data = dfTxt, aes(x = .data$x, y = .data$y, label = .data$l),
                           parse = needsParsing(labels),
                           size = getGraphOption("fontsize"), inherit.aes = FALSE)

  }

  return(g)

}

