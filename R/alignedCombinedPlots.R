# the code here is largely taken from/ inspired by ggExtra::ggMarginal

#' @importFrom ggplot2 ggplotGrob
#' @importFrom grid    unit
#' @importFrom gtable  gtable_add_padding gtable_add_grob gtable_add_rows gtable_add_cols

getPanelPos <- function(gtableGrob) {
  layDF <- gtableGrob$layout
  return(layDF[layDF$name == "panel", c("t", "l", "b", "r")])
}

getMargGrob <- function(margPlot, pattern = "panel") {
  margG <- ggplot2::ggplotGrob(margPlot)
  gtable::gtable_filter(margG, pattern = pattern)
}

makeGrobAlignedPlots <- function(mainplot, abovePlot = NULL, rightPlot = NULL, showLegend = FALSE, size = 5) {

  noTopPlot   <- is.null(abovePlot)
  noRightPlot <- is.null(rightPlot)
  if (noTopPlot && noRightPlot)
    return(ggplotGrob(mainplot))

  if (showLegend) {
    # extract the legend as grob
    legendGrob <- gtable::gtable_filter(ggplotGrob(mainplot), "guide-box")
    # remove the legend from the main plot
    pGrob <- ggplotGrob(mainplot + theme(legend.position = "none"))
  } else {
    pGrob <- ggplotGrob(mainplot)
  }

  # from ggExtra::ggMarginal
  if (!(noTopPlot || noRightPlot)) {
    ggxtraTmp   <- addTopMargPlot(pGrob, abovePlot, size)
    ggxtraNoTtl <- addRightMargPlot(ggxtraTmp, rightPlot, size)
  } else if (noRightPlot) {
    ggxtraTmp   <- gtable_add_padding(pGrob, unit(c(0, 0.5, 0, 0), "lines"))
    ggxtraNoTtl <- addTopMargPlot(ggxtraTmp, abovePlot, size)
  } else if (noTopPlot) {
    ggxtraTmp   <- gtable_add_padding(pGrob, unit(c(0.5, 0, 0, 0), "lines"))
    ggxtraNoTtl <- addRightMargPlot(ggxtraTmp, rightPlot, size)
  }
  # add the legend to the plot
  if (showLegend)
    ggxtraNoTtl <- addLegendMargPlot(ggxtraNoTtl, legendGrob, noTopPlot, noRightPlot, size)
  
  return(ggxtraNoTtl)
  
}

addTopMargPlot <- function(ggMargGrob, top, size) {
  panelPos <- getPanelPos(ggMargGrob)
  topMargG <- getMargGrob(top)
  gt <- gtable::gtable_add_rows(x = ggMargGrob, heights = unit(1/size, "null"), pos = 0)
  gtable::gtable_add_grob(x = gt, grobs = topMargG, t = 1, 
                          b = 1, l = panelPos[["l"]], r = panelPos[["r"]], z = Inf, 
                          clip = "on", name = "topMargPlot")
}

addRightMargPlot <- function(ggMargGrob, right, size) {
  panelPos <- getPanelPos(ggMargGrob)
  rightMargG <- getMargGrob(right)
  gt <- gtable::gtable_add_cols(x = ggMargGrob, widths = grid::unit(1/size, "null"), pos = -1)
  gtable::gtable_add_grob(x = gt, grobs = rightMargG, t = panelPos[["t"]], 
                          b = panelPos[["b"]], r = ncol(gt), l = ncol(gt), z = Inf, 
                          clip = "on", name = "rightMargPlot")
}

addLegendMargPlot <- function(ggMargGrob, legendGrob, noTopPlot, noRightPlot, size) {

  if (!noRightPlot) {
    if (noTopPlot) {
      gt <- gtable::gtable_add_cols(x = ggMargGrob, widths = grid::unit(1 / (1.5 * size), "null"), pos = -1)
    } else {
      gt <- ggMargGrob
    }
    return(
      gtable::gtable_add_grob(
        x = gt, grobs = legendGrob,
        t = 1 + noTopPlot, b = 1 + noTopPlot, r = ncol(gt), l = ncol(gt), z = Inf, clip = "on", name = "legendMargPlot")
    )
  } else {
    gt <- gtable::gtable_add_cols(x = ggMargGrob, widths = grid::unit(1 / (1.5 * size), "null"), pos = -1)
    return(
      gtable::gtable_add_grob(
        x = gt, grobs = legendGrob,
        t = 9, # TODO: this could be derived from the plot
        l = ncol(ggMargGrob),
        z = Inf, clip = "on", name = "legendMargPlot")
    )
  }
}

#' @title Build a scatter-with-marginals plot that supports both aligned static rendering and plotly
#'
#' @description
#' Returns a \code{jaspGraphsPlot} object that is also classed as \code{jaspMatrixPlot}.
#' The static render path uses \code{reDrawAlignedPlot} so the marginal panels are aligned
#' with the main panel via gtable (as in \code{\link{JASPScatterPlot}}), while the plotly
#' conversion uses the \code{jaspMatrixPlot} layout metadata stored in \code{plotArgs}.
#'
#' @param mainPlot a ggplot object used as the main panel.
#' @param topPlot optional ggplot shown above the main panel.
#' @param rightPlot optional ggplot shown right of the main panel.
#' @param size numeric; the marginal panels take up \code{1 / size} of the main panel. Defaults to 5.
#' @param showLegend passed to the aligned static renderer to place the legend next to the marginals.
#'
#' @export
makeAlignedMatrixPlot <- function(mainPlot, topPlot = NULL, rightPlot = NULL, size = 5, showLegend = FALSE) {

  hasTop   <- !is.null(topPlot)
  hasRight <- !is.null(rightPlot)

  subplots <- list(mainPlot = mainPlot)
  if (hasTop)   subplots$topPlot   <- topPlot
  if (hasRight) subplots$rightPlot <- rightPlot

  idxMain  <- 1L
  idxTop   <- if (hasTop)   2L                               else NA_integer_
  idxRight <- if (hasRight) 2L + as.integer(hasTop)          else NA_integer_

  marginSize <- 1 / size

  if (hasTop && hasRight) {
    layout  <- matrix(c(idxTop,  NA,      idxMain, idxRight), nrow = 2, byrow = TRUE)
    widths  <- c(1, marginSize)
    heights <- c(marginSize, 1)
  } else if (hasTop) {
    layout  <- matrix(c(idxTop, idxMain), nrow = 2, byrow = TRUE)
    widths  <- 1
    heights <- c(marginSize, 1)
  } else if (hasRight) {
    layout  <- matrix(c(idxMain, idxRight), nrow = 1, byrow = TRUE)
    widths  <- c(1, marginSize)
    heights <- 1
  } else {
    layout  <- matrix(idxMain, nrow = 1, ncol = 1)
    widths  <- 1
    heights <- 1
  }

  plot <- jaspGraphsPlot$new(
    subplots     = subplots,
    plotFunction = reDrawAlignedPlot,
    size         = size,
    showLegend   = showLegend,
    layout       = layout,
    widths       = widths,
    heights      = heights,
    names        = names(subplots)
  )
  class(plot) <- c("jaspMatrixPlot", "jaspMatrixplot", class(plot))
  return(plot)
}
