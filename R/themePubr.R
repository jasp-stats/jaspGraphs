#' @title Pubr theme for ggplot2 objects based on \code{ggpubr::theme_pubr}
#' @description Set a Pubr theme and additional options like other themes,
#' \code{graph + themePubrRaw()}.
#' @rdname themePubrRaw
#'
#' @description This function is a modification of
#'  \code{\link[ggpubr:theme_pubr]{ggpubr::theme_pubr}} that create a publication ready
#'  theme.
#' @param base_size base font size
#' @param base_family base font family
#' @param border logical value. Default is FALSE. If TRUE, add panel border.
#' @param margin logical value. Default is TRUE. If FALSE, reduce plot margin.
#' @param legend character specifying legend position. Allowed values are one of
#'  c("top", "bottom", "left", "right", "none"). Default is "top" side position.
#'  to remove the legend use legend = "none". Legend position can be also
#'  specified using a numeric vector c(x, y).  In this case it is possible to
#'  position the legend inside the plotting area. x and y are the coordinates of
#'  the legend box. Their values should be between 0 and 1. c(0,0) corresponds
#'  to the "bottom left" and c(1,1) corresponds to the "top right" position. For
#'  instance use legend = c(0.8, 0.2).
#' @param x.text.angle Rotation angle of x axis tick labels. Default value is 0.
#'  Use 90 for vertical text.
#' @param axisTickLength length of axis ticks.
#' @param axisTickWidth width of axis ticks.
#' @export
themePubrRaw <- function(base_size = getGraphOption("fontsize"),
                         base_family = getGraphOption("family"),
                         border = FALSE,
                         margin = TRUE,
                         legend = getGraphOption("legend.position"),
                         x.text.angle = 0,
                         axisTickLength = getGraphOption("axisTickLength"),
                         axisTickWidth  = getGraphOption("axisTickWidth")){

  half_line <- base_size / 2

  xhjust <- NULL
  if (x.text.angle > 5)
    xhjust <- 1

  if (border) {
    panel.border <- element_rect(fill = NA, colour = "black", size = 0.7)
    axis.line    <- element_blank()
  } else {
    panel.border <- element_blank()
    axis.line    <- element_line(colour = "black", size = 0.5)
  }

  plot.margin <- if (margin) margin(half_line, half_line, half_line, half_line) else unit(c(0.5, 0.3, 0.3, 0.3), "mm")

  .theme <- theme_bw(base_size = base_size, base_family = base_family) +
    theme(panel.border     = panel.border,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line        = axis.line,
          axis.text        = element_text(color = "black"),
          legend.key       = element_blank(),
          strip.background = element_rect(fill = "#F2F2F2", colour = "black", size = 0.7),
          plot.margin      = plot.margin,
          legend.position  = legend,
          complete         = TRUE)

  if (x.text.angle != 0)
    .theme <- .theme + theme(axis.text.x = element_text(angle = x.text.angle, hjust = xhjust))

  return(.theme)
}
