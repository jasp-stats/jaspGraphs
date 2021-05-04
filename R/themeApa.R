#' @title APA theme for ggplot2 objects based on \code{jtools::theme_apa}
#' @description Set a APA theme and additional options to a graph like
#' other themes, \code{graph + themeApaRaw()}.
#' @rdname themeApaRaw
#'
#' @param legend.pos One of `"right"`, `"left"`, `"top"`, `"bottom"`,
#'   `"topleft"`, `"topright"`, `"topmiddle"`, `"bottomleft"`,
#'    `"bottomright"`, or `"bottommiddle"`.
#'   Positions the legend, which will layer on top of any geoms, on the plane.
#' @param legend.use.title Logical. Specify whether to include a legend title.
#'   Defaults to \code{FALSE}.
#' @param legend.font.size Integer indicating the font size of the labels in the
#'   legend. Default and APA-recommended is 12, but if there are many labels it
#'   may be necessary to choose a smaller size.
#' @param font.size General font size.
#' @param x.font.size Font size of x-axis label.
#' @param y.font.size Font size of x-axis label.
#' @param facet.title.size Font size of facet labels.
#' @param remove.x.gridlines Should the coordinate grid on the x-axis (vertical
#'   lines) be removed? Default is TRUE.
#' @param remove.y.gridlines Should the coordinate grid on the y-axis
#'   (horizontal lines) be removed? Default is TRUE.
#' @param axisTickLength length of axis ticks.
#' @param axisTickWidth width of axis ticks.
#' @param family global font family.
#' @details This function is a modification of \code{\link[jtools:theme_apa]{jtools::theme_apa}}
#'   that applies a theme to \code{ggplot2} figures with a
#'   style that is roughly in line with APA guidelines. Users may need to
#'   perform further operations for their specific use cases.
#'
#'   There are some things to keep in mind about APA style figures:
#'   \itemize{
#'    \item Main titles should be written in the word processor or typesetter
#'    rather than on the plot image itself.
#'    \item In some cases, users can forgo a legend in favor of describing the
#'    figure in a caption (also written in the word processor/typesetter).
#'    \item Legends are typically embedded on the coordinate plane of the figure
#'    rather than next to it, as is default in \code{ggplot2}.
#'    \item Use of color is generally discouraged since most of the applications
#'    for which APA figures are needed involve eventual publication in non-color
#'    print media.
#'    \item There are no hard and fast rules on font size, though APA recommends
#'    choosing between 8 and 14-point. Fonts in figures should be sans serif.
#'   }
#'
#'   Because APA style calls for positioning legends on the plane itself, this
#'   function includes options for choosing a position--top left, top right, bottom
#'   left, bottom right--to place the legend. \code{ggplot2} provides no obvious
#'   way to automatically choose a position that overlaps least with the geoms (the
#'   plotted data), so users will need to choose one.
#'
#'   Facetting is supported, but APA guidelines are considerably less clear for
#'   such situations.
#'
#'   This theme was created with inspiration from Rudolf Cardinal's
#'   \href{http://egret.psychol.cam.ac.uk/statistics/R/graphs2.html}{code}, which
#'   required updating for newer versions of \code{ggplot2} and adaptations for
#'   APA style.
#'
#' @importFrom ggplot2 theme_bw
#'
#' @export
themeApaRaw <- function(legend.pos       = getGraphOption("legend.position"),
                        legend.use.title = FALSE,
                        legend.font.size = getGraphOption("axis.title.cex"),
                        font.size        = getGraphOption("fontsize"),
                        x.font.size      = getGraphOption("fontsize"),
                        y.font.size      = getGraphOption("fontsize"),
                        facet.title.size = getGraphOption("fontsize"),
                        remove.y.gridlines = TRUE,
                        remove.x.gridlines = TRUE,
                        family         = getGraphOption("family"),
                        axisTickLength = getGraphOption("axisTickLength"),
                        axisTickWidth  = getGraphOption("axisTickWidth")) {

  theme <- theme_bw() +
    theme(
      plot.title           = element_text(family = family, face = "bold", hjust = 0, size = 14),
      axis.title.x         = element_text(size = x.font.size),
      axis.title.y         = element_text(size = y.font.size, angle = 90),
      legend.text          = element_text(size = legend.font.size),
      legend.key.size      = unit(1.5, "lines"),
      legend.key           = element_blank(),
      legend.key.width     = unit(2,  "lines"),
      strip.text.x         = element_text(size = facet.title.size),
      strip.text.y         = element_text(size = facet.title.size),
      strip.background     = element_rect(colour = "white", fill = "white"),
      panel.background     = element_rect(fill = "white"),
      plot.title.position  = "panel",

      # additional options copied from themeJasp
      text                 = element_text(family = family, size = font.size),
      axis.ticks.length    = axisTickLength,
      axis.ticks           = element_line(size = axisTickWidth, color = "black")
    )

  if (is.character(legend.pos))
    theme <- theme + switch(
      legend.pos,
      "topleft"      = theme(legend.position = c(0.05, 0.95), legend.justification = c(0.05, 0.95)),
      "topmiddle"    = theme(legend.position = c(0.5, 0.95),  legend.justification = c(0.5, 0.95)),
      "topright"     = theme(legend.position = c(0.95, 0.95), legend.justification = c(0.95, 0.95)),
      "topmiddle"    = theme(legend.position = c(0.95, 0.95), legend.justification = c(0.95, 0.95)),
      "bottomleft"   = theme(legend.position = c(0.5, 0.95),  legend.justification = c(0.5, 0.95)),
      "bottomright"  = theme(legend.position = c(0.95, 0.05), legend.justification = c(0.95, 0.05)),
      "bottommiddle" = theme(legend.position = c(0.5, 0.05),  legend.justification = c(0.5, 0.05)),
      "none" = theme(legend.position = "none"),
      stop("unknown legend position string")
    )

  theme <- theme + theme(
    legend.title = if (!legend.use.title) element_blank() else element_text(size = 12, face = "bold")
  )

  theme <- theme + if (remove.y.gridlines) .drop_y_gridlines() else .add_y_gridlines()

  theme <- theme + if (remove.x.gridlines) .drop_x_gridlines() else .add_x_gridlines()

  return(theme)
}

# TODO merge this into one function!
.drop_x_gridlines <- function(minor.only = FALSE) {
  .drop_gridlines(x = TRUE, y = FALSE, minor.only = minor.only)
}

.drop_y_gridlines <- function(minor.only = FALSE) {
  .drop_gridlines(x = FALSE, y = TRUE, minor.only = minor.only)
}

.add_x_gridlines <- function(minor = TRUE) {
  .add_gridlines(x = TRUE, y = FALSE, minor = minor)
}

.add_y_gridlines <- function(minor = TRUE) {
  .add_gridlines(x = FALSE, y = TRUE, minor = minor)
}

.drop_gridlines <- function(x = TRUE, y = TRUE, minor.only = FALSE) {
  plot <- ggplot2::theme()
  if (y) {
    plot <- plot + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
    if (minor.only == FALSE) {
      plot <- plot + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    }
  }
  if (x == TRUE) {
    plot <- plot + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
    if (minor.only == FALSE) {
      plot <- plot + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
    }
  }
  return(plot)
}

.add_gridlines <- function(x = TRUE, y = TRUE, minor = TRUE) {
  plot <- theme()
  if (y == TRUE) {
    plot <- plot + theme(panel.grid.major.y = ggplot2::element_line(colour = "grey92"))
    if (minor == TRUE) {
      plot <- plot + theme(panel.grid.minor.y = ggplot2::element_line(colour = "grey92", size = 0.25))
    }
  }
  if (x == TRUE) {
    plot <- plot + theme(panel.grid.major.x = ggplot2::element_line(colour = "grey92"))
    if (minor == TRUE) {
      plot <- plot + theme(panel.grid.minor.x = ggplot2::element_line(colour = "grey92", size = 0.25))
    }
  }
  return(plot)
}
