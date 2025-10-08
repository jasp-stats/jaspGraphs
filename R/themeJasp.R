#' @importFrom ggplot2 element_line element_text element_rect margin coord_flip element_blank
#' @importFrom grid unit

#' @title Deprecated: use \link{themeJaspRaw} and \link{geom_rangeframe} instead.
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use \code{\link{themeJaspRaw}} and \link{geom_rangeframe} instead.
#'
#' @param xAxis Should there be a horizontal line for the x-axis?
#' @param yAxis Should there be a horizontal line for the x-axis?
#' @param graph a ggplot2 object
#' @param sides see \link{geom_rangeframe}
#' @param axis.title.cex scalar magnification for the title of the axes.
#' @param bty remake R's bty  = 'n'?
#' @param fontsize global font size.
#' @param family global font familiy.
#' @param horizontal flip graph?
#' @param legend.position where should the legend be?
#' @param legend.justification see theme
#' @param axisTickLength length of axis ticks.
#' @param axisTickWidth width of axis ticks.
#'
#' @export
themeJasp <- function(graph,
                     xAxis = TRUE,
                     yAxis = TRUE,
                     sides = "bl",
                     axis.title.cex = getGraphOption("axis.title.cex"),
                     bty = getGraphOption("bty"),
                     fontsize = getGraphOption("fontsize"),
                     family = getGraphOption("family"),
                     horizontal = FALSE,
                     legend.position = getGraphOption("legend.position"),
                     legend.justification = "top",
                     axisTickLength = getGraphOption("axisTickLength"),
                     axisTickWidth = getGraphOption("axisTickWidth")) {

  lifecycle::deprecate_warn(
    when = "0.5.2.6",
    what = "themeJasp()",
    with = "themeJaspRaw()"
  )

  if (!xAxis || !yAxis) {
    warning("Arguments xAxis and yAxis of themeJasp will be deprecated. Please use the argument \"sides\" instead.")

    if (horizontal) {
      if (!xAxis)
        sides <- str_remove_all(sides, "l")
      if (!yAxis)
        sides <- str_remove_all(sides, "b")
    } else {
      if (!xAxis)
        sides <- str_remove_all(sides, "b")
      if (!yAxis)
        sides <- str_remove_all(sides, "l")
    }
    if (sides == "")
      bty <- NULL
  }

  if (is.list(bty) && bty[["type"]] == "n")
    graph <- graph + geom_rangeframe(sides = sides)

  if (horizontal)
    graph <- graph + coord_flip()

  graph <- graph + themeJaspRaw(legend.position = legend.position,
                                axis.title.cex = axis.title.cex, family = family,
                                fontsize = fontsize, legend.justification = legend.justification,
                                axisTickLength = axisTickLength, axisTickWidth = axisTickWidth)

  return(graph)

}

#' @title JASP theme for ggplot2 objects
#'
#' @param legend.cex magnification of the font size for the legend.
#' @param Xvjust adjustment of x-axis tick labels
#' @param Yvjust adjustment of y-axis tick labels
#' @param axis.title.cex scalar magnification for the title of the axes.
#' @param fontsize global font size.
#' @param family global font familiy.
#' @param legend.title should legend have a title?
#' @param legend.position where should the legend be?
#' @param legend.justification see theme
#' @param axisTickLength length of axis ticks.
#' @param axisTickWidth width of axis ticks.
#' @export
themeJaspRaw = function(legend.position = "none",
                        legend.cex = 1,
                        axis.title.cex = 1,
                        family = getGraphOption("family"),
                        axisTickLength = getGraphOption("axisTickLength"),
                        axisTickWidth = getGraphOption("axisTickWidth"),
                        fontsize = getGraphOption("fontsize"),
                        legend.justification = "top",
                        legend.title = element_text(family = family, size = fontsize, hjust = 0.5),
                        Xvjust = NULL, Yvjust = NULL) {

    # TODO: shouldn't all element_rect(color = "transparent", fill = "transparent")
    # just be element_blank?
    theme(
        # generics
        rect = getBackgroundRect(getGraphOption("debug")),
        text = element_text(family = family, size = fontsize),
        # axis
        axis.line = element_line(color = "transparent", linewidth = 0),
        axis.ticks.length = axisTickLength, # tick length
        axis.title = element_text(size = axis.title.cex*fontsize),
        axis.ticks = element_line(linewidth = axisTickWidth, color = "black"), # tick width
        axis.title.x = element_text(margin = margin(t = 15, b = 5)),
        axis.title.y = element_text(margin = margin(r = 10, l = 5)),
        axis.text.x = element_text(size = fontsize, colour = "black", margin = margin(t = 7), vjust = Xvjust),
        axis.text.y = element_text(size = fontsize, colour = "black", margin = margin(r = 7), vjust = Yvjust),

        # legend
        legend.background     = element_rect(color = "transparent", fill = "transparent"),
        legend.box.background = element_rect(color = "transparent", fill = "transparent"),
        legend.justification  = legend.justification,
        legend.key            = element_rect(color = "transparent", fill = "transparent"),
        legend.key.size       = unit(1, "cm"),
        legend.text           = element_text(size = legend.cex * fontsize),
        legend.title          = legend.title,
        legend.position       = legend.position,

        # panel
        panel.border = element_rect(color = "transparent", fill = "transparent", linewidth = 0),
        panel.spacing = unit(2, "cm"),
        panel.grid = element_blank(),
        panel.background = element_rect(color = "transparent", fill = "transparent"),

        # plot
        plot.background = ggplot2::element_rect(fill = "transparent", color = "transparent"),
        plot.margin = ggplot2::margin(),
        plot.title = ggplot2::element_text(family = family, size = fontsize, hjust = 0.5), # center title

        # facet_wrap / facet_grid
        strip.background = element_rect(fill = "transparent", color = "transparent")

    )
}



# see http://stackoverflow.com/questions/43050399/ggplot-with-bty-n-or-how-to-add-grid-coordinates-to-plot-coordinates?noredirect=1&lq=1
x_custom <- function(...) {
    structure(
        list(...), # this ... information is not used, btw
        class = c("element_custom_x","element_blank", "element") # inheritance test workaround
    )

}
y_custom <- function(...) {
    structure(
        list(...), # this ... information is not used, btw
        class = c("element_custom_y","element_blank", "element") # inheritance test workaround
    )
}
