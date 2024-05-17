#' @title Histograms and Density plots for JASP
#'
#' @description A plot histogram with four optional components.
#' \describe{
#'   \item{\code{histogram}}{Histogram which can be tweaked with \code{breaks} and \code{histogramArgs} arguments.}
#'   \item{\code{density}}{Density line(s) which can be tweaked with \code{densityArgs}.}
#'   \item{\code{densityOverlay}}{Density line which can be tweaked with \code{densityOverlayArgs}. Only one line is shown for the full data regardless of whether `group` is used.}
#'   \item{\code{rug}}{Rugs underneath the figure which can be tweaked with \code{rugArgs}.}

#' }
#'
#' Each of these components can be enabled (or disabled) individually.
#'
#' `.marginalArgs`, `histogramArgs`, `.rugArgs`, and `.densityArgs` are helper functions for specifying the list of tweaking options for the individual components without overriding other default values.
#'
#' @details
#' Colors are taken from \code{graphOptions("palette")}.
#'
#' @return
#' `jaspMarginal` returns the ggplot object.
#' `.marginalArgs`, `histogramArgs`, `.rugArgs`, and `.densityArgs` return a list passed as arguments into their respective ggplot geoms.
#'
#' @param x, numeric, the data to show the plot for.
#' @param group, factor, show \code{histogram}, \code{density}, and \code{rug} split by groups?
#' @param xName, string, the title on the x-axis. Use \code{NULL} to hide the axis title. If `base::missing`, the value is inferred from the object name passed to \code{x}.
#' @param groupName, character, legend name of the grouping variable. Use \code{NULL} to hide the legend title. If `base::missing`, the value is inferred from the object name passed to \code{group}.
#' @param yName, string, the title on the y-axis. Use \code{NULL} to hide the axis title. If `base::missing`, "Density" or "Count" is used depending on the value of \code{type}.
#' @param type, string, should count or density be displayed on the y-axis? If \code{"auto"}, \code{"density"} is used if \code{density} or \code{densityOverlay} is used, otherwise \code{"count"} is used. \code{"count"} preserves marginal densities if split by group, \code{"density"} re-normalizes each subgroup.
#' @param breaks, see \code{breaks} from `graphics::hist`. Additionally allows \code{"doane"} method.
#' @param histogram, logical, should a histogram be shown?
#' @param histogramArgs, list, additional arguments passed to \code{\link[ggplot2]{geom_histogram}}. Use `.histogramArgs` to set the options.
#' @param rug, logical, should rugs be shown on the x-axis?
#' @param rugArgs, list, additional arguments passed to \code{\link[ggplot2]{geom_rug}}. Use `.rugArgs` to set the options.
#' @param density, logical, should a density be superimposed on the plot?
#' @param densityArgs, logical, additional arguments passed to \code{\link[ggplot2]{geom_density}}. Use `.densityArgs` to set the options.
#' @param densityOverlay, logical, should a density overlay be superimposed on the plot?
#' @param densityOverlayArgs, logical, additional arguments passed to \code{\link[ggplot2]{geom_density}}. Use `.densityArgs` to set the options.
#' @param axisLabels, string, which axes should have labels displayed? If \code{"auto"}, \code{"x"} is used if \code{type == "density"}, otherwise \code{"both"} is used.
#' @example inst/examples/ex-jaspMarginal.R
#' @rdname jaspMarginal
#' @export
jaspMarginal <- function(
    x,
    group = NULL,
    xName,
    groupName,
    yName,
    type               = c("auto", "count", "density"),
    breaks             = "sturges",
    histogram          = TRUE,
    histogramArgs      = .histogramArgs(),
    rug                = FALSE,
    rugArgs            = .rugArgs(),
    density            = FALSE,
    densityArgs        = .densityArgs(),
    densityOverlay     = FALSE,
    densityOverlayArgs = .densityArgs(linewidth = 1),
    axisLabels         = c("auto", "both", "x", "y", "none")
) {

  # validate input
  type <- match.arg(type)
  if(type == "auto") {
    type <- if(density || densityOverlay) "density" else "count"
  }

  if (!is.vector(x, mode = "numeric"))
    stop2("`x` must be a numeric vector but has class ", paste(class(x), collapse = ", "))

  if (missing(xName))
    xName <- deparse1(substitute(x)) # identical to plot.default

  if (missing(yName))
    yName <- if(type == "density") gettext("Density") else gettext("Count")

  if (!is.character(xName) && !is.null(xName))
    stop2("`xName` must be character but has class ", paste(class(xName), collapse = ", "), "!")

  if (!is.null(group) && !is.factor(group))
    stop2("`group` must be a factor vector but has class ", paste(class(group), collapse = ", "), "!")

  if (!is.null(group) && missing(groupName))
    groupName <- deparse1(substitute(group)) # identical to plot.default

  if (!missing(groupName) && !is.character(groupName) && !is.null(groupName))
    stop2("`groupName` must be character but has class ", paste(class(groupName), collapse = ", "), "!")

  axisLabels <- match.arg(axisLabels)
  if (axisLabels == "auto") {
    axisLabels <- if (type == "density") "x" else "both"
  }

  hasGroupingVariable <- !is.null(group)

  if(hasGroupingVariable) {
    data <- data.frame(x = x, group = group)
  } else {
    data <- data.frame(x = x)
  }
  data <- na.omit(data)

  h <- getJaspMarginalData(x = data[["x"]], breaks = breaks)
  xBreaks <- getPrettyAxisBreaks(c(data[["x"]], h[["breaks"]]), min.n = 3)

  histogramLayer <- densityLayer <- densityOverlayLayer <- rugLayer <- NULL
  if (histogram) {
    yy <- as.symbol(type)
    histogramAes <-
      if(hasGroupingVariable) {
        ggplot2::aes(x = x, y = ggplot2::after_stat({{yy}}), fill = group, group = group)
      } else {
        ggplot2::aes(x = x, y = ggplot2::after_stat({{yy}}))
      }
    # default gray filling
    if(is.null(histogramArgs[["fill"]]) && is.null(histogramAes[["fill"]])) histogramArgs[["fill"]] <- "gray"

    histogramArgs[["mapping"]] <- histogramAes
    histogramArgs[["breaks"]]  <- h[["breaks"]]

    histogramLayer <- do.call(ggplot2::geom_histogram, histogramArgs)
  }

  if (density) {
    bw <- diff(h[["breaks"]])[1]
    yy <- as.symbol(type)
    yy <- if(type == "density") {
      substitute(ggplot2::after_stat(yy))
    } else {
      substitute(bw * ggplot2::after_stat(yy))
    }

    densityAes <-
      if(hasGroupingVariable) {
        ggplot2::aes(x = x, y = {{yy}}, fill = group, group = group)
      } else {
        ggplot2::aes(x = x, y = {{yy}})
      }
    environment(densityAes$y) <- environment(densityAes$x)

    densityArgs[["mapping"]] <- densityAes
    densityLayer <- do.call(ggplot2::geom_density, densityArgs)

  }

  if (densityOverlay) {
    bw <- diff(h[["breaks"]])[1]
    yy <- as.symbol(type)
    yy <- if(type == "density") {
      substitute(ggplot2::after_stat(yy))
    } else {
      substitute(bw * ggplot2::after_stat(yy))
    }

    densityOverlayAes <- ggplot2::aes(x = x, y = {{yy}})
    environment(densityOverlayAes$y) <- environment(densityOverlayAes$x)

    densityOverlayArgs[["mapping"]] <- densityOverlayAes
    densityOverlayLayer <- do.call(ggplot2::geom_density, densityOverlayArgs)
  }

  if (rug) {
    rugAes <-
      if(!hasGroupingVariable) {
        ggplot2::aes(x = x)
      } else {
        ggplot2::aes(x = x, color = group, group = group)
      }

    rugArgs[["mapping"]] <- rugAes
    rugLayer <- do.call(ggplot2::geom_rug, rugArgs)
  }

  plot <- ggplot2::ggplot(data = data) +
    histogramLayer +
    densityLayer +
    densityOverlayLayer +
    rugLayer +
    geom_rangeframe() +
    themeJaspRaw(legend.position = "right") +
    scale_x_continuous(breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::xlab(xName) +
    ggplot2::ylab(yName)

  yRange <- ggplot2::layer_scales(plot)[["y"]][["range"]][["range"]]
  yBreaks <- getPrettyAxisBreaks(yRange)
  plot <- plot +
    scale_y_continuous(breaks = yBreaks, limits = range(yBreaks))

  if (hasGroupingVariable) plot <- plot + scale_JASPfill_discrete(name = groupName) + scale_JASPcolor_discrete(name = groupName)

  if (!axisLabels %in% c("x", "both"))
    plot <- plot + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())

  if (!axisLabels %in% c("y", "both"))
    plot <- plot + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

  return(plot)
}

#' @rdname jaspMarginal
#' @export
.marginalArgs <- function() {
  args <- as.list(environment())
  return(args)
}
formals(.marginalArgs) <- formals(jaspMarginal)


getJaspMarginalData <- function(x, breaks) {
  if (is.character(breaks)) {
    if(length(breaks) != 1) {
      stop2("`breaks` must be of length 1.")
    } else if (tolower(breaks) == "doane") {
      # https://en.wikipedia.org/wiki/Histogram#Doane's_formula
      sigma.g1 <- sqrt((6*(length(x) - 2)) / ((length(x) + 1)*(length(x) + 3)))
      g1 <- mean(abs(x)^3)
      k <- 1 + log2(length(x)) + log2(1 + (g1 / sigma.g1))
      breaks <- k
    } else if (tolower(breaks) == "fd" && grDevices::nclass.FD(x) > 10000) { # FD-method will produce extreme number of bins and crash ggplot, mention this in footnote
      warning2("The Freedman-Diaconis method would produce an extreme number of bins, setting the number of bins to 10,000.")
      breaks <- 10000
    }
  }

  h <- graphics::hist(x, plot = FALSE, breaks = breaks)
  return(h)
}

getJaspMarginalBreaks <- function(x, breaks) {
  h <- getJaspMarginalData(x, breaks)
  return(h[["breaks"]])
}

#' @rdname jaspMarginal
#' @export
.histogramArgs <- function(color = "black", size = 0.7, position = ggplot2::position_dodge(), ...) {
  args <- list(...)
  args[["color"]]    <- color
  args[["size"]]     <- size
  args[["position"]] <- position

  return(args)
}

#' @rdname jaspMarginal
#' @export
.rugArgs <- function(...) {
  args <- list(...)

  return(args)
}

#' @rdname jaspMarginal
#' @export
.densityArgs <- function(color = "black", linewidth = 0.7, alpha = 0.5, ...) {
  args <- list(...)
  args[["color"]]     <- color
  args[["linewidth"]] <- linewidth
  args[["alpha"]]     <- alpha

  return(args)
}
