#' @importFrom plotly ggplotly

#' @title convert a ggplot object to a plotly object and store the json in the image list
#' @param ggplotObj a ggplot object
#' @param returnJSON Should a plotly object be returned (FALSE) or a JSON object for JASP (TRUE)?
#'@export
convertGgplotToPlotly <- function(ggplotObj, returnJSON = TRUE) {
  # see https://github.com/Rdatatable/data.table/issues/5375
  # we must set getOption("datatable.alloccol"), verbose = getOption("datatable.verbose")
  # options(datatable.alloccol = 1024L, datatable.verbose = FALSE) # default values
  # https://github.com/Rdatatable/data.table/blob/35544d34ce779599cb2ed8900da2ffbac0ffbf29/R/onLoad.R#L76C11-L94C9
  options(
    "datatable.verbose" = FALSE,        # datatable.<argument name>
    "datatable.optimize" = Inf,             # datatable.<argument name>
    "datatable.print.nrows" = 100L,         # datatable.<argument name>
    "datatable.print.topn" = 5L,            # datatable.<argument name>
    "datatable.print.class" = TRUE,         # for print.data.table
    "datatable.print.rownames" = TRUE,      # for print.data.table
    "datatable.print.colnames" = "'auto'",    # for print.data.table
    "datatable.print.keys" = TRUE,          # for print.data.table
    "datatable.print.trunc.cols" = FALSE,   # for print.data.table
    "datatable.show.indices" = FALSE,       # for print.data.table
    "datatable.allow.cartesian" = FALSE,    # datatable.<argument name>
    "datatable.join.many" = TRUE,           # mergelist, [.data.table #4383 #914
    "datatable.dfdispatchwarn" = TRUE,                   # not a function argument
    "datatable.warnredundantby" = TRUE,                  # not a function argument
    "datatable.alloccol" = 1024L,           # argument 'n' of alloc.col. Over-allocate 1024 spare column slots
    "datatable.auto.index" = TRUE,          # DT[col=="val"] to auto add index so 2nd time faster
    "datatable.use.index" = TRUE,           # global switch to address #1422
    "datatable.prettyprint.char"   = NULL     # FR #1091
  )

  # TODO: a lot of the ggplot2 stuff below assumes ggplot2 4.0.0 or higher!
  # TODO: this would really benefit from rigourous unit-tests!

  e <- try({

    converted <- convertPlotObjectToPlotly(ggplotObj)

    # TODO: we should decode any column names in the data in plotlybuild$x... maybe we can do this through ggplot2 though
    if (returnJSON) {
      json <- toJSON(list(data = converted$plotly$x$data, layout = converted$plotly$x$layout, hasRangeFrame = converted$hasRangeFrame))
      json
    } else {
      converted$plotly
    }
  })
  return(e)
}

convertPlotObjectToPlotly <- function(plotObj) {
  if (inherits(plotObj, "jaspMatrixPlot"))
    return(convertJaspMatrixPlotToPlotly(plotObj))

  if (!ggplot2::is_ggplot(plotObj))
    stop2("convertGgplotToPlotly only supports ggplot objects and jaspMatrixPlot objects.")

  return(convertSingleGgplotToPlotly(plotObj))
}

convertSingleGgplotToPlotly <- function(ggplotObj) {

  # we need to remove the rangeframe layer, if there is one
  temp <- maybeRemoveRangeFrameLayer(ggplotObj = ggplotObj)

  # plotly does not support geom_label (https://github.com/plotly/plotly.R/issues/2425)
  # we extract them before ggplotly() and convert them to plotly annotations,
  # which support background color, border, and text angle.
  temp2 <- maybeRemoveGeomLabelLayers(temp$ggplotObjNoRangeFrame)

  pNoRangeframe <- temp2$ggplotObj
  plotlyplotje <- ggplotly(pNoRangeframe)
  plotlyplotje <- fixPlotlyLegendTitle(pNoRangeframe, plotlyplotje)
  hasRangeFrame <- FALSE

  # ensure plotly auto-sizes margins so axis titles don't overlap tick labels
  allXaxes <- grep("^xaxis", names(plotlyplotje$x$layout), value = TRUE)
  allYaxes <- grep("^yaxis", names(plotlyplotje$x$layout), value = TRUE)
  for (ax in allXaxes)
    plotlyplotje$x$layout[[ax]]$automargin <- TRUE
  for (ax in allYaxes) {
    plotlyplotje$x$layout[[ax]]$automargin <- TRUE
    plotlyplotje$x$layout[[ax]]$title$standoff <- 15L
  }

  if (!is.null(temp$shapes)) {

    plotlyplotje <- plotly::layout(plotlyplotje,
      shapes = temp$shapes,
      xaxis = list(zeroline = FALSE, showline = FALSE),
      yaxis = list(zeroline = FALSE, showline = FALSE)
    )

    # remove any background transparent background rectangle which blocks
    # the rangeframe lines
    # if (!is.null(plotlyplotje$x$layout$shapes) &&
    #     plotlyplotje$x$layout$shapes[[1L]]$type == "rect" &&
    #     plotlyplotje$x$layout$shapes[[1L]]$fillcolor == "transparent"
    #     ) {
    #   plotlyplotje$x$layout$shapes <- NULL
    # }

    # this might be too invasive, but it works for now
    plotlyplotje$x$layout$shapes <- temp$shapes

    sides <- temp$rangeFrameLayer$geom_rangeframe$geom_params$sides
    if (grepl("b", sides) || grepl("l", sides))
      hasRangeFrame <- TRUE

    # Get all x-axes
    xaxes <- grep("^xaxis", names(plotlyplotje$x$layout), value = TRUE)
    # Get all y-axes
    yaxes <- grep("^yaxis", names(plotlyplotje$x$layout), value = TRUE)
    x_domains <- vapply(xaxes, function(ax) plotlyplotje$x$layout[[ax]]$domain, numeric(2L))
    y_domains <- vapply(yaxes, function(ax) plotlyplotje$x$layout[[ax]]$domain, numeric(2L))

    # axis names as plotly expects them
    xnms <- sub("axis", "", xaxes)
    ynms <- sub("axis", "", yaxes)

    for (ax in xaxes)
      plotlyplotje <- adaptive_ggplotly_ticks(plotlyplotje, ax)

    for (ax in yaxes)
      plotlyplotje <- adaptive_ggplotly_ticks(plotlyplotje, ax)

    # repeat the rangeframe shape for all facets using the starting shape as a template
    # and the domains from the axes.
    shp <- plotlyplotje$x$layout$shapes
    shp0 <- shp
    for (i in seq_along(xaxes)) {
      for (j in seq_along(yaxes)) {

        if (i == 1L && j == 1L)
          next

        shp_new <- shp0
        shp_new[[1L]]$xref <- xnms[i]
        shp_new[[1L]]$y0 <- y_domains[1L, j]
        shp_new[[1L]]$y1 <- y_domains[1L, j]

        shp_new[[2L]]$yref <- ynms[j]
        shp_new[[2L]]$x0 <- x_domains[1L, i]
        shp_new[[2L]]$x1 <- x_domains[1L, i]

        shp <- c(shp, shp_new)
      }
    }
    plotlyplotje$x$layout$shapes <- shp


    plotlyplotje <- htmlwidgets::onRender(plotlyplotje, jsCode = js_code_rangeframe)

  }

  # add geom_label layers as plotly annotations (with background, border, angle)
  if (length(temp2$annotations) > 0L)
    plotlyplotje <- plotly::layout(plotlyplotje, annotations = temp2$annotations)

  return(list(plotly = plotly::plotly_build(plotlyplotje), hasRangeFrame = hasRangeFrame))
}

convertJaspMatrixPlotToPlotly <- function(plotObj) {
  layout <- plotObj$plotArgs[["layout"]]
  widths <- plotObj$plotArgs[["widths"]]
  heights <- plotObj$plotArgs[["heights"]]
  subplotNames <- plotObj$plotArgs[["names"]]
  shareX <- resolveMatrixPlotlyShareFlag(plotObj$plotArgs[["shareX"]], "shareX")
  shareY <- resolveMatrixPlotlyShareFlag(plotObj$plotArgs[["shareY"]], "shareY")
  titleX <- plotObj$plotArgs[["titleX"]] %||% TRUE
  titleY <- plotObj$plotArgs[["titleY"]] %||% TRUE

  if (!is.matrix(layout) || is.null(widths) || is.null(heights))
    stop2("jaspMatrixPlot is missing layout information required for plotly conversion.")

  subplotCount <- length(plotObj$subplots)
  plotCells <- vector("list", length(layout))
  hasRangeFrame <- FALSE

  for (cellIndex in seq_along(layout)) {
    subplotIndex <- layout[[cellIndex]]

    if (!is.na(subplotIndex) && subplotIndex <= subplotCount) {
      subplotName <- if (!is.null(subplotNames)) subplotNames[[subplotIndex]] else NULL
      converted <- if (isMatrixLabelSubplotName(subplotName)) {
        convertMatrixLabelSubplotToPlotly(plotObj$subplots[[subplotIndex]])
      } else {
        convertSingleGgplotToPlotly(plotObj$subplots[[subplotIndex]])
      }
      plotCells[[cellIndex]] <- stripPlotlyConfig(converted$plotly)
      hasRangeFrame <- hasRangeFrame || converted$hasRangeFrame
    } else {
      plotCells[[cellIndex]] <- makeEmptyPlotlySubplot()
    }
  }

  dim(plotCells) <- dim(layout)
  widths <- normalizeSubplotSpans(widths)
  heights <- normalizeSubplotSpans(heights)

  # need to add some extra space between the panels if there are axis titles  # any
  marginX <- if (titleX) 0.04 else 0
  marginY <- if (titleY) 0.04 else 0

  rowPlots <- vector("list", nrow(layout))
  for (rowIndex in seq_len(nrow(layout))) {
    rowPlots[[rowIndex]] <- stripPlotlyConfig(do.call(
      plotly::subplot,
      c(
        plotCells[rowIndex, ],
        list(
          nrows = 1,
          widths = widths,
          margin = marginY,
          shareX = shareX,
          shareY = shareY,
          titleX = titleX,
          titleY = titleY
        )
      )
    ))
  }

  matrixPlot <- stripPlotlyConfig(do.call(
    plotly::subplot,
    c(
      rowPlots,
      list(
        nrows = length(rowPlots),
        heights = heights,
        margin = marginX,
        shareX = shareX,
        shareY = shareY,
        titleX = titleX,
        titleY = titleY
      )
    )
  ))

  if (hasRangeFrame)
    matrixPlot <- htmlwidgets::onRender(matrixPlot, jsCode = js_code_rangeframe)

  return(list(plotly = plotly::plotly_build(matrixPlot), hasRangeFrame = hasRangeFrame))
}

resolveMatrixPlotlyShareFlag <- function(flag, flagName) {
  if (is.null(flag))
    return(FALSE)

  if (!is.logical(flag) || length(flag) != 1L || is.na(flag))
    stop2(sprintf("jaspMatrixPlot plotly %s should be either TRUE or FALSE.", flagName))

  return(flag)
}

isMatrixLabelSubplotName <- function(subplotName) {
  is.character(subplotName) && length(subplotName) == 1L && grepl("^(xlab-[tb]|ylab-[lr])-", subplotName)
}

convertMatrixLabelSubplotToPlotly <- function(ggplotObj) {
  built <- ggplot2::ggplot_build(ggplotObj)
  layerData <- built$data[[1L]]

  if (is.null(layerData) || nrow(layerData) == 0L)
    return(list(plotly = makeEmptyPlotlySubplot(), hasRangeFrame = FALSE))

  fontFamily <- layerData$family[[1L]]
  font <- list(
    size = (layerData$size[[1L]] %||% 3.88) * ggplot2::.pt,
    color = layerData$colour[[1L]] %||% "black"
  )
  if (!is.null(fontFamily) && !is.na(fontFamily) && nzchar(fontFamily))
    font$family <- fontFamily

  annotation <- list(
    x = layerData$x[[1L]] %||% 0.5,
    y = layerData$y[[1L]] %||% 0.5,
    xref = "x",
    yref = "y",
    text = layerData$label[[1L]] %||% "",
    showarrow = FALSE,
    xanchor = ggplotHjustToPlotlyXAnchor(layerData$hjust[[1L]] %||% "center"),
    yanchor = ggplotVjustToPlotlyYAnchor(layerData$vjust[[1L]] %||% "center"),
    textangle = -(layerData$angle[[1L]] %||% 0),
    font = font
  )

  plotlyplotje <- plotly::layout(
    plotly::plot_ly(
      x = c(0, 1),
      y = c(0, 1),
      type = "scatter",
      mode = "markers",
      hoverinfo = "skip",
      marker = list(opacity = 0),
      showlegend = FALSE
    ),
    annotations = list(annotation),
    xaxis = list(range = c(0, 1), visible = FALSE, showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, fixedrange = TRUE),
    yaxis = list(range = c(0, 1), visible = FALSE, showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, fixedrange = TRUE),
    margin = list(l = 0, r = 0, b = 0, t = 0),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    showlegend = FALSE
  )

  return(list(plotly = plotly::plotly_build(plotlyplotje), hasRangeFrame = FALSE))
}

ggplotHjustToPlotlyXAnchor <- function(hjust) {
  if (is.character(hjust))
    return(switch(hjust, inward = "right", outward = "left", hjust))

  if (hjust <= 0.25)
    return("left")
  if (hjust >= 0.75)
    return("right")
  return("center")
}

ggplotVjustToPlotlyYAnchor <- function(vjust) {
  if (is.character(vjust))
    return(switch(vjust, inward = "top", outward = "bottom", vjust))

  if (vjust <= 0.25)
    return("bottom")
  if (vjust >= 0.75)
    return("top")
  return("middle")
}

normalizeSubplotSpans <- function(spans) {
  spans <- as.numeric(spans)

  if (length(spans) == 0L || anyNA(spans) || any(spans <= 0))
    stop2("Matrix plot widths and heights should be positive numeric vectors.")

  return(spans / sum(spans))
}

makeEmptyPlotlySubplot <- function() {
  plotly::layout(
    plotly::plot_ly(
      x = numeric(),
      y = numeric(),
      type = "scatter",
      mode = "markers",
      hoverinfo = "skip",
      showlegend = FALSE
    ),
    xaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(visible = FALSE, showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    margin = list(l = 0, r = 0, b = 0, t = 0),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    showlegend = FALSE
  )
}

stripPlotlyConfig <- function(plotlyObj) {
  plotlyObj$x$config <- NULL
  return(plotlyObj)
}

findRangeFrame <- function(ggplot_obj) {
  which(vapply(ggplot_obj@layers, FUN = \(layer) inherits(layer$geom, "GeomRangeFrame"), FUN.VALUE = logical(1L)))
}

# Extract geom_label layers from a ggplot object and convert them to plotly annotations.
# Returns a list with the modified ggplot object (labels removed) and plotly annotations.
maybeRemoveGeomLabelLayers <- function(ggplotObj) {
  idx <- which(vapply(ggplotObj@layers, FUN = \(layer) inherits(layer$geom, "GeomLabel"), FUN.VALUE = logical(1L)))
  if (length(idx) == 0L)
    return(list(ggplotObj = ggplotObj, annotations = list()))

  # build the plot to resolve data, scales, and panel ranges
  built <- ggplot2::ggplot_build(ggplotObj)
  panelParams <- built$layout$panel_params

  annotations <- list()
  for (i in idx) {
    layerData    <- built$data[[i]]
    layerGeom    <- ggplotObj@layers[[i]]$geom
    aesParams    <- ggplotObj@layers[[i]]$aes_params
    geomParams   <- ggplotObj@layers[[i]]$geom_params %||% list()

    for (row in seq_len(nrow(layerData))) {
      d <- layerData[row, , drop = FALSE]
      panel <- as.integer(d$PANEL)
      pp <- panelParams[[panel]]

      # resolve I()/AsIs NPC coordinates to data coordinates
      x_val <- d$x
      y_val <- d$y
      if (inherits(ggplotObj@layers[[i]]$data$x, "AsIs")) {
        xrange <- pp$x.range %||% pp$x_range %||% pp$x.sec$range
        x_val <- xrange[1L] + as.numeric(x_val) * diff(xrange)
      }
      if (inherits(ggplotObj@layers[[i]]$data$y, "AsIs")) {
        yrange <- pp$y.range %||% pp$y_range %||% pp$y.sec$range
        y_val <- yrange[1L] + as.numeric(y_val) * diff(yrange)
      }

      angle     <- aesParams$angle     %||% d$angle     %||% 0
      colour    <- d$colour    %||% "black"
      fill      <- d$fill      %||% "white"
      alpha     <- d$alpha     %||% NA
      labelText <- d$label     %||% ""
      fontSize  <- (d$size %||% 3.88) * ggplot2::.pt  # ggplot2 size (mm) -> pt
      hjust     <- d$hjust %||% aesParams$hjust %||% 0.5
      vjust     <- d$vjust %||% aesParams$vjust %||% 0.5

      # map ggplot2 hjust/vjust to plotly xanchor/yanchor
      xanchor <- if (is.character(hjust)) {
        switch(hjust, inward = "right", outward = "left", hjust)
      } else if (hjust <= 0.25) "left" else if (hjust >= 0.75) "right" else "center"

      yanchor <- if (is.character(vjust)) {
        switch(vjust, inward = "top", outward = "bottom", vjust)
      } else if (vjust <= 0.25) "bottom" else if (vjust >= 0.75) "top" else "middle"

      fillRGB   <- plotly::toRGB(fill,   if (is.na(alpha)) 1 else alpha)
      borderRGB <- plotly::toRGB(colour, if (is.na(alpha)) 1 else alpha)
      textRGB   <- plotly::toRGB(colour, if (is.na(alpha)) 1 else alpha)

      annotations[[length(annotations) + 1L]] <- list(
        x         = x_val,
        y         = y_val,
        text      = labelText,
        showarrow = FALSE,
        font      = list(size = fontSize, color = textRGB),
        bgcolor   = fillRGB,
        bordercolor = borderRGB,
        borderwidth = 1,
        borderpad   = 4,
        xanchor   = xanchor,
        yanchor   = yanchor,
        textangle = -angle  # ggplot2 angle is counter-clockwise, plotly is clockwise
      )
    }
  }

  # remove the label layers
  ggplotObj@layers <- ggplotObj@layers[-idx]
  return(list(ggplotObj = ggplotObj, annotations = annotations))
}

maybeRemoveRangeFrameLayer <- function(ggplotObj) {
  idx <- findRangeFrame(ggplotObj)
  if (length(idx) == 0L) return(list(ggplotObjNoRangeFrame = ggplotObj))
  ggplotObjNoRangeFrame <- ggplotObj
  rangeFrameLayer <- ggplotObjNoRangeFrame@layers[idx]
  ggplotObjNoRangeFrame@layers <- ggplotObjNoRangeFrame@layers[-idx]
  shapes <- rangeFrameLayerToShapes(ggplotObjNoRangeFrame, rangeFrameLayer)
  return(list(ggplotObjNoRangeFrame = ggplotObjNoRangeFrame, rangeFrameLayer = rangeFrameLayer, shapes = shapes))
}

rangeFrameLayerToShapes <- function(ggplotObj, rangeFrameLayer) {

  opts <- getPlotEditingOptions(ggplotObj)
  ranges <- list(
    x = range(opts$xAxis$settings$breaks),
    y = range(opts$yAxis$settings$breaks)
  )

  geom <- rangeFrameLayer$geom_rangeframe
  sides <- geom$geom_params$sides
  color <- geom$aes_params$colour %||% geom$geom$default_aes$colour
  shapes <- list()

  if (grepl("b", sides)) shapes[[length(shapes) + 1L]] <- list(
    type = "line",
    line = list(color = color, width = 1.2),
    xref = "x",
    yref = "paper",
    x0 = ranges$x[1], x1 = ranges$x[2],
    y0 = 0, y1 = 0,
    name = "rangeframe_b"
  )
  if (grepl("l", sides)) shapes[[length(shapes) + 1L]] <- list(
    type = "line",
    line = list(color = color, width = 1.2),
    xref = "paper",
    yref = "y",
    x0 = 0, x1 = 0,
    y0 = ranges$y[1], y1 = ranges$y[2],
    name = "rangeframe_l"
  )
  return(shapes)
}

fixPlotlyLegendTitle <- function(ggplotObj, plotlyObj) {
  for (scale in ggplotObj$scales$scales) {
    guide <- scale$guide
    if (is.null(guide) || identical(guide, "none") || isFALSE(guide))
      next
    title <- tryCatch(guide$title, error = function(e) NULL)
    if (is.null(title) || inherits(title, "waiver"))
      next
    title <- as.character(title)
    if (!nzchar(title))
      next
    plotlyObj$x$layout$legend$title <- list(text = title)
    return(plotlyObj)
  }
  return(plotlyObj)
}

adaptive_ggplotly_ticks <- function(p, axis = "yaxis", nticks = NULL) {
  stopifnot(inherits(p, "plotly"))

  if (!is.null(nticks)) {
    p$x$layout[[axis]]$nticks <- as.integer(nticks)
  }

  axis_json <- jsonlite::toJSON(axis, auto_unbox = TRUE)

  js <- sprintf(
    r"{
function(el, x) {
  const axisName = %s;
  const inputAxis = x.layout[axisName] || {};

  // Save the original ggplotly tick specification.
  const originalTicks = {
    tickmode: inputAxis.tickmode,
    tick0: inputAxis.tick0,
    dtick: inputAxis.dtick,
    tickvals: inputAxis.tickvals,
    ticktext: inputAxis.ticktext
  };

  // Use the range Plotly actually rendered initially.
  const initialRange = el._fullLayout[axisName].range.slice();

  let updating = false;
  let usingAutoTicks = false;

  function approximatelyEqual(a, b) {
    const scale = Math.max(1, Math.abs(a), Math.abs(b));
    return Math.abs(a - b) <= 1e-9 * scale;
  }

  el.on('plotly_relayout', function(event) {
    if (updating) return;

    const key0 = axisName + '.range[0]';
    const key1 = axisName + '.range[1]';
    const autoKey = axisName + '.autorange';

    const hasRange =
      Object.prototype.hasOwnProperty.call(event, key0) &&
      Object.prototype.hasOwnProperty.call(event, key1);

    const hasAutorange =
      Object.prototype.hasOwnProperty.call(event, autoKey) &&
      event[autoKey] === true;

    if (!hasRange && !hasAutorange) return;

    // Reset Axes normally restores the original explicit range.
    const returnedToInitial =
      hasRange &&
      approximatelyEqual(event[key0], initialRange[0]) &&
      approximatelyEqual(event[key1], initialRange[1]);

    if (returnedToInitial || hasAutorange) {
      const update = {};

      update[axisName + '.tickmode'] =
        originalTicks.tickmode == null ? 'array' : originalTicks.tickmode;

      update[axisName + '.tick0'] =
        originalTicks.tick0 == null ? null : originalTicks.tick0;

      update[axisName + '.dtick'] =
        originalTicks.dtick == null ? null : originalTicks.dtick;

      update[axisName + '.tickvals'] =
        originalTicks.tickvals == null ? null : originalTicks.tickvals;

      update[axisName + '.ticktext'] =
        originalTicks.ticktext == null ? null : originalTicks.ticktext;

      usingAutoTicks = false;
      updating = true;

      Plotly.relayout(el, update).finally(function() {
        updating = false;
      });

      return;
    }

    if (!usingAutoTicks) {
      const update = {};

      update[axisName + '.tickmode'] = 'auto';
      update[axisName + '.tick0'] = null;
      update[axisName + '.dtick'] = null;
      update[axisName + '.tickvals'] = null;
      update[axisName + '.ticktext'] = null;

      usingAutoTicks = true;
      updating = true;

      Plotly.relayout(el, update).finally(function() {
        updating = false;
      });
    }
  });
}
}", axis_json
  )

  htmlwidgets::onRender(p, js)
}

js_code_rangeframe <- r"{
function(el, x) {

  var isUpdating = false;
  var lastUpdateTime = 0;
  var updateThrottle = 16;

  var finalUpdateTimer = null;
  var finalUpdateDelay = 60;

  function axisRefToKey(axisRef) {
    // "x"  -> "xaxis"
    // "x2" -> "xaxis2"
    // "y"  -> "yaxis"
    // "y3" -> "yaxis3"
    return axisRef.charAt(0) + "axis" + axisRef.slice(1);
  }

  function getAxisTickExtent(axisRef) {

    var axisKey = axisRefToKey(axisRef);
    var axisObj = el._fullLayout[axisKey];

    if (!axisObj) {
      return null;
    }

    /*
     * axisObj._vals contains the ticks produced by
     * Plotly's most recent axis drawing pass.
     *
     * Unlike _tmin and _tmax, this is also refreshed
     * when tickmode is "array", as happens after reset
     * when the original ggplot ticks are restored.
     */
    var vals = axisObj._vals || [];

    var majorTicks = vals
      .filter(function(tick) {
        return (
          tick &&
          !tick.minor &&
          !tick.noTick &&
          tick.x !== undefined &&
          tick.x !== null &&
          isFinite(Number(tick.x))
        );
      })
      .map(function(tick) {
        return Number(tick.x);
      });

    if (majorTicks.length > 0) {
      return [
        Math.min.apply(null, majorTicks),
        Math.max.apply(null, majorTicks)
      ];
    }

    /*
     * Fallback for an axis where _vals is temporarily
     * unavailable. This is not used for the usual
     * array-tick reset path.
     */
    if (
      axisObj._tmin !== undefined &&
      axisObj._tmax !== undefined &&
      isFinite(Number(axisObj._tmin)) &&
      isFinite(Number(axisObj._tmax))
    ) {
      return [
        Number(axisObj._tmin),
        Number(axisObj._tmax)
      ];
    }

    return null;
  }

  function approximatelyEqual(a, b) {

    if (a === b) {
      return true;
    }

    if (
      typeof a !== "number" ||
      typeof b !== "number" ||
      !isFinite(a) ||
      !isFinite(b)
    ) {
      return false;
    }

    var scale = Math.max(
      1,
      Math.abs(a),
      Math.abs(b)
    );

    return Math.abs(a - b) <= 1e-10 * scale;
  }

  function scheduleRangeFrameUpdate(eventType, delay) {

    delay = delay === undefined
      ? finalUpdateDelay
      : delay;

    if (finalUpdateTimer !== null) {
      clearTimeout(finalUpdateTimer);
    }

    finalUpdateTimer = setTimeout(
      function runFinalUpdate() {

        finalUpdateTimer = null;

        /*
         * A previous shape update may still be running.
         * Reschedule rather than losing the reset update.
         */
        if (isUpdating) {
          finalUpdateTimer = setTimeout(
            runFinalUpdate,
            finalUpdateDelay
          );

          return;
        }

        updateRangeFrame(
          null,
          eventType,
          true
        );
      },
      delay
    );
  }

  function updateRangeFrame(eventdata, eventType, force) {

    force = force || false;

    var now = Date.now();

    if (
      isUpdating ||
      (
        !force &&
        now - lastUpdateTime < updateThrottle
      )
    ) {
      return;
    }

    var currentShapes = el.layout.shapes || [];

    if (currentShapes.length === 0) {
      return;
    }

    var shapes = currentShapes.map(function(shape) {
      return Object.assign({}, shape);
    });

    var changed = false;

    function updateValue(object, property, value) {

      if (
        !approximatelyEqual(
          Number(object[property]),
          Number(value)
        )
      ) {
        object[property] = value;
        changed = true;
      }
    }

    shapes.forEach(function(shape) {

      if (
        !shape.name ||
        !shape.name.startsWith("rangeframe")
      ) {
        return;
      }

      var tickExtent;

      /*
       * Horizontal rangeframe:
       * use the first and last rendered x-axis ticks.
       */
      if (
        shape.xref &&
        shape.xref !== "paper"
      ) {
        tickExtent = getAxisTickExtent(shape.xref);

        if (tickExtent === null) {
          return;
        }

        updateValue(
          shape,
          "x0",
          tickExtent[0]
        );

        updateValue(
          shape,
          "x1",
          tickExtent[1]
        );

      /*
       * Vertical rangeframe:
       * use the first and last rendered y-axis ticks.
       */
      } else if (
        shape.yref &&
        shape.yref !== "paper"
      ) {
        tickExtent = getAxisTickExtent(shape.yref);

        if (tickExtent === null) {
          return;
        }

        updateValue(
          shape,
          "y0",
          tickExtent[0]
        );

        updateValue(
          shape,
          "y1",
          tickExtent[1]
        );
      }
    });

    /*
     * Avoid an infinite:
     *
     * afterplot
     * -> shape relayout
     * -> afterplot
     *
     * cycle.
     */
    if (!changed) {
      return;
    }

    isUpdating = true;
    lastUpdateTime = now;

    Plotly.relayout(
      el,
      { shapes: shapes }
    )
    .then(function() {
      isUpdating = false;

      /*
       * Make one final check after Plotly has applied
       * the shape update.
       */
      scheduleRangeFrameUpdate(
        "rangeframe_relayout_complete",
        finalUpdateDelay
      );
    })
    .catch(function(err) {
      isUpdating = false;

      console.error(
        "Error updating rangeframe:",
        eventType,
        err
      );
    });
  }

  /*
   * Continuous update while zooming or panning.
   */
  el.on(
    "plotly_relayouting",
    function(eventdata) {
      updateRangeFrame(
        eventdata,
        "plotly_relayouting",
        false
      );
    }
  );

  /*
   * Final update after zoom, pan, Reset Axes,
   * Autoscale, or adaptive-tick relayout.
   */
  el.on(
    "plotly_relayout",
    function(eventdata) {

      updateRangeFrame(
        eventdata,
        "plotly_relayout",
        false
      );

      scheduleRangeFrameUpdate(
        "plotly_relayout_final",
        finalUpdateDelay
      );
    }
  );

  /*
   * At this point Plotly has redrawn the axes and
   * axisObj._vals contains the newly rendered ticks.
   */
  el.on(
    "plotly_afterplot",
    function() {
      scheduleRangeFrameUpdate(
        "plotly_afterplot",
        0
      );
    }
  );

  /*
   * Double-click occurs near the beginning of the
   * reset. The afterplot handler performs the actual
   * post-reset update.
   */
  el.on(
    "plotly_doubleclick",
    function() {
      scheduleRangeFrameUpdate(
        "plotly_doubleclick",
        finalUpdateDelay
      );
    }
  );

  /*
   * Retain the custom/nonstandard event from the
   * original implementation.
   */
  el.on(
    "plotly_framework",
    function(eventdata) {

      updateRangeFrame(
        eventdata,
        "plotly_framework",
        false
      );

      scheduleRangeFrameUpdate(
        "plotly_framework_final",
        finalUpdateDelay
      );
    }
  );

  /*
   * Initial alignment after the widget has rendered.
   */
  setTimeout(
    function() {
      scheduleRangeFrameUpdate(
        "initial_update",
        0
      );
    },
    100
  );
}
}"
