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

    # we need to remove the rangeframe layer, if there is one
    temp <- maybeRemoveRangeFrameLayer(ggplotObj = ggplotObj)

    # plotly does not support geom_label (https://github.com/plotly/plotly.R/issues/2425)
    # we extract them before ggplotly() and convert them to plotly annotations,
    # which support background color, border, and text angle.
    temp2 <- maybeRemoveGeomLabelLayers(temp$ggplotObjNoRangeFrame)

    pNoRangeframe <- temp2$ggplotObj
    plotlyplotje <- ggplotly(pNoRangeframe)
    hasRangeFrame <- FALSE

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

      if (grepl("b", sides))
        for (ax in xaxes)
          plotlyplotje$x$layout[[ax]]$tickmode <- "auto"

      if (grepl("l", sides))
        for (ax in yaxes)
          plotlyplotje$x$layout[[ax]]$tickmode <- "auto"

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

    plotlybuild <- plotly::plotly_build(plotlyplotje)

    # TODO: we should decode any column names in the data in plotlybuild$x... maybe we can do this through ggplot2 though
    if (returnJSON) {
      json <- toJSON(list(data = plotlybuild$x$data, layout = plotlybuild$x$layout, hasRangeFrame = hasRangeFrame))
      json
    } else {
      plotlybuild
    }
  })
  return(e)
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

js_code_rangeframe <- r"{
function(el, x) {

console.log(el);

  var isUpdating = false;
  var lastUpdateTime = 0;
  var updateThrottle = 16; // ~60fps

  function getAxisRange(el, axisRef, eventdata) {
    // axisRef is "x", "x2", "y", "y3", etc.
    var axisKey = axisRef[0] + "axis" + axisRef.slice(1); // "x2" -> "xaxis2"
    var axisObj = el._fullLayout[axisKey];

    var tmin = axisObj._tmin;
    var tmax = axisObj._tmax;

    // console.log("Axis", axisRef, "range:", tmin, tmax);
    return [tmin, tmax];
  }

  function updateRangeFrame(eventdata, eventType) {

    var now = Date.now();
    if (isUpdating || (now - lastUpdateTime < updateThrottle)) {
      return;
    }

    var shapes = el.layout.shapes || [];
    if (shapes.length === 0) return;

    // Loop over shapes that are rangeframes
    shapes.forEach(function(shape) {

      // console.log("Processing shape:", shape);
      // Update only rangeframe shapes
      if (!(shape.name && shape.name.startsWith("rangeframe"))) return;

      var axisRange;
      if (shape.xref !== "paper") {
        axisRange = getAxisRange(el, shape.xref, eventdata);
        shape.x0 = axisRange[0];
        shape.x1 = axisRange[1];
      } else {
        axisRange = getAxisRange(el, shape.yref, eventdata);
        shape.y0 = axisRange[0];
        shape.y1 = axisRange[1];
      }
    });

    isUpdating = true;
    lastUpdateTime = now;

    Plotly.relayout(el, { shapes: shapes })
    .then(() => { isUpdating = false; })
    .catch(err => { console.error("Error updating rangeframe:", err); isUpdating = false; });
  }

  el.on('plotly_relayouting',   e => updateRangeFrame(e, 'plotly_relayouting'));
  el.on('plotly_relayout',      e => updateRangeFrame(e, 'plotly_relayout'));
  el.on('plotly_framework',     e => updateRangeFrame(e, 'plotly_framework'));

  // Initial call
  setTimeout(() => updateRangeFrame(null, 'initial_update'), 500);
}
}"


