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

  e <- try({

    # we need to remove the rangeframe layer, if there is one
    temp <- maybeRemoveRangeFrameLayer(ggplotObj = ggplotObj)

    pNoRangeframe <- temp$ggplotObjNoRangeFrame
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
      if (!is.null(plotlyplotje$x$layout$shapes) &&
          plotlyplotje$x$layout$shapes[[1L]]$type == "rect" &&
          plotlyplotje$x$layout$shapes[[1L]]$fillcolor == "transparent"
          ) {
        plotlyplotje$x$layout$shapes <- NULL
      }

      sides <- temp$rangeFrameLayer$geom_rangeframe$geom_params$sides
      if (grepl("b", sides) || grepl("l", sides))
        hasRangeFrame <- TRUE

      if (grepl("b", sides))
        plotlyplotje$x$layout$xaxis$tickmode <- "auto"

      if (grepl("l", sides))
        plotlyplotje$x$layout$yaxis$tickmode <- "auto"

      plotlyplotje <- htmlwidgets::onRender(plotlyplotje, jsCode = js_code_rangeframe)

    }

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

js_code_rangeframe <- "
(function(el, x) {

  var gd = el;

  // TODO: we only want to do the stuff below if tickmode is not array!

  var isUpdating = false;
  var lastUpdateTime = 0;
  var updateThrottle = 16; // milliseconds (roughly 60fps)

  function updateRangeFrame(eventdata, eventType) {

    console.log('updateRangeFrame got called!');
    console.log('Event type:', eventType);
    console.log(gd);
    console.log('eventdata:', eventdata);
    console.log('eventdata:', gd._fullLayout._draggers);

    var xaxis = gd.layout.xaxis;
    var yaxis = gd.layout.yaxis;
    var xaxis2 = gd._fullLayout.xaxis;
    var yaxis2 = gd._fullLayout.yaxis;

    // Check if this is a box select/zoom event and ignore it
    if (false && eventdata) {

        // test if the event lies within the current axes ranges
        var hasXUpdate = eventdata['xaxis.range[0]'] !== undefined ||
                          (eventdata['xaxis.range'] !== undefined) ||
                          (eventdata.xaxis && eventdata.xaxis.range);

        var hasYUpdate = eventdata['yaxis.range[0]'] !== undefined ||
                         (eventdata['yaxis.range'] !== undefined) ||
                         (eventdata.yaxis && eventdata.yaxis.range);

      if (hasXUpdate || hasYUpdate) {
          console.log('Ignoring range update during zoom dragmode');
          return;
      }
    }

    // Prevent recursive calls and throttle updates
    var now = Date.now();
    if (isUpdating || (now - lastUpdateTime < updateThrottle)) {
      return;
    }


    // Get tick ranges - these should ALWAYS be the actual tick positions, never the visible range
    var x0 = xaxis2._tmin;
    var x1 = xaxis2._tmax;
    var y0 = yaxis2._tmin;
    var y1 = yaxis2._tmax;
    var xmin = xaxis2._r[0];
    var ymin = yaxis2._r[0];

    var xmin2 = gd.layout.xaxis.range[0];
    var ymin2 = gd.layout.yaxis.range[0];

    // Get the current range - handle different event data structures
    var x0a, x1a, y0a, y1a;

    // Check for different possible eventdata structures
    if (eventdata) {
      if (eventdata['xaxis.range[0]'] !== undefined && eventdata['yaxis.range[0]'] !== undefined) {
        // Standard relayouting events
        x0a = eventdata['xaxis.range[0]'];
        x1a = eventdata['xaxis.range[1]'];
        y0a = eventdata['yaxis.range[0]'];
        y1a = eventdata['yaxis.range[1]'];
        xmin = Math.min(x0a, xmin);
        ymin = Math.min(y0a, ymin);
      } else if (eventdata['xaxis.range[0]'] !== undefined) {
        // Alternative eventdata structure
        x0a = eventdata['xaxis.range[0]'];
        x1a = eventdata['xaxis.range[1]'];
        xmin = Math.min(x0a, xmin);
        y0a = y0;
        y1a = y1;
      } else if (eventdata['yaxis.range[0]'] !== undefined) {
        // Check for axis-specific updates
        x0a = x0;
        x1a = x1;
        y0a = eventdata['yaxis.range[0]'];
        y1a = eventdata['yaxis.range[1]'];
        ymin = Math.min(y0a, ymin);
        //ymin = y0a;
      } else {
        // Fallback to _r ranges when eventdata is not available or doesn't contain range info
        console.log('Falling back to _r ranges');
        x0a = xaxis2._r[0];
        x1a = xaxis2._r[1];
        y0a = yaxis2._r[0];
        y1a = yaxis2._r[1];
      }
    }

    // Get current shapes from the layout
    var currentShapes = gd.layout.shapes || [];

    // TODO: the color should use currentShapes[0].line.color, but somehow that doesn't work?
    // could also perhaps clone the currentShapes and only update the coordinates?
    var newShapes = [
      {
        type: 'line', xref: 'x', yref: 'paper',
        x0: x0, x1: x1, y0: 0, y1: 0,  // Horizontal line from min to max x-break, at bottom of visible y-range
        line: { color: 'black', width: 1.2},
        name: 'rangeframe_b'
      },
      {
        type: 'line', xref: 'paper', yref: 'y',
        x0: 0, x1: 0, y0: y0, y1: y1,  // Vertical line from min to max y-break, at left of visible x-range
        line: { color: 'black', width: 1.2},
        name: 'rangeframe_l'
      }
    ];


    // Filter out non-rangeframe shapes and compare
    var currentRangeframeShapes = currentShapes.filter(shape =>
      shape.name && (shape.name === 'rangeframe_b' || shape.name === 'rangeframe_l')
    );

    // Check if the new shapes are the same as the current ones
    var shapesEqual = true;
    if (currentRangeframeShapes.length === newShapes.length) {
      for (var i = 0; i < newShapes.length; i++) {
        var newShape = newShapes[i];
        var currentShape = currentRangeframeShapes.find(shape => shape.name === newShape.name);

        if (!currentShape ||
            Math.abs(currentShape.x0 - newShape.x0) > 1e-10 ||
            Math.abs(currentShape.x1 - newShape.x1) > 1e-10 ||
            Math.abs(currentShape.y0 - newShape.y0) > 1e-10 ||
            Math.abs(currentShape.y1 - newShape.y1) > 1e-10// ||
           ) {
          shapesEqual = false;
          break;
        }
      }
    } else {
      shapesEqual = false;
    }

    // Only update if shapes are different
    if (!shapesEqual) {
      //console.log('Updating shapes...');
      console.log([currentRangeframeShapes, newShapes]);
      isUpdating = true;
      lastUpdateTime = now;

      // Merge existing non-rangeframe shapes with new rangeframe shapes
      var otherShapes = currentShapes.filter(shape =>
        !shape.name || (shape.name !== 'rangeframe_b' && shape.name !== 'rangeframe_l')
      );
      var allShapes = otherShapes.concat(newShapes);

      Plotly.relayout(gd, { shapes: allShapes })
        .then(function() {
          isUpdating = false;
        })
        .catch(function(error) {
          console.error('Error updating range frame:', error);
          isUpdating = false;
        });
    } else {
      console.log('Shapes are the same. No update needed.');
    }
  }
  // Attach event listeners for all relevant events
  gd.on('plotly_relayouting',   function(eventdata) {updateRangeFrame(eventdata,  'plotly_relayouting');});
  gd.on('plotly_relayout',      function(eventdata) { updateRangeFrame(eventdata, 'plotly_relayout'); });     // Fires after panning/zooming ends
  //gd.on('plotly_zoom',        function(eventdata) { updateRangeFrame(eventdata, 'plotly_zoom'); });         // Fires after zooming ends
  //gd.on('plotly_pan',         function(eventdata) { updateRangeFrame(eventdata, 'plotly_pan'); });          // Fires after panning ends
  //gd.on('plotly_autosize',    function(eventdata) { updateRangeFrame(eventdata, 'plotly_autosize'); });         // Fires during autosizing
  //gd.on('plotly_doubleclick', function(eventdata) { updateRangeFrame(eventdata, 'plotly_doubleclick'); });        // Fires on double-click (reset zoom)


  // Add a more comprehensive listener for any layout changes
  gd.on('plotly_framework',   function(eventdata) {
    console.log('plotly_framework event:', eventdata);
    updateRangeFrame(eventdata, 'plotly_framework');
  });

  // Add a periodic check as fallback for any missed events (like axis dragging)
  var lastKnownRange = null;

  // Initial update
  setTimeout(function() { updateRangeFrame(null, 'initial_update'); }, 500);
});
"

