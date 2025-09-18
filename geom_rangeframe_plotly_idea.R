library(plotly)

s <- seq.int(0, 15)
fig <- plot_ly(x = ~s, y = ~sin(s), mode = "lines")

# initiate a line shape object
line <- list(
  type = "line",
  line = list(color = "pink"),
  xref = "x",
  yref = "y"
)

lines <- list()
for (i in c(0, 3, 5, 7, 9, 13)) {
  line[["x0"]] <- i
  line[["x1"]] <- i + 2
  line[c("y0", "y1")] <- sin(i + 1)
  lines <- c(lines, list(line))
}

lines <- list(
  list(
    type = "line",
    line = list(color = "pink"),
    xref = "x",
    yref = "y",
    x0 = 0, x1 = 14,
    y0 = 0, y1 = 0,
    name = "rangeframe_l"
  ),
  list(
    type = "line",
    line = list(color = "pink"),
    xref = "x",
    yref = "y",
    x0 = 0, x1 = 0,
    y0 = -1, y1 = 1,
    name = "rangeframe_b"
  )
)

js_code <- "
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

    var newShapes = [
      {
        type: 'line', xref: 'x', yref: 'paper',
        x0: x0, x1: x1, y0: 0, y1: 0,  // Horizontal line from min to max x-break, at bottom of visible y-range
        line: { color: 'darkblue', width: 1.2},
        name: 'rangeframe_b'
      },
      {
        type: 'line', xref: 'paper', yref: 'y',
        x0: 0, x1: 0, y0: y0, y1: y1,  // Vertical line from min to max y-break, at left of visible x-range
        line: { color: 'darkblue', width: 1.2},
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

xyax <- list(
  zeroline = FALSE,
  showline = FALSE
)
fig <- layout(fig, title = 'Highlighting with Lines', shapes = lines,
              xaxis = xyax,
              yaxis = xyax)
fig$x$elementId <- "myPlotlyRangeFrame"

fig2 <- htmlwidgets::onRender(fig, jsCode = js_code)
# fig2
options("viewer" = NULL)#  utils::browseURL)
print(fig2)

dd <- read.csv("~/github/jasp/jasp-desktop/Resources/Data Sets/debug.csv")
p <- jaspGraphs::jaspHistogram(dd$contNormal)
xyax <- list(
  zeroline = FALSE,
  showline = FALSE
)
p2 <- ggplotly(p)

lines <- list(
  list(
    type = "line",
    line = list(color = "blue"),
    xref = "x",
    yref = "paper",
    x0 = -4, x1 = 4,
    y0 = 0, y1 = 0,
    name = "rangeframe_l"
  ),
  list(
    type = "line",
    line = list(color = "green"),
    xref = "paper",
    yref = "y",
    x0 = 0, x1 = 0,
    y0 = 0, y1 = 30,
    name = "rangeframe_b"
  )
)

# TODO: the size of the line should account for the width of the tick!
p2 <- layout(p2, shapes = lines, xaxis = xyax, yaxis = xyax)
p2$x$layout$shapes <- list()
p2$x$layout$shapes <- p2$x$layoutAttrs[[1]]$shapes
p2$x$layout$xaxis$tickmode <- "auto"
p2$x$layout$yaxis$tickmode <- "auto"
p2

p2$x$layout$xaxis$range
p2$x$layout$xaxis$ticks
p2$x$layout$xaxis$
p2

p3 <- htmlwidgets::onRender(p2, jsCode = js_code)
p3


#
# info <- jaspGraphs:::plotEditingOptions(p)
# p1 <- plotly::ggplotly(p)
# lines <- list(
#   list(
#     type = "line",
#     line = list(color = "pink"),
#     xref = "x",
#     yref = "y",
#     x0 = info$xAxis$settings$limits[1], x1 = info$xAxis$settings$limits[1],
#     y0 = info$yAxis$settings$breaks[1], y1 = info$yAxis$settings$breaks[length(info$yAxis$settings$breaks)],
#     name = "rangeframe_l"
#   ),
#   list(
#     type = "line",
#     line = list(color = "pink"),
#     xref = "x",
#     yref = "y",
#     x0 = info$xAxis$settings$breaks[1], x1 = info$xAxis$settings$breaks[length(info$xAxis$settings$breaks)],
#     y0 = info$yAxis$settings$limits[1], y1 = info$yAxis$settings$limits[1],
#     name = "rangeframe_b"
#   )
# )
# p1b <- layout(p1, shapes = lines)
# p1b$x$elementId <- "myPlotlyRangeFrame"
# p2 <- htmlwidgets::onRender(p1b, jsCode = sprintf(js_code, p$x$elementId))
# p2

