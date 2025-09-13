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

# Two bugs with the x-axis, the y-axis works fine.
# 1. if I drag the y-axis, only the y-axis updates correctly.
# The x-axis does not update its y-coordinate.
# 2. if I drag the x-axis, the y-axis updates correctly.
# However, while moving, the x-axis line becomes diagonal!
# Its x-coordinates are correct, but both the left and right y coordinate are wrong.
# The rightmost is largest
# nice to have:while selecting a region, the lines are drawn at incorrect positions.
# I think we should figure out what event type is triggered while selecting a region,
# and just make sure the callback is _not_ trigered then.
js_code <- "
(function(el, x) {
  // var gd = document.getElementById('%s');



  console.log('got called!');
  console.log(el);
  console.log(x);

  var gd = el;
  // if (!gd) return;

  var isUpdating = false;
  var lastUpdateTime = 0;
  var updateThrottle = 16; // milliseconds (roughly 60fps)

  function updateRangeFrame(eventdata, eventType) {

    console.log('updateRangeFrame got called!');
    console.log('Event type:', eventType);
    console.log(gd);
    console.log('eventdata:', eventdata);
    console.log('eventdata:', gd._fullLayout._draggers);

    // Check if this is a box select/zoom event and ignore it
    if (eventdata) {

        var hasXUpdate = eventdata['xaxis.range[0]'] !== undefined ||
                          (eventdata['xaxis.range'] !== undefined) ||
                          (eventdata.xaxis && eventdata.xaxis.range);
        var hasYUpdate = eventdata['yaxis.range[0]'] !== undefined ||
                         (eventdata['yaxis.range'] !== undefined) ||
                         (eventdata.yaxis && eventdata.yaxis.range);

      console.log({hasXUpdate, hasYUpdate});
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

    var xaxis = gd.layout.xaxis;
    var yaxis = gd.layout.yaxis;

    var xaxis2 = gd._fullLayout.xaxis;
    var yaxis2 = gd._fullLayout.yaxis;
    //console.log({xaxis, yaxis});
    //console.log({xaxis2, yaxis2});
    //console.log({x:xaxis2._r, y:yaxis2._r});

    if (!xaxis || !yaxis) return;

    // Get the current range - handle different event data structures
    var x0a, x1a, y0a, y1a;

    // Check for different possible eventdata structures
    if (eventdata && eventdata['xaxis.range[0]'] !== undefined) {
      // Standard relayouting events
      x0a = eventdata['xaxis.range[0]'];
      x1a = eventdata['xaxis.range[1]'];
      y0a = eventdata['yaxis.range[0]'];
      y1a = eventdata['yaxis.range[1]'];
    } else if (eventdata && eventdata['xaxis.range'] !== undefined) {
      // Alternative eventdata structure
      x0a = eventdata['xaxis.range'][0];
      x1a = eventdata['xaxis.range'][1];
      y0a = eventdata['yaxis.range'] ? eventdata['yaxis.range'][0] : yaxis2._r[0];
      y1a = eventdata['yaxis.range'] ? eventdata['yaxis.range'][1] : yaxis2._r[1];
    } else if (eventdata && (eventdata.xaxis || eventdata.yaxis)) {
      // Check for axis-specific updates
      if (eventdata.xaxis && eventdata.xaxis.range) {
        x0a = eventdata.xaxis.range[0];
        x1a = eventdata.xaxis.range[1];
      } else {
        x0a = xaxis2._r[0];
        x1a = xaxis2._r[1];
      }
      if (eventdata.yaxis && eventdata.yaxis.range) {
        y0a = eventdata.yaxis.range[0];
        y1a = eventdata.yaxis.range[1];
      } else {
        y0a = yaxis2._r[0];
        y1a = yaxis2._r[1];
      }
    } else {
      // Fallback to _r ranges when eventdata is not available or doesn't contain range info
      x0a = xaxis2._r[0];
      x1a = xaxis2._r[1];
      y0a = yaxis2._r[0];
      y1a = yaxis2._r[1];
    }

    // Get tick ranges - these should ALWAYS be the actual tick positions, never the visible range
    var x0 = xaxis2._tmin;
    var x1 = xaxis2._tmax;
    var y0 = yaxis2._tmin;
    var y1 = yaxis2._tmax;

    //console.log({x0, x1, y0, y1});
    //console.log({x0a, x1a, y0a, y1a});
    var newShapes = [
      {
        type: 'line', xref: 'x', yref: 'y',
        x0: x0, x1: x1, y0: y0a, y1: y0a,  // Horizontal line from min to max x-break, at bottom of visible y-range
        line: { color: 'darkblue', width: 1.2},
        name: 'rangeframe_b'
      },
      {
        type: 'line', xref: 'x', yref: 'y',
        x0: x0a, x1: x0a, y0: y0, y1: y1,  // Vertical line from min to max y-break, at left of visible x-range
        line: { color: 'darkblue', width: 1.2},
        name: 'rangeframe_l'
      }
    ];

    // Get current shapes from the layout
    var currentShapes = gd.layout.shapes || [];

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
            Math.abs(currentShape.y1 - newShape.y1) > 1e-10) {
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
  }  // Attach event listeners for all relevant events
  gd.on('plotly_relayouting', function(eventdata) {
    // Only respond to relayouting if it's not a box select
    if (eventdata && (eventdata['selections'] ||
                     (eventdata['xaxis.range[0]'] !== undefined && eventdata['yaxis.range[0]'] !== undefined))) {
      console.log('Ignoring plotly_relayouting during box select');
      return;
    }
    updateRangeFrame(eventdata, 'plotly_relayouting');
  });  // Fires continuously during panning/zooming
  gd.on('plotly_relayout',    function(eventdata) { updateRangeFrame(eventdata, 'plotly_relayout'); });     // Fires after panning/zooming ends
  gd.on('plotly_zoom',        function(eventdata) { updateRangeFrame(eventdata, 'plotly_zoom'); });         // Fires after zooming ends
  gd.on('plotly_pan',         function(eventdata) { updateRangeFrame(eventdata, 'plotly_pan'); });          // Fires after panning ends
  gd.on('plotly_autosize',    function(eventdata) { updateRangeFrame(eventdata, 'plotly_autosize'); });         // Fires during autosizing
  gd.on('plotly_doubleclick', function(eventdata) { updateRangeFrame(eventdata, 'plotly_doubleclick'); });        // Fires on double-click (reset zoom)

  // Add a more comprehensive listener for any layout changes
  gd.on('plotly_framework',   function(eventdata) {
    console.log('plotly_framework event:', eventdata);
    updateRangeFrame(eventdata, 'plotly_framework');
  });

  // Add a periodic check as fallback for any missed events (like axis dragging)
  var lastKnownRange = null;
  var periodicCheck = setInterval(function() {
    if (gd._fullLayout && gd._fullLayout.xaxis && gd._fullLayout.yaxis) {
      var currentRange = {
        x: [gd._fullLayout.xaxis._r[0], gd._fullLayout.xaxis._r[1]],
        y: [gd._fullLayout.yaxis._r[0], gd._fullLayout.yaxis._r[1]]
      };

      if (!lastKnownRange ||
          Math.abs(lastKnownRange.x[0] - currentRange.x[0]) > 1e-10 ||
          Math.abs(lastKnownRange.x[1] - currentRange.x[1]) > 1e-10 ||
          Math.abs(lastKnownRange.y[0] - currentRange.y[0]) > 1e-10 ||
          Math.abs(lastKnownRange.y[1] - currentRange.y[1]) > 1e-10) {

        console.log('Periodic check detected range change');
        lastKnownRange = currentRange;
        updateRangeFrame(null, 'periodic_check');
      }
    }
  }, 100); // Check every 100ms

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

# dd <- read.csv("~/github/jasp/jasp-desktop/Resources/Data Sets/debug.csv")
# p <- jaspGraphs::jaspHistogram(dd$contNormal)
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

