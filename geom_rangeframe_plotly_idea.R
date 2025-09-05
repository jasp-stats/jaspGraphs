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
  // var gd = document.getElementById('%s');

  console.log('got called!');
  console.log(el);
  console.log(x);

  var gd = el;
  // if (!gd) return;

  function updateRangeFrame() {

    console.log('updateRangeFrame got called!');
    console.log(gd);
    var xaxis = gd.layout.xaxis;
    var yaxis = gd.layout.yaxis;

    var xaxis2 = gd._fullLayout.xaxis;
    var yaxis2 = gd._fullLayout.yaxis;
    console.log({xaxis, yaxis});
    console.log({xaxis2, yaxis2});
    console.log({x:xaxis2._r, y:yaxis2._r});

    if (!xaxis || !yaxis) return;

    //var x0a = xaxis.range[0];
    //var x1a = xaxis.range[1];
    //var y0a = yaxis.range[0];
    //var y1a = yaxis.range[1];
    var x0a = xaxis2._r[0];
    var x1a = xaxis2._r[1];
    var y0a = yaxis2._r[0];
    var y1a = yaxis2._r[1];
    var x0 = xaxis2._tmin;
    var x1 = xaxis2._tmax;
    var y0 = yaxis2._tmin;
    var y1 = yaxis2._tmax;
    console.log({x0, x1, y0, y1});
    console.log({x0a, x1a, y0a, y1a});

    var newShapes = [
      {
        type: 'line', xref: 'x', yref: 'y',
        x0: x0, x1: x1, y0: y0a, y1: y0a,
        line: { color: 'darkblue', width: 1.2},
        name: 'rangeframe_b'
      },
      {
        type: 'line', xref: 'x', yref: 'y',
        x0: x0a, x1: x0a, y0: y0, y1: y1,
        line: { color: 'darkblue', width: 1.2},
        name: 'rangeframe_l'
      }
    ];

    //Plotly.relayout(gd, { shapes: shapes });
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
            currentShape.x0 !== newShape.x0 ||
            currentShape.x1 !== newShape.x1 ||
            currentShape.y0 !== newShape.y0 ||
            currentShape.y1 !== newShape.y1) {
          shapesEqual = false;
          break;
        }
      }
    } else {
      shapesEqual = false;
    }

    // Only update if shapes are different
    if (!shapesEqual) {
      console.log('Updating shapes...');
      Plotly.relayout(gd, { shapes: newShapes })
        .catch(function(error) {
          console.error('Error updating range frame:', error);
        });
    } else {
      console.log('Shapes are the same. No update needed.');
    }
  }

  // Attach event listeners for all relevant events
  gd.on('plotly_relayouting', updateRangeFrame);  // Fires continuously during panning/zooming
  gd.on('plotly_relayout', updateRangeFrame);     // Fires after panning/zooming ends
  gd.on('plotly_zoom', updateRangeFrame);         // Fires after zooming ends
  gd.on('plotly_pan', updateRangeFrame);          // Fires after panning ends

  // Initial update
  setTimeout(updateRangeFrame, 500);
});
"

fig <- layout(fig, title = 'Highlighting with Lines', shapes = lines)
fig$x$elementId <- "myPlotlyRangeFrame"

fig2 <- htmlwidgets::onRender(fig, jsCode = sprintf(js_code, fig$x$elementId))
fig2

