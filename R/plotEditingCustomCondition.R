warningPlotEditing <- function(message, call = sys.call(-1L), ...) {
  signalCondition(structure(
    class = c("plotediting", "condition"),
    list(message = message, call = call, ...)
  ))
}
