\dontrun{
demo <- function(n = 1e6) {
  # adapted from https://github.com/tidyverse/ggplot2/issues/4056
  big_object <- rnorm(n)
  df <- data.frame(x = -5:5, y = abs(-5:5))
  ggplot2::ggplot(df, ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::geom_point(ggplot2::aes(x = x+1, y = y+1), color = "green")
}

sizeOnDisk <- function(obj) {
  f <- tempfile()
  saveRDS(obj, file = f)
  sz <- file.size(f)
  file.remove(f)
  class(sz) <- "object_size"
  sz
}

obj <- demo()
obj

# object.size lies about the size but
print(object.size(obj), units = "auto")
# 7.1 Kb

print(object.size(obj$plot_env$big_object), units = "auto")
# 7.6 Mb

# lobstr::obj_size follows environments and does not lie about the size
if (require("lobstr")) {
  sz <- lobstr::obj_size(obj)
  class(sz) <- "object_size"
  print(sz, units = "auto")
  # 8.5 Mb
}

print(sizeOnDisk(obj), units = "auto")
# 7.4 Mb

# note that to an extent, removeEnvironments modifies in place
obj <- removeEnvironments(obj)

obj # still works

print(sizeOnDisk(obj), units = "auto")
# 33 Kb

# removeEnvironments may break plots that rely on looking things up in the parent environment
demoBad <- function() {
  x = -5:5
  y = abs(-5:5)
  ggplot2::ggplot(mapping = ggplot2::aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::geom_point(ggplot2::aes(x = x+1, y = y+1), color = "red")
}

obj <- demoBad()
obj # works

obj <- removeEnvironments(obj)
obj # Error in FUN(X[[i]], ...) : object 'x' not found

}
