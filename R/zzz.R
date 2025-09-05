.onLoad <- function(libname, pkgname) {

  # have to do this during onLoad because these S3 generics are not exported by plotly
  plotlyNamespace <- asNamespace("plotly")
  registerS3method("to_basic",   "abline2", to_basic.abline2,   envir = plotlyNamespace)
  # registerS3method("geom2trace", "abline2", geom2trace.abline2, envir = plotlyNamespace)

  registerS3method("to_basic",   "alignedtext", to_basic.alignedtext,   envir = plotlyNamespace)
  # registerS3method("geom2trace", "alignedtext", geom2trace.alignedtext, envir = plotlyNamespace)

}
