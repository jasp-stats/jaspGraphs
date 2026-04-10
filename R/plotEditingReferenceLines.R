
referencesToGeoms <- function(references) {

  # this is a bit ugly, but allows references to return multiple geoms
  result <- list()
  idx <- 1L
  for (i in seq_along(references)) {
    ref <- references[[i]]
    name <- referenceToName(ref)
    tmp <- referenceToGeom(ref)
    if (ggplot2::is_layer(tmp)) {
      attr(tmp, "hash") <- name
      result[[idx]] <- tmp
      names(result)[idx] <- name
      idx <- idx + 1L
    } else {
      ntmp <- length(tmp)
      fromTo <- idx:(idx + ntmp - 1L)
      for (j in seq_along(tmp)) {
        attr(tmp[[j]], "hash") <- name
      }
      result[fromTo] <- tmp
      names(result)[fromTo] <- name
      idx <- idx + ntmp
    }
  }

  return(result)
}

referenceToGeom <- function(references) {
  if (references[["point"]]) return(referenceToPoint(references))
  if (references[["horizontal"]]) return(referenceToHLine(references))
  return(referenceToVLine(references))
}

referenceToHLine <- function(reference) {
  # ggplot2::geom_hline(yintercept = reference$y)
  color    <- reference$color    %||% "black"
  lwd      <- reference$linewidth %||% 1
  lty      <- linetypeFromInt(reference$linetype %||% 0L)
  df <- data.frame(x = I(.985), y = reference$y, text = reference$text)
  if (reference$text == "")
    return(ggplot2::geom_hline(data = df, mapping = aes(yintercept = y), colour = color, linewidth = lwd, linetype = lty))
  else return(list(
    ggplot2::geom_hline(data = df, mapping = aes(yintercept = y), colour = color, linewidth = lwd, linetype = lty),
    ggplot2::geom_label(data = df, mapping = aes(x = x, y = y, label = text), hjust = "inward")
  ))
}

referenceToVLine <- function(reference) {
  color    <- reference$color    %||% "black"
  lwd      <- reference$linewidth %||% 1
  lty      <- linetypeFromInt(reference$linetype %||% 0L)
  df <- data.frame(x = reference$x, y = I(.985), text = reference$text)
  if (reference$text == "")
    return(ggplot2::geom_vline(data = df, mapping = aes(xintercept = x), colour = color, linewidth = lwd, linetype = lty))
  else return(list(
    ggplot2::geom_vline(data = df, mapping = aes(xintercept = x), colour = color, linewidth = lwd, linetype = lty),
    ggplot2::geom_label(data = df, mapping = aes(x = x, y = y, label = text), hjust = "inward")
  ))
}

referenceToPoint <- function(reference) {
  color <- reference$color     %||% "black"
  sz    <- reference$linewidth %||% 1
  shp   <- shapeFromInt(reference$linetype %||% 0L)
  df <- data.frame(x = reference$x, y = reference$y, text = reference$text)
  if (reference$text == "")
    return(ggplot2::geom_point(data = df, mapping = aes(x = x, y = y), colour = color, size = sz, shape = shp))
  else return(list(
    ggplot2::geom_point(data = df, mapping = aes(x = x, y = y), colour = color, size = sz, shape = shp),
    ggplot2::geom_text(data = df, mapping = aes(x = x, y = y, label = text), hjust = 0, nudge_x = 0.05)
  ))
}

shapeFromInt <- function(i) {
  switch(as.character(i),
    "0" = 16L,  # filled circle
    "1" = 15L,  # filled square
    "2" = 17L,  # filled triangle
    "3" = 18L,  # filled diamond
    "4" = 3L,   # cross / plus
    "5" = 8L,   # star / asterisk
    16L
  )
}

linetypeFromInt <- function(i) {
  switch(as.character(i),
    "0" = "solid",
    "1" = "dashed",
    "2" = "dotted",
    "3" = "dotdash",
    "4" = "longdash",
    "5" = "twodash",
    "solid"
  )
}

referencesToNames <- function(references) {
  vapply(references, referenceToName, character(1L))
}

referenceToName <- function(ref) {
  type <- if (ref[["point"]]) {
    "point"
  } else if (ref[["horizontal"]]) {
    "hline"
  } else {
    "vline"
  }
  return(paste0("jasp_ref_", type, "_", rlang::hash(ref)))
}
