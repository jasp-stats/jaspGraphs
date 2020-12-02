# getGeomNameFromLayer <- function(l) {
#   ans <- tolower(class(l$geom)[1L])
#   if (startsWith(ans, "jaspgeom"))
#     return(substr(ans, 9L, nchar(ans)))
#   else if (startsWith(ans, "geom"))
#     return(substr(ans, 5L, nchar(ans)))
#   return(ans)
# }
#
# getGeomParameters <- function(l, gb) {
#
#   all <- l$geom$default_aes
#   req <- l$geom$required_aes
#   set <- l$mapping
#
#   inherited <- character()
#   if (l$inherit.aes)
#     inherited <- gb$plot$mapping
#
#   return(list(
#     name      = getGeomNameFromLayer(l),
#     parameter = names(all),
#     set       = names(all) %in% c(names(set), names(req), names(inherited)),
#     value     = all
#   ))
# }
#
# getAllGeomParameters <- function(gb) {
#
#   layers <- gb$plot$layers
#   ans <- lapply(
#     layers,
#     getGeomParameters,
#     # list(name = character(),
#     #      parameter = character(),
#     #      set = logical(),
#     #      value = list()),
#     gb = gb
#   )
#   return(ans[lengths(ans) > 0L])
# }
#
# getScaleParameters <- function(s, gb) {
#
#   if (inherits(s, AxisTypes))
#     return(NULL)
#
#   browser()
#
#   list(
#     name = s$aesthetics,
#     parameter = if (s$is_discrete()) s$palette.cache
#   )
#
#
# }
#
# getAllScaleParameters <- function(gb) {
#   scales <- gb$plot$scales$scales
#   ans <- lapply(scales, getScaleParameters, gb)
#
#   return(ans[lengths(ans) > 0L])
# }
#
#
# ex <- ggplot(mtcars, aes(x = factor(cyl), y = disp, fill = factor(gear))) +
#   geom_line(aes(y = mpg, color = factor(gear))) +
#   ggplot2::geom_point()
#
# gb <- ggplot_build(ex)
#
# getAllGeomParameters(gb)
# getAllScaleParameters(gb)
#
#
#
# get_scales <- function(gb) {
#
#   scales <- gb$plot$scales$scales
#
# }
#
# ggplot
#
#
# class(gb$plot$scales$scales[[2]])
#
# scales <- gb$plot$scales$scales
#
# sapply(scales, class)
#
# gb$plot$scales$scales[[2]]$aesthetics
# gb$plot$scales$scales[[3]]$aesthetics
# gb$plot$scales$scales[[4]]$aesthetics
#
#
# ex$mapping
#
#
# l <- layers[[1]]
# for (l in layers)
#   l$geom$aesthetics()
#
# # gb$plot$
#
#
#
#
# l$geom$default_aes
#
#
#
# geom_list <- function(p) {
#   g <- unlist(sapply(p$layers, function(x) gsub("Geom", "", proto_features(x)[2])))
#
#
#   g.list <- sapply(unique(g), function(x) paste0(g[g == x], seq(1, table(g)[[x]])))
#
#   unlist(g.list)
# }
# proto_features <- function(l) {
#   a <- sapply(c("position", "geom", "stat"), function(x) {
#     class(l[[x]])[1]
#   })
#
#   data.frame(t(a), stringsAsFactors = FALSE)
# }
#
# geom_list(ex)
