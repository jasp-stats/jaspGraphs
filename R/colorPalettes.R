
#'@importFrom ggplot2 continuous_scale discrete_scale

jaspGraphs_data <- list2env(list(
  # discrete color scales
  colorblind     = list(colors = RColorBrewer::brewer.pal(8L, "Dark2")),
  colorblind2    = list(colors = RColorBrewer::brewer.pal(8L, "Set2")),
  colorblind3    = list(colors = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")), # from ggthemes
  jaspPalette    = list(colors = c("#00A9E6", "#00BA63", "#BA0057", "#FB8B00", "#956BF8", "#38BBBB", "#633F33", "#EA008B")),  # JASP´s own palette, created by Vincent Ott
  viridis        = list(colors = viridisLite::viridis(256L)), # viridis::scale_color_viridis
  blue           = list(colors = c("#d1e1ec", "#b3cde0", "#6497b1", "#005b96", "#03396c", "#011f4b")), # bayesplot::color_scheme_get("blue")
  gray           = list(colors = c("#DFDFDF", "#bfbfbf", "#999999", "#737373", "#505050", "#383838")), # bayesplot::color_scheme_get("gray")
  ggplot2        = list(colors = c("#F8766D", "#CD9600", "#7CAE00", "#00BE67", "#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC"),
                     fun    = scales::hue_pal()),
  sportsTeamsNBA = list(colors = c("#e13a3e", "#008348", "#061922", "#1d1160", "#860038", "#4d90cd", "#ffc633", "#0f586c", "#00471b", "#005083")), #  unique(teamcolors::league_pal("nba"))[-c(5,7,9,10,13)][1:10]
  grandBudapest  = list(colors = c("#E6A0C4", "#C6CDF7", "#D8A499", "#7294D4", "#F1BB7B", "#FD6467", "#5B1A18", "#D67236"))  # c( wesanderson:::wes_palettes[["GrandBudapest2"]],  wesanderson:::wes_palettes[["GrandBudapest1"]])
))

#'@title JASP color palettes
#'@param palette Palette to choose from.
#'@param asFunction Should a function be returned or the raw colors? If a function is returned, it either takes a single integer or a vector in 0, 1 as input.
#'@param ... Further arguments for \code{\link[ggplot2]{scale_colour_continuous}}.
#'
#'@details For ggplot2s, the convenience functions \code{scale_JASPcolor_\*} and \code{scale_JASPfill_\*} exist.
#'
#'@return Either a character vector of colors or a function.
#'@export
#'@example inst/examples/ex-colorPalettes.R
#'@rdname colors
JASPcolors <- function(palette = getGraphOption("palette"), asFunction = FALSE) {

  if (!is.character(palette)) {
    stop2("palette must be character!")
  } else if (!palette %in% names(jaspGraphs_data)) {
    stop2(sprintf("palette was %s but must be one of %s", as.character(palette), paste(names(jaspGraphs_data), collapse = ", ")))
  }
  colors <- jaspGraphs_data[[palette]][["colors"]]
  if (asFunction) {
    fun <- jaspGraphs_data[[palette]][["fun"]]
    if (!is.null(fun))
      return(fun)

    return(function(n) {
      scales::gradient_n_pal(colors, values = NULL)(seq(0, 1, length.out = n))
    })
  } else {
    return(colors)
  }
}

# functions below mimic setup from viridis::scale_color_viridis
#'@rdname colors
#'@export
scale_JASPcolor_continuous <- function(palette = getGraphOption("palette"), ...) {
  ggplot2::scale_color_gradientn(colours = JASPcolors(palette = palette), ...)
}

#'@rdname colors
#'@export
scale_JASPfill_continuous <- function(palette = getGraphOption("palette"), ...) {
  ggplot2::scale_fill_gradientn(colours = JASPcolors(palette = palette), ...)
}

#'@rdname colors
#'@export
scale_JASPcolor_discrete <- function(palette = getGraphOption("palette"), ...) {
  discrete_scale("color", "JASPcolor", JASPcolors(palette = palette, asFunction = TRUE), ...)
}

#'@rdname colors
#'@export
scale_JASPfill_discrete <- function(palette = getGraphOption("palette"), ...) {
  discrete_scale("fill", "JASPcolor", JASPcolors(palette = palette, asFunction = TRUE), ...)
}

