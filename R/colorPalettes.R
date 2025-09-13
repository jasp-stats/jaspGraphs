
#'@importFrom ggplot2 continuous_scale discrete_scale

# NOTE: some of these palettes are specified directly (e.g., those from RColorBrewer and viridisLite) to silence
# the R CMD CHECK about the packages not being imported.
jaspGraphs_data <- list2env(list(
  # discrete color scales
  colorblind     = list(colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")), # dput(RColorBrewer::brewer.pal(8L, "Dark2"))
  colorblind2    = list(colors = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")), # dput(RColorBrewer::brewer.pal(8L, "Set2"))
  colorblind3    = list(colors = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")), # from ggthemes
  colorblind4    = list(colors = c("#61D04F", "#DF536B", "#9E9E9E", "#2297E6", "#CD0BBC", "#000000")),  # From palette.colors(palette = "R4")
  jaspPalette    = list(colors = c("#00A9E6", "#00BA63", "#BA0057", "#FB8B00", "#956BF8", "#38BBBB", "#633F33", "#EA008B")),  # JASP´s own palette, created by Vincent Ott
  viridis        = list(colors = c("#440154FF", "#440256FF", "#450457FF", "#450559FF", "#46075AFF",
                                   "#46085CFF", "#460A5DFF", "#460B5EFF", "#470D60FF", "#470E61FF",
                                   "#471063FF", "#471164FF", "#471365FF", "#481467FF", "#481668FF",
                                   "#481769FF", "#48186AFF", "#481A6CFF", "#481B6DFF", "#481C6EFF",
                                   "#481D6FFF", "#481F70FF", "#482071FF", "#482173FF", "#482374FF",
                                   "#482475FF", "#482576FF", "#482677FF", "#482878FF", "#482979FF",
                                   "#472A7AFF", "#472C7AFF", "#472D7BFF", "#472E7CFF", "#472F7DFF",
                                   "#46307EFF", "#46327EFF", "#46337FFF", "#463480FF", "#453581FF",
                                   "#453781FF", "#453882FF", "#443983FF", "#443A83FF", "#443B84FF",
                                   "#433D84FF", "#433E85FF", "#423F85FF", "#424086FF", "#424186FF",
                                   "#414287FF", "#414487FF", "#404588FF", "#404688FF", "#3F4788FF",
                                   "#3F4889FF", "#3E4989FF", "#3E4A89FF", "#3E4C8AFF", "#3D4D8AFF",
                                   "#3D4E8AFF", "#3C4F8AFF", "#3C508BFF", "#3B518BFF", "#3B528BFF",
                                   "#3A538BFF", "#3A548CFF", "#39558CFF", "#39568CFF", "#38588CFF",
                                   "#38598CFF", "#375A8CFF", "#375B8DFF", "#365C8DFF", "#365D8DFF",
                                   "#355E8DFF", "#355F8DFF", "#34608DFF", "#34618DFF", "#33628DFF",
                                   "#33638DFF", "#32648EFF", "#32658EFF", "#31668EFF", "#31678EFF",
                                   "#31688EFF", "#30698EFF", "#306A8EFF", "#2F6B8EFF", "#2F6C8EFF",
                                   "#2E6D8EFF", "#2E6E8EFF", "#2E6F8EFF", "#2D708EFF", "#2D718EFF",
                                   "#2C718EFF", "#2C728EFF", "#2C738EFF", "#2B748EFF", "#2B758EFF",
                                   "#2A768EFF", "#2A778EFF", "#2A788EFF", "#29798EFF", "#297A8EFF",
                                   "#297B8EFF", "#287C8EFF", "#287D8EFF", "#277E8EFF", "#277F8EFF",
                                   "#27808EFF", "#26818EFF", "#26828EFF", "#26828EFF", "#25838EFF",
                                   "#25848EFF", "#25858EFF", "#24868EFF", "#24878EFF", "#23888EFF",
                                   "#23898EFF", "#238A8DFF", "#228B8DFF", "#228C8DFF", "#228D8DFF",
                                   "#218E8DFF", "#218F8DFF", "#21908DFF", "#21918CFF", "#20928CFF",
                                   "#20928CFF", "#20938CFF", "#1F948CFF", "#1F958BFF", "#1F968BFF",
                                   "#1F978BFF", "#1F988BFF", "#1F998AFF", "#1F9A8AFF", "#1E9B8AFF",
                                   "#1E9C89FF", "#1E9D89FF", "#1F9E89FF", "#1F9F88FF", "#1FA088FF",
                                   "#1FA188FF", "#1FA187FF", "#1FA287FF", "#20A386FF", "#20A486FF",
                                   "#21A585FF", "#21A685FF", "#22A785FF", "#22A884FF", "#23A983FF",
                                   "#24AA83FF", "#25AB82FF", "#25AC82FF", "#26AD81FF", "#27AD81FF",
                                   "#28AE80FF", "#29AF7FFF", "#2AB07FFF", "#2CB17EFF", "#2DB27DFF",
                                   "#2EB37CFF", "#2FB47CFF", "#31B57BFF", "#32B67AFF", "#34B679FF",
                                   "#35B779FF", "#37B878FF", "#38B977FF", "#3ABA76FF", "#3BBB75FF",
                                   "#3DBC74FF", "#3FBC73FF", "#40BD72FF", "#42BE71FF", "#44BF70FF",
                                   "#46C06FFF", "#48C16EFF", "#4AC16DFF", "#4CC26CFF", "#4EC36BFF",
                                   "#50C46AFF", "#52C569FF", "#54C568FF", "#56C667FF", "#58C765FF",
                                   "#5AC864FF", "#5CC863FF", "#5EC962FF", "#60CA60FF", "#63CB5FFF",
                                   "#65CB5EFF", "#67CC5CFF", "#69CD5BFF", "#6CCD5AFF", "#6ECE58FF",
                                   "#70CF57FF", "#73D056FF", "#75D054FF", "#77D153FF", "#7AD151FF",
                                   "#7CD250FF", "#7FD34EFF", "#81D34DFF", "#84D44BFF", "#86D549FF",
                                   "#89D548FF", "#8BD646FF", "#8ED645FF", "#90D743FF", "#93D741FF",
                                   "#95D840FF", "#98D83EFF", "#9BD93CFF", "#9DD93BFF", "#A0DA39FF",
                                   "#A2DA37FF", "#A5DB36FF", "#A8DB34FF", "#AADC32FF", "#ADDC30FF",
                                   "#B0DD2FFF", "#B2DD2DFF", "#B5DE2BFF", "#B8DE29FF", "#BADE28FF",
                                   "#BDDF26FF", "#C0DF25FF", "#C2DF23FF", "#C5E021FF", "#C8E020FF",
                                   "#CAE11FFF", "#CDE11DFF", "#D0E11CFF", "#D2E21BFF", "#D5E21AFF",
                                   "#D8E219FF", "#DAE319FF", "#DDE318FF", "#DFE318FF", "#E2E418FF",
                                   "#E5E419FF", "#E7E419FF", "#EAE51AFF", "#ECE51BFF", "#EFE51CFF",
                                   "#F1E51DFF", "#F4E61EFF", "#F6E620FF", "#F8E621FF", "#FBE723FF",
                                   "#FDE725FF")), # dput(viridisLite::viridis(256L))), # viridis::scale_color_viridis
  inferno        = list(colors = c("#000004FF", "#010005FF", "#010106FF", "#010108FF", "#02010AFF",
                                   "#02020CFF", "#02020EFF", "#030210FF", "#040312FF", "#040314FF",
                                   "#050417FF", "#060419FF", "#07051BFF", "#08051DFF", "#09061FFF",
                                   "#0A0722FF", "#0B0724FF", "#0C0826FF", "#0D0829FF", "#0E092BFF",
                                   "#10092DFF", "#110A30FF", "#120A32FF", "#140B34FF", "#150B37FF",
                                   "#160B39FF", "#180C3CFF", "#190C3EFF", "#1B0C41FF", "#1C0C43FF",
                                   "#1E0C45FF", "#1F0C48FF", "#210C4AFF", "#230C4CFF", "#240C4FFF",
                                   "#260C51FF", "#280B53FF", "#290B55FF", "#2B0B57FF", "#2D0B59FF",
                                   "#2F0A5BFF", "#310A5CFF", "#320A5EFF", "#340A5FFF", "#360961FF",
                                   "#380962FF", "#390963FF", "#3B0964FF", "#3D0965FF", "#3E0966FF",
                                   "#400A67FF", "#420A68FF", "#440A68FF", "#450A69FF", "#470B6AFF",
                                   "#490B6AFF", "#4A0C6BFF", "#4C0C6BFF", "#4D0D6CFF", "#4F0D6CFF",
                                   "#510E6CFF", "#520E6DFF", "#540F6DFF", "#550F6DFF", "#57106EFF",
                                   "#59106EFF", "#5A116EFF", "#5C126EFF", "#5D126EFF", "#5F136EFF",
                                   "#61136EFF", "#62146EFF", "#64156EFF", "#65156EFF", "#67166EFF",
                                   "#69166EFF", "#6A176EFF", "#6C186EFF", "#6D186EFF", "#6F196EFF",
                                   "#71196EFF", "#721A6EFF", "#741A6EFF", "#751B6EFF", "#771C6DFF",
                                   "#781C6DFF", "#7A1D6DFF", "#7C1D6DFF", "#7D1E6DFF", "#7F1E6CFF",
                                   "#801F6CFF", "#82206CFF", "#84206BFF", "#85216BFF", "#87216BFF",
                                   "#88226AFF", "#8A226AFF", "#8C2369FF", "#8D2369FF", "#8F2469FF",
                                   "#902568FF", "#922568FF", "#932667FF", "#952667FF", "#972766FF",
                                   "#982766FF", "#9A2865FF", "#9B2964FF", "#9D2964FF", "#9F2A63FF",
                                   "#A02A63FF", "#A22B62FF", "#A32C61FF", "#A52C60FF", "#A62D60FF",
                                   "#A82E5FFF", "#A92E5EFF", "#AB2F5EFF", "#AD305DFF", "#AE305CFF",
                                   "#B0315BFF", "#B1325AFF", "#B3325AFF", "#B43359FF", "#B63458FF",
                                   "#B73557FF", "#B93556FF", "#BA3655FF", "#BC3754FF", "#BD3853FF",
                                   "#BF3952FF", "#C03A51FF", "#C13A50FF", "#C33B4FFF", "#C43C4EFF",
                                   "#C63D4DFF", "#C73E4CFF", "#C83F4BFF", "#CA404AFF", "#CB4149FF",
                                   "#CC4248FF", "#CE4347FF", "#CF4446FF", "#D04545FF", "#D24644FF",
                                   "#D34743FF", "#D44842FF", "#D54A41FF", "#D74B3FFF", "#D84C3EFF",
                                   "#D94D3DFF", "#DA4E3CFF", "#DB503BFF", "#DD513AFF", "#DE5238FF",
                                   "#DF5337FF", "#E05536FF", "#E15635FF", "#E25734FF", "#E35933FF",
                                   "#E45A31FF", "#E55C30FF", "#E65D2FFF", "#E75E2EFF", "#E8602DFF",
                                   "#E9612BFF", "#EA632AFF", "#EB6429FF", "#EB6628FF", "#EC6726FF",
                                   "#ED6925FF", "#EE6A24FF", "#EF6C23FF", "#EF6E21FF", "#F06F20FF",
                                   "#F1711FFF", "#F1731DFF", "#F2741CFF", "#F3761BFF", "#F37819FF",
                                   "#F47918FF", "#F57B17FF", "#F57D15FF", "#F67E14FF", "#F68013FF",
                                   "#F78212FF", "#F78410FF", "#F8850FFF", "#F8870EFF", "#F8890CFF",
                                   "#F98B0BFF", "#F98C0AFF", "#F98E09FF", "#FA9008FF", "#FA9207FF",
                                   "#FA9407FF", "#FB9606FF", "#FB9706FF", "#FB9906FF", "#FB9B06FF",
                                   "#FB9D07FF", "#FC9F07FF", "#FCA108FF", "#FCA309FF", "#FCA50AFF",
                                   "#FCA60CFF", "#FCA80DFF", "#FCAA0FFF", "#FCAC11FF", "#FCAE12FF",
                                   "#FCB014FF", "#FCB216FF", "#FCB418FF", "#FBB61AFF", "#FBB81DFF",
                                   "#FBBA1FFF", "#FBBC21FF", "#FBBE23FF", "#FAC026FF", "#FAC228FF",
                                   "#FAC42AFF", "#FAC62DFF", "#F9C72FFF", "#F9C932FF", "#F9CB35FF",
                                   "#F8CD37FF", "#F8CF3AFF", "#F7D13DFF", "#F7D340FF", "#F6D543FF",
                                   "#F6D746FF", "#F5D949FF", "#F5DB4CFF", "#F4DD4FFF", "#F4DF53FF",
                                   "#F4E156FF", "#F3E35AFF", "#F3E55DFF", "#F2E661FF", "#F2E865FF",
                                   "#F2EA69FF", "#F1EC6DFF", "#F1ED71FF", "#F1EF75FF", "#F1F179FF",
                                   "#F2F27DFF", "#F2F482FF", "#F3F586FF", "#F3F68AFF", "#F4F88EFF",
                                   "#F5F992FF", "#F6FA96FF", "#F8FB9AFF", "#F9FC9DFF", "#FAFDA1FF",
                                   "#FCFFA4FF")), # dput(viridisLite::inferno(256L))),  # Also part of the viridis family
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

