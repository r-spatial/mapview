#' Default color palette for mapview
#'
#' @param n the number of colours to be created
#' @param name name of the color palette to be used. One of
#' "mapviewSpectralColors" (default) or "mapviewTopoColors"
#'
#' @author
#' Tim Appelhans
#'
#' @seealso
#' \code{\link{colorRampPalette}}
#'
#' @name mapviewPalette
#' @export mapviewPalette
#' @aliases mapviewPalette

mapviewPalette <- function(name, ...) {

  switch(name,
         mapviewRasterColors = viridisLite::inferno,
         mapviewVectorColors = viridisLite::viridis,
         mapviewSpectralColors =
           grDevices::colorRampPalette(c("#ebeaf7", "#92b9db",
                                         "#7db7c4", "#7dbbaa",
                                         "#7FB972", "#abb342",
                                         "#d6a62c", "#E68E34",
                                         "#E6642C", "#D92120",
                                         "#460000")),
         mapviewTopoColors =
           grDevices::colorRampPalette(c("#00555f", "#058353",
                                         "#7a8139", "#c1923b",
                                         "#ca9b7b", "#99D6D1",
                                         "#edf8f7"))
  )
}

## mapViewPalette =========================================================

#' @describeIn mapviewPalette for backward compatibility
#' @aliases mapViewPalette
#' @export mapViewPalette

mapViewPalette <- function(name, ...) {
  mapviewPalette(name)
}


# setClassPalette <- function(simple.class) {
#
#   pkgs <- c("viridis")
#   avl <- sapply(pkgs, "requireNamespace",
#                 quietly = TRUE, USE.NAMES = FALSE)
#
#   if (simple.class == "rst") {
#
#     function(type) {
#
#       if (type == "factor") {
#         if (avl) viridis::magma else mapviewPalette
#       } else {
#         if (avl) viridis::inferno else mapviewPalette
#       }
#
#     }
#
#   } else {
#
#     function(type) {
#
#       if (type == "factor") {
#         if (avl) viridis::magma else mapviewPalette
#       } else {
#         if (avl) viridis::inferno else mapviewPalette
#       }
#
#     }
#   }
#
# }
