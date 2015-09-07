#' mapView colour palette
#'
#' @param n the number of colours to be created
#'
#' @author
#' Tim Appelhans
#'
#' @seealso
#' \code{\link{colorRampPalette}}
#'
#' @name mapViewPalette
#' @export mapViewPalette
#' @aliases mapViewPalette
NULL

mapViewPalette <- function(n) {
  colorRampPalette(c("#ebeaf7", "#83b0d6",
                     "#55A1B1", "#63AD99",
                     "#7FB972", "#B5BD4C",
                     "#D9AD3C", "#E68E34",
                     "#E6642C", "#D92120",
                     "#460000"))(n)
}
