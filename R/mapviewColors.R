#' mapview version of leaflet::colorNumeric
#'
#' @param col.regions color vector to be used for coloring the levels
#' specified in at
#' @param at numeric vector giving the breakpoints for the colors
#' @param na.color the color for NA values. Defaults to "transparent"
#'
#' @author
#' Tim Appelhans
#'
#' @seealso
#' \code{\link{levelplot}}
#'
#' @name mapviewColors
#' @export mapviewColors
#' @aliases mapviewColors

mapviewColors <- function(col.regions,
                          at,
                          na.color) {

  f <- function(x) {

    cols <- lattice::level.colors(x,
                                  at = at,
                                  col.regions = col.regions)
    return(cols)

  }

  attributes(f) <- list(colorType = "bin",
                        colorArgs = list(bins = at,
                                         na.color = na.color))

  return(f)

}
