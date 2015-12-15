if ( !isGeneric('spplot') ) {
  setGeneric('spplot', function(obj)
    standardGeneric('spplot'))
}
#' spplot method for objects of class 'mapview'
#'
#' @description
#' this function attempts to produce a static version of the specified
#' mapview object that looks similar to the interactive version.
#'
#' @param x an object of class 'mapview'
#'
#' @author
#' Tim Appelhans
#'
#' @examples
#' \dontrun{
#' library(sp)
#' m1 <- mapview(breweries91, zcol = "zipcode")
#' spplot(m1)
#'
#' m2 <- mapview(gadmCHE, zcol = "NAME_1")
#' spplot(m2)
#' }
#'
#' @export spplot
#' @name spplot
#' @rdname spplot
#' @aliases spplot,mapview-method
#'
setMethod('spplot',
          signature('mapview'),
          function(obj, ...) {

            if (length(obj@object) == 1) {
              spplot(obj@object[[1]], col.regions = mapviewPalette(256),
                     alpha.regions = 0.4, ...)
            } else warning("layered plots not implemented yet")

          }
)
