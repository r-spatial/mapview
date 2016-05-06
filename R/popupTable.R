#' Create HTML strings for popup tables
#'
#' @description
#' Create HTML strings for \code{popup} tables used as input for
#' \code{\link{mapview}}. This optionally allows the user to include only a
#' subset of feature attributes.
#'
#' @param x A \code{Spatial*} object.
#' @param zcol \code{numeric} or \code{character} vector indicating the columns
#' included in the output popup table. If missing, all columns are displayed.
#' @param use_cpp \code{logical} determining whether or not to enable
#' \strong{Rcpp} functionality.
#'
#' @return
#' A \code{list} of HTML strings required to create feature popup table(s).
#'
#' @seealso \code{\link{popupGraph}} and \code{\link{popupImage}}.
#'
#' @examples
#' \dontrun{
#' library(sp)
#'
#' data(meuse)
#' coordinates(meuse) <- ~ x + y
#' proj4string(meuse) <- CRS("+init=epsg:28992")
#'
#' ## include columns 1 and 2 only
#' mapview(meuse, popup = popupTable(meuse, zcol = 1:2))
#' }
#'
#' @export popupTable
#' @name popupTable
popupTable <- function(x, zcol, use_cpp = TRUE) {

  if (!missing(zcol))
    x <- x[, zcol]

  brewPopupTable(x, use_cpp = use_cpp)
}
