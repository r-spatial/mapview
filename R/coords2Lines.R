if ( !isGeneric("coords2Lines") ) {
  setGeneric("coords2Lines", function(coords, ...)
    standardGeneric("coords2Lines"))
}
#' Convert points to SpatialLines*
#'
#' @description
#' Create a \code{SpatialLines*} object from a \code{Line} object or set of
#' point coordinates in one go, i.e. without being required to run through the
#' single steps outlined in \code{\link{SpatialLines}}.
#'
#' @param coords \code{Line} object or 2-column \code{numeric matrix} with x and
#' y coordinates.
#' @param ID \code{character}, see \code{\link{Lines}}.
#' @param data \code{data.frame} with data to add to the output
#' \code{SpatialLines*} object (optional).
#' @param match.ID \code{logical}, see \code{\link{SpatialLinesDataFrame}}.
#' @param ... Further arguments passed on to \code{\link{SpatialLines}} (i.e.,
#' \code{proj4string}).
#'
#' @return If \code{data} is missing, a \code{SpatialLines} object; else a
#' \code{SpatialLinesDataFrame} object.
#'
#' @seealso
#' \code{\link{SpatialLines-class}}, \code{\link{SpatialLinesDataFrame}}.
#'
#' @examples
#' library(sp)
#'
#' coords1 <- cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2))
#' sln1 <- coords2Lines(coords1, ID = "A")
#'
#' coords2 <- cbind(c(5, 4, 2, 5), c(2, 3, 2, 2))
#' sln2 <- coords2Lines(coords2, ID = "B")
#'
#' plot(sln1, col = "grey75")
#' plot(sln2, col = "grey25", add = TRUE)
#'
#' @export coords2Lines
#' @name coords2Lines
NULL

### matrix method -----
#' @aliases coords2Lines,matrix-method
#' @rdname coords2Lines
setMethod("coords2Lines",
          signature(coords = "matrix"),
          function(coords, ID, data, match.ID = TRUE, ...) {

            # convert coordinates matrix to 'Line' object
            ln <- sp::Line(coords)

            # pass object on to 'Line'-method
            coords2Lines(ln, ID, data, match.ID, ...)
          }
)


### Line method -----
#' @aliases coords2Lines,Line-method
#' @rdname coords2Lines
setMethod("coords2Lines",
          signature(coords = "Line"),
          function(coords, ID, data, match.ID = TRUE, ...) {

            # convert 'Line' to 'Lines' object
            lns <- sp::Lines(list(coords), ID)

            # 'Lines' to 'SpatialLines'
            sln <- sp::SpatialLines(list(lns), ...)

            if (!missing("data")) {
              return(sp::SpatialLinesDataFrame(sln, data, match.ID))
            } else {
              return(sln)
            }
          }
)
