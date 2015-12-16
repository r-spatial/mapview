if ( !isGeneric('coords2JSON') ) {
  setGeneric('coords2JSON', function(x, xy = c(1, 2))
    standardGeneric('coords2JSON'))
}
#' Convert a vector/matrix of coordinates to JSON format
#'
#' @description
#' Similar to \code{toJSON} from \strong{jsonlite}, this function takes a
#' set of coordinates as input and converts them to proper JSON format. Note
#' that the function is powered by \strong{Rcpp} which makes it a convenient
#' alternative to existing methods when it comes to processing big datasets.
#'
#' @param x A 'numeric' vector with a single pair of coordinates or a matrix
#' with multiple pairs of input coordinates, typically projected in EPSG:4326
#' (\url{http://spatialreference.org/ref/epsg/wgs-84/}).
#' @param xy An 'integer' vector specifying the coordinate columns.
#'
#' @return A single 'character' object in JSON format.
#'
#' @author
#' Florian Detsch
#'
#' @examples
#' crd <- matrix(ncol = 3, nrow = 12)
#'
#' # x-coordinates
#' set.seed(10)
#' crd[, 1] <- rnorm(nrow(crd), 10, 3)
#'
#' # y-coordinates
#' set.seed(10)
#' crd[, 2] <- rnorm(nrow(crd), 50, 3)
#'
#' # additional data
#' crd[, 3] <- month.abb
#'
#' # reformat a single pair of coordinates
#' coords2JSON(crd[1, ])
#'
#' # reformat multiple pairs of coordinates at once
#' coords2JSON(crd)
#'
#' @export coords2JSON
#' @name coords2JSON

################################################################################
### function using numeric input (i.e., single x and y coordinates) ############
#' @aliases coords2JSON,numeric-method
#' @rdname coords2JSON

setMethod("coords2JSON",
          signature(x = "numeric"),
          function(x) {

            ## round to 7 decimal places
            x <- round(x, 7)

            ## convert to 'character'
            x <- as.character(x)

            ## integer-formatted 'character' to float
            id_flt <- grep("\\.", x)
            x[-id_flt] <- paste0(x[-id_flt], ".0")

            ## create JSON string
            chr_json <- one2JSON(x)

            return(chr_json)
          })


################################################################################
### function using numeric input (i.e., single x and y coordinates) ############
#' @aliases coords2JSON,character-method
#' @rdname coords2JSON

setMethod("coords2JSON",
          signature(x = "character"),
          function(x, xy = c(1, 2)) {

            ## round to 7 decimal places
            x[xy] <- as.character(round(as.numeric(x[xy]), 7))

#            ## integer-formatted 'character' to float
#            id_flt <- grep("\\.", x[xy])
#            id_int <- xy[!xy %in% id_flt]
#            x[id_int] <- paste0(x[id_int], ".0")

            ## create JSON string
            chr_json <- one2JSON(x)

            return(chr_json)
          })


################################################################################
### function using matrix input (i.e., multiple x and y coordinates) ###########
#' @aliases coords2JSON,matrix-method
#' @rdname coords2JSON

setMethod("coords2JSON",
          signature(x = "matrix"),
          function(x, xy = c(1, 2)) {

            ## convert to 'character'
            if (class(x[1, ]) != "character") {
               #x <- sprintf("%s",x)
               x <- apply(x, 2, "as.character")

            }

            ## round to 7 decimal places
            x[, xy] <- apply(x[, xy], 2, FUN = function(i) {
              as.character(round(as.numeric(i), 7))
            })

            ## integer-formatted 'character' to float
#            crd <- x[, xy]
#            id_flt <- grep("\\.", crd)
#            crd[-id_flt] <- paste0(crd[-id_flt], ".0")
#            x[, xy] <- crd

            ## create list with JSON entries
            lst_json <- all2JSONlist(x)

            ## concatenate single list entries
            if (length(lst_json) == 1) {
              chr_json <- lst_json[[1]]
            } else {
              chr_json <- do.call(function(...) paste(..., sep = ","), lst_json)
              chr_json <- paste0("[", chr_json, "]")
            }

            return(chr_json)
          })
