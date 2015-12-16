if ( !isGeneric('viewRGB') ) {
  setGeneric('viewRGB', function(x, ...)
    standardGeneric('viewRGB'))
}

#' Red-Green-Blue map view of a multi-layered Raster object
#'
#' @description
#' Make a Red-Green-Blue plot based on three layers (in a RasterBrick or RasterStack).
#' Three layers (sometimes referred to as "bands" because they may represent
#' different bandwidths in the electromagnetic spectrum) are combined such
#' that they represent the red, green and blue channel. This function can
#' be used to make 'true (or false) color images' from Landsat and other
#' multi-band satellite images. Note, this text is plagirized, i.e. copied
#' from \code{\link{plotRGB}}.
#'
#' @param x a RasterBrick or RasterStack
#' @param r integer. Index of the Red channel, between 1 and nlayers(x)
#' @param g integer. Index of the Green channel, between 1 and nlayers(x)
#' @param b integer. Index of the Blue channel, between 1 and nlayers(x)
#' @param quantiles the upper and lower quantiles used for color stretching
#' @param map the map to which the layer should be added
#' @param maxpixels integer > 0. Maximum number of cells to use for the plot.
#' If maxpixels < \code{ncell(x)}, sampleRegular is used before plotting.
#' @param map.types character spcifications for the base maps.
#' see \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' for available options.
#' @param na.color the color to be used for NA pixels
#' @param layer.name the name of the layer to be shown on the map
#' @param ... additional arguments passed on to \code{\link{mapView}}
#'
#' @author
#' Tim Appelhans
#'
#' @examples
#' ### raster data ###
#' library(sp)
#' library(raster)
#'
#' data(poppendorf)
#'
#' viewRGB(poppendorf, 4, 3, 2) # true-color
#' viewRGB(poppendorf, 5, 4, 3) # false-color
#'
#' \dontrun{
#' ### can also be used to view any type of image (here an example of the
#' ### package author teaching R on the research station at Kilimanjaro)
#' ### solution on how to read images from the web found here
#' ### http://r.789695.n4.nabble.com/readJPEG-function-cannot-open-jpeg-files-td4655487.html
#' library(jpeg)
#' library(raster)
#'
#' web_img <- "http://umweltinformatik-marburg.de/uploads/tx_rzslider/teaching_header_kili_resized.jpg"
#'
#' jpg <- readJPEG(readBin(web_img, "raw", 1e6))
#'
#' # Convert imagedata to raster
#' rst_blue <- raster(jpg[, , 1])
#' rst_green <- raster(jpg[, , 2])
#' rst_red <- raster(jpg[, , 3])
#'
#' img <- brick(rst_red, rst_green, rst_blue)
#'
#' viewRGB(img)
#' }
#'
#' @export
#' @docType methods
#' @name viewRGB
#' @rdname viewRGB
#' @aliases viewRGB,RasterStackBrick-method
NULL

setMethod("viewRGB", signature(x = "RasterStackBrick"),
          function(x, r = 3, g = 2, b = 1,
                   quantiles = c(0.02, 0.98),
                   map = NULL,
                   maxpixels = mapviewGetOption("maxpixels"),
                   map.types = mapviewGetOption("basemaps"),
                   na.color = mapviewGetOption("na.color"),
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   ...) {

            m <- initMap(map, map.types, projection(x))
            x <- rasterCheckSize(x, maxpixels)
            xout <- rasterCheckAdjustProjection(x)

            mat <- cbind(xout[[r]][],
                         xout[[g]][],
                         xout[[b]][])

            for(i in seq(ncol(mat))){
              z <- mat[, i]
              lwr <- stats::quantile(z, quantiles[1], na.rm = TRUE)
              upr <- stats::quantile(z, quantiles[2], na.rm = TRUE)
              z <- (z - lwr) / (upr - lwr)
              z[z < 0] <- 0
              z[z > 1] <- 1
              mat[, i] <- z
            }

            na_indx <- apply(mat, 1, anyNA)
            cols <- mat[, 1]
            cols[na_indx] <- na.color
            cols[!na_indx] <- grDevices::rgb(mat[!na_indx, ], alpha = 1)
            p <- function(x) cols

            grp <- layer.name
            lyrs <- paste(r, g, b, sep = ".")
            grp <- paste(grp, lyrs, sep = "_")

            m <- leaflet::addRasterImage(map = m, x = xout[[r]], colors = p,
                                         group = grp)
            m <- mapViewLayersControl(map = m,
                                      map.types = map.types,
                                      names = grp)

            out <- methods::new('mapview', object = list(xout), map = m)

            return(out)

          }
)
