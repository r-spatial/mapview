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
#' @param r integer. Index of the Red channel/band, between 1 and nlayers(x)
#' @param g integer. Index of the Green channel/band, between 1 and nlayers(x)
#' @param b integer. Index of the Blue channel/band, between 1 and nlayers(x)
#' @param quantiles the upper and lower quantiles used for color stretching. If set to NULL, no stretching is applied.
#' @param map the map to which the layer should be added
#' @param maxpixels integer > 0. Maximum number of cells to use for the plot.
#' If maxpixels < \code{ncell(x)}, sampleRegular is used before plotting.
#' @param map.types character spcifications for the base maps.
#' see \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' for available options.
#' @param na.color the color to be used for NA pixels
#' @param layer.name the name of the layer to be shown on the map
#' @param method Method used to compute
#' values for the resampled layer that is passed on to leaflet. mapview does
#' projection on-the-fly to ensure correct display and therefore needs to know
#' how to do this projection. The default is 'bilinear' (bilinear interpolation),
#' which is appropriate for continuous variables. The other option, 'ngb'
#' (nearest neighbor), is useful for categorical variables.
#' @param ... additional arguments passed on to \code{\link{mapView}}
#'
#' @author
#' Tim Appelhans
#'
#' @examples
#' if (interactive()) {
#'   library(raster)
#'   library(plainview)
#'
#'   viewRGB(plainview::poppendorf, 4, 3, 2) # true-color
#'   viewRGB(plainview::poppendorf, 5, 4, 3) # false-color
#' }
#'
#' @export
#' @docType methods
#' @name viewRGB
#' @rdname viewRGB
#' @aliases viewRGB,RasterStackBrick-method

setMethod("viewRGB", signature(x = "RasterStackBrick"),
          function(x, r = 3, g = 2, b = 1,
                   quantiles = c(0.02, 0.98),
                   map = NULL,
                   maxpixels = mapviewGetOption("mapview.maxpixels"),
                   map.types = mapviewGetOption("basemaps"),
                   na.color = mapviewGetOption("na.color"),
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   method = c("bilinear", "ngb"),
                   ...) {

            method = match.arg(method)
            m <- initMap(map, map.types, projection(x))
            x <- rasterCheckSize(x, maxpixels)
            xout <- rasterCheckAdjustProjection(x, method)

            mat <- cbind(xout[[r]][],
                         xout[[g]][],
                         xout[[b]][])

            if (!is.null(quantiles)) {

              for(i in seq(ncol(mat))){
                z <- mat[, i]
                lwr <- stats::quantile(z, quantiles[1], na.rm = TRUE)
                upr <- stats::quantile(z, quantiles[2], na.rm = TRUE)
                z <- (z - lwr) / (upr - lwr)
                z[z < 0] <- 0
                z[z > 1] <- 1
                mat[, i] <- z
              }
            } else {
              # If there is no stretch we just scale the data between 0 and 1
              mat <- apply(mat, 2, scales::rescale)
            }

            na_indx <- apply(mat, 1, anyNA)
            cols <- mat[, 1]
            cols[na_indx] <- na.color
            cols[!na_indx] <- grDevices::rgb(mat[!na_indx, ], alpha = 1)
            p <- function(x) cols

            grp <- layer.name
            lyrs <- paste(r, g, b, sep = ".")
            grp <- paste(grp, lyrs, sep = "_")

            ext = createExtent(xout)

            m <- leaflet::addRasterImage(map = m, x = xout[[r]], colors = p,
                                         group = grp)
            m <- mapViewLayersControl(map = m,
                                      map.types = map.types,
                                      names = grp)

            m <- leaflet::addScaleBar(map = m, position = "bottomleft")
            m <- leafem::addMouseCoordinates(m)
            m = addHomeButton(m, ext, layer.name = layer.name)

            out <- methods::new('mapview', object = list(xout), map = m)

            return(out)

          }
)


#' @describeIn viewRGB \code{\link{stars}}
setMethod("viewRGB", signature(x = "stars"),
          function(x, r = 3, g = 2, b = 1,
                   quantiles = c(0.02, 0.98),
                   map = NULL,
                   maxpixels = mapviewGetOption("mapview.maxpixels"),
                   map.types = mapviewGetOption("basemaps"),
                   na.color = mapviewGetOption("na.color"),
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   method = c("bilinear", "ngb"),
                   ...) {

            method = match.arg(method)
            m <- initMap(map, map.types, sf::st_crs(x)$proj4string)
            # x <- rasterCheckSize(x, maxpixels)
            xout <- starsCheckAdjustProjection(x, method)

            mat <- cbind(as.vector(xout[[1]][, , r]),
                         as.vector(xout[[1]][, , g]),
                         as.vector(xout[[1]][, , b]))

            if (!is.null(quantiles)) {

              for(i in seq(ncol(mat))){
                z <- mat[, i]
                lwr <- stats::quantile(z, quantiles[1], na.rm = TRUE)
                upr <- stats::quantile(z, quantiles[2], na.rm = TRUE)
                z <- (z - lwr) / (upr - lwr)
                z[z < 0] <- 0
                z[z > 1] <- 1
                mat[, i] <- z
              }
            } else {
              # If there is no stretch we just scale the data between 0 and 1
              mat <- apply(mat, 2, scales::rescale)
            }

            na_indx <- apply(mat, 1, anyNA)
            cols <- mat[, 1]
            cols[na_indx] <- na.color
            cols[!na_indx] <- grDevices::rgb(mat[!na_indx, ], alpha = 1)
            p <- function(x) cols

            grp <- layer.name
            lyrs <- paste(r, g, b, sep = ".")
            grp <- paste(grp, lyrs, sep = "_")

            ext = createExtent(sf::st_transform(sf::st_as_sfc(sf::st_bbox(x)), crs = 4326))

            m <- addStarsImage(map = m, x = xout, band = r,
                               colors = p, group = grp)
            m <- mapViewLayersControl(map = m,
                                      map.types = map.types,
                                      names = grp)

            m <- leaflet::addScaleBar(map = m, position = "bottomleft")
            m <- leafem::addMouseCoordinates(m)
            m = leafem::addHomeButton(m, ext, layer.name = layer.name)

            out <- methods::new('mapview', object = list(xout), map = m)

            return(out)

          }
)
