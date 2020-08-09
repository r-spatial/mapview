#' Red-Green-Blue map view of a multi-layered Raster object
#'
#' @description
#' Make a Red-Green-Blue plot based on three layers (in a RasterBrick, RasterStack or stars).
#' Three layers (sometimes referred to as "bands" because they may represent
#' different bandwidths in the electromagnetic spectrum) are combined such
#' that they represent the red, green and blue channel. This function can
#' be used to make 'true (or false) color images' from Landsat and other
#' multi-band satellite images. Note, this text is plagirized, i.e. copied
#' from \code{\link{plotRGB}}.
#'
#' @param x a RasterBrick, RasterStack or stars
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
#' @importFrom leafem addRasterRGB
#' @importFrom raster projection
#' @importFrom sf st_crs
#' @export
#' @docType methods
#' @name viewRGB
#' @rdname viewRGB
#' @aliases viewRGB,RasterStackBrick-method

viewRGB = function(x, r = 3, g = 2, b = 1,
                    quantiles = c(0.02, 0.98),
                    map = NULL,
                    maxpixels = mapviewGetOption("mapview.maxpixels"),
                    map.types = mapviewGetOption("basemaps"),
                    na.color = mapviewGetOption("na.color"),
                    layer.name = NULL,
                    method = c("bilinear", "ngb"),
                    ...) {

  if(!inherits(x, "Raster") & !inherits(x, "stars")) {
    stop("'x' must be a Raster* or stars object.")
  }

  if (is.null(layer.name)) layer.name = makeLayerName(x, zcol = NULL, up = 1)

  method = match.arg(method)
  x = rasterCheckSize(x, maxpixels = maxpixels)
  x = rasterCheckAdjustProjection(x, method)
  projstring = if (inherits(x, "Raster")) {
    projection(x)
  } else {
    sf::st_crs(x)$proj4string
  }
  m = initMap(map, map.types, projstring)

  lyrs = paste(r, g, b, sep = ".")
  grp = paste(layer.name, lyrs, sep = "_")

  ext = createExtent(x)

  m = leafem::addRasterRGB(map = m, x = x, r = r, g = g, b = b,
                            quantiles = quantiles,
                            # maxpixels = maxpixels,
                            na.color = na.color,
                            method = method,
                            group = grp)
  m = mapViewLayersControl(map = m,
                            map.types = map.types,
                            names = grp)

  m = leaflet::addScaleBar(map = m, position = "bottomleft")
  m = leafem::addMouseCoordinates(m)
  m = leafem::addCopyExtent(m)
  m = leafem::addHomeButton(m, ext, group = grp)

  out = methods::new('mapview', object = list(x), map = m)

  return(out)

}
