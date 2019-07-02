#' Add a RGB stars image as a layer
#'
#' @description
#' Create a Red-Green-Blue image overlay from a \code{stars} object
#' based on three layers.
#' This is an adaptation of \code{mapview::viewRGB} function written in order to
#' give the possibility to add a RGB image to a leaflet / leafletProxy object.
#'
#' @param map a map widget object created from `leaflet()``
#' @param x a RasterBrick or RasterStack
#' @param r integer. Index of the Red channel/band, between 1 and nlayers(x)
#' @param g integer. Index of the Green channel/band, between 1 and nlayers(x)
#' @param b integer. Index of the Blue channel/band, between 1 and nlayers(x)
#' @param quantiles the upper and lower quantiles used for color stretching. If set to NULL, no stretching is applied.
#' @param maxpixels integer > 0. Maximum number of cells to use for the plot.
#' If maxpixels < \code{ncell(x)}, sampleRegular is used before plotting.
#' @param na.color the color to be used for NA pixels
#' @param method Method used to compute
#' values for the resampled layer that is passed on to leaflet. mapview does
#' projection on-the-fly to ensure correct display and therefore needs to know
#' how to do this projection. The default is 'bilinear' (bilinear interpolation),
#' which is appropriate for continuous variables. The other option, 'ngb'
#' (nearest neighbor), is useful for categorical variables.
#' @param ... additional arguments passed on to \code{\link{addStarsImage}}
#'
#' @author
#' Tim Appelhans, Luigi Ranghetti
#'
#' @examples
#' if (interactive()) {
#'   library(raster)
#'   library(stars)
#'   library(plainview)
#'   library(leflet)
#'
#'   poppendorf <- st_as_stars(plainview::poppendorf)
#'   leaflet() %>%
#'     addTiles(group = "OpenStreetMap") %>%
#'     addStarsRGB(poppendorf, 4,3,2, group = "True colours") %>%
#'     addStarsRGB(poppendorf, 5,4,3, group = "False colours") %>%
#'     addLayersControl(
#'       baseGroups = c("Satellite"),
#'       overlayGroups = c("True colours", "False colours"),
#'     )
#' }
#'
#' @export

addStarsRGB <-function(map, x, r = 3, g = 2, b = 1,
                       quantiles = c(0.02, 0.98),
                       maxpixels = mapviewGetOption("mapview.maxpixels"),
                       na.color = mapviewGetOption("na.color"),
                       method = c("bilinear", "ngb"),
                       ...) {

  method = match.arg(method)
  m <- map
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

  lyrs <- paste(r, g, b, sep = ".")

  ext = createExtent(sf::st_transform(sf::st_as_sfc(sf::st_bbox(x)), crs = 4326))

  out <- addStarsImage(map = m, x = xout, band = r, colors = p, ...)

  return(out)

}
