#' View extent/bbox of spatial objects interactively
#' @description
#' This function produces an interactive view of the extent/bbox
#' of the supplied spatial object
#'
#' @param x either a Raster*, sf* or Spatial* object
#' @param data either a Raster*, sf* or Spatial* object
#' @param map a leaflet or mapview map the extent should be added to. If NULL
#' standard background layers are created.
#' @param alpha.regions opacity of the fills or the raster layer(s).
#' @param layer.name the name of the layer to be shown on the map.
#' @param popup a \code{list} of HTML strings with the popup contents, usually
#' created from \code{\link{popupTable}}. See \code{\link{addControl}} for
#' details.
#' @param label a character vector of labels to be shown on mouseover. See
#' \code{\link{addControl}} for details.
#' @param ... additional arguments passed on to \code{\link{addRectangles}}
#'
#' @author
#' Tim Appelhans
#'
#' @examples
#' library(leaflet)
#'
#' viewExtent(breweries)
#' viewExtent(franconia) + breweries
#' mapview(franconia) %>% leafem::addExtent(franconia, fillColor = "yellow")
#' leaflet() %>% addProviderTiles("OpenStreetMap") %>% leafem::addExtent(breweries)
#' leaflet() %>% addProviderTiles("OpenStreetMap") %>% leafem::addExtent(breweries)
#'
#' @export viewExtent
#' @name viewExtent
#' @aliases viewExtent
#'
NULL

## View Extent ============================================================
#' @rdname viewExtent
viewExtent <- function(x,
                       map = NULL,
                       popup = NULL,
                       layer.name = NULL,
                       alpha.regions = 0.2,
                       label = NULL,
                       ...) {

  # new line allows to do m <- mapview(trails) ; viewExtent(trails, m)
  if (inherits(map, "mapview")) map <- mapview2leaflet(map)

  if (is.null(layer.name)) {
    layer.name = paste(deparse(substitute(x)), "extent", sep = "-")
  }

  x = sf::st_as_sfc(
    sf::st_bbox(
      checkAdjustProjection(x)
    )
  )

  if (is.null(popup)) {
    pop = leafpop::popupTable(as.data.frame(t(attributes(x)$bbox[1:4])))
  }

  mapView(x,
          map = map,
          popup = pop,
          layer.name = layer.name,
          alpha.regions = alpha.regions,
          label = label,
          ...)

}

## Add Extent =============================================================
#' @describeIn viewExtent add extent/bbox of spatial/sf objects to a leaflet map -
#' This function is deprecated.
#' Please use leafem::\code{\link[leafem]{addExtent}} instead.
#'
#' @export addExtent

addExtent <- function(map, data, ...) {

  .Defunct(new = "leafem::addExtent", package = "mapview")

}



## combined extent ===========================================================
combineExtent = function(lst, sf = FALSE, crs = 4326) {
  # lst = list(breweries, st_as_sf(atlStorms2005), st_as_sf(gadmCHE))
  # bb = do.call(rbind, lapply(lst, sf::st_bbox))
  bb = do.call(rbind, lapply(seq(lst), function(i) {

  if (!is.null(lst[[i]])) {
    if (!is.na(getProjection(lst[[i]]))) {
      sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(lst[[i]])),
                                   crs = crs))
    } else {
      sf::st_bbox(sf::st_as_sfc(sf::st_bbox(lst[[i]])))
    }
  }
  }))

  bbmin = apply(bb, 2, min)
  bbmax = apply(bb, 2, max)
  bb = c(bbmin[1], bbmin[2], bbmax[3], bbmax[4])
  if (sf) {
    attr(bb, which = "class") = "bbox"
    attr(bb, "crs") = sf::st_crs(crs)
    return(sf::st_as_sfc(bb))
  }
  return(bb)
}
