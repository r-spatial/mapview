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
#' @param obj an object of class 'mapview'
#' @param zoom Zoom level, see \code{\link{openmap}}.
#' @param col.regions,alpha.regions See \code{\link[sp]{spplot}}.
#' @param ... Further arguments passed on to \code{\link[sp]{spplot}}.
#' @param sp.layout (list with) layout elements, see \code{\link[sp]{spplot}}
#'
#' @author
#' Tim Appelhans, Florian Detsch
#'
#' @examples
#' \dontrun{
#' library(sp)
#' m1 <- mapview(breweries91, zcol = "zipcode")
#' spplot(m1)
#'
#' m2 <- mapview(gadmCHE, zcol = "NAME_1")
#' spplot(m2)
#'
#' demo(meuse,ask=F,echo=FALSE)
#' m = spTransform(meuse.area, CRS("+init=epsg:4326"))
#' spplot(mapView(meuse["zinc"]), sp.layout = m)
#' }
#'
#' @export spplot
#' @name spplot
#' @rdname spplot
#' @aliases spplot,mapview-method
#'
setMethod('spplot',
          signature('mapview'),
          function(obj, zoom = NULL, col.regions = mapviewPalette(256),
                   alpha.regions = 0.8, ..., sp.layout = NULL) {

            if (length(obj@object) == 1) {

              ## clone object
              obj_osm <- obj@object[[1]]

              ## compare crs
              crs_obj <- CRS(sp::proj4string(obj_osm))
              crs_ref <- CRS(mapview:::llcrs)

              ## if required, transform 'obj' to epsg:4326
              if (!raster::compareCRS(crs_obj, crs_ref) &
                  attr(class(obj_osm), "package") == "sp") {
                obj_osm <- sp::spTransform(obj_osm, CRS = crs_ref)
              } else if (!raster::compareCRS(crs_obj, crs_ref) &
                         attr(class(obj_osm), "package") == "raster") {
                obj_osm <- raster::projectRaster(obj_osm, crs = crs_ref,
                                                 method = "ngb")
                obj_osm <- raster::trim(obj_osm)
              }

              ## bounding box
              x_ext <- lattice:::extend.limits(c(raster::xmin(obj_osm),
                                                 raster::xmax(obj_osm)))
              y_ext <- lattice:::extend.limits(c(raster::ymin(obj_osm),
                                                 raster::ymax(obj_osm)))

              upperLeft <- c(y_ext[2], x_ext[1])
              lowerRight <- c(y_ext[1], x_ext[2])

              ## map type
              type <- "osm"

              ## acquire map
              rgb <- OpenStreetMap::openmap(upperLeft, lowerRight,
                                            type = type, zoom = zoom)

              ## rasterize rgb image
              rgb <- raster::raster(rgb)

              if (!raster::compareCRS(crs_obj, raster::projection(rgb)))
                rgb <- projectRaster(rgb, crs = crs_obj)

              ## convert to list format compatible with 'sp.layout'
              rgb <- rgb2spLayout(rgb)

              ## create plot

              # factorial raster
              if (any(raster::is.factor(obj@object[[1]]))) {
                obj_bg <- raster::raster(obj@object[[1]])
                obj_bg <- raster::setValues(obj_bg, rep(0, raster::ncell(obj_bg)))
                rasterVis::levelplot(obj@object[[1]],
                                     col.regions = col.regions,
                                     alpha.regions = alpha.regions, ...) +
                  latticeExtra::as.layer(spplot(obj_bg, colorkey = FALSE,
                                                col.regions = "transparent",
                                                alpha.regions = alpha.regions,
                                                sp.layout = append(list(rgb), sp.layout)), 
												under = TRUE)

              # all other objects
              } else {
                sp::spplot(obj@object[[1]], col.regions = col.regions,
                           alpha.regions = alpha.regions,
                           sp.layout = append(list(rgb), sp.layout), ...)
              }


            } else {
              warning("layered plots not implemented yet")
            }

          }
)
