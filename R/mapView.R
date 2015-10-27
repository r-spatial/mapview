if ( !isGeneric('mapView') ) {
  setGeneric('mapView', function(x, ...)
    standardGeneric('mapView'))
}

#' View spatial objects interactively
#'
#' @description
#' this function produces an interactive GIS-like view of the specified
#' spatial object(s) on top of the specified base maps.
#'
#' @param x a \code{\link{raster}}* object
#' @param map an optional existing map to be updated/added to
#' @param maxpixels integer > 0. Maximum number of cells to use for the plot.
#' If maxpixels < \code{ncell(x)}, sampleRegular is used before plotting.
#' @param color color (palette) of the points/polygons/lines/pixels
#' @param na.color color for missing values
#' @param use.layer.names should layer names of the Raster* object be used?
#' @param values a vector of values for the visualisation of the layers.
#' Per default these are calculated based on the supplied raster* object.
#' @param map.types character spcifications for the base maps.
#' see \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' for available options.
#' @param layer.opacity opacity of the raster layer(s)
#' @param legend should a legend be plotted
#' @param legend.opacity opacity of the legend
#' @param trim should the raster be trimmed in case there are NAs on the egdes
#' @param verbose should some details be printed during the process
#' @param layer.name the name of the layer to be shown on the map
#' @param popup a character vector of the HTML content for the popups. See
#' \code{\link{addControl}} for details.
#' @param ... additional arguments passed on to repective functions.
#' See \code{\link{addRasterImage}}, \code{\link{addCircles}},
#' \code{\link{addPolygons}}, \code{\link{addPolylines}} for details
#'
#' @author
#' Tim Appelhans
#'
#' @examples
#' \dontrun{
#' mapView()
#'
#' ### raster data ###
#' library(sp)
#' library(raster)
#'
#' data(meuse.grid)
#' coordinates(meuse.grid) = ~x+y
#' proj4string(meuse.grid) <- CRS("+init=epsg:28992")
#' gridded(meuse.grid) = TRUE
#' meuse_rst <- stack(meuse.grid)
#'
#' # raster stack
#' m1 <- mapView(meuse_rst)
#' m1
#'
#' # factorial RasterLayer
#' m2 <- mapView(raster::as.factor(meuse_rst[[4]]))
#' m2
#'
#' # SpatialPixelsDataFrame
#' mapView(meuse.grid, zcol = "soil")
#'
#'
#' ### point vector data ###
#' ## SpatialPointsDataFrame ##
#' data(meuse)
#' coordinates(meuse) <- ~x+y
#' proj4string(meuse) <- CRS("+init=epsg:28992")
#'
#' # all layers of meuse
#' mapView(meuse, burst = TRUE)
#'
#' # only one layer, all info in popups
#' mapView(meuse)
#'
#' ## SpatialPoints ##
#' meuse_pts <- as(meuse, "SpatialPoints")
#' mapView(meuse_pts)
#'
#'
#'
#' ### overlay vector on top of raster ###
#' mapView(meuse.grid, zcol = "ffreq") + meuse
#'
#' ### polygon vector data ###
#' data("gadmCHE")
#' m <- mapView(gadmCHE)
#' m
#'
#' ## points on polygons ##
#' centres <- data.frame(coordinates(gadmCHE))
#' names(centres) <- c("x", "y")
#' coordinates(centres) <- ~ x + y
#' projection(centres) <- projection(gadmCHE)
#' m + centres
#'
#' ### lines vector data
#' data("atlStorms2005")
#' mapView(atlStorms2005)
#' mapView(atlStorms2005, burst = TRUE)
#' }
#'
#' @export mapView
#' @name mapView
#' @rdname mapView
#' @aliases mapView
NULL

## RasterLayer ============================================================
#' @describeIn mapView \code{\link{raster}}
setMethod('mapView', signature(x = 'RasterLayer'),
          function(x,
                   map = NULL,
                   maxpixels = 500000,
                   color = mapViewPalette(7),
                   na.color = "transparent",
                   use.layer.names = FALSE,
                   values = NULL,
                   map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   layer.opacity = 0.8,
                   legend = TRUE,
                   legend.opacity = 1,
                   trim = TRUE,
                   verbose = FALSE,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   ...) {

            pkgs <- c("leaflet", "raster", "magrittr")
            tst <- sapply(pkgs, "requireNamespace",
                          quietly = TRUE, USE.NAMES = FALSE)

            is.fact <- raster::is.factor(x)

            m <- initMap(map, map.types, proj4string(x))
            x <- rasterCheckAdjustProjection(x, maxpixels = maxpixels)

            if (!is.na(raster::projection(x)) & trim) x <- trim(x)

            if (is.fact) x <- raster::as.factor(x)

            if (is.null(values)) {
              if (is.fact) {
                values <- x@data@attributes[[1]]$ID
              } else {
                offset <- diff(range(x[], na.rm = TRUE)) * 0.05
                top <- max(x[], na.rm = TRUE) + offset
                bot <- min(x[], na.rm = TRUE) - offset
                values <- seq(bot, top, length.out = 10)
                values <- round(values, 5)
              }
            } else {
              values <- round(values, 5)
            }

            if (is.fact) {
              pal <- leaflet::colorFactor(color,
                                          domain = NULL,
                                          na.color = na.color)
            } else {
              pal <- leaflet::colorNumeric(color,
                                           domain = values,
                                           na.color = na.color)
            }

            if (use.layer.names) {
              grp <- names(x)
            } else {
              grp <- layer.name
            }

            ## add layers to base map
            m <- leaflet::addRasterImage(map = m,
                                         x = x,
                                         colors = pal,
                                         project = FALSE,
                                         opacity = layer.opacity,
                                         group = grp,
                                         ...)

            if (legend) {
              ## add legend
              m <- leaflet::addLegend(map = m,
                                      pal = pal,
                                      opacity = legend.opacity,
                                      values = values,
                                      title = grp)
            }

            m <- mapViewLayersControl(map = m,
                                      map.types = map.types,
                                      names = grp)

            out <- new('mapview', object = list(x), map = m)

            return(out)

          }

)

## Raster Stack/Brick ===========================================================
#' @describeIn mapView \code{\link{stack}} / \code{\link{stack}}

setMethod('mapView', signature(x = 'RasterStackBrick'),
          function(x,
                   map = NULL,
                   maxpixels = 500000,
                   color = mapViewPalette(7),
                   na.color = "transparent",
                   values = NULL,
                   map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   legend = TRUE,
                   legend.opacity = 1,
                   trim = TRUE,
                   verbose = FALSE,
                   ...) {

            pkgs <- c("leaflet", "raster", "magrittr")
            tst <- sapply(pkgs, "requireNamespace",
                          quietly = TRUE, USE.NAMES = FALSE)

            m <- initMap(map, map.types, proj4string(x))

            if (nlayers(x) == 1) {
              x <- raster(x, layer = 1)
              m <- mapView(x, map = m, map.types = map.types,
                           use.layer.names = TRUE, ...)
              out <- new('mapview', object = list(x), map = m@map)
            } else {
              m <- mapView(x[[1]], map = m, map.types = map.types,
                           use.layer.names = TRUE, ...)
              for (i in 2:nlayers(x)) {
                m <- mapView(x[[i]], map = m@map, map.types = map.types,
                             use.layer.names = TRUE, ...)
              }

              if (length(getLayerNamesFromMap(m@map)) > 1) {
                m <- leaflet::hideGroup(map = m@map,
                                        group = layers2bHidden(m@map))
              }
              out <- new('mapview', object = list(x), map = m)
            }

            return(out)

          }

)



## Satellite object =======================================================
#' @describeIn mapView \code{\link{satellite}}

setMethod('mapView', signature(x = 'Satellite'),
          function(x,
                   ...) {

            pkgs <- c("leaflet", "satellite", "magrittr")
            tst <- sapply(pkgs, "requireNamespace",
                          quietly = TRUE, USE.NAMES = FALSE)

            lyrs <- x@layers

            m <- mapView(lyrs[[1]], ...)

            if (length(lyrs) > 1) {
              for (i in 2:length(lyrs)) {
                m <- mapView(lyrs[[i]], m, ...)
              }
            }

            if (length(getLayerNamesFromMap(m)) > 1) {
              m <- leaflet::hideGroup(map = m, group = layers2bHidden(m))
            }

            out <- new('mapview', object = list(x), map = m)

            return(out)

          }

)


## SpatialPixelsDataFrame =================================================
#' @describeIn mapView \code{\link{SpatialPixelsDataFrame}}
#'
setMethod('mapView', signature(x = 'SpatialPixelsDataFrame'),
          function(x,
                   zcol = NULL,
                   ...) {

            pkgs <- c("leaflet", "sp", "magrittr")
            tst <- sapply(pkgs, "requireNamespace",
                          quietly = TRUE, USE.NAMES = FALSE)

            if(!is.null(zcol)) x <- x[, zcol]

            stck <- do.call("stack", lapply(seq(ncol(x)), function(i) {
              r <- raster::raster(x[, i])
              if (is.factor(x[, i])) r <- raster::as.factor(r)
              return(r)
            }))

            m <- mapView(stck, ...)

            out <- new('mapview', object = list(x), map = m@map)

            return(out)

          }

)


## SpatialPointsDataFrame =================================================
#' @describeIn mapView \code{\link{SpatialPointsDataFrame}}
#' @param burst whether to show all (TRUE) or only one (FALSE) layers
#' @param zcol attribute name(s) or column number(s) in attribute table
#' of the column(s) to be rendered
#' @param radius attribute name(s) or column number(s) in attribute table
#' of the column(s) to be used for defining the size of circles

setMethod('mapView', signature(x = 'SpatialPointsDataFrame'),
          function(x,
                   zcol = NULL,
                   map = NULL,
                   burst = FALSE,
                   color = mapViewPalette(7),
                   na.color = "transparent",
                   radius = 10,
                   map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   legend = TRUE,
                   legend.opacity = 1,
                   verbose = FALSE,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   popup = NULL,
                   ...) {

            pkgs <- c("leaflet", "sp", "magrittr")
            tst <- sapply(pkgs, "requireNamespace",
                          quietly = TRUE, USE.NAMES = FALSE)

            rad_vals <- circleRadius(x, radius)
            if(!is.null(zcol)) x <- x[, zcol]
            if(!is.null(zcol)) burst <- TRUE

            pop.null <- is.null(popup)

            x <- spCheckObject(x, verbose = verbose)
            x <- spCheckAdjustProjection(x)

            m <- initMap(map, map.types, proj4string(x))

            if (burst) {
              lst <- lapply(names(x), function(j) x[j])

              vals <- lapply(seq(lst), function(i) lst[[i]]@data[, 1])

              pal_n <- lapply(seq(lst), function(i) {
                if (is.factor(lst[[i]]@data[, 1])) {
                  leaflet::colorFactor(color, lst[[i]]@data[, 1],
                                       levels = levels(lst[[i]]@data[, 1]))
                } else {
                  leaflet::colorNumeric(color, vals[[i]],
                                        na.color = na.color)
                }
              })

              for (i in seq(lst)) {

                x <- lst[[i]]

                if (pop.null) popup <- createPopupTable(x)

                m <- leaflet::addCircleMarkers(m,
                                               lng = coordinates(x)[, 1],
                                               lat = coordinates(x)[, 2],
                                               group = names(lst[[i]]),
                                               color = pal_n[[i]](vals[[i]]),
                                               popup = popup,
                                               #data = x,
                                               radius = rad_vals,
                                               ...)

                if (legend) {
                  m <- leaflet::addLegend(map = m, position = "topright",
                                          pal = pal_n[[i]],
                                          opacity = 1, values = vals[[i]],
                                          title = names(lst[[i]]),
                                          layerId = names(lst[[i]]))
                }

                m <- mapViewLayersControl(map = m,
                                          map.types = map.types,
                                          names = names(lst[[i]]))

              }

              if (length(getLayerNamesFromMap(m)) > 1) {
                m <- leaflet::hideGroup(map = m, group = layers2bHidden(m))
              }

            } else {

              grp <- layer.name

              if (pop.null) popup <- createPopupTable(x)

              m <- leaflet::addCircleMarkers(map = m,
                                             lng = coordinates(x)[, 1],
                                             lat = coordinates(x)[, 2],
                                             group = grp,
                                             color = color[length(color)],
                                             popup = popup,
                                             #data = x,
                                             radius = rad_vals,
                                             ...)

              m <- mapViewLayersControl(map = m,
                                        map.types = map.types,
                                        names = grp)
            }

            out <- new('mapview', object = list(x), map = m)

            return(out)

          }

)



## SpatialPoints ==========================================================
#' @describeIn mapView \code{\link{SpatialPoints}}

setMethod('mapView', signature(x = 'SpatialPoints'),
          function(x,
                   map = NULL,
                   na.color = "transparent",
                   map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   verbose = FALSE,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   ...) {

            pkgs <- c("leaflet", "sp", "magrittr")
            tst <- sapply(pkgs, "requireNamespace",
                          quietly = TRUE, USE.NAMES = FALSE)

            x <- spCheckAdjustProjection(x)

            m <- initMap(map, map.types, proj4string(x))

            txt <- createPopupTable(x)

            grp <- layer.name

            m <- leaflet::addCircleMarkers(m, lng = coordinates(x)[, 1],
                                           lat = coordinates(x)[, 2],
                                           group = grp,
                                           popup = txt,
                                           ...)

            m <- mapViewLayersControl(map = m,
                                      map.types = map.types,
                                      names = grp)

            out <- new('mapview', object = list(x), map = m)

            return(out)

          }
)




## SpatialPolygonsDataFrame ===============================================
#' @describeIn mapView \code{\link{SpatialPolygonsDataFrame}}
#' @param weight line width (see \code{\link{leaflet}} for details)

setMethod('mapView', signature(x = 'SpatialPolygonsDataFrame'),
          function(x,
                   zcol = NULL,
                   map = NULL,
                   burst = FALSE,
                   color = mapViewPalette(7),
                   na.color = "transparent",
                   values = NULL,
                   map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   legend = TRUE,
                   legend.opacity = 1,
                   weight = 2,
                   verbose = FALSE,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   popup = NULL,
                   ...) {

            pkgs <- c("leaflet", "sp", "magrittr")
            tst <- sapply(pkgs, "requireNamespace",
                          quietly = TRUE, USE.NAMES = FALSE)

            x <- spCheckObject(x, verbose = verbose)

            if(!is.null(zcol)) x <- x[, zcol]
            if(!is.null(zcol)) burst <- TRUE

            pop.null <- is.null(popup)

            x <- spCheckAdjustProjection(x)

            m <- initMap(map, map.types, proj4string(x))

            if (burst) {

              lst <- lapply(names(x), function(j) x[j])

              df_all <- lapply(seq(lst), function(i) {
                dat <- data.frame(lst[[i]], stringsAsFactors = TRUE)
                if (is.character(dat[, 1])) {
                  dat[, 1] <- factor(dat[, 1], levels = unique(dat[, 1]))
                }
                return(dat)
              })

              vals <- lapply(seq(lst), function(i) df_all[[i]][, 1])

              pal_n <- lapply(seq(lst), function(i) {
                if (is.factor(df_all[[i]][, 1])) {
                  leaflet::colorFactor(color, vals[[i]],
                                       levels = levels(vals[[i]]),
                                       na.color = na.color)
                } else {
                  leaflet::colorNumeric(color, vals[[i]],
                                        na.color = na.color)
                }
              })

              for (i in seq(lst)) {

                x <- lst[[i]]

                df <- as.data.frame(sapply(x@data, as.character),
                                    stringsAsFactors = FALSE)

                grp <- names(df)

                if (pop.null) popup <- createPopupTable(x)

                clrs <- pal_n[[i]](vals[[i]])
                m <- leaflet::addPolygons(m,
                                          weight = weight,
                                          group = grp,
                                          color = clrs,
                                          popup = popup,
                                          data = x,
                                          ...)

                m <- leaflet::addLegend(map = m, position = "topright",
                                        pal = pal_n[[i]], opacity = 1,
                                        values = vals[[i]],
                                        title = grp)

                m <- mapViewLayersControl(map = m,
                                          map.types = map.types,
                                          names = grp)

              }

              if (length(getLayerNamesFromMap(m)) > 1) {
                m <- leaflet::hideGroup(map = m, group = layers2bHidden(m))
              }

            } else {

              df <- as.data.frame(sapply(x@data, as.character),
                                  stringsAsFactors = FALSE)

              grp <- layer.name

              if (pop.null) popup <- createPopupTable(x)

              m <- leaflet::addPolygons(m,
                                        weight = weight,
                                        group = grp,
                                        color = color[length(color)],
                                        popup = popup,
                                        data = x,
                                        ...)

              m <- mapViewLayersControl(map = m,
                                        map.types = map.types,
                                        names = grp)
            }

            out <- new('mapview', object = list(x), map = m)

            return(out)

          }

)



## SpatialPolygons ========================================================
#' @describeIn mapView \code{\link{SpatialPolygons}}

setMethod('mapView', signature(x = 'SpatialPolygons'),
          function(x,
                   map = NULL,
                   na.color = "transparent",
                   map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   weight = 2,
                   verbose = FALSE,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   ...) {

            pkgs <- c("leaflet", "sp", "magrittr")
            tst <- sapply(pkgs, "requireNamespace",
                          quietly = TRUE, USE.NAMES = FALSE)

            x <- spCheckAdjustProjection(x)

            m <- initMap(map, map.types, proj4string(x))

            grp <- layer.name

            m <- leaflet::addPolygons(m,
                                      weight = weight,
                                      group = grp,
                                      data = x,
                                      ...)

            m <- mapViewLayersControl(map = m,
                                      map.types = map.types,
                                      names = grp)

            out <- new('mapview', object = list(x), map = m)

            return(out)

          }
)


## SpatialLinesDataFrame =================================================
#' @describeIn mapView \code{\link{SpatialLinesDataFrame}}

setMethod('mapView', signature(x = 'SpatialLinesDataFrame'),
          function(x,
                   zcol = NULL,
                   map = NULL,
                   burst = FALSE,
                   color = mapViewPalette(7),
                   na.color = "transparent",
                   values = NULL,
                   map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   legend = TRUE,
                   legend.opacity = 1,
                   verbose = FALSE,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   popup = NULL,
                   ...) {

            pkgs <- c("leaflet", "sp", "magrittr")
            tst <- sapply(pkgs, "requireNamespace",
                          quietly = TRUE, USE.NAMES = FALSE)

            if(!is.null(zcol)) x <- x[, zcol]
            if(!is.null(zcol)) burst <- TRUE

            pop.null <- is.null(popup)

            x <- spCheckObject(x, verbose = verbose)
            x <- spCheckAdjustProjection(x)

            m <- initMap(map, map.types, proj4string(x))

            if (burst) {

              lst <- lapply(names(x), function(j) x[j])

              df_all <- lapply(seq(lst), function(i) {
                dat <- data.frame(lst[[i]], stringsAsFactors = TRUE)
                if (any(class(dat[, 1]) == "POSIXt")) {
                  dat[, 1] <- as.character(dat[, 1])
                }
                if (is.character(dat[, 1])) {
                  dat[, 1] <- factor(dat[, 1], levels = unique(dat[, 1]))
                }
                return(dat)
              })

              vals <- lapply(seq(lst), function(i) df_all[[i]][, 1])

              pal_n <- lapply(seq(lst), function(i) {
                if (is.factor(df_all[[i]][, 1])) {
                  leaflet::colorFactor(color, vals[[i]],
                                       levels = levels(vals[[i]]),
                                       na.color = na.color)
                } else {
                  leaflet::colorNumeric(color, vals[[i]],
                                        na.color = na.color)
                }
              })

              for (i in seq(lst)) {

                x <- lst[[i]]

                df <- as.data.frame(sapply(x@data, as.character),
                                    stringsAsFactors = FALSE)

                grp <- names(df)

                if (pop.null) popup <- createPopupTable(x)

                m <- leaflet::addPolylines(m,
                                           group = grp,
                                           color = pal_n[[i]](vals[[i]]),
                                           popup = popup,
                                           data = x,
                                           ...)

                m <- leaflet::addLegend(map = m, position = "topright",
                                        pal = pal_n[[i]], opacity = 1,
                                        values = vals[[i]], title = grp)

                m <- mapViewLayersControl(map = m,
                                          map.types = map.types,
                                          names = grp)

              }

              if (length(getLayerNamesFromMap(m)) > 1) {
                m <- leaflet::hideGroup(map = m, group = layers2bHidden(m))
              }

            } else {

              df <- as.data.frame(sapply(x@data, as.character),
                                  stringsAsFactors = FALSE)

              grp <- layer.name

              if (pop.null) popup <- createPopupTable(x)

              m <- leaflet::addPolylines(m,
                                         group = grp,
                                         color = color[length(color)],
                                         popup = popup,
                                         data = x,
                                         ...)

              m <- mapViewLayersControl(map = m,
                                        map.types = map.types,
                                        names = grp)
            }

            out <- new('mapview', object = list(x), map = m)

            return(out)

          }

)




## SpatialLines ===========================================================
#' @describeIn mapView \code{\link{SpatialLines}}

setMethod('mapView', signature(x = 'SpatialLines'),
          function(x,
                   map = NULL,
                   na.color = "transparent",
                   map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   verbose = FALSE,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   ...) {

            pkgs <- c("leaflet", "sp", "magrittr")
            tst <- sapply(pkgs, "requireNamespace",
                          quietly = TRUE, USE.NAMES = FALSE)

            llcrs <- CRS("+init=epsg:4326")@projargs

            x <- spCheckAdjustProjection(x)

            m <- initMap(map, map.types, proj4string(x))

            grp <- layer.name

            m <- leaflet::addPolylines(m,
                                       group = grp,
                                       data = x,
                                       ...)

            m <- mapViewLayersControl(map = m,
                                      map.types = map.types,
                                      names = grp)

            out <- new('mapview', object = list(x), map = m)

            return(out)

          }

)


## Missing ================================================================
#' @describeIn mapView initiate a map without an object
#'
#' @param easter.egg well, you might find out if you set this to TRUE
setMethod('mapView', signature(x = 'missing'),
          function(map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   easter.egg = FALSE) {

            if(easter.egg) {
              envinMR <- data.frame(x = 8.771676,
                                    y = 50.814891,
                                    envinMR = "envinMR")
              coordinates(envinMR) <- ~x+y
              proj4string(envinMR) <- sp::CRS(llcrs)
              m <- initBaseMaps(map.types)

              pop <- paste("<center>", "<b>", "mapview", "</b>", "<br>", " was created at",
                           "<br>",
                           '<a target="_blank" href="http://environmentalinformatics-marburg.de/">Environmental Informatics Marburg</a>',
                           "<br>", "by ", "<br>",
                           '<a target="_blank" href="http://umweltinformatik-marburg.de/en/staff/tim-appelhans/">Tim Appelhans</a>',
                           "<br>", "and is released under", "<br>",
                           strsplit(utils::packageDescription("mapview", fields = "License"), "\\|")[[1]][1],
                           "<br>", "<br>",
                           '<hr width=50% style="border: none; height: 1px; color: #D8D8D8; background: #D8D8D8;"/>',
                           "<br>",
                           "Please cite as: ", "<br>",
                           attr(unclass(utils::citation("mapview"))[[1]], "textVersion"),
                           "<br>", "<br>",
                           'A BibTeX entry for LaTeX users can be created with',
                           "<br>",
                           '<font face="courier">',
                           'citation("mapview")',
                           '</font face="courier">',
                           "</center>")
              m <- leaflet::addCircles(data = envinMR, map = m,
                                       fillColor = "white",
                                       color = "black",
                                       weight = 6,
                                       opacity = 0.8,
                                       fillOpacity = 0.5,
                                       group = "envinMR",
                                       popup = pop)
              m <- leaflet::addPopups(map = m,
                                      lng = 8.771676,
                                      lat = 50.814891,
                                      popup = pop)
              m <- mapViewLayersControl(map = m, map.types = map.types,
                                        names = "envinMR")
              m <- leaflet::setView(map = m, 8.771676, 50.814891, zoom = 18)
              out <- new('mapview', object = list(NULL), map = m)
            } else {
              m <- initBaseMaps(map.types)
              m <- leaflet::setView(map = m, 8.770862, 50.814772, zoom = 18)
              m <- leaflet::addLayersControl(map = m, baseGroups = map.types,
                                             position = "bottomleft")
              out <- new('mapview', object = list(NULL), map = m)
            }
            return(out)
          }
)
