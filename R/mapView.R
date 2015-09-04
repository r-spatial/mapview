if ( !isGeneric('mapView') ) {
  setGeneric('mapView', function(x, ...)
    standardGeneric('mapView'))
}

#' view spatial objects interactively
#'
#' @description
#' this function produces an interactive GIS-like view of the specified
#' spatial object(s) on top of the specified base maps.
#'
#' @param x a \code{\link{raster}}* object
#' @param map an optional existing map to be updated/added to
#' @param cols color palette for the layers
#' @param na.color color for missing values
#' @param values a vector of values for the visualisation of the layers.
#' Per default these are calculated based on the supplied raster* object.
#' @param map.types character spcifications for the base maps.
#' see \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' for available options.
#' @param layer.opacity opacity of the layers
#' @param legend should a legend be plotted
#' @param legend.opacity opacity of the legend
#' @param trim should the raster be trimmed in case there are NAs on the egdes
#' @param verbose should some details be printed during the process
#' @param ... additional arguments passed on to repective functions.
#' See \code{\link{addRasterImage}}, \code{\link{addCircles}},
#' \code{\link{addPolygons}}, \code{\link{addPolylines}} for details
#'
#' @author
#' Tim Appelhans
#'
#' @examples
#' \dontrun{
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
#' m1 <- mapView(meuse_rst)
#' m1
#'
#' # factorial RasterLayer
#' m2 <- mapView(raster::as.factor(meuse_rst[[4]]))
#' m2
#'
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
#' mapView(meuse, burst = FALSE)
#'
#' ## SpatialPoints ##
#' meuse_pts <- as(meuse, "SpatialPoints")
#' mapView(meuse_pts)
#'
#'
#'
#' ### overlay vector on top of raster ###
#' m3 <- mapView(meuse, map = m2)
#' m3
#'
#' m4 <- mapView(meuse, map = m2, burst = TRUE)
#' m4 # is the same as
#' m5 <- addMapLayer(meuse, m2, burst = TRUE)
#' m5
#'
#'
#'
#' ### polygon vector data ###
#' data("DEU_admin2")
#' m <- mapView(DEU_admin2, burst = FALSE)
#' m
#'
#' ## points on polygons ##
#' centres <- data.frame(coordinates(DEU_admin2))
#' names(centres) <- c("x", "y")
#' coordinates(centres) <- ~ x + y
#' projection(centres) <- projection(DEU_admin2)
#' addMapLayer(centres, m)
#'
#'
#'
#' ### lines vector data
#' data("atlStorms2005")
#' mapView(atlStorms2005, burst = FALSE)
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
                   cols = mapViewPalette(7),
                   na.color = "transparent",
                   values = NULL,
                   map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   layer.opacity = 0.8,
                   legend = TRUE,
                   legend.opacity = 1,
                   trim = TRUE,
                   verbose = FALSE,
                   ...) {

            pkgs <- c("leaflet", "raster", "magrittr")
            tst <- sapply(pkgs, "requireNamespace",
                          quietly = TRUE, USE.NAMES = FALSE)

            is.fact <- raster::is.factor(x)

            if (!identical(projection(x), leaflet:::epsg3857)) {
              if(verbose) cat("\n", "reprojecting to web mercator", "\n\n")
              x <- projectRasterForMapView(x)
            }

            m <- initMap(map, map.types, proj4string(x))

            if (trim) x <- trim(x)

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
              pal <- leaflet::colorFactor(cols,
                                          domain = NULL,
                                          na.color = na.color)
            } else {
              pal <- leaflet::colorNumeric(cols,
                                           domain = values,
                                           na.color = na.color)
            }

            ## add layers to base map
            m <- leaflet::addRasterImage(map = m,
                                         x = x,
                                         colors = pal,
                                         project = FALSE,
                                         opacity = layer.opacity,
                                         group = names(x),
                                         ...)

            if (legend) {
              ## add legend
              m <- leaflet::addLegend(map = m,
                                      pal = pal,
                                      opacity = legend.opacity,
                                      values = values,
                                      title = names(x))
            }

            ## add layer control buttons
            #if (is.null(map)) {
              m <- mapViewLayersControl(map = m,
                                        map.types = map.types,
                                        names = names(x))
#               m <- leaflet::addLayersControl(map = m,
#                                              position = "bottomleft",
#                                              baseGroups = map.types,
#                                              overlayGroups = names(x))
#             } else {
#               m <- leaflet::addLayersControl(map = m,
#                                              position = "bottomleft",
#                                              baseGroups = map.types,
#                                              overlayGroups =
#                                                c(getLayerNamesFromMap(m),
#                                                  names(x)))
#             }

            out <- new('mapview', object = x, map = m)

            return(out)

          }

)

## Raster Stack ===========================================================
#' @describeIn mapView \code{\link{stack}}

setMethod('mapView', signature(x = 'RasterStack'),
          function(x,
                   map = NULL,
                   cols = mapViewPalette(7),
                   na.color = "transparent",
                   values = NULL,
                   map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   layer.opacity = 0.8,
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
              m <- mapView(x[[1]], map = m, map.types = map.types, ...)
            } else {
              m <- mapView(x[[1]], map = m, map.types = map.types, ...)
              for (i in 2:nlayers(x)) {
                m <- mapView(x[[i]], map = m@map, map.types = map.types, ...)
              }

              if (length(getLayerNamesFromMap(m@map)) > 1) {
                m <- leaflet::hideGroup(map = m@map,
                                        group = layers2bHidden(m@map))
              }

            }

            out <- new('mapview', object = x, map = m)

            return(out)

          }

)


## Raster Brick ===========================================================
#' @describeIn mapView \code{\link{brick}}

setMethod('mapView', signature(x = 'RasterBrick'),
          function(x,
                   map = NULL,
                   cols = mapViewPalette(7),
                   na.color = "transparent",
                   values = NULL,
                   map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   layer.opacity = 0.8,
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
              m <- mapView(x[[1]], map = m, map.types = map.types, ...)
            } else {
              m <- mapView(x[[1]], map = m, map.types = map.types, ...)
              for (i in 2:nlayers(x)) {
                m <- mapView(x[[i]], map = m@map, map.types = map.types, ...)
              }

              if (length(getLayerNamesFromMap(m@map)) > 1) {
                m <- leaflet::hideGroup(map = m@map,
                                        group = layers2bHidden(m@map))
              }

            }

            out <- new('mapview', object = x, map = m)

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

            out <- new('mapview', object = x, map = m)

            return(out)

          }

)




## SpatialPointsDataFrame =================================================
#' @describeIn mapView \code{\link{SpatialPointsDataFrame}}
#' @param burst whether to show all (TRUE) or only one (FALSE) layers
#' @param zcol attribute name(s) or column number(s) in attribute table
#' of the column(s) to be rendered

setMethod('mapView', signature(x = 'SpatialPointsDataFrame'),
          function(x,
                   zcol = NULL,
                   map = NULL,
                   burst = FALSE,
                   cols = mapViewPalette(7),
                   na.color = "transparent",
                   values = NULL,
                   map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   layer.opacity = 0.8,
                   legend = TRUE,
                   legend.opacity = 1,
                   verbose = FALSE,
                   ...) {

            pkgs <- c("leaflet", "sp", "magrittr")
            tst <- sapply(pkgs, "requireNamespace",
                          quietly = TRUE, USE.NAMES = FALSE)

            if(!is.null(zcol)) x <- x[, zcol]
            if(!is.null(zcol)) burst <- TRUE

            x <- spCheckAdjustProjection(x, verbose)
            if (is.na(proj4string(x))) {
              slot(x, "coords") <- scaleCoordinates(coordinates(x)[, 1],
                                                    coordinates(x)[, 2])
            }

            m <- initMap(map, map.types, proj4string(x))

            if (burst) {
              lst <- lapply(names(x), function(j) x[j])

              vals <- lapply(seq(lst), function(i) lst[[i]]@data[, 1])

              pal_n <- lapply(seq(lst), function(i) {
                if (is.factor(lst[[i]]@data[, 1])) {
                  leaflet::colorFactor(cols, lst[[i]]@data[, 1],
                                       levels = levels(lst[[i]]@data[, 1]))
                } else {
                  leaflet::colorNumeric(cols, vals[[i]],
                                        na.color = na.color)
                }
              })

              for (i in seq(lst)) {
                pop <- paste(names(lst[[i]]),
                             as.character(vals[[i]]),
                             sep = ": ")

                txt_x <- paste0("x: ", round(coordinates(lst[[i]])[, 1], 2))
                txt_y <- paste0("y: ", round(coordinates(lst[[i]])[, 2], 2))

                txt <- sapply(seq(pop), function(j) {
                  paste(pop[j], txt_x[j], txt_y[j], sep = "<br/>")
                })

                m <- leaflet::addCircleMarkers(m, lng = coordinates(lst[[i]])[, 1],
                                               lat = coordinates(lst[[i]])[, 2],
                                               group = names(lst[[i]]),
                                               color = pal_n[[i]](vals[[i]]),
                                               popup = txt,
                                               ...)

                m <- leaflet::addLegend(map = m, position = "topright",
                                        pal = pal_n[[i]],
                                        opacity = 1, values = vals[[i]],
                                        title = names(lst[[i]]),
                                        layerId = names(lst[[i]]))

                m <- mapViewLayersControl(map = m,
                                          map.types = map.types,
                                          names = names(lst[[i]]))
#                 m <- leaflet::addLayersControl(map = m,
#                                                position = "bottomleft",
#                                                baseGroups = map.types,
#                                                overlayGroups = c(
#                                                  getLayerNamesFromMap(m),
#                                                  names(lst[[i]])))

              }

              if (length(getLayerNamesFromMap(m)) > 1) {
                m <- leaflet::hideGroup(map = m, group = layers2bHidden(m))
              }

            } else {

              df <- as.data.frame(sapply(x@data, as.character),
                                  stringsAsFactors = FALSE)

              nms <- names(df)
              grp <- layerName()

              txt_x <- paste0("x: ", round(coordinates(x)[, 1], 2))
              txt_y <- paste0("y: ", round(coordinates(x)[, 2], 2))

              txt <- rbind(sapply(seq(nrow(x@data)), function(i) {
                paste(nms, df[i, ], sep = ": ")
              }), txt_x, txt_y)

              txt <- sapply(seq(ncol(txt)), function(j) {
                paste(txt[, j], collapse = " <br/> ")
              })

              m <- leaflet::addCircleMarkers(m, lng = coordinates(x)[, 1],
                                             lat = coordinates(x)[, 2],
                                             group = grp,
                                             color = cols[length(cols)],
                                             popup = txt,
                                             ...)

              m <- mapViewLayersControl(map = m,
                                        map.types = map.types,
                                        names = grp)
#               m <- leaflet::addLayersControl(map = m,
#                                              position = "bottomleft",
#                                              baseGroups = map.types,
#                                              overlayGroups = c(
#                                                getLayerNamesFromMap(m),
#                                                grp))
            }

            out <- new('mapview', object = x, map = m)

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
                   layer.opacity = 0.8,
                   verbose = FALSE,
                   ...) {

            pkgs <- c("leaflet", "sp", "magrittr")
            tst <- sapply(pkgs, "requireNamespace",
                          quietly = TRUE, USE.NAMES = FALSE)

            x <- spCheckAdjustProjection(x, verbose)

            m <- initMap(map, map.types, proj4string(x))

            txt_x <- paste0("x: ", round(coordinates(x)[, 1], 2))
            txt_y <- paste0("y: ", round(coordinates(x)[, 2], 2))

            txt <- sapply(seq(txt_x), function(j) {
              paste(txt_x[j], txt_y[j], sep = "<br/>")
            })

            grp <- layerName()

            m <- leaflet::addCircleMarkers(m, lng = coordinates(x)[, 1],
                                           lat = coordinates(x)[, 2],
                                           group = grp,
                                           popup = txt,
                                           ...)

            m <- mapViewLayersControl(map = m,
                                      map.types = map.types,
                                      names = grp)
#             m <- leaflet::addLayersControl(map = m,
#                                            position = "bottomleft",
#                                            baseGroups = map.types,
#                                            overlayGroups = c(
#                                              getLayerNamesFromMap(m),
#                                              grp))

            out <- new('mapview', object = x, map = m)

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
                   cols = mapViewPalette(7),
                   na.color = "transparent",
                   values = NULL,
                   map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   layer.opacity = 0.8,
                   legend = TRUE,
                   legend.opacity = 1,
                   weight = 2,
                   verbose = FALSE,
                   ...) {

            pkgs <- c("leaflet", "sp", "magrittr")
            tst <- sapply(pkgs, "requireNamespace",
                          quietly = TRUE, USE.NAMES = FALSE)

            if(!is.null(zcol)) x <- x[, zcol]
            if(!is.null(zcol)) burst <- TRUE

            x <- spCheckAdjustProjection(x, verbose)

            m <- initMap(map, map.types, proj4string(x))

            coord_lst <- lapply(slot(x, "polygons"), function(x) {
              lapply(slot(x, "Polygons"), function(y) slot(y, "coords"))
            })

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
                  leaflet::colorFactor(cols, vals[[i]],
                                       levels = levels(vals[[i]]),
                                       na.color = na.color)
                } else {
                  leaflet::colorNumeric(cols, vals[[i]],
                                        na.color = na.color)
                }
              })

              for (i in seq(lst)) {

                x <- lst[[i]]

                df <- as.data.frame(sapply(x@data, as.character),
                                    stringsAsFactors = FALSE)

                nms <- names(df)
                grp <- nms

                txt <- sapply(seq(nrow(x@data)), function(i) {
                  paste(nms, df[i, ], sep = ": ")
                })

                len <- length(m$x$calls)

                for (j in seq(coord_lst)) {
                  for (h in seq(coord_lst[[j]])) {
                    if (is.na(proj4string(x))) {
                      x <- scalePolygonsCoordinates(x)
                    }
                    x_coord <- coordinates(x@polygons[[j]]@Polygons[[h]])[, 1]
                    y_coord <- coordinates(x@polygons[[j]]@Polygons[[h]])[, 2]
                    clrs <- pal_n[[i]](vals[[i]])
                    m <- leaflet::addPolygons(m,
                                              lng = x_coord,
                                              lat = y_coord,
                                              weight = weight,
                                              group = grp,
                                              color = clrs[j],
                                              popup = txt[j],
                                              ...)
                  }
                }

                m <- leaflet::addLegend(map = m, position = "topright",
                                        pal = pal_n[[i]], opacity = 1,
                                        values = vals[[i]],
                                        title = grp)

                m <- mapViewLayersControl(map = m,
                                          map.types = map.types,
                                          names = grp)
#                 m <- leaflet::addLayersControl(map = m,
#                                                position = "bottomleft",
#                                                baseGroups = map.types,
#                                                overlayGroups = c(
#                                                  getLayerNamesFromMap(m),
#                                                  grp))

              }

              if (length(getLayerNamesFromMap(m)) > 1) {
                m <- leaflet::hideGroup(map = m, group = layers2bHidden(m))
              }

            } else {

              df <- as.data.frame(sapply(x@data, as.character),
                                  stringsAsFactors = FALSE)

              nms <- names(df)

              grp <- layerName()

              txt <- as.matrix(sapply(seq(nrow(x@data)), function(i) {
                paste(nms, df[i, ], sep = ": ")
              }))

              if (length(zcol) == 1) txt <- t(txt)

              txt <- sapply(seq(ncol(txt)), function(j) {
                paste(txt[, j], collapse = " <br> ")
              })

              len <- length(m$x$calls)

#               coord_lst <- lapply(slot(x, "polygons"), function(x) {
#                 lapply(slot(x, "Polygons"), function(y) slot(y, "coords"))
#               })

              for (j in seq(coord_lst)) {
                for (h in seq(coord_lst[[j]])) {
                  if (is.na(proj4string(x))) {
                    x <- scalePolygonsCoordinates(x)
                  }
                  x_coord <- coordinates(x@polygons[[j]]@Polygons[[h]])[, 1]
                  y_coord <- coordinates(x@polygons[[j]]@Polygons[[h]])[, 2]
                  m <- leaflet::addPolygons(m,
                                            lng = x_coord,
                                            lat = y_coord,
                                            weight = weight,
                                            group = grp,
                                            color = cols[length(cols)],
                                            popup = txt[j],
                                            ...)
                }
              }

              m <- mapViewLayersControl(map = m,
                                        map.types = map.types,
                                        names = grp)
#               m <- leaflet::addLayersControl(map = m,
#                                              position = "bottomleft",
#                                              baseGroups = map.types,
#                                              overlayGroups = c(
#                                                getLayerNamesFromMap(m),
#                                                grp))
            }

            out <- new('mapview', object = x, map = m)

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
                   layer.opacity = 0.8,
                   weight = 2,
                   verbose = FALSE,
                   ...) {

            pkgs <- c("leaflet", "sp", "magrittr")
            tst <- sapply(pkgs, "requireNamespace",
                          quietly = TRUE, USE.NAMES = FALSE)

            x <- spCheckAdjustProjection(x, verbose)

            m <- initMap(map, map.types, proj4string(x))

            grp <- layerName()

            coord_lst <- lapply(slot(x, "polygons"), function(x) {
              lapply(slot(x, "Polygons"), function(y) slot(y, "coords"))
            })

            for (j in seq(coord_lst)) {
              for (h in seq(coord_lst[[j]])) {
                x_coord <- coordinates(x@polygons[[j]]@Polygons[[h]])[, 1]
                y_coord <- coordinates(x@polygons[[j]]@Polygons[[h]])[, 2]
                m <- leaflet::addPolygons(m,
                                          lng = x_coord,
                                          lat = y_coord,
                                          weight = weight,
                                          group = grp,
                                          ...)
              }
            }

            m <- mapViewLayersControl(map = m,
                                      map.types = map.types,
                                      names = grp)
#             m <- leaflet::addLayersControl(map = m,
#                                            position = "bottomleft",
#                                            baseGroups = map.types,
#                                            overlayGroups = c(
#                                              getLayerNamesFromMap(m),
#                                              grp))

            out <- new('mapview', object = x, map = m)

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
                   cols = mapViewPalette(7),
                   na.color = "transparent",
                   values = NULL,
                   map.types = c("OpenStreetMap",
                                 "Esri.WorldImagery"),
                   layer.opacity = 0.8,
                   legend = TRUE,
                   legend.opacity = 1,
                   weight = 2,
                   verbose = FALSE,
                   ...) {

            pkgs <- c("leaflet", "sp", "magrittr")
            tst <- sapply(pkgs, "requireNamespace",
                          quietly = TRUE, USE.NAMES = FALSE)

            if(!is.null(zcol)) x <- x[, zcol]
            if(!is.null(zcol)) burst <- TRUE

            x <- spCheckAdjustProjection(x, verbose)

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
                  leaflet::colorFactor(cols, vals[[i]],
                                       levels = levels(vals[[i]]),
                                       na.color = na.color)
                } else {
                  leaflet::colorNumeric(cols, vals[[i]],
                                        na.color = na.color)
                }
              })

              for (i in seq(lst)) {

                x <- lst[[i]]

                df <- as.data.frame(sapply(x@data, as.character),
                                    stringsAsFactors = FALSE)

                nms <- names(df)
                grp <- nms

                txt <- sapply(seq(nrow(x@data)), function(i) {
                  paste(nms, df[i, ], sep = ": ")
                })

                len <- length(m$x$calls)

                coord_lst <- lapply(slot(x, "lines"), function(x) {
                  lapply(slot(x, "Lines"), function(y) slot(y, "coords"))
                })

                for (j in seq(coord_lst)) {
                  for (h in seq(coord_lst[[j]])) {
                    if (is.na(proj4string(x))) {
                      x <- scaleLinesCoordinates(x)
                    }
                    x_coord <- coordinates(x@lines[[j]]@Lines[[h]])[, 1]
                    y_coord <- coordinates(x@lines[[j]]@Lines[[h]])[, 2]
                    clrs <- pal_n[[i]](vals[[i]])
                    m <- leaflet::addPolylines(m,
                                               lng = x_coord,
                                               lat = y_coord,
                                               weight = weight,
                                               group = grp,
                                               color = clrs[j],
                                               popup = txt[j],
                                               ...)
                  }
                }

                m <- leaflet::addLegend(map = m, position = "topright",
                                        pal = pal_n[[i]], opacity = 1,
                                        values = vals[[i]], title = grp)

                m <- mapViewLayersControl(map = m,
                                          map.types = map.types,
                                          names = grp)
#                 m <- leaflet::addLayersControl(map = m,
#                                                position = "bottomleft",
#                                                baseGroups = map.types,
#                                                overlayGroups = c(
#                                                  getLayerNamesFromMap(m),
#                                                  grp))

              }

              if (length(getLayerNamesFromMap(m)) > 1) {
                m <- leaflet::hideGroup(map = m, group = layers2bHidden(m))
              }

            } else {

              df <- as.data.frame(sapply(x@data, as.character),
                                  stringsAsFactors = FALSE)

              nms <- names(df)

              grp <- layerName()

              txt <- sapply(seq(nrow(x@data)), function(i) {
                paste(nms, df[i, ], sep = ": ")
              })

              txt <- sapply(seq(ncol(txt)), function(j) {
                paste(txt[, j], collapse = " <br> ")
              })

              len <- length(m$x$calls)

              coord_lst <- lapply(slot(x, "lines"), function(x) {
                lapply(slot(x, "Lines"), function(y) slot(y, "coords"))
              })

              for (j in seq(coord_lst)) {
                for (h in seq(coord_lst[[j]])) {
                  if (is.na(proj4string(x))) {
                    x <- scaleLinesCoordinates(x)
                  }
                  x_coord <- coordinates(x@lines[[j]]@Lines[[h]])[, 1]
                  y_coord <- coordinates(x@lines[[j]]@Lines[[h]])[, 2]
                  m <- leaflet::addPolylines(m,
                                             lng = x_coord,
                                             lat = y_coord,
                                             weight = weight,
                                             group = grp,
                                             color = cols[length(cols)],
                                             popup = txt[j],
                                             ...)
                }
              }

              m <- mapViewLayersControl(map = m,
                                        map.types = map.types,
                                        names = grp)

#               m <- leaflet::addLayersControl(map = m,
#                                              position = "bottomleft",
#                                              baseGroups = map.types,
#                                              overlayGroups = c(
#                                                getLayerNamesFromMap(m),
#                                                grp))
            }

            out <- new('mapview', object = x, map = m)

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
                   layer.opacity = 0.8,
                   weight = 2,
                   verbose = FALSE,
                   ...) {

            pkgs <- c("leaflet", "sp", "magrittr")
            tst <- sapply(pkgs, "requireNamespace",
                          quietly = TRUE, USE.NAMES = FALSE)

            llcrs <- CRS("+init=epsg:4326")@projargs

            x <- spCheckAdjustProjection(x, verbose)

            m <- initMap(map, map.types, proj4string(x))

            grp <- layerName()

            coord_lst <- lapply(slot(x, "lines"), function(x) {
              lapply(slot(x, "Lines"), function(y) slot(y, "coords"))
            })

            for (j in seq(coord_lst)) {
              for (h in seq(coord_lst[[j]])) {
                x_coord <- coordinates(x@lines[[j]]@Lines[[h]])[, 1]
                y_coord <- coordinates(x@lines[[j]]@Lines[[h]])[, 2]
                m <- leaflet::addPolylines(m,
                                           lng = x_coord,
                                           lat = y_coord,
                                           weight = weight,
                                           group = grp,
                                           ...)
              }
            }

            m <- mapViewLayersControl(map = m,
                                      map.types = map.types,
                                      names = grp)
#             m <- leaflet::addLayersControl(map = m,
#                                            position = "bottomleft",
#                                            baseGroups = map.types,
#                                            overlayGroups = c(
#                                              getLayerNamesFromMap(m),
#                                              grp))

            out <- new('mapview', object = x, map = m)

            return(out)

          }

)

