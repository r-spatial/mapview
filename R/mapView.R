if ( !isGeneric('mapView') ) {
  setGeneric('mapView', function(x, ...)
    standardGeneric('mapView'))
}

#' view raster layers interactively
#' 
#' @description 
#' this function produces an interactive GIS-like view of the specified 
#' raster layers on top of the specified base maps.
#' 
#' @param x a \code{\link{raster}}* object
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
#' @param ... additional arguments passed on to \code{\link{addLegend}} 
#' 
#' @author
#' Tim Appelhans
#' 
#' @examples
#' ### raster data ###
#' data(meuse.grid)
#' coordinates(meuse.grid) = ~x+y
#' proj4string(meuse.grid) <- CRS("+init=epsg:28992")
#' gridded(meuse.grid) = TRUE
#' meuse_rst <- stack(meuse.grid)
#' 
#' m1 <- mapView(meuse_rst)
#' m1
#' 
#' m2 <- mapView(meuse_rst[[1]])
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
#' m5 <- addLayer(meuse, m2, burst = TRUE)
#' m5
#' 
#' 
#' 
#' ### polygon vector data ###
#' data("DEU_adm2")
#' m <- mapView(gadm, burst = FALSE)
#' m
#' 
#' ## points on polygons ##
#' centres <- data.frame(coordinates(gadm))
#' names(centres) <- c("x", "y")
#' coordinates(centres) <- ~ x + y
#' projection(centres) <- projection(gadm)
#' addLayer(centres, m)
#' 
#' 
#' 
#' ### lines vector data
#' data("atlStorms2005")
#' mapView(atlStorms2005, burst = FALSE)
#' mapView(atlStorms2005, burst = TRUE)
#' 
#' @export mapView
#' @name mapView
#' @rdname mapView
#' @aliases mapView


## RasterLayer ============================================================
setMethod('mapView', signature(x = 'RasterLayer'), 
          function(x,
                   map = NULL,
                   cols = envinmrPalette(7), 
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
            
            #llcrs <- CRS("+init=epsg:3857")@projargs
            llcrs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
            
            if (!identical(projection(x), llcrs)) {
              if(verbose) cat("\n", "reprojecting to web mercator", "\n\n")
            }
            
            ## create base map using specified map types
            if (is.null(map)) {
              m <- leaflet::leaflet()
              m <- leaflet::addTiles(m, group = map.types[1])
              m <- leaflet::addProviderTiles(m, provider = map.types[2],
                                             group = map.types[2])
            } else {
              m <- map
            }
            
            if (trim) x <- trim(x)
            
            if (is.fact) x <- raster::as.factor(x)
            
            if (is.null(values)) {
              if (is.fact) {
                values <- x@data@attributes[[1]]$ID
              } else {
                offset <- diff(range(x[], na.rm = TRUE)) * 0.05
                top <- ceiling(max(x[], na.rm = TRUE)) + offset
                bot <- floor(min(x[], na.rm = TRUE)) - offset
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
                                         project = TRUE,
                                         opacity = layer.opacity,
                                         group = names(x),
                                         ...)
            
            if (legend) {
              ## add legend
              m <- leaflet::addLegend(map = m,
                                      pal = pal,
                                      opacity = legend.opacity, 
                                      values = values, 
                                      title = names(x),
                                      ...)
            }
            
            ## add layer control buttons
            if (is.null(map)) {
              m <- leaflet::addLayersControl(map = m,
                                             position = "topleft",
                                             baseGroups = map.types,
                                             overlayGroups = names(x))
            } else {
              m <- leaflet::addLayersControl(map = m,
                                             position = "topleft",
                                             baseGroups = map.types,
                                             overlayGroups = 
                                               c(Rsenal:::getLayerNamesFromMap(m), 
                                                 names(x)))
            }
            
            return(m)
            
          }
          
)

## Raster Stack ===========================================================
#' @describeIn mapView

setMethod('mapView', signature(x = 'RasterStack'), 
          function(x, 
                   map = NULL,
                   cols = envinmrPalette(7), 
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
            
            ## create base map using specified map types
            if (is.null(map)) {
              m <- leaflet::leaflet()
              m <- leaflet::addTiles(m, group = map.types[1])
              m <- leaflet::addProviderTiles(m, provider = map.types[2],
                                             group = map.types[2])
            } else {
              m <- map
            }
            
            if (nlayers(x) == 1) {
              m <- mapView(x[[1]], m, ...)
            } else {
              m <- mapView(x[[1]], m, ...)
              for (i in 2:nlayers(x)) {
                m <- mapView(x[[i]], m, ...)
              }
              
              m <- leaflet::hideGroup(map = m, group = Rsenal:::layers2bHidden(m))
              
            }       
            
            return(m)
            
          }
          
)


## Raster Brick ===========================================================
#' @describeIn mapView

setMethod('mapView', signature(x = 'RasterBrick'), 
          function(x,
                   map = NULL,
                   cols = envinmrPalette(7), 
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
            
            ## create base map using specified map types
            if (is.null(map)) {
              m <- leaflet::leaflet()
              m <- leaflet::addTiles(m, group = map.types[1])
              m <- leaflet::addProviderTiles(m, provider = map.types[2],
                                             group = map.types[2])
            } else {
              m <- map
            }
            
            if (nlayers(x) == 1) {
              m <- mapView(x[[1]], m, ...)
            } else {
              m <- mapView(x[[1]], m, ...)
              for (i in 2:nlayers(x)) {
                m <- mapView(x[[i]], m, ...)
              }
              
              m <- leaflet::hideGroup(map = m, group = Rsenal:::layers2bHidden(m))
              
            }
            
            return(m)
            
          }
          
)



## Satellite object =======================================================

#' @describeIn mapView

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
            
            m <- leaflet::hideGroup(map = m, group = Rsenal:::layers2bHidden(m))
            
            return(m)
            
          }
          
)




## SpatialPointsDataFrame =================================================
#' @describeIn mapView
#' @param burst whether to show all (TRUE) or only one (FALSE) layers 

setMethod('mapView', signature(x = 'SpatialPointsDataFrame'), 
          function(x,
                   map = NULL,
                   burst = FALSE,
                   cols = envinmrPalette(7), 
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
            
            llcrs <- CRS("+init=epsg:4326")@projargs
            
            if (!identical(projection(x), llcrs)) {
              if(verbose) cat("\n", "reprojecting to web mercator", "\n\n")
              x <- spTransform(x, CRSobj = llcrs)
            }
            
            if (is.null(map)) {
              m <- leaflet::leaflet()
              m <- leaflet::addTiles(m, group = map.types[1])
              m <- leaflet::addProviderTiles(m, provider = map.types[2],
                                             group = map.types[2])
            } else {
              m <- map
            }
            
            if (burst) {
              lst <- lapply(names(x), function(j) x[j])
              
              vals <- lapply(seq(lst), function(i) lst[[i]]@data[, 1])
              
              pal_n <- lapply(seq(lst), function(i) {
                if (is.factor(lst[[i]]@data[, 1])) {
                  leaflet::colorFactor(cols, lst[[i]]@data[, 1], 
                                       levels = levels(lst[[i]]@data[, 1]))
                } else {
                  leaflet::colorNumeric(cols, vals[[i]], 
                                        na.color = "transparent")
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
                                               ...)
                
                
                m <- leaflet::addMarkers(m, lng = coordinates(lst[[i]])[, 1],
                                         lat = coordinates(lst[[i]])[, 2],
                                         group = names(lst[[i]]),
                                         options = leaflet::markerOptions(opacity = 0),
                                         popup = txt)
                
                m <- leaflet::addLegend(map = m, position = "topright", 
                                        pal = pal_n[[i]],
                                        opacity = 1, values = vals[[i]], 
                                        title = names(lst[[i]]),
                                        layerId = names(lst[[i]]))
                
                if (i == 1) {
                  m <- leaflet::addLayersControl(map = m,
                                                 position = "topleft",
                                                 baseGroups = c("OpenStreetMap",
                                                                "Esri.WorldImagery"),
                                                 overlayGroups = c(
                                                   Rsenal:::getLayerNamesFromMap(m),
                                                   names(lst[[i]])))
                } else {
                  m <- leaflet::addLayersControl(map = m,
                                                 position = "topleft",
                                                 baseGroups = c("OpenStreetMap",
                                                                "Esri.WorldImagery"),
                                                 overlayGroups = c(
                                                   Rsenal:::getLayerNamesFromMap(m),
                                                   names(lst[[i]])))
                }
              }
              
              m <- leaflet::hideGroup(map = m, group = Rsenal:::layers2bHidden(m))
              
            } else {
              
              df <- as.data.frame(sapply(x@data, as.character),
                                  stringsAsFactors = FALSE)
              
              nms <- names(df)
              
              nam <- sys.call(-1)
              grp <- as.character(nam)[2]
              
              len <- length(m$x$calls)
              
              m <- leaflet::addCircleMarkers(m, lng = coordinates(x)[, 1],
                                             lat = coordinates(x)[, 2],
                                             group = grp,
                                             color = cols[length(cols)],
                                             ...)
              
              txt_x <- paste0("x: ", round(coordinates(x)[, 1], 2))
              txt_y <- paste0("y: ", round(coordinates(x)[, 2], 2))
              
              txt <- rbind(sapply(seq(nrow(x@data)), function(i) {
                paste(nms, df[i, ], sep = ": ")
              }), txt_x, txt_y)
              
              txt <- sapply(seq(ncol(txt)), function(j) {
                paste(txt[, j], collapse = " <br/> ")
              })
              
              m <- leaflet::addMarkers(m, lng = coordinates(x)[, 1],
                                       lat = coordinates(x)[, 2],
                                       group = grp,
                                       options = leaflet::markerOptions(opacity = 0),
                                       popup = txt)
              
              m <- leaflet::addLayersControl(map = m,
                                             position = "topleft",
                                             baseGroups = c("OpenStreetMap",
                                                            "Esri.WorldImagery"),
                                             overlayGroups = c(
                                               Rsenal:::getLayerNamesFromMap(m),
                                               grp))
            }
            
            return(m)
            
          }
          
)



## SpatialPoints ==========================================================
#' @describeIn mapView

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
            
            llcrs <- CRS("+init=epsg:4326")@projargs
            
            if (!identical(projection(x), llcrs)) {
              if(verbose) cat("\n", "reprojecting to web mercator", "\n\n")
              x <- spTransform(x, CRSobj = llcrs)
            }
            
            if (is.null(map)) {
              m <- leaflet::leaflet()
              m <- leaflet::addTiles(m, group = map.types[1])
              m <- leaflet::addProviderTiles(m, provider = map.types[2],
                                             group = map.types[2])
            } else {
              m <- map
            }
            
            txt_x <- paste0("x: ", round(coordinates(x)[, 1], 2))
            txt_y <- paste0("y: ", round(coordinates(x)[, 2], 2))
            
            txt <- sapply(seq(txt_x), function(j) {
              paste(txt_x[j], txt_y[j], sep = "<br/>")
            })
            
            nam <- sys.call(-1)
            grp <- as.character(nam)[2]
            
            m <- leaflet::addCircleMarkers(m, lng = coordinates(x)[, 1],
                                           lat = coordinates(x)[, 2],
                                           group = grp,
                                           ...)
            
            
            m <- leaflet::addMarkers(m, lng = coordinates(x)[, 1],
                                     lat = coordinates(x)[, 2],
                                     group = grp,
                                     options = leaflet::markerOptions(opacity = 0),
                                     popup = txt)
            
            m <- leaflet::addLayersControl(map = m,
                                           position = "topleft",
                                           baseGroups = c("OpenStreetMap",
                                                          "Esri.WorldImagery"),
                                           overlayGroups = c(
                                             Rsenal:::getLayerNamesFromMap(m),
                                             grp))
            
            return(m)
            
          }
)




## SpatialPolygonsDataFrame ===============================================
#' @describeIn mapView

setMethod('mapView', signature(x = 'SpatialPolygonsDataFrame'), 
          function(x,
                   map = NULL,
                   burst = FALSE,
                   cols = envinmrPalette(7), 
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
            
            llcrs <- CRS("+init=epsg:4326")@projargs
            
            if (!identical(projection(x), llcrs)) {
              if(verbose) cat("\n", "reprojecting to web mercator", "\n\n")
              x <- spTransform(x, CRSobj = llcrs)
            }
            
            if (is.null(map)) {
              m <- leaflet::leaflet()
              m <- leaflet::addTiles(m, group = map.types[1])
              m <- leaflet::addProviderTiles(m, provider = map.types[2],
                                             group = map.types[2])
            } else {
              m <- map
            }
            
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
                                       na.color = "transparent")
                } else {
                  leaflet::colorNumeric(cols, vals[[i]], 
                                        na.color = "transparent")
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
                
                coord_lst <- lapply(slot(x, "polygons"), function(x) {
                  lapply(slot(x, "Polygons"), function(y) slot(y, "coords"))
                })
                
                for (j in seq(coord_lst)) {
                  for (h in seq(coord_lst[[j]])) {
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
                
                m <- leaflet::addLayersControl(map = m,
                                               position = "topleft",
                                               baseGroups = c("OpenStreetMap",
                                                              "Esri.WorldImagery"),
                                               overlayGroups = c(
                                                 Rsenal:::getLayerNamesFromMap(m),
                                                 grp))
                
              }
              
              m <- leaflet::hideGroup(map = m, group = Rsenal:::layers2bHidden(m))
              
            } else {
              
              df <- as.data.frame(sapply(x@data, as.character),
                                  stringsAsFactors = FALSE)
              
              nms <- names(df)
              
              nam <- sys.call(-1)
              grp <- as.character(nam)[2]
              
              txt <- sapply(seq(nrow(x@data)), function(i) {
                paste(nms, df[i, ], sep = ": ")
              })
              
              txt <- sapply(seq(ncol(txt)), function(j) {
                paste(txt[, j], collapse = " <br> ")
              })
              
              len <- length(m$x$calls)
              
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
                                            color = cols[length(cols)],
                                            popup = txt[j],
                                            ...)
                }
              }
              
              m <- leaflet::addLayersControl(map = m,
                                             position = "topleft",
                                             baseGroups = c("OpenStreetMap",
                                                            "Esri.WorldImagery"),
                                             overlayGroups = c(
                                               Rsenal:::getLayerNamesFromMap(m),
                                               grp))
            }
            
            return(m)
            
          }
          
)



## SpatialPolygons ========================================================
#' @describeIn mapView

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
            
            llcrs <- CRS("+init=epsg:4326")@projargs
            
            if (!identical(projection(x), llcrs)) {
              if(verbose) cat("\n", "reprojecting to web mercator", "\n\n")
              x <- spTransform(x, CRSobj = llcrs)
            }
            
            if (is.null(map)) {
              m <- leaflet::leaflet()
              m <- leaflet::addTiles(m, group = map.types[1])
              m <- leaflet::addProviderTiles(m, provider = map.types[2],
                                             group = map.types[2])
            } else {
              m <- map
            }
            
            nam <- sys.call(-1)
            grp <- as.character(nam)[2]
            
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
            
            m <- leaflet::addLayersControl(map = m,
                                           position = "topleft",
                                           baseGroups = c("OpenStreetMap",
                                                          "Esri.WorldImagery"),
                                           overlayGroups = c(
                                             Rsenal:::getLayerNamesFromMap(m),
                                             grp))
          
            return(m)
            
          }
)


## SpatialLinesDataFrame =================================================
#' @describeIn mapView

setMethod('mapView', signature(x = 'SpatialLinesDataFrame'), 
          function(x,
                   map = NULL,
                   burst = FALSE,
                   cols = envinmrPalette(7), 
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
            
            llcrs <- CRS("+init=epsg:4326")@projargs
            
            if (!identical(projection(x), llcrs)) {
              if(verbose) cat("\n", "reprojecting to web mercator", "\n\n")
              x <- spTransform(x, CRSobj = llcrs)
            }
            
            if (is.null(map)) {
              m <- leaflet::leaflet()
              m <- leaflet::addTiles(m, group = map.types[1])
              m <- leaflet::addProviderTiles(m, provider = map.types[2],
                                             group = map.types[2])
            } else {
              m <- map
            }
            
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
                                       na.color = "transparent")
                } else {
                  leaflet::colorNumeric(cols, vals[[i]], 
                                        na.color = "transparent")
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
                
                m <- leaflet::addLayersControl(map = m,
                                               position = "topleft",
                                               baseGroups = c("OpenStreetMap",
                                                              "Esri.WorldImagery"),
                                               overlayGroups = c(
                                                 Rsenal:::getLayerNamesFromMap(m),
                                                 grp))
                
              }
              
              m <- leaflet::hideGroup(map = m, group = Rsenal:::layers2bHidden(m))
              
            } else {
              
              df <- as.data.frame(sapply(x@data, as.character),
                                  stringsAsFactors = FALSE)
              
              nms <- names(df)
              
              nam <- sys.call(-1)
              grp <- as.character(nam)[2]
              
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
              
              m <- leaflet::addLayersControl(map = m,
                                             position = "topleft",
                                             baseGroups = c("OpenStreetMap",
                                                            "Esri.WorldImagery"),
                                             overlayGroups = c(
                                               Rsenal:::getLayerNamesFromMap(m),
                                               grp))
            }
            
            return(m)
            
          }
          
)




## SpatialLines ===========================================================
#' @describeIn mapView

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
            
            if (!identical(projection(x), llcrs)) {
              if(verbose) cat("\n", "reprojecting to web mercator", "\n\n")
              x <- spTransform(x, CRSobj = llcrs)
            }
            
            if (is.null(map)) {
              m <- leaflet::leaflet()
              m <- leaflet::addTiles(m, group = map.types[1])
              m <- leaflet::addProviderTiles(m, provider = map.types[2],
                                             group = map.types[2])
            } else {
              m <- map
            }
            
            nam <- sys.call(-1)
            grp <- as.character(nam)[2]
            
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
            
            m <- leaflet::addLayersControl(map = m,
                                           position = "topleft",
                                           baseGroups = c("OpenStreetMap",
                                                          "Esri.WorldImagery"),
                                           overlayGroups = c(
                                             Rsenal:::getLayerNamesFromMap(m),
                                             grp))
            
            return(m)
            
          }
          
)




## leaflet ================================================================
#' @describeIn mapView
 
setMethod('mapView', signature(x = 'leaflet'), 
          function(x, y) {
            Rsenal::addLayer(y, map = x)
          }
)

