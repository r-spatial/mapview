if ( !isGeneric('+') ) {
  setGeneric('+', function(x, y, ...)
    standardGeneric('+'))
}

#' mapview + mapview adds data from the second map to the first
#'
#' @param e1 a leaflet or mapview map to which e2 should be added.
#' @param e2 a (spatial) object to be added or a mapview object from which
#' the objects should be added to e1.
#'
#' @examples
#'   m1 <- mapView(franconia, col.regions = "red")
#'   m2 <- mapView(breweries)
#'
#'   ### add two mapview objects
#'   m1 + m2
#'
#'   ### add layers to a mapview object
#'   if (interactive()) {
#'     library(plainview)
#'     m1 + breweries + plainview::poppendorf[[4]]
#'   }
#'
#' @name ops
#' @docType methods
#' @rdname ops
#' @aliases +,mapview,mapview-method

setMethod("+",
          signature(e1 = "mapview",
                    e2 = "mapview"),
          function (e1, e2) {

            # Use the mapgl_plus function for mapgl platform
            if (mapviewGetOption("platform") == "mapgl") {
              return(mapgl_plus(e1, e2))
            }

            if (mapviewGetOption("platform") %in% c("leaflet", "leafgl")) {

              # if (length(
              #   getCallEntryFromMap(e1@map, "addProviderTiles")
              # ) == 0) {
                idx = getCallEntryFromMap(e2@map, "addProviderTiles")
                if (length(idx) > 0) {
                  e2@map$x$calls[idx] = NULL
                }
                idx = getCallEntryFromMap(e2@map, "addLayersControl")
                if (length(idx) > 0) {
                  e2@map$x$calls[idx][[1]]$args[[1]] = character(0)
                }
              # }



              m <- appendMapCallEntries_lf(e1@map, e2@map)
              # m = removeDuplicatedMapCalls(m)
              out_obj <- append(e1@object, e2@object)
              # avoids error if calling, for example, mapview() + viewExtent(in)
              out_obj <- out_obj[lengths(out_obj) != 0]

              crs_e1 = e1@map$x$options$crs$crsClass
              crs_e2 = e2@map$x$options$crs$crsClass

              crs = ifelse(
                !any(sapply(c(crs_e1, crs_e2), "==", "L.CRS.Simple"))
                , 4326
                , getProjection(e1@object[[1]])
              )

              bb = combineExtent(out_obj, sf = FALSE, crs)
              names(bb) = NULL
              m <- leaflet::fitBounds(map = m,
                                      lng1 = bb[1],
                                      lat1 = bb[2],
                                      lng2 = bb[3],
                                      lat2 = bb[4])

              hbcalls = getCallEntryFromMap(m, "addHomeButton")
              zf = grep("Zoom full", m$x$calls[hbcalls])
              ind = hbcalls[zf]
              if (length(zf) > 0) m$x$calls[ind] = NULL
              m = leafem:::addZoomFullButton(m, out_obj)

              hide_idx = getCallEntryFromMap(m, "hideGroup")
              if (length(hide_idx) > 0) {
                hide_lst = m$x$calls[hide_idx]
                m$x$calls[hide_idx] = NULL
                m$x$calls = append(m$x$calls, hide_lst)
              }
            }

            if (mapviewGetOption("platform") == "mapdeck") {
              m = appendMapCallEntries_md(e1@map, e2@map)
              out_obj <- append(e1@object, e2@object)
            }

            out <- methods::new('mapview', object = out_obj, map = m)
            return(out)
          }
)

#' mapview + data adds spatial data (raster*, sf*, sp*) to a mapview map
#'
#' @name ops
#' @docType methods
#' @rdname ops
#' @aliases +,mapview,ANY-method
#'
setMethod("+",
          signature(e1 = "mapview",
                    e2 = "ANY"),
          function (e1, e2) {

            nm <- deparse(substitute(e2))
            e1 + mapview(e2, layer.name = nm, update_view = TRUE)

            # nm <- deparse(substitute(e2))
            # e1@map = removeMouseCoordinates(e1@map)
            # # e1 + mapview(e2, layer.name = nm)
            # m = mapview(e2, map = e1, layer.name = nm)
            # print(str(m, 5))
            # out_obj = append(e1@object, m@object)
            #
            # hbcalls = getCallEntryFromMap(m@map, "addHomeButton")
            # zf = grep("Zoom full", m@map$x$calls[hbcalls])
            # ind = hbcalls[zf]
            # if (length(zf) > 0) m@map$x$calls[ind] = NULL
            #
            # m = addZoomFullButton(m@map, out_obj)
            # out = methods::new('mapview', object = out_obj, map = m)
            # return(out)
          }
)


#' mapview + NULL returns the LHS map
#'
#' @name ops
#' @docType methods
#' @rdname ops
#' @aliases +,mapview,NULL-method
#'
setMethod("+",
          signature(e1 = "mapview",
                    e2 = "NULL"),
          function (e1, e2) {
            return(e1)
          }
)


# #' @name +
# #' @docType methods
# #' @rdname plus
# #' @aliases +,leaflet,ANY-method
#'
# setMethod("+",
#           signature(e1 = "leaflet",
#                     e2 = "ANY"),
#           function (e1, e2)
#           {
#
#             nm <- deparse(substitute(e2))
#             m <- mapView(e2, map = e1, layer.name = nm,
#                          map.types = getProviderTileNamesFromMap(e1))
#             out_obj <- list(e2)
#             ext <- createExtent(e2)
#             m <- leaflet::fitBounds(map = m@map,
#                                     lng1 = ext@xmin,
#                                     lat1 = ext@ymin,
#                                     lng2 = ext@xmax,
#                                     lat2 = ext@ymax)
#             out <- methods::new('mapview', object = out_obj, map = m)
#             return(out)
#           }
# )

#' [...]
#' @name ops
#' @docType methods
#' @rdname ops
#' @aliases +,mapview,character-method
#'
setMethod("+",
          signature(e1 = "mapview",
                    e2 = "character"),
          function (e1, e2) {

            if (e2 %in% c("easteregg", "easter.egg", "easter_egg",
                          "easter", "easterEgg", "EasterEgg", "eegg",
                          "easter", "egg", "Easter", "Egg", "Nobody",
                          "Terence Hill", "trinity", "Trinity",
                          "easter egg", "Easter Egg", "Easter egg")) {
              cat("\nBehold! Someone's drawing quicker than the rest...\n\n")
              mapView(easter.egg = TRUE)
            } else {
              stop("\n\nSorry, but there seems to be someone who draws quicker than you...\n\n
                   Try again!")
            }

          }
)


#' mapview | mapview provides a slider in the middle to compare two maps.
#'
#' @param e1 a leaflet or mapview map, or NULL.
#' @param e2 a leaflet or mapview map, or NULL.
#'
#' @examples
#'   m1 <- mapView(franconia, col.regions = "red")
#'   m2 <- mapView(breweries)
#'
#'   ### add two mapview objects
#'   m1 | m2
#'
#' @name ops
#' @docType methods
#' @rdname ops
#' @aliases |,mapview,mapview-method

if ( !isGeneric('|') ) {
  setGeneric('|', function(x, y, ...)
    standardGeneric('|'))
}

setMethod("|",
          signature(e1 = "mapview",
                    e2 = "mapview"),
          function (e1, e2) {

            if (mapviewGetOption("platform") %in% c("leaflet")) {

              if (!requireNamespace("leaflet.extras2")) {
                stop(
                  "\nPackage 'leaflet.extras2' is needed for the '|' operator to work.\n"
                  , "Install with install.packages('leaflet.extras2')"
                  , call. = FALSE
                )
              }

              e1_tile_idx = getCallEntryFromMap(e1@map, "addProviderTiles")
              if (length(e1_tile_idx) > 1) {
                e1@map$x$calls[2:length(e1_tile_idx)] = NULL
              }
              e1_tile_idx = e1_tile_idx[1]
              e1_tile_pane_idx = grep("pane", e1@map$x$calls[[e1_tile_idx]]$args)
              e1@map$x$calls[[e1_tile_idx]]$args[[2]] = "left"
              e1@map$x$calls[[e1_tile_idx]]$args[[e1_tile_pane_idx]][[4]] = "left"

              e1_lyrctrl_idx = getCallEntryFromMap(e1@map, "addLayersControl")
              # bsgrps = e1@map$x$calls[[e1_lyrctrl_idx]]$args[[1]][1]
              # e1@map$x$calls[[e1_lyrctrl_idx]]$args[[1]] = bsgrps
              e1_ovrlygrps = e1@map$x$calls[[e1_lyrctrl_idx]]$args[[2]]
              e1@map$x$calls[[e1_lyrctrl_idx]] = NULL

              e1_pane_idx = getCallEntryFromMap(e1@map, "createMapPane")
              e1_feat_idx = getCallEntryFromMap(
                e1@map
                , c(
                  "addPolygons"
                  , "addPolylines"
                  , "addCircleMarkers"
                  , "addFlatGeoBuf"
                  , "addGeotiff"
                )
              )
              for (i in seq_along(e1_feat_idx)) {
                idx = grep("pane", e1@map$x$calls[[e1_feat_idx[i]]]$args)
                e1@map$x$calls[[e1_feat_idx[i]]]$args[[idx]]$pane = "left"
              }

              for (i in e1_pane_idx) {
                e1@map$x$calls[[i]]$args[[1]] = "left"
              }

              e1_pane_tmp = e1@map$x$calls[e1_pane_idx]

              e1@map$x$calls = append(
                e1_pane_tmp
                , e1@map$x$calls
              )

              e1_deps = e1@map$dependencies



              ## e2 - right
              e2_tile_idx = getCallEntryFromMap(e2@map, "addProviderTiles")
              if (length(e2_tile_idx) > 1) {
                e2@map$x$calls[2:length(e2_tile_idx)] = NULL
              }
              e2_tile_idx = e2_tile_idx[1]
              e2_tile_pane_idx = grep("pane", e2@map$x$calls[[e2_tile_idx]]$args)
              e2@map$x$calls[[e2_tile_idx]]$args[[2]] = "right"
              e2@map$x$calls[[e2_tile_idx]]$args[[e2_tile_pane_idx]][[4]] = "right"

              e2_lyrctrl_idx = getCallEntryFromMap(e2@map, "addLayersControl")
              # bsgrps = e2@map$x$calls[[e2_lyrctrl_idx]]$args[[1]][1]
              # e2@map$x$calls[[e2_lyrctrl_idx]]$args[[1]] = bsgrps
              e2_ovrlygrps = e2@map$x$calls[[e2_lyrctrl_idx]]$args[[2]]
              e2@map$x$calls[[e2_lyrctrl_idx]] = NULL

              e2_pane_idx = getCallEntryFromMap(e2@map, "createMapPane")
              e2_feat_idx = getCallEntryFromMap(
                e2@map
                , c(
                  "addPolygons"
                  , "addPolylines"
                  , "addCircleMarkers"
                  , "addFlatGeoBuf"
                  , "addGeotiff"
                )
              )
              for (i in seq_along(e2_feat_idx)) {
                idx = grep("pane", e2@map$x$calls[[e2_feat_idx[i]]]$args)
                e2@map$x$calls[[e2_feat_idx[i]]]$args[[idx]]$pane = "right"
              }

              for (i in e2_pane_idx) {
                e2@map$x$calls[[i]]$args[[1]] = "right"
              }

              e2_pane_tmp = e2@map$x$calls[e2_pane_idx]

              e2@map$x$calls = append(
                e2_pane_tmp
                , e2@map$x$calls
              )

              e2_deps = e2@map$dependencies



              # map - left + right
              m = e1

              m@map$x$calls = append(
                m@map$x$calls
                , e2@map$x$calls
              )

              m_pane_idx = getCallEntryFromMap(m@map, "createMapPane")
              m_pane_tmp = m@map$x$calls[m_pane_idx]
              m@map$x$calls[m_pane_idx] = NULL

              m@map$x$calls = append(
                m_pane_tmp
                , m@map$x$calls
              )

              ### make sure all dependencies are present
              m_deps = append(e1_deps, e2_deps)
              m_deps = m_deps[!duplicated(m_deps)]
              m@map$dependencies = m_deps

              m@map = leaflet::addLayersControl(
                m@map
                , overlayGroups = c(
                  e1_ovrlygrps
                  , e2_ovrlygrps
                )
                , position = "topleft"
              )

              m = leaflet.extras2::addSidebyside(
                m@map
                , layerId = "mvSideBySide"
                , rightId = "right"
                , leftId = "left"
              )

              out_obj = append(
                e1@object
                , e2@object
              )

            }

            if (mapviewGetOption("platform") %in% c("mapdeck", "leafgl")) {
              stop(
                "'|' currently only implemented for leaflet maps"
                , call. = FALSE
              )
            }

            out = methods::new('mapview', object = out_obj, map = m)
            return(out)
          }
)

#' mapview | NULL returns the LHS map
#'
#' @name ops
#' @docType methods
#' @rdname ops
#' @aliases |,mapview,NULL-method
#'
setMethod("|",
          signature(e1 = "mapview",
                    e2 = "NULL"),
          function (e1, e2) {
            return(e1)
          }
)

#' NULL | mapview returns the RHS map
#'
#' @name ops
#' @docType methods
#' @rdname ops
#' @aliases |,NULL,mapview-method
#'
setMethod("|",
          signature(e1 = "NULL",
                    e2 = "mapview"),
          function (e1, e2) {
            return(e2)
          }
)
