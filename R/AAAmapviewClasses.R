#' Class mapview
#'
#' @slot object the spatial object
#' @slot map the leaflet map object
#'
#' @exportClass mapview

setClass('mapview',
         slots = c(object = 'list',
                   map = 'ANY'))
NULL

setOldClass("leaflet")
setOldClass(c("XY", #"XYZ", "XYM", "XYZM",
              #"sf", "sfc", "sfg",
              "POINT", "MULTIPOINT",
              "POLYGON", "MULTIPOLYGON",
              "LINESTRING", "MULTILINESTRING",
              "GEOMETRYCOLLECTION"))
