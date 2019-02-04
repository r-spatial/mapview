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

setOldClass(c("POINT", "MULTIPOINT",
              "POLYGON", "MULTIPOLYGON",
              "LINESTRING", "MULTILINESTRING"))

setOldClass("XY")
setOldClass("sf")
setOldClass("sfc")
setOldClass("sfg")
setOldClass("XYM")
setOldClass("XYZM")
setOldClass("bbox")

setOldClass("stars")
