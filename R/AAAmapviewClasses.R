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
