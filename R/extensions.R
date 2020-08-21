# ### addMouseCoordinates ######################################################
# ##############################################################################
# #' Add mouse coordinate information at top of map.
# #'
# #' @description
# #' These functions are deprecated.
# #' Please use leafem::\code{\link[leafem]{addMouseCoordinates}}
# #' and leafem::\code{\link[leafem]{removeMouseCoordinates}} instead.
# #'
# #' @param map a mapview or leaflet object.
# #' @param epsg the epsg string to be shown.
# #' @param proj4string the proj4string to be shown.
# #' @param native.crs logical. whether to use the native crs in the coordinates box.
# #'
# #' @export addMouseCoordinates
# #' @name addMouseCoordinates
# #' @rdname addMouseCoordinates
# #' @aliases addMouseCoordinates
#
# addMouseCoordinates <- function(map,
#                                 epsg = NULL,
#                                 proj4string = NULL,
#                                 native.crs = FALSE) {
#
#   .Defunct(new = "leafem::addMouseCoordinates", package = "mapview")
#
# }
#
# #' Remove mouse coordinates information at top of map.
# #'
# #' @describeIn addMouseCoordinates remove mouse coordinates information from a map
# #' @aliases removeMouseCoordinates
# #' @export removeMouseCoordinates
# removeMouseCoordinates = function(map) {
#   .Defunct(new = "leafem::addMouseCoordinates", package = "mapview")
# }
#
# ##############################################################################
#
#
# ### addHomeButton ############################################################
# ##############################################################################
# #' Add a home button / zoom-to-layer button to a map.
# #'
# #' @description
# #' These functions are deprecated.
# #' Please use leafem::\code{\link[leafem]{addHomeButton}}
# #' and leafem::\code{\link[leafem]{removeHomeButton}} instead.
# #'
# #' @param map a mapview or leaflet object.
# #' @param ext the \code{\link{extent}} / \code{\link{bbox}} to zoom to.
# #' @param layer.name the name of the layer to be zoomed to (or any character
# #' string)
# #' @param position the position of the button (one of 'topleft', 'topright',
# #' 'bottomleft', 'bottomright'). Defaults to 'bottomright'.
# #' @param add logical. Whether to add the button to the map (mainly for internal use).
# #'
# #' @export addHomeButton
# #' @name addHomeButton
# #' @rdname addHomeButton
# #' @aliases addHomeButton
# addHomeButton <- function(map, ext, layer.name = "layer",
#                           position = 'bottomright', add = TRUE) {
#   .Defunct(new = "leafem::addHomeButton", package = "mapview")
# }
#
#
# #' Use removeHomeButton to remove home button
# #'
# #' @describeIn addHomeButton remove a homeButton from a map
# #' @aliases removeHomeButton
# #' @export removeHomeButton
# removeHomeButton <- function(map) {
#   .Defunct(new = "leafem::removeHomeButton", package = "mapview")
# }
#
#
# ### addLogo ##################################################################
# ##############################################################################
# #' add a local or remote image (png, jpg, gif, bmp, ...) to a leaflet map
# #'
# #' @description
# #' This function is deprecated.
# #' Please use leafem::\code{\link[leafem]{addLogo}} instead.
# #'
# #' @param map a mapview or leaflet object.
# #' @param img the image to be added to the map.
# #' @param alpha opacity of the added image.
# #' @param src character specifying the source location ("local" for images from
# #' the disk, "remote" for web image sources).
# #' @param url an optional URL to be opened when clicking on the image
# #' (e.g. company's homepage).
# #' @param position one of "topleft", "topright", "bottomleft", "bottomright".
# #' @param offset.x the offset in x direction from the chosen position (in pixels).
# #' @param offset.y the offset in y direction from the chosen position (in pixels).
# #' @param width width of the rendered image in pixels.
# #' @param height height of the rendered image in pixels.
# #'
# #' @export addLogo
# #' @name addLogo
# #' @rdname addLogo
# #' @aliases addLogo
#
# ## courtesy of
# ## http://gis.stackexchange.com/questions/203265/add-logo-to-a-map-using-leaflet-mapbox
# ## http://jsfiddle.net/3v7hd2vx/76/
#
# addLogo <- function(map,
#                     img,
#                     alpha = 1,
#                     src = c("remote", "local"),
#                     url,
#                     position = c("topleft", "topright",
#                                  "bottomleft", "bottomright"),
#                     offset.x = 50,
#                     offset.y = 13,
#                     width = 60,
#                     height = 60) {
#
#   .Defunct(new = "leafem::addLogo", package = "mapview")
#
# }
#
#
#
# ##############################################################################
#
#
# ### addFeatures ##############################################################
# ##############################################################################
# #' Type agnositc version of \code{leaflet::add*} functions.
# #'
# #' @description
# #' This function is deprecated.
# #' Please use leafem::\code{\link[leafem]{addFeatures}} instead.
# #'
# #' @param map A \code{leaflet} or \code{mapview} map.
# #' @param data A \code{sf} object to be added to the \code{map}.
# #' @param pane The name of the map pane for the features to be rendered in.
# #' @param ... Further arguments passed to the respective \code{leaflet::add*}
# #' functions. See \code{\link{addCircleMarkers}}, \code{\link{addPolylines}}
# #' and \code{\link{addPolygons}}.
# #'
# #' @export addFeatures
# #' @name addFeatures
# #' @rdname addFeatures
# addFeatures <- function(map,
#                         data,
#                         pane = "overlayPane",
#                         ...) {
#
#   .Defunct(new = "leafem::addFeatures", package = "mapview")
#
# }
#
#
#
# ### addImageQuery ############################################################
# ##############################################################################
# #' Add image query functionality to leaflet/mapview map.
# #'
# #' @details
# #' This function is deprecated.
# #' Please use leafem::\code{\link[leafem]{addImageQuery}} instead.
# #'
# #' @param map the map with the RasterLayer to be queried.
# #' @param x the RasterLayer that is to be queried.
# #' @param band for stars layers, the band number to be queried.
# #' @param group the group of the RasterLayer to be queried.
# #' @param layerId the layerId of the RasterLayer to be queried. Needs to be the
# #'   same as supplied in \code{\link[leaflet]{addRasterImage}} or
# #'   \code{link{addStrasImage}}.
# #' @param project whether to project the RasterLayer to conform with leaflets
# #'   expected crs. Defaults to \code{TRUE} and things are likely to go haywire
# #'   if set to \code{FALSE}.
# #' @param type whether query should occur on 'mousemove' or 'click'. Defaults
# #'   to 'mousemove'.
# #' @param digits the number of digits to be shown in the display field.
# #' @param position where to place the display field. Default is 'topright'.
# #' @param prefix a character string to be shown as prefix for the layerId.
# #' @param ... currently not used.
# #'
# #' @export addImageQuery
# #' @name addImageQuery
# #' @rdname addImageQuery
# addImageQuery = function(map,
#                          x,
#                          band = 1,
#                          group = NULL,
#                          layerId = NULL,
#                          project = TRUE,
#                          type = c("mousemove", "click"),
#                          digits,
#                          position = 'topright',
#                          prefix = 'Layer',
#                          ...) {
#   .Defunct(new = "leafem::addImageQuery", package = "mapview")
# }
#
# ##############################################################################
#
#
#
# ### addStaticLabels ##########################################################
# ##############################################################################
# #' Add static labels to \code{leaflet} or \code{mapview} objects
# #' @details
# #' This function is deprecated.
# #' Please use leafem::\code{\link[leafem]{addStaticLabels}} instead.
# #'
# #' @description
# #' Being a wrapper around \code{\link[leaflet]{addLabelOnlyMarkers}}, this
# #' function provides a smart-and-easy solution to add custom text labels to an
# #' existing \code{leaflet} or \code{mapview} map object.
# #'
# #' @param map A \code{leaflet} or \code{mapview} object.
# #' @param data A \code{sf} or \code{Spatial*} object used for label placement,
# #' defaults to the locations of the first dataset in 'map'.
# #' @param label The labels to be placed at the positions indicated by 'data' as
# #' \code{character}, or any vector that can be coerced to this type.
# #' @param group the group of the static labels layer.
# #' @param layerId the layerId of the static labels layer.
# #' @param ... Additional arguments passed to
# #' \code{\link[leaflet]{labelOptions}}.
# #'
# #' @return
# #' A labelled \strong{mapview} object.
# #'
# #' @author
# #' Florian Detsch
# #'
# #' @seealso
# #' \code{\link[leafem]{addStaticLabels}}, \code{\link[leaflet]{addLabelOnlyMarkers}}.
# #'
# #' @examples
# #' \dontrun{
# #' ## leaflet label display options
# #' library(leaflet)
# #'
# #' lopt = labelOptions(noHide = TRUE,
# #'                     direction = 'top',
# #'                     textOnly = TRUE)
# #'
# #' ## point labels
# #' m1 = mapview(breweries)
# #' l1 = addStaticLabels(m1,
# #'                      label = breweries$number.of.types,
# #'                      labelOptions = lopt)
# #' l1
# #'
# #' ## polygon centroid labels
# #' m2 = mapview(franconia)
# #' l2 = addStaticLabels(m2,
# #'                      label = franconia$NAME_ASCI,
# #'                      labelOptions = lopt)
# #' l2
# #'
# #' ## custom labels
# #' m3 = m2 + m1
# #' l3 = addStaticLabels(m3,
# #'                      data = franconia,
# #'                      label = franconia$NAME_ASCI,
# #'                      labelOptions = lopt)
# #' l3
# #' }
# #'
# #' @export addStaticLabels
# #' @name addStaticLabels
# addStaticLabels = function(map,
#                            data,
#                            label,
#                            group = NULL,
#                            layerId = NULL,
#                            ...) {
#   .Defunct(new = "leafem::addStaticLabels", package = "mapview")
# }
#
