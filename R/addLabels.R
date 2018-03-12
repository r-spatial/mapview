#' Add labels to \code{mapview} object
#'
#' @description
#' Being a wrapper around \code{\link[leaflet]{addLabelOnlyMarkers}}, this
#' function provides a smart-and-easy solution to add custom text labels to an
#' existing \code{mapview} object.
#'
#' @param map A \code{mapview} object.
#' @param data A \code{sf} or \code{Spatial*} object used for label placement,
#' defaults to the locations of the first dataset in 'map'.
#' @param label The labels to be placed at the positions indicated by 'data' as
#' \code{character}, or any vector that can be coerced to this type.
#' @param ... Additional arguments passed to
#' \code{\link[leaflet]{addLabelOnlyMarkers}}.
#'
#' @return
#' A labelled \strong{mapview} object.
#'
#' @author
#' Florian Detsch
#'
#' @seealso
#' \code{\link[leaflet]{addLabelOnlyMarkers}}.
#'
#' @examples
#' \dontrun{
#' ## leaflet label display options
#' library(leaflet)
#'
#' lopt = labelOptions(noHide = TRUE
#'                     , direction = 'top'
#'                     , textOnly = TRUE)
#'
#' ## point labels
#' m1 = mapview(breweries)
#' l1 = addLabels(m1
#'                , label = breweries$number.of.types
#'                , labelOptions = lopt)
#'
#' ## polygon centroid labels
#' m2 = mapview(franconia)
#' l2 = addLabels(m2
#'                , label = franconia$NAME_ASCI
#'                , labelOptions = lopt)
#'
#' ## custom labels
#' m3 = m1 + m2
#' l3 = addLabels(m3
#'                , data = franconia
#'                , label = franconia$NAME_ASCI
#'                , labelOptions = lopt)
#' }
#'
#' @export addLabels
#' @name addLabels
addLabels = function(map
                     , data = map@object[[1]]
                     , label
                     , ...) {

  ## 'Raster*' locations not supported so far -> error
  if (inherits(data, "Raster")) {
    stop(paste("'Raster*' input is not supported, yet."
               , "Please refer to ?addLabels for compatible input formats.\n"))
  }

  ## if input is 'Spatial*', convert to 'sf'
  if (inherits(data, "Spatial")) {
    data = sf::st_as_sf(data)
  }

  ## if input is not points, calculate centroid coordinates
  is_pts = regexpr("point"
                   , getSFClass(sf::st_geometry(data))
                   , ignore.case = TRUE) > -1

  is_sfc = inherits(data, "sfc")

  fun = if (is_sfc) length else nrow
  end = do.call(fun, list(data))

  cnt = if (!is_pts) {
    do.call(rbind, lapply(1:end, function(i) {
      ext = sf::st_bbox(if (inherits(data, "sfc")) data[i] else data[i, ])
      data.frame(lng = mean(ext[c(1, 3)]), lat = mean(ext[c(2, 4)]))
    }))
  } else {
    mat = sf::st_coordinates(data)
    data.frame(lng = mat[, 1], lat = mat[, 2])
  }

  ## add labels to map
  map@map = map@map %>% leaflet::addLabelOnlyMarkers(data = cnt
                                                     , label = as.character(label)
                                                     , ...)

  return(map)
}
