#' Garnish/decorate leaflet or mapview maps.
#'
#' @description
#' This function provides a versatile interface to add components to a
#' leaflet or mapview map. It takes functions such as "addMouseCoordinates"
#' or \code{\link{addLayersControl}} and their repsective arguments and adds
#' them to the map. Arguments must be named. Functions can be plain or
#' charatcer strings.
#'
#' @param map a mapview or leaflet object.
#' @param ... functions and their arguments to add things to a map.
#'
#' @examples
#' \dontrun{
#' m <- leaflet() %>% addTiles()
#' garnishMap(m, "addMouseCoordinates") # same as
#' garnishMap(m, addMouseCoordinates)
#'
#' ## add more than one with named argument
#' library(raster)
#' m1 <- garnishMap(m, addMouseCoordinates, mapview:::addHomeButton,
#'                  ext = extent(breweries91))
#' m1
#'
#' ## even more flexible
#' m2 <- garnishMap(m1, addCircleMarkers, data = breweries91)
#' garnishMap(m2, addPolygons, data = gadmCHE, popup = popupTable(gadmCHE),
#'            fillOpacity = 0.8, color = "black", fillColor = "#BEBEBE")
#'
#' }
#'
#' @export garnishMap
#' @name garnishMap
#' @rdname garnishMap
#' @aliases garnishMap
garnishMap <- function(map, ...) {

  if (inherits(map, "mapview")) map <- mapview2leaflet(map)
  stopifnot(inherits(map, "leaflet"))

  ls <- list(...)

  funs <- sapply(ls, is.function)

  fn_lst <- lapply(ls, function(i) {
    tst <- try(match.fun(i), silent = TRUE)
    if (class(tst) == "try-error") tst <- NULL
    return(tst)
  })
  fn_lst <- fn_lst[!sapply(fn_lst, is.null)]

  args <- !funs

  arg_lst <- ls[args]
  nms <- names(arg_lst)[names(arg_lst) != ""]

  arg_nms <- lapply(fn_lst, function(i) {
    ma <- match.arg(c("map", nms), names(as.list(args(i))),
                    several.ok = TRUE)
    ma[!ma %in% "map"]
  })

  for (i in seq(fn_lst)) {
    vec <- arg_nms[[i]]
    map <- do.call(fn_lst[[i]], append(list(map), arg_lst[vec]))
  }
  return(map)
}
